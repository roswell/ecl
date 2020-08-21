/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * threads.h - wrapper for mutex and condition variable operating
 *             system primitives
 *
 * Copyright (c) 2020, Marius Gerbershagen
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#ifdef ECL_THREADS

#ifndef ECL_MUTEX_H
#define ECL_MUTEX_H

#include <errno.h>
#ifdef ECL_WINDOWS_THREADS
# include <windows.h>
# include <synchapi.h>
#else
# include <pthread.h>
#endif
#include <time.h>
#ifdef HAVE_GETTIMEOFDAY
# include <sys/time.h>
#endif
#include <math.h>

#if !defined(ECL_WINDOWS_THREADS)

#define ECL_MUTEX_SUCCESS 0
#define ECL_MUTEX_LOCKED EBUSY
#define ECL_MUTEX_NOT_OWNED EPERM
#define ECL_MUTEX_TIMEOUT ETIMEDOUT
#define ECL_MUTEX_DEADLOCK EDEADLK

/* MUTEX */

/* Non-recursive locks are only provided as an optimization; on
 * Windows locks are always recursive. If ECL_MUTEX_DEADLOCK is
 * undefined, recursive locks are not available. */

static inline void
ecl_mutex_init(ecl_mutex_t *mutex, bool recursive)
{
  pthread_mutexattr_t mutexattr[1];
  pthread_mutexattr_init(mutexattr);
  if (recursive) {
    pthread_mutexattr_settype(mutexattr, PTHREAD_MUTEX_RECURSIVE);
  } else {
    pthread_mutexattr_settype(mutexattr, PTHREAD_MUTEX_ERRORCHECK);
  }
  pthread_mutex_init(mutex, mutexattr);
}

static inline void
ecl_mutex_destroy(ecl_mutex_t *mutex)
{
  pthread_mutex_destroy(mutex);
}

static inline int
ecl_mutex_unlock(ecl_mutex_t *mutex)
{
  return pthread_mutex_unlock(mutex);
}

static inline int
ecl_mutex_trylock(ecl_mutex_t *mutex)
{
  return pthread_mutex_trylock(mutex);
}

static inline int
ecl_mutex_lock(ecl_mutex_t *mutex)
{
  return pthread_mutex_lock(mutex);
}

/* CONDITION VARIABLE */

static inline void
add_timeout_delta(struct timespec *ts, double seconds)
{
  struct timeval tp;

  gettimeofday(&tp, NULL);
  /* Convert from timeval to timespec */
  ts->tv_sec  = tp.tv_sec;
  ts->tv_nsec = tp.tv_usec * 1000;

  /* Add `seconds' delta */
  ts->tv_sec += (time_t)floor(seconds);
  ts->tv_nsec += (long)((seconds - floor(seconds)) * 1e9);
  if (ts->tv_nsec >= 1e9) {
    ts->tv_nsec -= 1e9;
    ts->tv_sec++;
  }
}


static inline void
ecl_cond_var_init(ecl_cond_var_t *cv)
{
  pthread_cond_init(cv, NULL);
}

static inline void
ecl_cond_var_destroy(ecl_cond_var_t *cv)
{
  pthread_cond_destroy(cv);
}

static inline int
ecl_cond_var_wait(ecl_cond_var_t *cv, ecl_mutex_t *mutex)
{
  return pthread_cond_wait(cv, mutex);
}

static inline int
ecl_cond_var_timedwait(ecl_cond_var_t *cv, ecl_mutex_t *mutex, double seconds)
{
  struct timespec ts;
  add_timeout_delta(&ts, seconds);
  return pthread_cond_timedwait(cv, mutex, &ts);
}

static inline int
ecl_cond_var_signal(ecl_cond_var_t *cv)
{
  return pthread_cond_signal(cv);
}

static inline int
ecl_cond_var_broadcast(ecl_cond_var_t *cv)
{
  return pthread_cond_broadcast(cv);
}

#else /* ECL_WINDOWS_THREADS */

/* To allow for timed wait operations on locks and for interrupting
 * deadlocked threads, we use Windows mutexes instead of critical
 * section objects (which would serve the same purpose and be slightly
 * faster). This also requires implementing our own version of
 * condition variables, since the ones provided by Windows only work
 * together with critical sections. */

#define ECL_MUTEX_SUCCESS 0
#define ECL_MUTEX_LOCKED -1
#define ECL_MUTEX_NOT_OWNED ERROR_NOT_OWNER
#define ECL_MUTEX_TIMEOUT ERROR_TIMEOUT
#undef ECL_MUTEX_DEADLOCK

/* MUTEX */

static inline void
ecl_mutex_init(ecl_mutex_t *mutex, bool recursive)
{
  *mutex = CreateMutex(NULL, FALSE, NULL);
}

static inline void
ecl_mutex_destroy(ecl_mutex_t *mutex)
{
  CloseHandle(*mutex);
}

static inline int
ecl_mutex_unlock(ecl_mutex_t *mutex)
{
  return ReleaseMutex(*mutex) ? ECL_MUTEX_SUCCESS : GetLastError();
}

static inline int
ecl_mutex_trylock(ecl_mutex_t *mutex)
{
  switch (WaitForSingleObject(*mutex, 0)) {
  case WAIT_OBJECT_0:
  case WAIT_ABANDONED:
    return ECL_MUTEX_SUCCESS;
  case WAIT_TIMEOUT:
    return ECL_MUTEX_LOCKED;
  default:
    return GetLastError();
  }
}

static inline int
ecl_mutex_lock(ecl_mutex_t *mutex)
{
 AGAIN:
  switch (WaitForSingleObjectEx(*mutex, INFINITE, TRUE)) {
  case WAIT_OBJECT_0:
  case WAIT_ABANDONED:
    return ECL_MUTEX_SUCCESS;
  case WAIT_IO_COMPLETION:
    goto AGAIN;
  case WAIT_TIMEOUT:
    return ECL_MUTEX_LOCKED;
  default:
    return GetLastError();
  }
}

static inline DWORD
remaining_milliseconds(double seconds, DWORD start_ticks)
{
  DWORD ret = ((DWORD) seconds * 1000.0) - (GetTickCount() - start_ticks);
  return (ret < 0) ? 0 : ret;
}

/* CONDITION VARIABLE */

/* IMPLEMENTATION
 * --------------
 * Condition variables are implemented on top of the event
 * synchronization objects provided by the Windows operating
 * system.
 *
 * Event objects allow a thread to wait until a certain event is
 * signaled. If the event has already been signaled before the thread
 * checks for the event, no wait is performed and WaitForSingleObject,
 * WaitForMultipleObjects returns immediately. Event objects can be of
 * the manual reset flavor which stays signaled until explicitely
 * reset and auto reset which wakes up only one thread and then resets
 * itself automatically. We use a manual reset event for broadcasting
 * (waking up all threads waiting on the condition variable) and an
 * auto reset event for signaling (waking up only a single thread).
 *
 * The complete state of the condition variable (how many threads are
 * waiting and whether we are waiting, waking up threads or reseting
 * the broadcasting event) is stored in an integer which is
 * manipulated using atomic operations.
 *
 * RACE CONDITION SAFETY
 * ---------------------
 * To prevent wakeup signals from getting lost, condition variables
 * are required to atomically unlock the associated mutex when
 * starting a wait operation. While it is not possible to atomically
 * unlock a mutex and wait for an event on Windows, the implementation
 * is nevertheless race condition free in this regard.
 *
 * Consider two threads, thread A which starts a wait operation on a
 * condition variable and thread B which wakes up the same condition
 * variable. A increases the wait count, releases the mutex but
 * doesn't wait for a wakeup event yet. Thread B acquires the mutex,
 * and signals an event (broadcast or signal). Since the event is
 * reset only after a waiting thread has been released, at the time
 * that thread A starts waiting the event is still signaled and so the
 * wakeup event is not lost. Note that for this to work, it is crucial
 * that the wait count is increased before the mutex is released in
 * order for thread B to know that a thread is about to start waiting.
 *
 * Further race conditions between multiple broadcasting/signaling
 * threads threads are prevented by allowing only one operation
 * (signaling or broadcasting) at a time to happen. The case in which
 * threads start waiting during a wakeup operation is treated as if
 * the waiting thread had arrived before the wakeup. Race conditions
 * between threads which start waiting while the broadcast event is
 * reset are prevented by letting the about to be waiting threads spin
 * until the reset has finished.
 *
 * INTERRUPT SAFETY
 * ----------------
 * INV: On Windows, interrupts happen only when accessing the thread
 * local environment or when entering an alertable wait state. In the
 * latter case, an arriving interrupt causes the wait function to
 * return WAIT_IO_COMPLETION.
 *
 * This allows us to provide an interrupt safe implementation by
 * queueing the interrupt, fixing up the wait count of the condition
 * variable and then executing the interrupt. If the interrupt doesn't
 * execute a non-local jump, we resume waiting again.
 *
 * Note that while this prevents the state of the condition variable
 * from getting corrupted, it doesn't prevent wakup events which
 * arrive while the interrupt is executing from getting lost. It is up
 * to the user to prevent this type of logical bugs.
 */

#define ECL_COND_VAR_BROADCAST 1
#define ECL_COND_VAR_SIGNAL 2
#define ECL_COND_VAR_RESET 3

static inline cl_index
ecl_cond_var_wait_count(cl_index state)
{
  return state >> 2;
}

static inline cl_index
ecl_cond_var_status(cl_index state)
{
  return state & 3;
}

static inline cl_index
ecl_cond_var_make_state(cl_index wait_count, cl_index status)
{
  return (wait_count << 2) | status;
}

static inline void
ecl_cond_var_increment_wait_count(ecl_cond_var_t *cv)
{
  cl_index old_state, new_state;
  do {
    old_state = AO_load_acquire((AO_t*)&cv->state);
    new_state = ecl_cond_var_make_state(ecl_cond_var_wait_count(old_state) + 1,
                                        ecl_cond_var_status(old_state));
  } while (ecl_cond_var_status(old_state) == ECL_COND_VAR_RESET ||
           !AO_compare_and_swap_full((AO_t*)&cv->state, (AO_t)old_state, (AO_t)new_state));
}

static inline void
ecl_cond_var_decrement_wait_count(ecl_cond_var_t *cv)
{
  cl_index old_state, new_state;
  do {
    old_state = AO_load_acquire((AO_t*)&cv->state);
    new_state = ecl_cond_var_make_state(ecl_cond_var_wait_count(old_state) - 1,
                                        ecl_cond_var_status(old_state));
  } while (!AO_compare_and_swap_full((AO_t*)&cv->state, (AO_t)old_state, (AO_t)new_state));
}

static inline int
ecl_cond_var_handle_event(DWORD rc, ecl_cond_var_t *cv, ecl_mutex_t *mutex)
{
  cl_index old_state, new_state, wait_count;
  switch (rc) {
  case WAIT_OBJECT_0:
  case WAIT_ABANDONED:
    /* broadcast event */
    do {
      /* INV: ecl_cond_var_status(old_state) == ECL_COND_VAR_BROADCAST */
      old_state = AO_load_acquire((AO_t*)&cv->state);
      wait_count = ecl_cond_var_wait_count(old_state) - 1;
      if (wait_count == 0) {
        new_state = ecl_cond_var_make_state(0, ECL_COND_VAR_RESET);
        while (!AO_compare_and_swap_full((AO_t*)&cv->state, (AO_t)old_state, (AO_t)new_state));
        ResetEvent(cv->broadcast_event);
        AO_store_release((AO_t*)&cv->state, 0);
        break;
      }
      new_state = ecl_cond_var_make_state(wait_count, ECL_COND_VAR_BROADCAST);
    } while(!AO_compare_and_swap_full((AO_t*)&cv->state, (AO_t)old_state, (AO_t)new_state));
    return ecl_mutex_lock(mutex);
  case WAIT_OBJECT_0 + 1:
  case WAIT_ABANDONED + 1:
    /* signal event */
    do {
      /* INV: ecl_cond_var_status(old_state) == ECL_COND_VAR_SIGNAL */
      old_state = AO_load_acquire((AO_t*)&cv->state);
      wait_count = ecl_cond_var_wait_count(old_state) - 1;
      new_state = ecl_cond_var_make_state(wait_count, 0);
    } while(!AO_compare_and_swap_full((AO_t*)&cv->state, (AO_t)old_state, (AO_t)new_state));
    return ecl_mutex_lock(mutex);
  case WAIT_TIMEOUT:
    ecl_cond_var_decrement_wait_count(cv);
    rc = ecl_mutex_lock(mutex);
    return (rc == ECL_MUTEX_SUCCESS) ? ECL_MUTEX_TIMEOUT : rc;
  default:
    ecl_cond_var_decrement_wait_count(cv);
    return GetLastError();
  }
}

static inline void
ecl_cond_var_init(ecl_cond_var_t *cv)
{
  cv->broadcast_event = CreateEvent(NULL, TRUE, FALSE, NULL); /* manual reset event */
  cv->signal_event = CreateEvent(NULL, FALSE, FALSE, NULL); /* auto reset event */
  cv->state = 0;
}

static inline void
ecl_cond_var_destroy(ecl_cond_var_t *cv)
{
  CloseHandle(cv->broadcast_event);
  CloseHandle(cv->signal_event);
}

static inline int
ecl_cond_var_wait(ecl_cond_var_t *cv, ecl_mutex_t *mutex)
{
  cl_env_ptr env = ecl_process_env();
  DWORD rc;
  HANDLE events[2];
  events[0] = cv->broadcast_event;
  events[1] = cv->signal_event;
  ecl_disable_interrupts_env(env);
  ecl_cond_var_increment_wait_count(cv);
  if (!ReleaseMutex(*mutex)) {
    return GetLastError();
  }
  while ((rc = WaitForMultipleObjectsEx(2, events, FALSE, INFINITE, TRUE)) == WAIT_IO_COMPLETION) {
    /* we got an interrupt; first fix up the state of the condition variable, ... */
    ecl_cond_var_decrement_wait_count(cv);
    /* ... then handle the interrupt ... */
    ecl_enable_interrupts_env(env);
    ecl_check_pending_interrupts(env);
    ecl_disable_interrupts_env(env);
    /* ... and start waiting again ... */
    ecl_cond_var_increment_wait_count(cv);
  }
  return ecl_cond_var_handle_event(rc, cv, mutex);
}

static inline int
ecl_cond_var_timedwait(ecl_cond_var_t *cv, ecl_mutex_t *mutex, double seconds)
{
  cl_env_ptr env = ecl_process_env();
  DWORD rc;
  DWORD start_ticks = GetTickCount();
  HANDLE events[2];
  events[0] = cv->broadcast_event;
  events[1] = cv->signal_event;
  ecl_disable_interrupts_env(env);
  ecl_cond_var_increment_wait_count(cv);
  if (!ReleaseMutex(*mutex)) {
    return GetLastError();
  }
  while ((rc = WaitForMultipleObjectsEx(2, events, FALSE, remaining_milliseconds(seconds, start_ticks), TRUE)) == WAIT_IO_COMPLETION) {
    ecl_cond_var_decrement_wait_count(cv);
    ecl_enable_interrupts_env(env);
    ecl_check_pending_interrupts(env);
    ecl_disable_interrupts_env(env);
    ecl_cond_var_increment_wait_count(cv);
  }
  return ecl_cond_var_handle_event(rc, cv, mutex);
}

static inline int
ecl_cond_var_signal(ecl_cond_var_t *cv)
{
  cl_index old_state, new_state, wait_count, status;
  do {
    old_state = AO_load_acquire((AO_t*)&cv->state);
    wait_count = ecl_cond_var_wait_count(old_state);
    status = ecl_cond_var_status(old_state);
    if (wait_count == 0 || status == ECL_COND_VAR_BROADCAST) {
      return ECL_MUTEX_SUCCESS;
    }
    new_state = ecl_cond_var_make_state(wait_count, ECL_COND_VAR_SIGNAL);
  } while(status != 0 ||
          !AO_compare_and_swap_full((AO_t*)&cv->state, (AO_t)old_state, (AO_t)new_state));
  return SetEvent(cv->signal_event) ? ECL_MUTEX_SUCCESS : GetLastError();
}

static inline int
ecl_cond_var_broadcast(ecl_cond_var_t *cv)
{
  cl_index old_state, new_state, wait_count, status;
  do {
    old_state = AO_load_acquire((AO_t*)&cv->state);
    wait_count = ecl_cond_var_wait_count(old_state);
    status = ecl_cond_var_status(old_state);
    if (wait_count == 0 || status == ECL_COND_VAR_BROADCAST) {
      return ECL_MUTEX_SUCCESS;
    }
    new_state = ecl_cond_var_make_state(wait_count, ECL_COND_VAR_BROADCAST);
  } while(status != 0 ||
          !AO_compare_and_swap_full((AO_t*)&cv->state, (AO_t)old_state, (AO_t)new_state));
  return SetEvent(cv->broadcast_event) ? ECL_MUTEX_SUCCESS : GetLastError();
}
#endif /* ECL_MUTEX_H */

#endif /* ECL_THREADS */
