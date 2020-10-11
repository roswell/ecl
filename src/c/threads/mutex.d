/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * mutex.d - mutually exclusive locks.
 *
 * Copyright (c) 2003, Juan Jose Garcia Ripoll
 * Copyright (c) 2020, Marius Gerbershagen
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#ifndef __sun__ /* See unixinit.d for this */
#define _XOPEN_SOURCE 600	/* For pthread mutex attributes */
#endif
#include <errno.h>
#include <ecl/ecl.h>
#include <ecl/internal.h>

/*----------------------------------------------------------------------
 * LOCKS or MUTEX
 */


/* THREAD SAFETY
 *
 * mp:lock-owner, mp:holding-lock-p and mp:lock-count will return
 * wrong values in the following scenarios:
 * 1. Another thread is in the process of locking/unlocking the mutex.
 *    This in unavoidable since count and owner cannot both be stored
 *    atomically.
 * 2. A call to mp:get-lock-wait is interrupted after the mutex has
 *    been locked but before count and owner have been set. In this
 *    case, the mutex will appear to be unlocked even though it is
 *    already locked. If the interrupting code performs a nonlocal
 *    jump up the call stack, this will persist even after the
 *    interrupt. However, the mutex can still be unlocked by
 *    mp:giveup-lock since the check whether the mutex is locked or
 *    not is done by OS level functions.
 * 3. A call to mp:condition-variable-(timed)wait is interrupted after
 *    the mutex has been unlocked/relocked but after/before count and
 *    owner have been set. The consequences are equivalent to scenario 2.
 * In summary, owner can be nil and count 0 even though the mutex is
 * locked but the converse (owner != nil and count != 0 when the mutex
 * is unlocked) cannot happen.
 *
 */

static void
FEerror_not_a_recursive_lock(cl_object lock)
{
  FEerror("Attempted to recursively lock ~S which is already owned by ~S",
          2, lock, lock->lock.owner);
}

cl_object
ecl_make_lock(cl_object name, bool recursive)
{
  cl_env_ptr env = ecl_process_env();
  cl_object output = ecl_alloc_object(t_lock);
  output->lock.name = name;
  output->lock.owner = ECL_NIL;
  output->lock.counter = 0;
  output->lock.recursive = recursive;
  ecl_disable_interrupts_env(env);
  ecl_mutex_init(&output->lock.mutex, recursive);
  ecl_set_finalizer_unprotected(output, ECL_T);
  ecl_enable_interrupts_env(env);
  return output;
}

@(defun mp::make-lock (&key name (recursive ECL_NIL))
  @
  @(return ecl_make_lock(name, !Null(recursive)))
  @)

cl_object
mp_recursive_lock_p(cl_object lock)
{
  cl_env_ptr env = ecl_process_env();
  if (ecl_unlikely(ecl_t_of(lock) != t_lock)) {
    FEwrong_type_only_arg(@[mp::recursive-lock-p], lock, @[mp::lock]);
  }
  ecl_return1(env, lock->lock.recursive? ECL_T : ECL_NIL);
}

cl_object
mp_lock_name(cl_object lock)
{
  cl_env_ptr env = ecl_process_env();
  if (ecl_unlikely(ecl_t_of(lock) != t_lock)) {
    FEwrong_type_only_arg(@[mp::lock-name], lock, @[mp::lock]);
  }
  ecl_return1(env, lock->lock.name);
}

cl_object
mp_lock_owner(cl_object lock)
{
  cl_env_ptr env = ecl_process_env();
  if (ecl_unlikely(ecl_t_of(lock) != t_lock)) {
    FEwrong_type_only_arg(@[mp::lock-owner], lock, @[mp::lock]);
  }
  ecl_return1(env, lock->lock.owner);
}

cl_object
mp_holding_lock_p(cl_object lock)
{
  cl_env_ptr env = ecl_process_env();
  if (ecl_unlikely(ecl_t_of(lock) != t_lock)) {
    FEwrong_type_only_arg(@[mp::holding-lock-p], lock, @[mp::lock]);
  }
  ecl_return1(env, (lock->lock.owner == mp_current_process())? ECL_T : ECL_NIL);
}

cl_object
mp_lock_count(cl_object lock)
{
  cl_env_ptr env = ecl_process_env();
  if (ecl_unlikely(ecl_t_of(lock) != t_lock)) {
    FEwrong_type_only_arg(@[mp::lock-count], lock, @[mp::lock]);
  }
  ecl_return1(env, ecl_make_fixnum(lock->lock.counter));
}

cl_object
mp_giveup_lock(cl_object lock)
{
  cl_env_ptr env = ecl_process_env();
  int rc;
  if (ecl_unlikely(ecl_t_of(lock) != t_lock)) {
    FEwrong_type_only_arg(@[mp::giveup-lock], lock, @[mp::lock]);
  }
  ecl_disable_interrupts_env(env);
  if ((lock->lock.counter > 0 ? --lock->lock.counter : 0) == 0) {
    lock->lock.owner = ECL_NIL;
  }
  rc = ecl_mutex_unlock(&lock->lock.mutex);
  ecl_enable_interrupts_env(env);
  if (ecl_likely(rc == ECL_MUTEX_SUCCESS)) {
    ecl_return1(env, ECL_T);
  } else if (rc == ECL_MUTEX_NOT_OWNED) {
    FEerror_not_owned(lock);
  } else {
    FEunknown_lock_error(lock);
  }
}

cl_object
mp_get_lock_nowait(cl_object lock)
{
  cl_env_ptr env = ecl_process_env();
  cl_object own_process = env->own_process;
  int rc;
  if (ecl_unlikely(ecl_t_of(lock) != t_lock)) {
    FEwrong_type_nth_arg(@[mp::get-lock], 1, lock, @[mp::lock]);
  }
#if !defined(ECL_MUTEX_DEADLOCK)
  if (ecl_unlikely(lock->lock.owner == own_process && !lock->lock.recursive)) {
    /* INV: owner != nil only if the mutex is locked */
    FEerror_not_a_recursive_lock(lock);
  }
#endif
  ecl_disable_interrupts_env(env);
  if ((rc = ecl_mutex_trylock(&lock->lock.mutex)) == ECL_MUTEX_SUCCESS) {
    lock->lock.counter++;
    lock->lock.owner = own_process;
  }
  ecl_enable_interrupts_env(env);
  if (rc == ECL_MUTEX_SUCCESS) {
    ecl_return1(env,lock);
  } else if (rc == ECL_MUTEX_LOCKED) {
    ecl_return1(env,ECL_NIL);
#if defined(ECL_MUTEX_DEADLOCK)
  } else if (ecl_unlikely(rc == ECL_MUTEX_DEADLOCK)) {
    FEerror_not_a_recursive_lock(lock);
#endif
  } else {
    FEunknown_lock_error(lock);
  }
}

cl_object
mp_get_lock_wait(cl_object lock)
{
  cl_env_ptr env = ecl_process_env();
  cl_object own_process = env->own_process;
  int rc;
  if (ecl_unlikely(ecl_t_of(lock) != t_lock)) {
    FEwrong_type_nth_arg(@[mp::get-lock], 1, lock, @[mp::lock]);
  }
#if !defined(ECL_MUTEX_DEADLOCK)
  if (ecl_unlikely(lock->lock.owner == own_process && !lock->lock.recursive)) {
    /* INV: owner != nil only if the mutex is locked */
    FEerror_not_a_recursive_lock(lock);
  }
#endif
  rc = ecl_mutex_lock(&lock->lock.mutex);
  if (ecl_likely(rc == ECL_MUTEX_SUCCESS)) {
    ecl_disable_interrupts_env(env);
    lock->lock.counter++;
    lock->lock.owner = own_process;
    ecl_enable_interrupts_env(env);
    ecl_return1(env, lock);
#if defined(ECL_MUTEX_DEADLOCK)
  } else if (ecl_unlikely(rc == ECL_MUTEX_DEADLOCK)) {
    FEerror_not_a_recursive_lock(lock);
#endif
  } else {
    FEunknown_lock_error(lock);
  }
}

static cl_object
si_abort_wait_on_mutex(cl_narg narg, ...)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object env = the_env->function->cclosure.env;
  cl_object lock = CAR(env);
  if (ECL_SYM_VAL(the_env, @'si::mutex-timeout') == lock) {
    ECL_SETQ(the_env, @'si::mutex-timeout', ECL_T);
    cl_throw(@'si::mutex-timeout');
  }
  @(return)
}

cl_object
si_mutex_timeout(cl_object process, cl_object lock, cl_object timeout)
{
  const cl_env_ptr the_env = ecl_process_env();
  if (cl_plusp(timeout)) {
    cl_sleep(timeout);
  }
  ECL_WITH_NATIVE_LOCK_BEGIN(the_env, &process->process.start_stop_lock) {
    if (ecl_likely(mp_process_active_p(process) != ECL_NIL)) {
      ecl_interrupt_process(process,
                            ecl_make_cclosure_va(si_abort_wait_on_mutex,
                                                 cl_list(1, lock),
                                                 @'si::mutex-timeout',
                                                 0));
    }
  } ECL_WITH_NATIVE_LOCK_END;
  @(return)
}

cl_object
mp_get_lock_timedwait(cl_object lock, cl_object timeout)
{
  cl_env_ptr env = ecl_process_env();
  cl_object own_process = env->own_process;
  if (ecl_unlikely(ecl_t_of(lock) != t_lock)) {
    FEwrong_type_nth_arg(@[mp::get-lock], 1, lock, @[mp::lock]);
  }
#if !defined(ECL_MUTEX_DEADLOCK)
  if (ecl_unlikely(lock->lock.owner == own_process && !lock->lock.recursive)) {
    /* INV: owner != nil only if the mutex is locked */
    FEerror_not_a_recursive_lock(lock);
  }
#endif
#if defined(ECL_WINDOWS_THREADS) || defined(HAVE_PTHREAD_MUTEX_TIMEDLOCK)
  int rc = ecl_mutex_timedlock(&lock->lock.mutex, ecl_to_double(timeout));
#else
  /* If we don't have pthread_mutex_timedlock available, we create a
   * timer thread which interrupts our thread after the specified
   * timeout. si::mutex-timeout serves a dual purpose below: the
   * symbol itself denotes a catchpoint and its value is used to
   * determine a) if the catchpoint is active and b) if the timer has
   * fired. */
  volatile int rc;
  volatile cl_object timer_thread;
  ecl_bds_bind(env, @'si::mutex-timeout', lock);
  ECL_CATCH_BEGIN(env, @'si::mutex-timeout') {
    timer_thread = mp_process_run_function(5, @'si::mutex-timeout',
                                           @'si::mutex-timeout',
                                           env->own_process,
                                           lock,
                                           timeout);
    rc = ecl_mutex_lock(&lock->lock.mutex);
    ECL_SETQ(env, @'si::mutex-timeout', ECL_NIL);
  } ECL_CATCH_END;
  ECL_WITH_NATIVE_LOCK_BEGIN(env, &timer_thread->process.start_stop_lock) {
    if (mp_process_active_p(timer_thread)) {
      ecl_interrupt_process(timer_thread, @'mp::exit-process');
    }
  } ECL_WITH_NATIVE_LOCK_END;
  if (ECL_SYM_VAL(env, @'si::mutex-timeout') == ECL_T) {
    rc = ECL_MUTEX_TIMEOUT;
    /* The mutex might have been locked before we could kill the timer
     * thread. Therefore, we unconditionally try to unlock the mutex
     * again and treat the operation as having timed out. */
    ecl_mutex_unlock(&lock->lock.mutex);
  }
  ecl_bds_unwind1(env);
#endif
  if (rc == ECL_MUTEX_SUCCESS) {
    ecl_disable_interrupts_env(env);
    lock->lock.counter++;
    lock->lock.owner = own_process;
    ecl_enable_interrupts_env(env);
    ecl_return1(env, lock);
  } else if (rc == ECL_MUTEX_TIMEOUT) {
    ecl_return1(env,ECL_NIL);
#if defined(ECL_MUTEX_DEADLOCK)
  } else if (ecl_unlikely(rc == ECL_MUTEX_DEADLOCK)) {
    FEerror_not_a_recursive_lock(lock);
#endif
  } else {
    FEunknown_lock_error(lock);
  }
}

@(defun mp::get-lock (lock &optional (wait ECL_T))
  @
  if (Null(wait)) {
    return mp_get_lock_nowait(lock);
  } else if (ecl_realp(wait)) {
    return mp_get_lock_timedwait(lock, wait);
  } else {
    return mp_get_lock_wait(lock);
  }
  @)
