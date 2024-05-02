/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * process.c - managing the process environment(s)
 *
 * Copyright (c) 2003 Juan Jose Garcia Ripoll
 * Copyright (c) 2023 Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>            /* includes ECL_WINDOWS_THREADS */
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>

#include <errno.h>
#include <time.h>
#include <signal.h>
#include <string.h>
#ifdef ECL_WINDOWS_THREADS
# include <windows.h>
#else
# include <pthread.h>
#endif
#ifdef HAVE_GETTIMEOFDAY
# include <sys/time.h>
#endif
#ifdef HAVE_SCHED_H
# include <sched.h>
#endif

/* -- Thread-local variables ------------------------------------------------ */

#ifdef ECL_THREADS
# ifdef ECL_WINDOWS_THREADS
#  define ecl_process_eq(t1, t2) (GetThreadId(t1) == GetThreadId(t2))
#  define ecl_set_process_self(var)             \
  {                                             \
    HANDLE aux = GetCurrentThread();            \
    DuplicateHandle(GetCurrentProcess(),        \
                    aux,                        \
                    GetCurrentProcess(),        \
                    &var,                       \
                    0,                          \
                    FALSE,                      \
                    DUPLICATE_SAME_ACCESS);     \
  }
#  define ecl_process_key_t DWORD
#  define ecl_process_key_create(key) key = TlsAlloc()
#  define ecl_process_get_tls(key) TlsGetValue(key)
#  define ecl_process_set_tls(key,val) (TlsSetValue(key,val)!=0)
# else
#  define ecl_process_eq(t1, t2) (t1 == t2)
#  define ecl_set_process_self(var) (var = pthread_self())
#  define ecl_process_key_t static pthread_key_t
#  define ecl_process_key_create(key) pthread_key_create(&key, NULL)
#  define ecl_process_get_tls(key) pthread_getspecific(key)
#  define ecl_process_set_tls(key,val) (pthread_setspecific(key,val)==0)
# endif  /* ECL_WINDOWS_THREADS */

/* Accessing a thread-local variable representing the environment. */

ecl_process_key_t cl_env_key;

cl_env_ptr
ecl_process_env_unsafe(void)
{
  return ecl_process_get_tls(cl_env_key);
}

cl_env_ptr
ecl_process_env(void)
{
  cl_env_ptr rv = ecl_process_get_tls(cl_env_key);
  if(!rv) {
    ecl_thread_internal_error("pthread_getspecific() failed.");
  }
  return rv;
}

void
ecl_set_process_env(cl_env_ptr env)
{
  if(!ecl_process_set_tls(cl_env_key, env)) {
    ecl_thread_internal_error("pthread_setspecific() failed.");
  }
}
#else
/* The current global environment for single-threaded builds. */
cl_env_ptr cl_env_p = NULL;
#endif  /* ECL_THREADS */

/* -- Managing the collection of processes ---------------------------------- */

#ifdef ECL_THREADS

static void
add_env(cl_env_ptr the_env)
{
  ecl_mutex_lock(&ecl_core.processes_lock);
  if (ecl_core.nthreads >= ecl_core.sthreads) {
    cl_index ns = (1 + ecl_core.nthreads) + ecl_core.nthreads/2;
    ecl_core.threads = ecl_realloc(ecl_core.threads, ns*sizeof(cl_env_ptr));
    ecl_core.sthreads = ns;
  }
  ecl_core.threads[ecl_core.nthreads++] = the_env;
  ecl_mutex_unlock(&ecl_core.processes_lock);
}

static void
del_env(cl_env_ptr the_env)
{
  ecl_mutex_lock(&ecl_core.processes_lock);
  for(cl_index idx = 0, ndx = ecl_core.nthreads; idx < ndx; idx++) {
    if(ecl_core.threads[idx] == the_env) {
      do { ecl_core.threads[idx] = ecl_core.threads[idx+1]; }
      while (++idx <= ndx);
      ecl_core.nthreads--;
      break;
    }
  }
  ecl_mutex_unlock(&ecl_core.processes_lock);
}

/* Run a process in the current system thread. */
cl_env_ptr
ecl_adopt_cpu()
{
  struct cl_env_struct env_aux[1];
  cl_env_ptr the_env = ecl_process_env_unsafe();
  ecl_thread_t current;
  int registered;
  if (the_env != NULL)
    return the_env;
  /* 1. Ensure that the thread is known to the GC. */
  /* FIXME this should be executed with hooks. */
#ifdef GBC_BOEHM
  {
    struct GC_stack_base stack;
    GC_get_stack_base(&stack);
    switch (GC_register_my_thread(&stack)) {
    case GC_SUCCESS:
      registered = 1;
      break;
    case GC_DUPLICATE:
      /* Thread was probably created using the GC hooks for thread creation. */
      registered = 0;
      break;
    default:
      ecl_internal_error("gc returned an impossible answer.");
    }
  }
#endif
  ecl_set_process_self(current);
  /* We need a fake env to allow for interrupts blocking and to set up frame
   * stacks or other stuff that is needed by ecl_init_env. Since the fake env is
   * allocated on the stack, we can safely store pointers to memory allocated by
   * the gc there. */
  memset(env_aux, 0, sizeof(*env_aux));
  env_aux->disable_interrupts = 1;
  env_aux->interrupt_struct = ecl_alloc_unprotected(sizeof(*env_aux->interrupt_struct));
  env_aux->interrupt_struct->pending_interrupt = ECL_NIL;
  ecl_mutex_init(&env_aux->interrupt_struct->signal_queue_lock, FALSE);
  env_aux->interrupt_struct->signal_queue = ECL_NIL;
  ecl_set_process_env(env_aux);
  env_aux->thread = current;
  env_aux->cleanup = registered;
  ecl_init_env(env_aux);

  /* Allocate, initialize and switch to the real environment. */
  the_env = _ecl_alloc_env(0);
  memcpy(the_env, env_aux, sizeof(*the_env));
  ecl_set_process_env(the_env);
  add_env(the_env);

  return the_env;
}

/* Run a process in a new system thread. */
cl_env_ptr
ecl_spawn_cpu()
{
  return NULL;
}


void
ecl_add_process(cl_object process)
{
  add_env(process->process.env);
}

void
ecl_del_process(cl_object process)
{
  del_env(process->process.env);
}
#endif

/* -- Initialiation --------------------------------------------------------- */

void
init_process(void)
{
  cl_env_ptr env = ecl_core.first_env;
#ifdef ECL_THREADS
  ecl_process_key_create(cl_env_key);
  ecl_mutex_init(&ecl_core.processes_lock, 1);
  ecl_mutex_init(&ecl_core.global_lock, 1);
  ecl_mutex_init(&ecl_core.error_lock, 1);
  ecl_rwlock_init(&ecl_core.global_env_lock);
  ecl_core.threads = ecl_malloc(ecl_core.sthreads * sizeof(cl_env_ptr));
#endif
  ecl_set_process_env(env);
  env->default_sigmask = NULL;
  env->method_cache = NULL;
  env->slot_cache = NULL;
  env->interrupt_struct = NULL;
  env->disable_interrupts = 1;
}
