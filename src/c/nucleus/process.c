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

#ifndef __sun__ /* See unixinit.d for this */
#define _XOPEN_SOURCE 600       /* For pthread mutex attributes */
#endif
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
#  define ecl_process_key_t DWORD
#  define ecl_process_key_create(key) key = TlsAlloc()
#  define ecl_process_get_tls(key) TlsGetValue(key)
#  define ecl_process_set_tls(key,val) (TlsSetValue(key,val)!=0)
# else
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
#endif
  ecl_set_process_env(env);
  env->default_sigmask = NULL;
  env->method_cache = NULL;
  env->slot_cache = NULL;
  env->interrupt_struct = NULL;
  env->disable_interrupts = 1;
}
