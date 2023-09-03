/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * condition_variable.d - condition variables
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
#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#ifdef ECL_WINDOWS_THREADS
# include <windows.h>
#else
# include <pthread.h>
#endif
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>

/*----------------------------------------------------------------------
 * CONDITION VARIABLES
 */

cl_object
mp_make_condition_variable(void)
{
  cl_env_ptr env = ecl_process_env();
  cl_object output = ecl_alloc_object(t_condition_variable);
  ecl_disable_interrupts_env(env);
  ecl_cond_var_init(&output->condition_variable.cv);
  ecl_set_finalizer_unprotected(output, ECL_T);
  ecl_enable_interrupts_env(env);
  @(return output)
}

cl_object
mp_condition_variable_wait(cl_object cv, cl_object lock)
{
  cl_env_ptr env = ecl_process_env();
  int rc;
  cl_fixnum counter;
  cl_object owner;
  if (ecl_unlikely(ecl_t_of(cv) != t_condition_variable)) {
    FEwrong_type_nth_arg(@[mp::condition-variable-wait], 1, cv,
                         @[mp::condition-variable]);
  }
  if (ecl_unlikely(ecl_t_of(lock) != t_lock)) {
    FEwrong_type_nth_arg(@[mp::condition-variable-wait], 2, lock,
                         @[mp::lock]);
  }
  if (ecl_unlikely(lock->lock.recursive)) {
    FEerror("mp:condition-variable-wait can not be used with recursive"
            " locks:~%~S", 1, lock);
  }
  if (ecl_unlikely(lock->lock.owner != env->own_process)) {
    FEerror("Attempt to wait on a condition variable using lock~%~S"
            "~%which is not owned by process~%~S", 2, lock, env->own_process);
  }
  ecl_disable_interrupts_env(env);
  counter = lock->lock.counter;
  owner = lock->lock.owner;
  lock->lock.counter = 0;
  lock->lock.owner = ECL_NIL;
  ecl_enable_interrupts_env(env);
  rc = ecl_cond_var_wait(&cv->condition_variable.cv, &lock->lock.mutex);
  ecl_disable_interrupts_env(env);
  lock->lock.owner = owner;
  lock->lock.counter = counter;
  ecl_enable_interrupts_env(env);
  if (ecl_unlikely(rc != ECL_MUTEX_SUCCESS)) {
    if (rc == ECL_MUTEX_NOT_OWNED) {
      FEerror_not_owned(lock);
    } else {
      FEunknown_lock_error(lock);
    }
  }
  @(return ECL_T)
}

cl_object
mp_condition_variable_timedwait(cl_object cv, cl_object lock, cl_object seconds)
{
  cl_env_ptr env = ecl_process_env();
  int rc;
  cl_fixnum counter;
  cl_object owner;

  if (ecl_unlikely(ecl_t_of(cv) != t_condition_variable)) {
    FEwrong_type_nth_arg(@[mp::condition-variable-timedwait],
                         1, cv, @[mp::condition-variable]);
  }
  if (ecl_unlikely(ecl_t_of(lock) != t_lock)) {
    FEwrong_type_nth_arg(@[mp::condition-variable-timedwait],
                         2, lock, @[mp::lock]);
  }
  if (ecl_unlikely(lock->lock.recursive)) {
    FEerror("mp:condition-variable-timedwait can not be used with recursive"
            " locks:~%~S", 1, lock);
  }
  if (ecl_unlikely(lock->lock.owner != env->own_process)) {
    FEerror("Attempt to wait on a condition variable using lock~%~S"
            "~%which is not owned by process~%~S", 2, lock, env->own_process);
  }
  /* INV: ecl_minusp() makes sure `seconds' is real */
  if (ecl_unlikely(ecl_minusp(seconds))) {
    cl_error(9, @'simple-type-error', @':format-control',
             make_constant_base_string("Not a non-negative number ~S"),
             @':format-arguments', cl_list(1, seconds),
             @':expected-type', @'real', @':datum', seconds);
  }
  ecl_disable_interrupts_env(env);
  counter = lock->lock.counter;
  owner = lock->lock.owner;
  lock->lock.counter = 0;
  lock->lock.owner = ECL_NIL;
  ecl_enable_interrupts_env(env);

  rc = ecl_cond_var_timedwait(&cv->condition_variable.cv, &lock->lock.mutex, ecl_to_double(seconds));

  ecl_disable_interrupts_env(env);
  lock->lock.owner = owner;
  lock->lock.counter = counter;
  ecl_enable_interrupts_env(env);

  if (ecl_unlikely(rc != ECL_MUTEX_SUCCESS && rc != ECL_MUTEX_TIMEOUT)) {
    if (rc == ECL_MUTEX_NOT_OWNED) {
      FEerror_not_owned(lock);
    } else {
      FEunknown_lock_error(lock);
    }
  }
  @(return (rc == ECL_MUTEX_SUCCESS ? ECL_T : ECL_NIL))
}

cl_object
mp_condition_variable_signal(cl_object cv)
{
  if (ecl_unlikely(ecl_t_of(cv) != t_condition_variable)) {
    FEwrong_type_only_arg(@[mp::condition-variable-signal],
                          cv, @[mp::condition-variable]);
  }
  ecl_cond_var_signal(&cv->condition_variable.cv);
  @(return ECL_T)
}

cl_object
mp_condition_variable_broadcast(cl_object cv)
{
  if (ecl_unlikely(ecl_t_of(cv) != t_condition_variable)) {
    FEwrong_type_only_arg(@[mp::condition-variable-broadcast],
                          cv, @[mp::condition-variable]);
  }
  ecl_cond_var_broadcast(&cv->condition_variable.cv);
  @(return ECL_T)
}

