/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * rwlock.d - POSIX read-write locks
 *
 * Copyright (c) 2003 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#ifndef __sun__ /* See unixinit.d for this */
#define _XOPEN_SOURCE 600       /* For pthread mutex attributes */
#endif
#include <errno.h>
#include <ecl/ecl.h>
#ifdef ECL_WINDOWS_THREADS
# include <windows.h>
#else
# include <pthread.h>
#endif
#include <ecl/internal.h>

/*----------------------------------------------------------------------
 * READ/WRITE LOCKS
 */

cl_object
ecl_make_rwlock(cl_object name)
{
  cl_env_ptr env = ecl_process_env();
  cl_object output = ecl_alloc_object(t_rwlock);
  output->rwlock.name = name;
  ecl_disable_interrupts_env(env);
  ecl_rwlock_init(&output->rwlock.mutex);
  ecl_set_finalizer_unprotected(output, ECL_T);
  ecl_enable_interrupts_env(env);
  return output;
}

@(defun mp::make-rwlock (&key name)
  @
  @(return ecl_make_rwlock(name));
  @)

cl_object
mp_rwlock_name(cl_object lock)
{
  const cl_env_ptr env = ecl_process_env();
  if (ecl_unlikely(ecl_t_of(lock) != t_rwlock)) {
    FEwrong_type_only_arg(@[mp::rwlock-name], lock, @[mp::rwlock]);
  }
  ecl_return1(env, lock->rwlock.name);
}

cl_object
mp_giveup_rwlock_read(cl_object lock)
{
  const cl_env_ptr env = ecl_process_env();
  int rc;
  if (ecl_unlikely(ecl_t_of(lock) != t_rwlock)) {
    FEwrong_type_only_arg(@[mp::giveup-rwlock-read], lock, @[mp::rwlock]);
  }
  rc = ecl_rwlock_unlock_read(&lock->rwlock.mutex);
  if (ecl_likely(rc == ECL_MUTEX_SUCCESS)) {
    ecl_return1(env, ECL_T);
  } else if (rc == ECL_MUTEX_NOT_OWNED) {
    FEerror_not_owned(lock);
  } else {
    FEunknown_lock_error(lock);
  }
}

cl_object
mp_giveup_rwlock_write(cl_object lock)
{
  const cl_env_ptr env = ecl_process_env();
  int rc;
  if (ecl_unlikely(ecl_t_of(lock) != t_rwlock)) {
    FEwrong_type_only_arg(@[mp::giveup-rwlock-write], lock, @[mp::rwlock]);
  }
  rc = ecl_rwlock_unlock_write(&lock->rwlock.mutex);
  if (ecl_likely(rc == ECL_MUTEX_SUCCESS)) {
    ecl_return1(env, ECL_T);
  } else if (rc == ECL_MUTEX_NOT_OWNED) {
    FEerror_not_owned(lock);
  } else {
    FEunknown_lock_error(lock);
  }
}

cl_object
mp_get_rwlock_read_nowait(cl_object lock)
{
  const cl_env_ptr env = ecl_process_env();
  int rc;
  if (ecl_unlikely(ecl_t_of(lock) != t_rwlock)) {
    FEwrong_type_only_arg(@[mp::get-rwlock-read], lock, @[mp::rwlock]);
  }
  rc = ecl_rwlock_trylock_read(&lock->rwlock.mutex);
  if (rc == ECL_MUTEX_SUCCESS) {
    ecl_return1(env, ECL_T);
  } else if (rc == ECL_MUTEX_LOCKED) {
    ecl_return1(env, ECL_NIL);
  } else {
    FEunknown_lock_error(lock);
  }
}

cl_object
mp_get_rwlock_read_wait(cl_object lock)
{
  const cl_env_ptr env = ecl_process_env();
  int rc;
  if (ecl_unlikely(ecl_t_of(lock) != t_rwlock)) {
    FEwrong_type_only_arg(@[mp::get-rwlock-read], lock, @[mp::rwlock]);
  }
  rc = ecl_rwlock_lock_read(&lock->rwlock.mutex);
  if (ecl_likely(rc == ECL_MUTEX_SUCCESS)) {
    ecl_return1(env, ECL_T);
  } else {
    FEunknown_lock_error(lock);
  }
}

@(defun mp::get-rwlock-read (lock &optional (wait ECL_T))
  @
  if (Null(wait)) {
    return mp_get_rwlock_read_nowait(lock);
  } else {
    return mp_get_rwlock_read_wait(lock);
  }
  @)

cl_object
mp_get_rwlock_write_nowait(cl_object lock)
{
  const cl_env_ptr env = ecl_process_env();
  int rc;
  if (ecl_unlikely(ecl_t_of(lock) != t_rwlock)) {
    FEwrong_type_only_arg(@[mp::get-rwlock-write], lock, @[mp::rwlock]);
  }
  rc = ecl_rwlock_trylock_write(&lock->rwlock.mutex);
  if (rc == ECL_MUTEX_SUCCESS) {
    ecl_return1(env, ECL_T);
  } else if (rc == ECL_MUTEX_LOCKED) {
    ecl_return1(env, ECL_NIL);
  } else {
    FEunknown_lock_error(lock);
  }
}

cl_object
mp_get_rwlock_write_wait(cl_object lock)
{
  const cl_env_ptr env = ecl_process_env();
  int rc;
  if (ecl_unlikely(ecl_t_of(lock) != t_rwlock)) {
    FEwrong_type_only_arg(@[mp::get-rwlock-write], lock, @[mp::rwlock]);
  }
  rc = ecl_rwlock_lock_write(&lock->rwlock.mutex);
  if (ecl_likely(rc == ECL_MUTEX_SUCCESS)) {
    ecl_return1(env, ECL_T);
  } else {
    FEunknown_lock_error(lock);
  }
}

@(defun mp::get-rwlock-write (lock &optional (wait ECL_T))
  @
  if (Null(wait)) {
    return mp_get_rwlock_write_nowait(lock);
  } else {
    return mp_get_rwlock_write_wait(lock);
  }
  @)
