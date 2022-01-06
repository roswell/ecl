/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * semaphore.d - POSIX-like semaphores
 *
 * Copyright (c) 2011 Juan Jose Garcia Ripoll
 * Copyright (c) 2020 Marius Gerbershagen
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/internal.h>

cl_object
ecl_make_semaphore(cl_object name, cl_fixnum count)
{
  cl_env_ptr env = ecl_process_env();
  cl_object output = ecl_alloc_object(t_semaphore);
  output->semaphore.name = name;
  output->semaphore.counter = count;
  output->semaphore.wait_count = 0;
  ecl_disable_interrupts_env(env);
  ecl_cond_var_init(&output->semaphore.cv);
  ecl_mutex_init(&output->semaphore.mutex, FALSE);
  ecl_set_finalizer_unprotected(output, ECL_T);
  ecl_enable_interrupts_env(env);
  return output;
}

@(defun mp::make-semaphore (&key name (count ecl_make_fixnum(0)))
  @ {
    @(return ecl_make_semaphore(name, fixnnint(count)));
  } @)

cl_object
mp_semaphore_name(cl_object semaphore)
{
  cl_env_ptr env = ecl_process_env();
  unlikely_if (ecl_t_of(semaphore) != t_semaphore) {
    FEwrong_type_only_arg(@[mp::semaphore-name], semaphore, @[mp::semaphore]);
  }
  ecl_return1(env, semaphore->semaphore.name);
}

cl_object
mp_semaphore_count(cl_object semaphore)
{
  cl_env_ptr env = ecl_process_env();
  unlikely_if (ecl_t_of(semaphore) != t_semaphore) {
    FEwrong_type_only_arg(@[mp::semaphore-count], semaphore, @[mp::semaphore]);
  }
  ecl_return1(env, ecl_make_fixnum(semaphore->semaphore.counter));
}

cl_object
mp_semaphore_wait_count(cl_object semaphore)
{
  cl_env_ptr env = ecl_process_env();
  unlikely_if (ecl_t_of(semaphore) != t_semaphore) {
    FEwrong_type_only_arg(@[mp::semaphore-wait-count], semaphore, @[mp::semaphore]);
  }
  ecl_return1(env, ecl_make_fixnum(semaphore->semaphore.wait_count));
}

@(defun mp::signal-semaphore (semaphore &optional (count ecl_make_fixnum(1)))
  @ {
    cl_fixnum n = fixnnint(count);
    unlikely_if (ecl_t_of(semaphore) != t_semaphore) {
      FEwrong_type_nth_arg(@[mp::signal-semaphore], 1, semaphore, @[mp::semaphore]);
    }
    ecl_disable_interrupts_env(the_env);
    ecl_mutex_lock(&semaphore->semaphore.mutex);
    semaphore->semaphore.counter += n;
    for (; n > 0; n--) {
      ecl_cond_var_signal(&semaphore->semaphore.cv);
    }
    ecl_mutex_unlock(&semaphore->semaphore.mutex);
    ecl_enable_interrupts_env(the_env);
    @(return);
  } @)

static inline void
semaphore_wait_unprotected(cl_object semaphore, cl_object count, cl_object timeout)
{
  int rc;
  cl_env_ptr the_env = ecl_process_env();
  cl_fixnum counter = fixnnint(count);
  ecl_mutex_t *mutex = &semaphore->semaphore.mutex;
  ecl_cond_var_t *cv = &semaphore->semaphore.cv;
  if (timeout == ECL_NIL) {
    do {
      ecl_setq(the_env, ECL_INTERRUPTS_ENABLED, ECL_T);
      ecl_cond_var_wait(cv, mutex);
      ecl_setq(the_env, ECL_INTERRUPTS_ENABLED, ECL_NIL);
    } while (semaphore->semaphore.counter < counter);
  } else {
    cl_object deadline = ecl_plus(cl_get_internal_real_time(),
                                  ecl_times(timeout, ecl_make_fixnum(1000)));
    double seconds = ecl_to_double(timeout);
    do {
      ecl_setq(the_env, ECL_INTERRUPTS_ENABLED, ECL_T);
      rc = ecl_cond_var_timedwait(cv, mutex, seconds);
      ecl_setq(the_env, ECL_INTERRUPTS_ENABLED, ECL_NIL);
      timeout = ecl_minus(deadline, cl_get_internal_real_time());
      seconds = ecl_to_double(timeout);
    } while(semaphore->semaphore.counter < counter
            && rc != ECL_MUTEX_TIMEOUT
            && seconds >= 0);
  }
}

cl_object
mp_semaphore_wait(cl_object semaphore, cl_object count, cl_object timeout)
{
  cl_env_ptr the_env = ecl_process_env();
  cl_fixnum counter = fixnnint(count);
  volatile cl_object output;
  unlikely_if (ecl_t_of(semaphore) != t_semaphore) {
    FEwrong_type_only_arg(@[mp::semaphore-wait], semaphore, @[mp::semaphore]);
  }
  ecl_bds_bind(the_env, ECL_INTERRUPTS_ENABLED, ECL_NIL);
  ecl_mutex_lock(&semaphore->semaphore.mutex);
  if (semaphore->semaphore.counter >= counter) {
    output = ecl_make_fixnum(semaphore->semaphore.counter);
    semaphore->semaphore.counter -= counter;
    ecl_mutex_unlock(&semaphore->semaphore.mutex);
  } else if (timeout == ECL_NIL || ecl_plusp(timeout)) {
    semaphore->semaphore.wait_count++;
    ECL_UNWIND_PROTECT_BEGIN(the_env) {
      semaphore_wait_unprotected(semaphore, count, timeout);
      if (semaphore->semaphore.counter >= counter) {
        output = ecl_make_fixnum(semaphore->semaphore.counter);
        semaphore->semaphore.counter -= counter;
      } else {
        output = ECL_NIL;
      }
    } ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT {
      semaphore->semaphore.wait_count--;
      ecl_mutex_unlock(&semaphore->semaphore.mutex);
    } ECL_UNWIND_PROTECT_THREAD_SAFE_END;
  } else {
    output = ECL_NIL;
    ecl_mutex_unlock(&semaphore->semaphore.mutex);
  }
  ecl_bds_unwind1(the_env);
  ecl_check_pending_interrupts(the_env);
  ecl_return1(the_env, output);
}

@(defun mp::wait-on-semaphore (semaphore &key (count ecl_make_fixnum(1))
                                              (timeout ECL_NIL))
  @ {
    cl_object output = mp_semaphore_wait(semaphore, count, timeout);
    @(return output);
  } @)

@(defun mp::try-get-semaphore (semaphore &optional (count ecl_make_fixnum(1)))
  @ {
    cl_object timeout = ecl_make_fixnum(0);
    cl_object output = mp_semaphore_wait(semaphore, count, timeout);
    @(return output);
  } @)
