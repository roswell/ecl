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

cl_object
mp_wait_on_semaphore(cl_object semaphore)
{
  cl_env_ptr the_env = ecl_process_env();
  volatile cl_object output;
  unlikely_if (ecl_t_of(semaphore) != t_semaphore) {
    FEwrong_type_only_arg(@[mp::wait-on-semaphore], semaphore, @[mp::semaphore]);
  }
  ecl_bds_bind(the_env, ECL_INTERRUPTS_ENABLED, ECL_NIL);
  ecl_mutex_lock(&semaphore->semaphore.mutex);
  if (semaphore->semaphore.counter == 0) {
    semaphore->semaphore.wait_count++;
    ECL_UNWIND_PROTECT_BEGIN(the_env) {
      do {
        ecl_setq(the_env, ECL_INTERRUPTS_ENABLED, ECL_T);
        ecl_cond_var_wait(&semaphore->semaphore.cv, &semaphore->semaphore.mutex);
        ecl_setq(the_env, ECL_INTERRUPTS_ENABLED, ECL_NIL);
      } while (semaphore->semaphore.counter == 0);
      output = ecl_make_fixnum(semaphore->semaphore.counter--);
    } ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT {
      semaphore->semaphore.wait_count--;
      ecl_mutex_unlock(&semaphore->semaphore.mutex);
    } ECL_UNWIND_PROTECT_THREAD_SAFE_END;
  } else {
    output = ecl_make_fixnum(semaphore->semaphore.counter--);
    ecl_mutex_unlock(&semaphore->semaphore.mutex);
  }
  ecl_bds_unwind1(the_env);
  ecl_check_pending_interrupts(the_env);
  ecl_return1(the_env, output);
}

cl_object
mp_try_get_semaphore(cl_object semaphore)
{
  cl_env_ptr the_env = ecl_process_env();
  cl_object output;
  unlikely_if (ecl_t_of(semaphore) != t_semaphore) {
    FEwrong_type_only_arg(@[mp::try-get-semaphore], semaphore, @[mp::semaphore]);
  }
  ecl_disable_interrupts_env(the_env);
  ecl_mutex_lock(&semaphore->semaphore.mutex);
  if (semaphore->semaphore.counter > 0) {
    output = ecl_make_fixnum(semaphore->semaphore.counter--);
  } else {
    output = ECL_NIL;
  }
  ecl_mutex_unlock(&semaphore->semaphore.mutex);
  ecl_enable_interrupts_env(the_env);
  ecl_return1(the_env, output);
}

