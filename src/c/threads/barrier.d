/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * barrier.d - wait barriers
 *
 * Copyright (c) 2012 Juan Jose Garcia Ripoll
 * Copyright (c) 2020 Marius Gerbershagen
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/internal.h>

cl_object
ecl_make_barrier(cl_object name, cl_index count)
{
  cl_env_ptr env = ecl_process_env();
  cl_object output = ecl_alloc_object(t_barrier);
  output->barrier.disabled = FALSE;
  output->barrier.wakeup = 0;
  output->barrier.name = name;
  output->barrier.arrivers_count = 0;
  output->barrier.count = count;
  ecl_disable_interrupts_env(env);
  ecl_cond_var_init(&output->barrier.cv);
  ecl_mutex_init(&output->barrier.mutex, FALSE);
  ecl_set_finalizer_unprotected(output, ECL_T);
  ecl_enable_interrupts_env(env);
  return output;
}

@(defun mp::make-barrier (count &key name)
  @
  if (count == ECL_T)
    count = ecl_make_fixnum(MOST_POSITIVE_FIXNUM);
  @(return ecl_make_barrier(name, fixnnint(count)));
  @)

cl_object
mp_barrier_name(cl_object barrier)
{
  cl_env_ptr env = ecl_process_env();
  unlikely_if (ecl_t_of(barrier) != t_barrier) {
    FEwrong_type_only_arg(@[mp::barrier-name], barrier, @[mp::barrier]);
  }
  ecl_return1(env, barrier->barrier.name);
}

cl_object
mp_barrier_count(cl_object barrier)
{
  cl_env_ptr env = ecl_process_env();
  unlikely_if (ecl_t_of(barrier) != t_barrier) {
    FEwrong_type_only_arg(@[mp::barrier-count], barrier, @[mp::barrier]);
  }
  ecl_return1(env, ecl_make_fixnum(barrier->barrier.count));
}

cl_object
mp_barrier_arrivers_count(cl_object barrier)
{
  cl_env_ptr env = ecl_process_env();
  unlikely_if (ecl_t_of(barrier) != t_barrier) {
    FEwrong_type_only_arg(@[mp::barrier-arrivers_count], barrier, @[mp::barrier]);
  }
  ecl_return1(env, ecl_make_fixnum(barrier->barrier.arrivers_count));
}

/* INV: locking the mutex in mp_barrier_unblock and mp_barrier_wait
 * will always succeed since the functions are not reentrant and only
 * lock/unlock the mutex while interrupts are disabled, therefore
 * deadlocks can't happen. */

@(defun mp::barrier-unblock (barrier &key reset_count disable kill_waiting)
  @
  unlikely_if (ecl_t_of(barrier) != t_barrier) {
    FEwrong_type_nth_arg(@[mp::barrier-unblock], 1, barrier, @[mp::barrier]);
  }
  ecl_disable_interrupts_env(the_env);
 AGAIN:
  ecl_mutex_lock(&barrier->barrier.mutex);
  if (barrier->barrier.wakeup) {
    /* we are currently waking up blocked threads; loop until all
     * threads have woken up */
    ecl_mutex_unlock(&barrier->barrier.mutex);
    goto AGAIN;
  }
  if (!Null(reset_count)) {
    barrier->barrier.count = fixnnint(reset_count);
  }
  if (!Null(disable)) {
    barrier->barrier.disabled = TRUE;
  } else {
    barrier->barrier.disabled = FALSE;
  }
  if (barrier->barrier.arrivers_count > 0) {
    if (!Null(kill_waiting)) {
      barrier->barrier.wakeup = ECL_BARRIER_WAKEUP_KILL;
    } else {
      barrier->barrier.wakeup = ECL_BARRIER_WAKEUP_NORMAL;
    }
    ecl_cond_var_broadcast(&barrier->barrier.cv);
  }
  ecl_mutex_unlock(&barrier->barrier.mutex);
  ecl_enable_interrupts_env(the_env);
  @(return);
  @)

cl_object
mp_barrier_wait(cl_object barrier) {
  cl_env_ptr the_env = ecl_process_env();
  volatile int wakeup = 0;
  unlikely_if (ecl_t_of(barrier) != t_barrier) {
    FEwrong_type_only_arg(@[mp::barrier-wait], barrier, @[mp::barrier]);
  }
  ecl_bds_bind(the_env, ECL_INTERRUPTS_ENABLED, ECL_NIL);
  /* check if the barrier is disabled */
  do {
    ecl_mutex_lock(&barrier->barrier.mutex);
    if (barrier->barrier.disabled) {
      ecl_mutex_unlock(&barrier->barrier.mutex);
      ecl_bds_unwind1(the_env);
      ecl_check_pending_interrupts(the_env);
      return ECL_NIL;
    }
    if (barrier->barrier.wakeup) {
      /* We are currently waking up blocked threads; loop until all threads have
       * woken up. */
      ecl_mutex_unlock(&barrier->barrier.mutex);
    } else {
      break;
    }
  } while(1);
  /* check if we have reached the maximum count */
  if ((barrier->barrier.arrivers_count+1) == barrier->barrier.count) {
    if (barrier->barrier.arrivers_count > 0) {
      barrier->barrier.wakeup = ECL_BARRIER_WAKEUP_NORMAL;
      ecl_cond_var_broadcast(&barrier->barrier.cv);
    }
    ecl_mutex_unlock(&barrier->barrier.mutex);
    ecl_bds_unwind1(the_env);
    ecl_check_pending_interrupts(the_env);
    return @':unblocked';
  }
  /* barrier is neither disabled nor unblocked, start waiting */
  barrier->barrier.arrivers_count++;
  ECL_UNWIND_PROTECT_BEGIN(the_env) {
    do {
      ECL_SETQ(the_env, ECL_INTERRUPTS_ENABLED, ECL_T);
      ecl_cond_var_wait(&barrier->barrier.cv, &barrier->barrier.mutex);
      ECL_SETQ(the_env, ECL_INTERRUPTS_ENABLED, ECL_NIL);
    } while(!barrier->barrier.wakeup);
    wakeup = barrier->barrier.wakeup;
    if (barrier->barrier.arrivers_count - 1 == 0) {
      /* we are the last thread to wake up, reset the barrier */
      barrier->barrier.wakeup = 0;
    }
  } ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT {
    --barrier->barrier.arrivers_count;
    ecl_mutex_unlock(&barrier->barrier.mutex);
    if (wakeup == ECL_BARRIER_WAKEUP_KILL) {
      mp_exit_process();
    }
  } ECL_UNWIND_PROTECT_THREAD_SAFE_END;
  ecl_bds_unwind1(the_env);
  ecl_check_pending_interrupts(the_env);
  return ECL_T;
}

