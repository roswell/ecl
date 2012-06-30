/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    condition_variable.d -- Native threads.
*/
/*
    Copyright (c) 2003, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <ecl/internal.h>

/*----------------------------------------------------------------------
 * CONDITION VARIABLES
 */

cl_object
mp_make_condition_variable(void)
{
	cl_object output = ecl_alloc_object(t_condition_variable);
	output->condition_variable.queue_list = ECL_NIL;
	output->condition_variable.queue_spinlock = ECL_NIL;
	output->condition_variable.lock = ECL_NIL;
	@(return output)
}

static cl_object
condition_variable_wait(cl_env_ptr env, cl_object cv)
{
	cl_object own_process = env->own_process;
	if (own_process->process.waiting_for != cv) {
		/* We have been signaled */
		cl_object lock = cv->condition_variable.lock;
		if (mp_get_lock_nowait(lock) != ECL_NIL)
			return ECL_T;
		own_process->process.waiting_for = cv;
	}
	return ECL_NIL;
}

cl_object
mp_condition_variable_wait(cl_object cv, cl_object lock)
{
        cl_env_ptr env = ecl_process_env();
	cl_object own_process = env->own_process;
	unlikely_if (type_of(cv) != t_condition_variable) {
                FEwrong_type_nth_arg(@[mp::condition-variable-wait], 1, cv,
                                     @[mp::condition-variable]);
	}
	unlikely_if (type_of(lock) != t_lock) {
                FEwrong_type_nth_arg(@[mp::condition-variable-wait], 2, lock,
                                     @[mp::lock]);
	}
        unlikely_if (cv->condition_variable.lock != ECL_NIL &&
		     cv->condition_variable.lock != lock)
	{
                FEerror("Attempt to associate lock ~A~%with condition variable ~A,"
			"~%which is already associated to lock ~A", 2, lock,
			cv, cv->condition_variable.lock);
        }
        unlikely_if (lock->lock.owner != own_process) {
                FEerror("Attempt to wait on a condition variable using lock~%~S"
                        "~%which is not owned by process~%~S", 2, lock, own_process);
        }
        unlikely_if (lock->lock.counter > 1) {
                FEerror("mp:condition-variable-wait can not be used with recursive"
                        " locks:~%~S", 1, lock);
        }
	env->own_process->process.waiting_for = cv;
	mp_giveup_lock(cv->condition_variable.lock = lock);
	ecl_wait_on(env, condition_variable_wait, cv);
	@(return ECL_T)
}

cl_object
mp_condition_variable_timedwait(cl_object cv, cl_object lock, cl_object seconds)
{
	FEerror("Timed condition variables are not supported.", 0);
}

cl_object
mp_condition_variable_signal(cl_object cv)
{
	ecl_wakeup_waiters(ecl_process_env(), cv,
			   ECL_WAKEUP_RESET_FLAG | ECL_WAKEUP_ONE);
	@(return ECL_T)
}

cl_object
mp_condition_variable_broadcast(cl_object cv)
{
	ecl_wakeup_waiters(ecl_process_env(), cv,
			   ECL_WAKEUP_RESET_FLAG | ECL_WAKEUP_ALL);
	@(return ECL_T)
}
