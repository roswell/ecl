/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    queue.d -- waiting queue for threads.
*/
/*
    Copyright (c) 2011, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#define AO_ASSUME_WINDOWS98 /* We need this for CAS */
#include <ecl/ecl.h>
#include <ecl/internal.h>

/*----------------------------------------------------------------------
 * THREAD SCHEDULER & WAITING
 */

static cl_object
bignum_set_time(cl_object bignum, struct ecl_timeval *time)
{
	_ecl_big_set_index(bignum, time->tv_sec);
	_ecl_big_mul_ui(bignum, bignum, 1000);
	_ecl_big_add_ui(bignum, bignum, (time->tv_usec + 999) / 1000);
	return bignum;
}

static cl_object
elapsed_time(struct ecl_timeval *start)
{
	cl_object delta_big = _ecl_big_register0();
	cl_object aux_big = _ecl_big_register1();
	struct ecl_timeval now;
	ecl_get_internal_real_time(&now);
	bignum_set_time(aux_big, start);
	bignum_set_time(delta_big, &now);
	_ecl_big_sub(delta_big, delta_big, aux_big);
	_ecl_big_register_free(aux_big);
	return delta_big;
}

static double
waiting_time(cl_index iteration, struct ecl_timeval *start)
{
	/* Waiting time is smaller than 0.10 s */
	double time;
	cl_object top = MAKE_FIXNUM(10 * 1000);
	cl_object delta_big = elapsed_time(start);
	_ecl_big_div_ui(delta_big, delta_big, iteration);
	if (ecl_number_compare(delta_big, top) < 0) {
		time = ecl_to_double(delta_big);
	} else {
		time = 0.10;
	}
	_ecl_big_register_free(delta_big);
	return time;
}

void
ecl_wait_on(cl_object (*condition)(cl_env_ptr, cl_object), cl_object o)
{
	cl_env_ptr env = ecl_process_env();
	cl_fixnum iteration = 0;
	struct ecl_timeval start;
	ecl_get_internal_real_time(&start);
	/* Fast spinlock */
	for (iteration = 0; iteration < 10; iteration++) {
		if (condition(env, o) != Cnil)
			return;
	}
	/* Slow path */
	{
		cl_object process = env->own_process;
		ecl_bds_bind(env, @'ext::*interrupts-enabled*', Cnil);
		CL_UNWIND_PROTECT_BEGIN(env) {
			process->process.waiting_for = o;
			o->lock.waiter = process;
			ecl_bds_bind(env, @'ext::*interrupts-enabled*', Ct);
			ecl_check_pending_interrupts();
			do {
				ecl_musleep(waiting_time(iteration++, &start), 1);
			} while (condition(env, o) == Cnil);
			ecl_bds_unwind1(env);
		} CL_UNWIND_PROTECT_EXIT {
			process->process.waiting_for = Cnil;
		} CL_UNWIND_PROTECT_END;
		ecl_bds_unwind1(env);
	}
}

static void
wakeup_process(cl_object p)
{
	mp_interrupt_process(p, @'+');
}

void
ecl_wakeup_waiters(cl_object o, int flags)
{
	cl_object v = cl_core.processes;
	cl_index size = v->vector.fillp;
	cl_index i = size;
	cl_index ndx = rand() % size;
	while (i--) {
		cl_object p = v->vector.self.t[ndx];
		if (!Null(p) && p->process.waiting_for == o && p->process.active == 1) {
			if (flags & ECL_WAKEUP_RESET_FLAG)
				p->process.waiting_for = Cnil;
			wakeup_process(p);
			if (flags & ECL_WAKEUP_ALL == 0)
				return;
		}
		if (++ndx >= size)
			ndx = 0;
	}
}
