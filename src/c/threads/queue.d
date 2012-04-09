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
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif
#include <signal.h>
#include <ecl/ecl.h>
#include <ecl/internal.h>

void ECL_INLINE
ecl_process_yield()
{
#if defined(HAVE_SCHED_H)
	sched_yield();
#elif defined(ECL_WINDOWS_THREADS)
	Sleep(0);
#else
	ecl_musleep(0.0, 1);*/
#endif
}

void ECL_INLINE
ecl_get_spinlock(cl_env_ptr the_env, cl_object *lock)
{
	cl_object own_process = the_env->own_process;
	while (!AO_compare_and_swap_full((AO_t*)lock, (AO_t)Cnil,
					 (AO_t)own_process)) {
		ecl_process_yield();
	}
}

void ECL_INLINE
ecl_giveup_spinlock(cl_object *lock)
{
	AO_store((AO_t*)lock, (AO_t)Cnil);
}

static ECL_INLINE void
wait_queue_nconc(cl_env_ptr the_env, cl_object q, cl_object new_tail)
{
	ecl_get_spinlock(the_env, &q->queue.spinlock);
	q->queue.list = ecl_nconc(q->queue.list, new_tail);
	ecl_giveup_spinlock(&q->queue.spinlock);
}

static ECL_INLINE cl_object
wait_queue_pop_all(cl_env_ptr the_env, cl_object q)
{
	cl_object output;
	ecl_disable_interrupts_env(the_env);
	{
		ecl_get_spinlock(the_env, &q->queue.spinlock);
		output = q->queue.list;
		q->queue.list = Cnil;
		ecl_giveup_spinlock(&q->queue.spinlock);
	}
	ecl_enable_interrupts_env(the_env);
	return output;
}

static ECL_INLINE cl_object
wait_queue_first_one(cl_env_ptr the_env, cl_object q)
{
	cl_object output;
	ecl_disable_interrupts_env(the_env);
	ecl_get_spinlock(the_env, &q->queue.spinlock);
	{
		output = q->queue.list;
		if (output != Cnil)
			output = ECL_CONS_CAR(output);
	}
	ecl_giveup_spinlock(&q->queue.spinlock);
	ecl_enable_interrupts_env(the_env);
	return output;
}

static ECL_INLINE void
wait_queue_delete(cl_env_ptr the_env, cl_object q, cl_object item)
{
	ecl_get_spinlock(the_env, &q->queue.spinlock);
	q->queue.list = ecl_delete_eq(item, q->queue.list);
	ecl_giveup_spinlock(&q->queue.spinlock);
}

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
		time = ecl_to_double(delta_big) * 1.5;
	} else {
		time = 0.10;
	}
	_ecl_big_register_free(delta_big);
	return time;
}

static cl_object
ecl_wait_on_timed(cl_env_ptr env, cl_object (*condition)(cl_env_ptr, cl_object), cl_object o)
{
	volatile const cl_env_ptr the_env = env;
	volatile cl_object own_process = the_env->own_process;
	volatile cl_object record;
	volatile cl_object output;
	cl_fixnum iteration = 0;
	struct ecl_timeval start;
	ecl_get_internal_real_time(&start);

	/* This spinlock is here because the default path (fair) is
	 * too slow */
	for (iteration = 0; iteration < 10; iteration++) {
		cl_object output = condition(the_env,o);
		if (output != Cnil)
			return output;
	}

	/* 0) We reserve a record for the queue. In order to avoid
	 * using the garbage collector, we reuse records */
	record = own_process->process.queue_record;
	unlikely_if (record == Cnil) {
		record = ecl_list1(own_process);
	} else {
		own_process->process.queue_record = Cnil;
	}

	ecl_bds_bind(the_env, @'ext::*interrupts-enabled*', Cnil);
	CL_UNWIND_PROTECT_BEGIN(the_env) {
		/* 2) Now we add ourselves to the queue. In order to
		 * avoid a call to the GC, we try to reuse records. */
		wait_queue_nconc(the_env, o, record);
		ecl_bds_bind(the_env, @'ext::*interrupts-enabled*', Ct);
		ecl_check_pending_interrupts(the_env);

		/* 3) Unlike the sigsuspend() implementation, this
		 * implementation does not block signals and the
		 * wakeup event might be lost before the sleep
		 * function is invoked. We must thus spin over short
		 * intervals of time to ensure that we check the
		 * condition periodically. */
		do {
			ecl_musleep(waiting_time(iteration++, &start), 1);
		} while (Null(output = condition(the_env, o)));
		ecl_bds_unwind1(the_env);
	} CL_UNWIND_PROTECT_EXIT {
		/* 4) At this point we wrap up. We remove ourselves
		 * from the queue and restore signals, which were
		 * blocked. */
		wait_queue_delete(the_env, o, own_process);
		own_process->process.waiting_for = Cnil;
		own_process->process.queue_record = record;
		ECL_RPLACD(record, Cnil);
	} CL_UNWIND_PROTECT_END;
	ecl_bds_unwind1(the_env);
	return output;
}


cl_object
ecl_wait_on(cl_env_ptr env, cl_object (*condition)(cl_env_ptr, cl_object), cl_object o)
{
#if defined(HAVE_SIGPROCMASK)
	volatile const cl_env_ptr the_env = env;
	volatile cl_object own_process = the_env->own_process;
	volatile cl_object record;
	volatile sigset_t original;
	volatile int aborting = 1;
	volatile cl_object output;

	/* 0) We reserve a record for the queue. In order to avoid
	 * using the garbage collector, we reuse records */
	record = own_process->process.queue_record;
	unlikely_if (record == Cnil) {
		record = ecl_list1(own_process);
	} else {
		own_process->process.queue_record = Cnil;
	}

	/* 1) First we block all signals. */
	{
		sigset_t empty;
		sigemptyset(&empty);
		sigaddset(&empty, ecl_option_values[ECL_OPT_THREAD_INTERRUPT_SIGNAL]);
		pthread_sigmask(SIG_BLOCK, &empty, &original);
	}

	/* 2) Now we add ourselves to the queue. In order to avoid a
	 * call to the GC, we try to reuse records. */
	wait_queue_nconc(the_env, o, record);

	CL_UNWIND_PROTECT_BEGIN(the_env) {
		/* 3) At this point we may receive signals, but we
		 * might have missed a wakeup event if that happened
		 * between 0) and 2), which is why we start with the
		 * check*/
		if (ECL_CONS_CAR(o->queue.list) != own_process ||
		    Null(output = condition(the_env, o)))
		{
			print_lock("suspending %p", o, o);
			do {
				/* This will wait until we get a signal that
				 * demands some code being executed. Note that
				 * this includes our communication signals and
				 * the signals used by the GC. Note also that
				 * as a consequence we might throw / return
				 * which is why need to protect it all with
				 * UNWIND-PROTECT. */
				sigsuspend(&original);
			} while (Null(output = condition(the_env, o)));
		}
		aborting = 0;
	} CL_UNWIND_PROTECT_EXIT {
		/* 4) If we are aborting and we are the first waiting
		 * process in the queue, it may happen that our wakeup
		 * signal got lost We must wake up another process
		 * after removing ourselves. */
		cl_object firstone = o->queue.list;

		/* 5) At this point we wrap up. We remove ourselves
		   from the queue and restore signals, which were */
		wait_queue_delete(the_env, o, own_process);
		own_process->process.waiting_for = Cnil;
		own_process->process.queue_record = record;
		ECL_RPLACD(record, Cnil);

		/* 6) ... we continue wat was started in 4) */
		if (aborting && (firstone == record)) {
			ecl_wakeup_waiters(the_env, o, 0);
		}

		/* 7) Restoring signals is done last, to ensure that
		   all cleanup steps are performed. */
		pthread_sigmask(SIG_SETMASK, &original, NULL);
	} CL_UNWIND_PROTECT_END;
	return output;
#else
	return ecl_wait_on_timed(env, condition, o);
#endif
}

static void
wakeup_this(cl_object p, int flags)
{
	if (flags & ECL_WAKEUP_RESET_FLAG)
		p->process.waiting_for = Cnil;
	print_lock("awaking %p", p, p);
	if (flags & ECL_WAKEUP_KILL)
		mp_process_kill(p);
	else
		ecl_interrupt_process(p, Cnil);
}

static void
wakeup_all(cl_env_ptr the_env, cl_object q, int flags)
{
	cl_object queue = wait_queue_pop_all(the_env, q);
	while (!Null(queue)) {
		cl_object process = ECL_CONS_CAR(queue);
		queue = ECL_CONS_CDR(queue);
		if (process->process.phase != ECL_PROCESS_INACTIVE)
			wakeup_this(process, flags);
	}
}

static void
wakeup_one(cl_env_ptr the_env, cl_object q, int flags)
{
	do {
		cl_object next = wait_queue_first_one(the_env, q);
		if (Null(next)) {
			print_lock("no process to awake", q);
			return;
		}
		print_lock("awaking %p", q, next);
		if (next->process.phase != ECL_PROCESS_INACTIVE) {
			wakeup_this(next, flags);
			return;
		}
	} while (1);
}

void
ecl_wakeup_waiters(cl_env_ptr the_env, cl_object q, int flags)
{
	if (q->queue.list != Cnil) {
		if (flags & ECL_WAKEUP_ALL) {
			wakeup_all(the_env, q, flags);
		} else {
			wakeup_one(the_env, q, flags);
		}
	}
	ecl_process_yield();
}

#undef print_lock

void
print_lock(char *prefix, cl_object l, ...)
{
	static cl_object lock = Cnil;
	va_list args;
	va_start(args, l);
	return;
	if (l == Cnil || FIXNUMP(l->lock.name)) {
		cl_env_ptr env = ecl_process_env();
		ecl_get_spinlock(env, &lock);
		printf("\n%ld\t", fix(env->own_process->process.name));
		vprintf(prefix, args);
		if (l != Cnil) {
			cl_object p = l->lock.queue_list;
			while (p != Cnil) {
				printf(" %lx", fix(ECL_CONS_CAR(p)->process.name));
				p = ECL_CONS_CDR(p);
			}
		}
		fflush(stdout);
		ecl_giveup_spinlock(&lock);
	}
}
/*#define print_lock(a,b,c) (void)0*/
