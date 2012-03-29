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

#define print_lock(a,b,...) (void)0

#ifndef HAVE_SCHED_H
static int
sched_yield()
{
	ecl_musleep(0.0, 1);
}
#endif

void ECL_INLINE
ecl_get_spinlock(cl_env_ptr the_env, cl_object *lock)
{
	cl_object own_process = the_env->own_process;
	while (!AO_compare_and_swap_full((AO_t*)lock, (AO_t)Cnil,
					 (AO_t)own_process)) {
		sched_yield();
	}
}

void ECL_INLINE
ecl_giveup_spinlock(cl_object *lock)
{
	*lock = Cnil;
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

void
ecl_wait_on(cl_object (*condition)(cl_env_ptr, cl_object), cl_object o)
{
	const cl_env_ptr the_env = ecl_process_env();
	volatile cl_object own_process = the_env->own_process;
	volatile cl_object record;
	volatile sigset_t original;

	/* 0) We reserve a record for the queue. In order to a void
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
		sigaddset(&empty, ecl_option_values[ECL_OPT_TRAP_INTERRUPT_SIGNAL]);
		pthread_sigmask(SIG_BLOCK, &original, &empty);
	}

	/* 2) Now we add ourselves to the queue. In order to avoid a
	 * call to the GC, we try to reuse records. */
	wait_queue_nconc(the_env, o, record);
	own_process->process.waiting_for = o;

	CL_UNWIND_PROTECT_BEGIN(the_env) {
		/* 3) At this point we may receive signals, but we
		 * might have missed a wakeup event if that happened
		 * between 0) and 2), which is why we start with the
		 * check*/
		if (ECL_CONS_CAR(o->queue.list) != own_process ||
		    condition(the_env, o) == Cnil)
		{
			do {
				/* This will wait until we get a signal that
				 * demands some code being executed. Note that
				 * this includes our communication signals and
				 * the signals used by the GC. Note also that
				 * as a consequence we might throw / return
				 * which is why need to protect it all with
				 * UNWIND-PROTECT. */
				sigsuspend(&original);
			} while (condition(the_env, o) == Cnil);
		}
	} CL_UNWIND_PROTECT_EXIT {
		/* 4) At this point we wrap up. We remove ourselves
		   from the queue and restore signals, which were */
		own_process->process.waiting_for = Cnil;
		wait_queue_delete(the_env, o, own_process);
		own_process->process.queue_record = record;
		ECL_RPLACD(record, Cnil);
		pthread_sigmask(SIG_SETMASK, NULL, &original);
	} CL_UNWIND_PROTECT_END;
}

static void
wakeup_this(cl_object p, int flags)
{
	if (flags & ECL_WAKEUP_RESET_FLAG)
		p->process.waiting_for = Cnil;
	print_lock("awaking\t\t%d", Cnil, fix(p->process.name));
	ecl_interrupt_process(p, Cnil);
}

static void
wakeup_all(cl_env_ptr the_env, cl_object q, int flags)
{
	cl_object queue = wait_queue_pop_all(the_env, q);
	queue = cl_nreverse(queue);
	while (!Null(queue)) {
		cl_object process = ECL_CONS_CAR(queue);
		queue = ECL_CONS_CDR(queue);
		if (process->process.active)
			wakeup_this(process, flags);
	}
}

static void
wakeup_one(cl_env_ptr the_env, cl_object q, int flags)
{
	do {
		cl_object next = q->queue.list;
		if (Null(next))
			return;
		next = ECL_CONS_CAR(next);
		if (next->process.active) {
			wakeup_this(next, flags);
			return;
		}
	} while (1);
}

void
ecl_wakeup_waiters(cl_env_ptr the_env, cl_object q, int flags)
{
	print_lock("releasing\t", o);
	if (q->queue.list != Cnil) {
		if (flags & ECL_WAKEUP_ALL) {
			wakeup_all(the_env, q, flags);
		} else {
			wakeup_one(the_env, q, flags);
		}
	}
	sched_yield();
}

#undef print_lock

void
print_lock(char *prefix, cl_object l, ...)
{
	static cl_object lock = Cnil;
	va_list args;
	va_start(args, lock);
	if (l == Cnil || l->lock.name == MAKE_FIXNUM(0)) {
		cl_env_ptr env = ecl_process_env();
		ecl_get_spinlock(env, &lock);
		printf("\n%d\t", fix(env->own_process->process.name));
		vprintf(prefix, args);
		if (l != Cnil) {
			cl_object p = l->lock.queue_list;
			while (p != Cnil) {
				printf(" %d", fix(ECL_CONS_CAR(p)->process.name));
				p = ECL_CONS_CDR(p);
			}
		}
		fflush(stdout);
		ecl_giveup_spinlock(&lock);
	}
}
/*#define print_lock(a,b,c) (void)0*/
