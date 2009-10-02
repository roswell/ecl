/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    threads.d -- Native threads.
*/
/*
    Copyright (c) 2003, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifndef __sun__ /* See unixinit.d for this */
#define _XOPEN_SOURCE 600	/* For pthread mutex attributes */
#endif
#if defined(_MSC_VER) || defined(mingw32)
# include <windows.h>
#else
# include <pthread.h>
#endif
#include <errno.h>
#include <time.h>
#include <signal.h>
#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#include <ecl/internal.h>
#ifdef HAVE_GETTIMEOFDAY
# include <sys/time.h>
#endif
#ifdef HAVE_SCHED_YIELD
# include <sched.h>
#endif
#ifdef HAVE_SEMAPHORE_H
# include <semaphore.h>
#endif

#ifdef ECL_WINDOWS_THREADS
/*
 * We have to put this explicit definition here because Boehm GC
 * is designed to produce a DLL and we rather want a static
 * reference
 */
# include <gc.h>
extern HANDLE WINAPI GC_CreateThread(
    LPSECURITY_ATTRIBUTES lpThreadAttributes, 
    DWORD dwStackSize, LPTHREAD_START_ROUTINE lpStartAddress, 
    LPVOID lpParameter, DWORD dwCreationFlags, LPDWORD lpThreadId );
# ifndef WITH___THREAD
DWORD cl_env_key;
# endif
#else
# ifndef WITH___THREAD
static pthread_key_t cl_env_key;
# endif
#endif /* ECL_WINDOWS_THREADS */

extern void ecl_init_env(struct cl_env_struct *env);

#if !defined(WITH___THREAD)
cl_env_ptr
ecl_process_env(void)
{
#ifdef ECL_WINDOWS_THREADS
	return TlsGetValue(cl_env_key);
#else
	struct cl_env_struct *rv = pthread_getspecific(cl_env_key);
        if (rv)
		return rv;
	FElibc_error("pthread_getspecific() failed.", 0);
	return NULL;
#endif
}
#endif

static void
ecl_set_process_env(cl_env_ptr env)
{
#ifdef WITH___THREAD
	cl_env_p = env;
#else
# ifdef ECL_WINDOWS_THREADS
	TlsSetValue(cl_env_key, env);
# else
	if (pthread_setspecific(cl_env_key, env))
		FElibc_error("pthread_setcspecific() failed.", 0);
# endif
#endif
}

cl_object
mp_current_process(void)
{
	return ecl_process_env()->own_process;
}

/*----------------------------------------------------------------------
 * THREAD OBJECT
 */

static void
assert_type_process(cl_object o)
{
	if (type_of(o) != t_process)
		FEwrong_type_argument(@'mp::process', o);
}

static void
thread_cleanup(void *aux)
{
	/* This routine performs some cleanup before a thread is completely
	 * killed. For instance, it has to remove the associated process
	 * object from the list, an it has to dealloc some memory.
	 *
	 * NOTE: thread_cleanup() does not provide enough "protection". In
	 * order to ensure that all UNWIND-PROTECT forms are properly
	 * executed, never use pthread_cancel() to kill a process, but
	 * rather use the lisp functions mp_interrupt_process() and
	 * mp_process_kill().
	 */
	cl_object process = (cl_object)aux;
	process->process.active = 0;
	mp_giveup_lock(process->process.exit_lock);
	THREAD_OP_LOCK();
	cl_core.processes = ecl_remove_eq(process, cl_core.processes);
	THREAD_OP_UNLOCK();
	if (process->process.env)
		_ecl_dealloc_env(process->process.env);
	process->process.env = NULL;
}

#ifdef ECL_WINDOWS_THREADS
static DWORD WINAPI thread_entry_point(void *arg)
#else
static void *
thread_entry_point(void *arg)
#endif
{
        cl_object process = (cl_object)arg;
	cl_env_ptr env;

#ifndef ECL_WINDOWS_THREADS
	pthread_cleanup_push(thread_cleanup, (void *)process);
#endif
	/* 1) Setup the environment for the execution of the thread */
	process->process.env = env = _ecl_alloc_env();
	env->own_process = process;
	ecl_set_process_env(env);
	THREAD_OP_LOCK();
	cl_core.processes = CONS(process, cl_core.processes);
	THREAD_OP_UNLOCK();
	ecl_init_env(env);
	env->bindings_hash = process->process.initial_bindings;
	ecl_enable_interrupts_env(env);
        env->trap_fpe_bits = process->process.trap_fpe_bits;
        si_trap_fpe(@'last', Ct);

	/* 2) Execute the code. The CATCH_ALL point is the destination
	*     provides us with an elegant way to exit the thread: we just
	*     do an unwind up to frs_top.
	*/
	mp_get_lock(1, process->process.exit_lock);
	process->process.active = 1;
	CL_CATCH_ALL_BEGIN(env) {
		ecl_bds_bind(env, @'mp::*current-process*', process);
		cl_apply(2, process->process.function, process->process.args);
		ecl_bds_unwind1(env);
	} CL_CATCH_ALL_END;

	/* 4) If everything went right, we should be exiting the thread
	 *    through this point. thread_cleanup is automatically invoked
	 *    marking the process as inactive.
	 */
#ifdef ECL_WINDOWS_THREADS
	thread_cleanup(process);
	return 1;
#else
	pthread_cleanup_pop(1);
	return NULL;
#endif
}

static cl_object
alloc_process(cl_object name, cl_object initial_bindings)
{
	cl_object process = ecl_alloc_object(t_process);
	process->process.active = 0;
	process->process.name = name;
	process->process.function = Cnil;
	process->process.args = Cnil;
	process->process.interrupt = Cnil;
	process->process.env = NULL;
	if (initial_bindings != OBJNULL) {
		process->process.initial_bindings
			= cl__make_hash_table(@'eq', MAKE_FIXNUM(1024),
					      ecl_make_singlefloat(1.5),
					      ecl_make_singlefloat(0.7),
					      Cnil); /* no need for locking */
	} else {
		cl_env_ptr this_env = ecl_process_env();
		process->process.initial_bindings
			= si_copy_hash_table(this_env->bindings_hash);
	}
	process->process.exit_lock = mp_make_lock(0);
	return process;
}

bool
ecl_import_current_thread(cl_object name, cl_object bindings)
{
	cl_object process, l;
	pthread_t current;
	cl_env_ptr env;
#ifdef ECL_WINDOWS_THREADS
	{
	HANDLE aux = GetCurrentThread();
	DuplicateHandle(GetCurrentProcess(),
			aux,
			GetCurrentProcess(),
			&current,
			0,
			FALSE,
			DUPLICATE_SAME_ACCESS);
	CloseHandle(current);
	}
#else
	current = pthread_self();
#endif
#ifdef GBC_BOEHM
	GC_register_my_thread((void*)&name);
#endif
	for (l = cl_core.processes; l != Cnil; l = ECL_CONS_CDR(l)) {
		cl_object p = ECL_CONS_CAR(l);
		if (p->process.thread == current) {
			return 0;
		}
	}
	env = _ecl_alloc_env();
	ecl_set_process_env(env);
	env->own_process = process = alloc_process(name, bindings);
	process->process.active = 2;
	process->process.thread = current;
	process->process.env = env;
	THREAD_OP_LOCK();
	cl_core.processes = CONS(process, cl_core.processes);
	THREAD_OP_UNLOCK();
	ecl_init_env(env);
	env->bindings_hash = process->process.initial_bindings;
	mp_get_lock(1, process->process.exit_lock);
	process->process.active = 1;
	ecl_enable_interrupts_env(env);
	return 1;
}

void
ecl_release_current_thread(void)
{
	thread_cleanup(ecl_process_env()->own_process);
#ifdef GBC_BOEHM
	GC_unregister_my_thread();
#endif
}

@(defun mp::make-process (&key name ((:initial-bindings initial_bindings) Ct))
	cl_object process;
@
	process = alloc_process(name, initial_bindings);
	@(return process)
@)

cl_object
mp_process_preset(cl_narg narg, cl_object process, cl_object function, ...)
{
	cl_va_list args;
	cl_va_start(args, function, narg, 2);
	if (narg < 2)
		FEwrong_num_arguments(@'mp::process-preset');
	assert_type_process(process);
	process->process.function = function;
	process->process.args = cl_grab_rest_args(args);
	@(return process)
}

cl_object
mp_interrupt_process(cl_object process, cl_object function)
{
	if (mp_process_active_p(process) == Cnil)
		FEerror("Cannot interrupt the inactive process ~A", 1, process);
        ecl_interrupt_process(process, function);
	@(return Ct)
}

cl_object
mp_suspend_loop()
{
        cl_env_ptr env = ecl_process_env();
        CL_CATCH_BEGIN(env,@'mp::suspend-loop') {
                for ( ; ; ) {
                        cl_sleep(MAKE_FIXNUM(100));
                }
        } CL_CATCH_END;
}

cl_object
mp_break_suspend_loop()
{
        if (frs_sch(@'mp::suspend-loop')) {
                cl_throw(@'mp::suspend-loop');
        }
}

cl_object
mp_process_suspend(cl_object process)
{
        mp_interrupt_process(process, @'mp::suspend-loop');
}

cl_object
mp_process_resume(cl_object process)
{
        mp_interrupt_process(process, @'mp::break-suspend-loop');
}

cl_object
mp_process_kill(cl_object process)
{
	return mp_interrupt_process(process, @'mp::exit-process');
}

cl_object
mp_process_yield(void)
{
#ifdef HAVE_SCHED_YIELD
	sched_yield();
#else
# ifdef ECL_WINDOWS_THREADS
	Sleep(0);
# else
	sleep(0); /* Use sleep(0) to yield to a >= priority thread */
# endif
#endif
	@(return)
}

cl_object
mp_process_enable(cl_object process)
{
	cl_object output;
#ifdef ECL_WINDOWS_THREADS
	HANDLE code;
	DWORD threadId;

	if (mp_process_active_p(process) != Cnil)
		FEerror("Cannot enable the running process ~A.", 1, process);
        process->process.parent = mp_current_process();
	process->process.trap_fpe_bits =
		process->process.parent->process.env->trap_fpe_bits;
	code = (HANDLE)CreateThread(NULL, 0, thread_entry_point, process, 0, &threadId);
	output = (process->process.thread = code)? process : Cnil;
#else
	int code;
        pthread_attr_t pthreadattr;

	pthread_attr_init(&pthreadattr);
	pthread_attr_setdetachstate(&pthreadattr, PTHREAD_CREATE_DETACHED);
	if (mp_process_active_p(process) != Cnil)
		FEerror("Cannot enable the running process ~A.", 1, process);
        process->process.parent = mp_current_process();
	/*
	 * We launch the thread with the signal mask specified in cl_core.
	 * The reason is that we might need to block certain signals
	 * to be processed by the signal handling thread in unixint.d
	 */
#ifdef HAVE_SIGPROCMASK
	{
		sigset_t previous;
		pthread_sigmask(SIG_SETMASK, cl_core.default_sigmask, &previous);
		code = pthread_create(&process->process.thread, &pthreadattr,
				      thread_entry_point, process);
		pthread_sigmask(SIG_SETMASK, &previous, NULL);
	}
#else
	code = pthread_create(&process->process.thread, &pthreadattr, thread_entry_point, process);
#endif
	output = code? Cnil : process;
#endif
	@(return output)
}

cl_object
mp_exit_process(void)
{
	/* We simply undo the whole of the frame stack. This brings up
	   back to the thread entry point, going through all possible
	   UNWIND-PROTECT.
	*/
	const cl_env_ptr env = ecl_process_env();
	ecl_unwind(env, env->frs_org);
}

cl_object
mp_all_processes(void)
{
	/* No race condition here because this list is never destructively
	 * modified. When we add or remove processes, we create new lists. */
	@(return cl_copy_list(cl_core.processes))
}

cl_object
mp_process_name(cl_object process)
{
	assert_type_process(process);
	@(return process->process.name)
}

cl_object
mp_process_active_p(cl_object process)
{
	assert_type_process(process);
	@(return (process->process.active? Ct : Cnil))
}

cl_object
mp_process_whostate(cl_object process)
{
	assert_type_process(process);
	@(return (cl_core.null_string))
}

cl_object
mp_process_join(cl_object process)
{
	assert_type_process(process);
	/* We only wait for threads that we have created */
        if (process->process.active != 1) {
		cl_object l = process->process.exit_lock;
		if (!Null(l)) {
			l = mp_get_lock(1, l);
			if (Null(l)) {
				FEerror("MP:PROCESS-JOIN: Error when "
					"joining process ~A",
					1, process);
			}
			mp_giveup_lock(l);
                }
        }
        @(return Cnil)
}

cl_object
mp_process_run_function(cl_narg narg, cl_object name, cl_object function, ...)
{
	cl_object process;
	cl_va_list args;
	cl_va_start(args, function, narg, 2);
	if (narg < 2)
		FEwrong_num_arguments(@'mp::process-run-function');
	if (CONSP(name)) {
		process = cl_apply(2, @'mp::make-process', name);
	} else {
		process = mp_make_process(2, @':name', name);
	}
	cl_apply(4, @'mp::process-preset', process, function,
		 cl_grab_rest_args(args));
	return mp_process_enable(process);
}

/*----------------------------------------------------------------------
 * INTERRUPTS
 */

#ifndef ECL_WINDOWS_THREADS
static cl_object
mp_get_sigmask(void)
{
        cl_object data = ecl_alloc_simple_vector(sizeof(sigset_t), aet_b8);
        sigset_t *mask_ptr = (sigset_t*)data->vector.self.b8;
        sigset_t no_signals;
        sigemptyset(&no_signals);
        if (pthread_sigmask(SIG_BLOCK, &no_signals, mask_ptr))
                FElibc_error("MP:GET-SIGMASK failed in a call to pthread_sigmask", 0);
        @(return data)
}

static cl_object
mp_set_sigmask(cl_object data)
{
        sigset_t *mask_ptr = (sigset_t*)data->vector.self.b8;
        if (pthread_sigmask(SIG_SETMASK, mask_ptr, NULL))
                FElibc_error("MP:SET-SIGMASK failed in a call to pthread_sigmask", 0);
        @(return data)
}
#endif

cl_object
mp_block_signals(void)
{
#ifdef ECL_WINDOWS_THREADS
        cl_env_ptr the_env = ecl_process_env();
        cl_object previous = ecl_symbol_value(@'si::*interrupts-enabled*');
        ECL_SETQ(the_env, @'si::*interrupts-enabled*', Cnil);
        @(return previous)
#else
        cl_object previous = mp_get_sigmask();
        sigset_t all_signals;
        sigfillset(&all_signals);
        if (pthread_sigmask(SIG_SETMASK, &all_signals, NULL))
                FElibc_error("MP:BLOCK-SIGNALS failed in a call to pthread_sigmask",0);
        @(return previous)
#endif
}

cl_object
mp_restore_signals(cl_object sigmask)
{
#ifdef ECL_WINDOWS_THREADS
        cl_env_ptr the_env = ecl_process_env();
        ECL_SETQ(the_env, @'si::*interrupts-enabled*', sigmask);
        ecl_check_pending_interrupts();
        @(return sigmask)
#else
        return mp_set_sigmask(sigmask);
#endif
}

/*----------------------------------------------------------------------
 * CONDITION VARIABLES
 */

cl_object
mp_make_condition_variable(void)
{
#ifdef ECL_WINDOWS_THREADS
	FEerror("Condition variables are not supported under Windows.", 0);
	@(return Cnil)
#else
	cl_object output;

	output = ecl_alloc_object(t_condition_variable);
	pthread_cond_init(&output->condition_variable.cv, NULL);
	si_set_finalizer(output, Ct);
	@(return output)
#endif
}

cl_object
mp_condition_variable_wait(cl_object cv, cl_object lock)
{
#ifdef ECL_WINDOWS_THREADS
	FEerror("Condition variables are not supported under Windows.", 0);
#else
        int count, rc;
        cl_object own_process = mp_current_process();
	if (type_of(cv) != t_condition_variable)
		FEwrong_type_argument(@'mp::condition-variable', cv);
	if (type_of(lock) != t_lock)
		FEwrong_type_argument(@'mp::lock', lock);
        if (lock->lock.holder != own_process) {
                FEerror("Attempt to wait on a condition variable using lock~%~S"
                        "~%which is not owned by process~%~S", 2, lock, own_process);
        }
        if (lock->lock.counter > 1) {
                FEerror("mp:condition-variable-wait can not be used with recursive"
                        " locks:~%~S", 1, lock);
        }
        /* Note: this is highly unsafe. We are marking the lock as released
         * without knowing whether pthread_cond_wait worked as expected. */
        lock->lock.counter = 0;
        lock->lock.holder = Cnil;
	rc = pthread_cond_wait(&cv->condition_variable.cv,
                               &lock->lock.mutex);
        lock->lock.holder = own_process;
        lock->lock.counter = 1;
        if (rc != 0) {
                FEerror("System returned error code ~D "
                        "when waiting on condition variable~%~A~%and lock~%~A.",
                        3, MAKE_FIXNUM(rc), cv, lock);
        }
#endif
	@(return Ct)
}

cl_object
mp_condition_variable_timedwait(cl_object cv, cl_object lock, cl_object seconds)
{
#ifdef ECL_WINDOWS_THREADS
	FEerror("Condition variables are not supported under Windows.", 0);
#else
        int rc;
        cl_object own_process = mp_current_process();
	double r;
	struct timespec   ts;
	struct timeval    tp;

	if (type_of(cv) != t_condition_variable)
		FEwrong_type_argument(@'mp::condition-variable', cv);
	if (type_of(lock) != t_lock)
		FEwrong_type_argument(@'mp::lock', lock);
        if (lock->lock.holder != own_process) {
                FEerror("Attempt to wait on a condition variable using lock~%~S"
                        "~%which is not owned by process~%~S", 2, lock, own_process);
        }
        if (lock->lock.counter > 1) {
                FEerror("mp:condition-variable-wait can not be used with recursive"
                        " locks:~%~S", 1, lock);
        }
	/* INV: ecl_minusp() makes sure `seconds' is real */
	if (ecl_minusp(seconds))
		cl_error(9, @'simple-type-error', @':format-control',
			 make_constant_base_string("Not a non-negative number ~S"),
			 @':format-arguments', cl_list(1, seconds),
			 @':expected-type', @'real', @':datum', seconds);
	gettimeofday(&tp, NULL);
	/* Convert from timeval to timespec */
	ts.tv_sec  = tp.tv_sec;
	ts.tv_nsec = tp.tv_usec * 1000;

	/* Add `seconds' delta */
	r = ecl_to_double(seconds);
	ts.tv_sec += (time_t)floor(r);
	ts.tv_nsec += (long)((r - floor(r)) * 1e9);
	if (ts.tv_nsec >= 1e9) {
		ts.tv_nsec -= 1e9;
		ts.tv_sec++;
	}
        /* Note: this is highly unsafe. We are marking the lock as released
         * without knowing whether pthread_cond_wait worked as expected. */
        lock->lock.counter = 0;
        lock->lock.holder = Cnil;
	rc = pthread_cond_timedwait(&cv->condition_variable.cv,
                                    &lock->lock.mutex, &ts);
        lock->lock.holder = own_process;
        lock->lock.counter = 1;
        if (rc != 0 && rc != ETIMEDOUT) {
                FEerror("System returned error code ~D "
                        "when waiting on condition variable~%~A~%and lock~%~A.",
                        3, MAKE_FIXNUM(rc), cv, lock);
        }
        @(return (rc? Ct : Cnil))
#endif
}

cl_object
mp_condition_variable_signal(cl_object cv)
{
#ifdef ECL_WINDOWS_THREADS
	FEerror("Condition variables are not supported under Windows.", 0);
#else
	if (type_of(cv) != t_condition_variable)
		FEwrong_type_argument(@'mp::condition-variable', cv);
	pthread_cond_signal(&cv->condition_variable.cv);
#endif
	@(return Ct)
}

cl_object
mp_condition_variable_broadcast(cl_object cv)
{
#ifdef ECL_WINDOWS_THREADS
	FEerror("Condition variables are not supported under Windows.", 0);
#else
	if (type_of(cv) != t_condition_variable)
		FEwrong_type_argument(@'mp::condition-variable', cv);
	pthread_cond_broadcast(&cv->condition_variable.cv);
#endif
	@(return Ct)
}

/*----------------------------------------------------------------------
 * SEMAPHORES
 */

#ifdef ECL_SEMAPHORES

# ifdef ECL_MACH_SEMAPHORES
struct ecl_semaphore_inner {
        task_t owner;
        semaphore_t counter[1];
};
# endif

@(defun mp::make-semaphore (max &key name ((:count count) MAKE_FIXNUM(0)))
	cl_object output;
	cl_index initial_count, max_count;
@
{
        output = ecl_alloc_object(t_semaphore);
        ecl_disable_interrupts_env(the_env);
        output->semaphore.name = name;
        output->semaphore.handle = NULL;
        ecl_set_finalizer_unprotected(output, Ct);
        max_count = ecl_fixnum_in_range(@'mp:make-semaphore', "maximum count",
                                        max, 0, 0xFFFF);
        initial_count = ecl_fixnum_in_range(@'mp:make-semaphore', "initial count",
                                            count, 0, max_count);
#ifdef ECL_WINDOWS_THREADS
        {
                HANDLE h = CreateSemaphore(NULL,
                                           initial_count,
                                           0xFFFF,
                                           NULL);
                output->semaphore.handle = h;
                ecl_enable_interrupts_env(the_env);
                if (h == NULL)
                        FEwin32_error("Unable to create semaphore object.", 0);
        }
#else
# ifdef HAVE_SEM_INIT
        {
                sem_t *h = ecl_alloc_atomic(sizeof(sem_t));
                int rc = sem_init(h, 0, initial_count);
                if (!rc)
                        output->semaphore.handle = h;
                ecl_enable_interrupts();
                if (rc)
                        FEerror("Unable to create semaphore object.", 0);
        }
# endif /* HAVE_SEM_INIT */
#endif /* ECL_WINDOWS_THREADS */
        @(return output)
}
@)

cl_object
mp_semaphore_trywait(cl_object sem)
{
        cl_object output;
        if (typeof(sem) != t_semaphore)
                FEwrong_type_argument(@'mp::semaphore', cv);
 AGAIN:
#ifdef ECL_WINDOWS_THREADS
        {
                HANDLE h = (HANDLE)(sem->semaphore.handle);
                switch (WaitForSingleObject(h, 0)) {
		case WAIT_OBJECT_0:
                        output = Ct;
			break;
                case WAIT_TIMEOUT:
                        output = Cnil;
                        break;
		default:
			FEwin32_error("Unable to wait on semaphore", 0);
                        output = Cnil;
                }
        }
#else
# ifdef HAVE_SEM_INIT
        {
                sem_t *h = (sem_t *)(sem->semaphore.handle);
                int rc = sem_trywait(h);
                if (sem_trywait(h)) {
                        if (errno != EAGAIN) {
                                FElibc_error("Unable to wait on semaphore", 0);
                        }
                        output = Cnil;
                } else {
                        output = Ct;
                }
        }
# endif /* HAVE_SEM_INIT */
#endif /* ECL_WINDOWS_THREADS */
        @(return output)
}


cl_object
mp_semaphore_wait(cl_object sem)
{
        cl_object output;
        if (typeof(sem) != t_semaphore)
                FEwrong_type_argument(@'mp::semaphore', cv);
 AGAIN:
#ifdef ECL_WINDOWS_THREADS
        {
                HANDLE h = (HANDLE)(sem->semaphore.handle);
                if (WaitForSingleObject(h, INFINITE) != WAIT_OBJECT_0) {
			FEwin32_error("Unable to wait on semaphore", 0);
                }
        }
#else
# ifdef HAVE_SEM_INIT
        {
                sem_t *h = (sem_t *)(sem->semaphore.handle);
                int rc = sem_wait(h);
                if (sem_wait(h)) {
                        if (errno == EINTR) {
                                ecl_check_pending_interrupts();
                                goto AGAIN;
                        }
                        FElibc_error("Unable to wait on semaphore", 0);
                }
        }
# endif /* HAVE_SEM_INIT */
#endif /* ECL_WINDOWS_THREADS */
        @(return Ct)
}

cl_object
mp_semaphore_signal(cl_object sem)
{
        if (typeof(sem) != t_semaphore)
                FEwrong_type_argument(@'mp::semaphore', cv);
 AGAIN:
#ifdef ECL_WINDOWS_THREADS
        {
                HANDLE h = (HANDLE)(sem->semaphore.handle);
                if (!ReleaseSemaphore(h, 1, NULL)) {
                        FEwin32_error("Unable to post on semaphore ~A" 1, sem);
                }
        }
#else
# ifdef HAVE_SEM_INIT
        {
                sem_t *h = (sem_t *)(sem->semaphore.handle);
                int rc = sem_wait(h);
                if (sem_wait(h)) {
                        if (errno == EINTR) {
                                ecl_check_pending_interrupts();
                                goto AGAIN;
                        }
                        FElibc_error("Unable to post on semaphore ~A", 1, sem);
                }
        }
# endif /* HAVE_SEM_INIT */
#endif /* ECL_WINDOWS_THREADS */
        @(return Ct)
}

cl_object
mp_semaphore_close(cl_object sem)
{
        if (typeof(sem) != t_semaphore)
                FEwrong_type_argument(@'mp::semaphore', cv);
#ifdef ECL_WINDOWS_THREADS
        {
                HANDLE h = (HANDLE)(sem->semaphore.handle);
                if (h) CloseHandle(h);
        }
#else
# ifdef HAVE_SEM_INIT
        /*
         * No need for closing.
         */
# endif /* HAVE_SEM_INIT */
#endif /* ECL_WINDOWS_THREADS */
        @(return Ct)
}

#endif /* ECL_SEMAPHORES */

/*----------------------------------------------------------------------
 * INITIALIZATION
 */

void
init_threads(cl_env_ptr env)
{
	cl_object process;
	pthread_t main_thread;

	cl_core.processes = OBJNULL;

	/* We have to set the environment before any allocation takes place,
	 * so that the interrupt handling code works. */
#if !defined(WITH___THREAD)
# if defined(ECL_WINDOWS_THREADS)
	cl_env_key = TlsAlloc();
# else
	pthread_key_create(&cl_env_key, NULL);
# endif
#endif
	ecl_set_process_env(env);

#ifdef ECL_WINDOWS_THREADS
	{
	HANDLE aux = GetCurrentThread();
	DuplicateHandle(GetCurrentProcess(),
			aux,
			GetCurrentProcess(),
			&main_thread,
			0,
			FALSE,
			DUPLICATE_SAME_ACCESS);
	}
#else
	main_thread = pthread_self();
#endif
	process = ecl_alloc_object(t_process);
	process->process.active = 1;
	process->process.name = @'si::top-level';
	process->process.function = Cnil;
	process->process.args = Cnil;
	process->process.thread = main_thread;
	process->process.env = env;

	env->own_process = process;

        cl_core.global_lock = ecl_make_lock(@'si::package-lock', 0);
	cl_core.processes = ecl_list1(process);
}
