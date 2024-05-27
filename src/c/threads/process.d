/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * process.d - native threads
 *
 * Copyright (c) 2003 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>            /* includes ECL_WINDOWS_THREADS */
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>

#ifndef __sun__ /* See unixinit.d for this */
#define _XOPEN_SOURCE 600       /* For pthread mutex attributes */
#endif
#include <errno.h>
#include <time.h>
#include <signal.h>
#include <string.h>
#ifdef ECL_WINDOWS_THREADS
# include <windows.h>
#else
# include <pthread.h>
#endif
#ifdef HAVE_GETTIMEOFDAY
# include <sys/time.h>
#endif
#ifdef HAVE_SCHED_H
# include <sched.h>
#endif

/* -- Macros -------------------------------------------------------- */

#ifdef ECL_WINDOWS_THREADS
# define ecl_process_key_t DWORD
# define ecl_process_key_create(key) key = TlsAlloc()
# define ecl_process_get_tls(key) TlsGetValue(key)
# define ecl_process_set_tls(key,val) (TlsSetValue(key,val)!=0)
# define ecl_process_eq(t1, t2) (GetThreadId(t1) == GetThreadId(t2))
# define ecl_set_process_self(var)              \
  {                                             \
    HANDLE aux = GetCurrentThread();            \
    DuplicateHandle(GetCurrentProcess(),        \
                    aux,                        \
                    GetCurrentProcess(),        \
                    &var,                       \
                    0,                          \
                    FALSE,                      \
                    DUPLICATE_SAME_ACCESS);     \
  }
#else
# define ecl_process_key_t static pthread_key_t
# define ecl_process_key_create(key) pthread_key_create(&key, NULL)
# define ecl_process_get_tls(key) pthread_getspecific(key)
# define ecl_process_set_tls(key,val) (pthread_setspecific(key,val)==0)
# define ecl_process_eq(t1, t2) (t1 == t2)
# define ecl_set_process_self(var) (var = pthread_self())
#endif  /* ECL_WINDOWS_THREADS */

/* -- Core ---------------------------------------------------------- */

/* Accessing a thread-local variable representing the environment. */

ecl_process_key_t cl_env_key;

cl_env_ptr
ecl_process_env_unsafe(void)
{
  return ecl_process_get_tls(cl_env_key);
}

cl_env_ptr
ecl_process_env(void)
{
  cl_env_ptr rv = ecl_process_get_tls(cl_env_key);
  if(!rv) {
    ecl_thread_internal_error("pthread_getspecific() failed.");
  }
  return rv;
}

static void
ecl_set_process_env(cl_env_ptr env)
{
  if(!ecl_process_set_tls(cl_env_key, env)) {
    ecl_thread_internal_error("pthread_setspecific() failed.");
  }
}

/* Managing the collection of processes. */

static void
extend_process_vector()
{
  cl_object v = cl_core.processes;
  cl_index new_size = v->vector.dim + v->vector.dim/2;
  cl_env_ptr the_env = ecl_process_env();
  ECL_WITH_NATIVE_LOCK_BEGIN(the_env, &cl_core.processes_lock) {
    cl_object other = cl_core.processes;
    if (new_size > other->vector.dim) {
      cl_object new = si_make_vector(ECL_T,
                                     ecl_make_fixnum(new_size),
                                     ecl_make_fixnum(other->vector.fillp),
                                     ECL_NIL, ECL_NIL, ECL_NIL);
      ecl_copy_subarray(new, 0, other, 0, other->vector.dim);
      cl_core.processes = new;
    }
  } ECL_WITH_NATIVE_LOCK_END;
}

static void
ecl_list_process(cl_object process)
{
  cl_env_ptr the_env = ecl_process_env();
  bool ok = 0;
  do {
    ECL_WITH_NATIVE_LOCK_BEGIN(the_env, &cl_core.processes_lock) {
      cl_object vector = cl_core.processes;
      cl_index size = vector->vector.dim;
      cl_index ndx = vector->vector.fillp;
      if (ndx < size) {
        vector->vector.self.t[ndx++] = process;
        vector->vector.fillp = ndx;
        ok = 1;
      }
    } ECL_WITH_NATIVE_LOCK_END;
    if (ok) break;
    extend_process_vector();
  } while (1);
}

/* Must be called with disabled interrupts to prevent race conditions
 * in thread_cleanup */
static void
ecl_unlist_process(cl_object process)
{
  ecl_mutex_lock(&cl_core.processes_lock);
  cl_object vector = cl_core.processes;
  cl_index i;
  for (i = 0; i < vector->vector.fillp; i++) {
    if (vector->vector.self.t[i] == process) {
      vector->vector.fillp--;
      do {
        vector->vector.self.t[i] =
          vector->vector.self.t[i+1];
      } while (++i < vector->vector.fillp);
      break;
    }
  }
  ecl_mutex_unlock(&cl_core.processes_lock);
}

static cl_object
ecl_process_list()
{
  cl_env_ptr the_env = ecl_process_env();
  cl_object output = ECL_NIL;
  ECL_WITH_NATIVE_LOCK_BEGIN(the_env, &cl_core.processes_lock) {
    cl_object vector = cl_core.processes;
    cl_object *data = vector->vector.self.t;
    cl_index i;
    for (i = 0; i < vector->vector.fillp; i++) {
      cl_object p = data[i];
      if (p != ECL_NIL)
        output = ecl_cons(p, output);
    }
  } ECL_WITH_NATIVE_LOCK_END;
  return output;
}

/* Initialiation */

static void
init_process(void)
{
  ecl_process_key_create(cl_env_key);
  ecl_mutex_init(&cl_core.processes_lock, 1);
  ecl_mutex_init(&cl_core.global_lock, 1);
  ecl_mutex_init(&cl_core.error_lock, 1);
  ecl_rwlock_init(&cl_core.global_env_lock);
}

/* -- Environment --------------------------------------------------- */

extern void ecl_init_env(struct cl_env_struct *env);

cl_object
mp_current_process(void)
{
  return ecl_process_env()->own_process;
}

/* -- Thread object ------------------------------------------------- */

static void
assert_type_process(cl_object o)
{
  if (ecl_t_of(o) != t_process)
    FEwrong_type_argument(@[mp::process], o);
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
  cl_env_ptr env = process->process.env;

  /* The following flags will disable all interrupts. */
  if (env) {
    ecl_disable_interrupts_env(env);
    ecl_clear_bignum_registers(env);
  }
  ecl_mutex_lock(&process->process.start_stop_lock);
  process->process.phase = ECL_PROCESS_EXITING;
#ifdef HAVE_SIGPROCMASK
  /* ...but we might get stray signals. */
  {
    sigset_t new[1];
    sigemptyset(new);
    sigaddset(new, ecl_option_values[ECL_OPT_THREAD_INTERRUPT_SIGNAL]);
    pthread_sigmask(SIG_BLOCK, new, NULL);
  }
#endif
  process->process.env = NULL;
  ecl_unlist_process(process);
#ifdef ECL_WINDOWS_THREADS
  CloseHandle(process->process.thread);
#endif
  ecl_set_process_env(NULL);
  if (env) _ecl_dealloc_env(env);

  process->process.phase = ECL_PROCESS_INACTIVE;
  ecl_cond_var_broadcast(&process->process.exit_barrier);
  ecl_mutex_unlock(&process->process.start_stop_lock);
}

#ifdef ECL_WINDOWS_THREADS
static DWORD WINAPI
#else
static void *
#endif
thread_entry_point(void *arg)
{
  cl_object process = (cl_object)arg;
  cl_env_ptr env = process->process.env;

  /*
   * Upon entering this routine
   *      process.env = our environment for lisp
   *      process.phase = ECL_PROCESS_BOOTING
   *      signals are disabled in the environment
   *      the communication interrupt is disabled (sigmasked)
   *
   * This process will not receive signals that originate from
   * other processes. Furthermore, we expect not to get any
   * other interrupts (SIGSEGV, SIGFPE) if we do things right.
   */
  /* 1) Setup the environment for the execution of the thread */
  ecl_set_process_env(env = process->process.env);
#ifndef ECL_WINDOWS_THREADS
  pthread_cleanup_push(thread_cleanup, (void *)process);
#endif
  ecl_cs_set_org(env);
  ecl_mutex_lock(&process->process.start_stop_lock);

  /* 2) Execute the code. The CATCH_ALL point is the destination
   *     provides us with an elegant way to exit the thread: we just
   *     do an unwind up to frs_top.
   */
  ECL_CATCH_ALL_BEGIN(env) {
#ifdef HAVE_SIGPROCMASK
    {
      sigset_t *new = (sigset_t*)env->default_sigmask;
      pthread_sigmask(SIG_SETMASK, new, NULL);
    }
#endif
    process->process.phase = ECL_PROCESS_ACTIVE;
    ecl_mutex_unlock(&process->process.start_stop_lock);
    ecl_enable_interrupts_env(env);
    si_trap_fpe(@'last', ECL_T);
    ecl_bds_bind(env, @'mp::*current-process*', process);

    ECL_RESTART_CASE_BEGIN(env, @'abort') {
      env->values[0] = cl_apply(2, process->process.function,
                                process->process.args);
      {
        cl_object output = ECL_NIL;
        int i = env->nvalues;
        while (i--) {
          output = CONS(env->values[i], output);
        }
        process->process.exit_values = output;
      }
    } ECL_RESTART_CASE(1,args) {
      /* ABORT restart. */
      process->process.exit_values = args;
    } ECL_RESTART_CASE_END;
    ecl_bds_unwind1(env);
  } ECL_CATCH_ALL_END;

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
  cl_env_ptr env = ecl_process_env();
  cl_object process = ecl_alloc_object(t_process), array;
  process->process.phase = ECL_PROCESS_INACTIVE;
  process->process.name = name;
  process->process.function = ECL_NIL;
  process->process.args = ECL_NIL;
  process->process.interrupt = ECL_NIL;
  process->process.exit_values = ECL_NIL;
  process->process.env = NULL;
  if (initial_bindings != OBJNULL) {
    array = si_make_vector(ECL_T, ecl_make_fixnum(256),
                           ECL_NIL, ECL_NIL, ECL_NIL, ECL_NIL);
    si_fill_array_with_elt(array, ECL_NO_TL_BINDING, ecl_make_fixnum(0), ECL_NIL);
  } else {
    array = cl_copy_seq(ecl_process_env()->bindings_array);
  }
  process->process.initial_bindings = array;
  process->process.woken_up = ECL_NIL;
  ecl_disable_interrupts_env(env);
  ecl_mutex_init(&process->process.start_stop_lock, TRUE);
  ecl_cond_var_init(&process->process.exit_barrier);
  ecl_set_finalizer_unprotected(process, ECL_T);
  ecl_enable_interrupts_env(env);
  return process;
}

bool
ecl_import_current_thread(cl_object name, cl_object bindings)
{
  struct cl_env_struct env_aux[1];
  cl_object process;
  ecl_thread_t current;
  cl_env_ptr env;
  int registered;
  struct GC_stack_base stack;
  ecl_set_process_self(current);
#ifdef GBC_BOEHM
  GC_get_stack_base(&stack);
  switch (GC_register_my_thread(&stack)) {
  case GC_SUCCESS:
    registered = 1;
    break;
  case GC_DUPLICATE:
    /* Thread was probably created using the GC hooks
     * for thread creation */
    registered = 0;
    break;
  default:
    return 0;
  }
#endif
  {
    cl_object processes = cl_core.processes;
    cl_index i, size;
    for (i = 0, size = processes->vector.fillp; i < size; i++) {
      cl_object p = processes->vector.self.t[i];
      if (!Null(p) && ecl_process_eq(p->process.thread, current)) {
          return 0;
      }
    }
  }
  /* We need a fake env to allow for interrupts blocking and to set up
   * frame stacks or other stuff which may be needed by alloc_process
   * and ecl_list_process. Since the fake env is allocated on the stack,
   * we can safely store pointers to memory allocated by the gc there. */
  memset(env_aux, 0, sizeof(*env_aux));
  env_aux->disable_interrupts = 1;
  env_aux->interrupt_struct = ecl_alloc_unprotected(sizeof(*env_aux->interrupt_struct));
  env_aux->interrupt_struct->pending_interrupt = ECL_NIL;
  ecl_mutex_init(&env_aux->interrupt_struct->signal_queue_lock, FALSE);
  env_aux->interrupt_struct->signal_queue = ECL_NIL;
  ecl_set_process_env(env_aux);
  ecl_init_env(env_aux);

  /* Allocate real environment, link it together with process */
  env = _ecl_alloc_env(0);
  process = alloc_process(name, bindings);
  process->process.env = env;
  process->process.phase = ECL_PROCESS_BOOTING;
  process->process.thread = current;

  /* Copy initial bindings from process to the fake environment */
  env_aux->cleanup = registered;
  env_aux->bindings_array = process->process.initial_bindings;
  env_aux->thread_local_bindings_size = env_aux->bindings_array->vector.dim;
  env_aux->thread_local_bindings = env_aux->bindings_array->vector.self.t;

  /* Switch over to the real environment */
  memcpy(env, env_aux, sizeof(*env));
  env->own_process = process;
  ecl_set_process_env(env);
  ecl_list_process(process);
  ecl_enable_interrupts_env(env);

  process->process.phase = ECL_PROCESS_ACTIVE;

  ecl_bds_bind(env, @'mp::*current-process*', process);
  return 1;
}

void
ecl_release_current_thread(void)
{
  cl_env_ptr env = ecl_process_env();

  int cleanup = env->cleanup;
  cl_object own_process = env->own_process;
  thread_cleanup(own_process);
#ifdef GBC_BOEHM
  if (cleanup) {
    GC_unregister_my_thread();
  }
#endif
}

@(defun mp::make-process (&key name ((:initial-bindings initial_bindings) ECL_T))
  cl_object process;
  @
  process = alloc_process(name, initial_bindings);
  @(return process);
  @)

cl_object
mp_process_preset(cl_narg narg, cl_object process, cl_object function, ...)
{
  ecl_va_list args;
  if (narg < 2)
    FEwrong_num_arguments(@[mp::process-preset]);
  ecl_va_start(args, function, narg, 2);
  assert_type_process(process);
  process->process.function = function;
  process->process.args = cl_grab_rest_args(args);
  ecl_va_end(args);
  @(return process);
}

cl_object
mp_interrupt_process(cl_object process, cl_object function)
{
  cl_env_ptr env = ecl_process_env();
  /* Make sure we don't interrupt an exiting process */
  ECL_WITH_NATIVE_LOCK_BEGIN(env, &process->process.start_stop_lock) {
    unlikely_if (mp_process_active_p(process) == ECL_NIL)
      FEerror("Cannot interrupt the inactive process ~A", 1, process);
    ecl_interrupt_process(process, function);
  } ECL_WITH_NATIVE_LOCK_END;
  @(return ECL_T);
}

cl_object
mp_suspend_loop()
{
  cl_env_ptr env = ecl_process_env();
  ECL_CATCH_BEGIN(env,@'mp::suspend-loop') {
    for ( ; ; ) {
      cl_sleep(ecl_make_fixnum(100));
    }
  } ECL_CATCH_END;
  ecl_return0(env);
}

cl_object
mp_break_suspend_loop()
{
  cl_env_ptr the_env = ecl_process_env();
  if (frs_sch(@'mp::suspend-loop')) {
    cl_throw(@'mp::suspend-loop');
  }
  ecl_return0(the_env);
}

cl_object
mp_process_suspend(cl_object process)
{
  return mp_interrupt_process(process, @'mp::suspend-loop');
}

cl_object
mp_process_resume(cl_object process)
{
  return mp_interrupt_process(process, @'mp::break-suspend-loop');
}

cl_object
mp_process_kill(cl_object process)
{
  return mp_interrupt_process(process, @'mp::exit-process');
}

cl_object
mp_process_yield(void)
{
#if defined(ECL_WINDOWS_THREADS)
  Sleep(0);
#elif defined(HAVE_SCHED_H)
  sched_yield();
#else
  ecl_musleep(0.0);
#endif
  @(return);
}

cl_object
mp_process_enable(cl_object process)
{
  /* process_env and ok are changed after the setjmp call in
   * ECL_UNWIND_PROTECT_BEGIN, so they need to be declared volatile */
  volatile cl_env_ptr process_env = NULL;
  cl_env_ptr the_env = ecl_process_env();
  volatile int ok = 1;
  ECL_UNWIND_PROTECT_BEGIN(the_env) {
    /* Try to gain exclusive access to the process. This prevents two
     * concurrent calls to process-enable from different threads on
     * the same process */
    ecl_mutex_lock(&process->process.start_stop_lock);
    /* Ensure that the process is inactive. */
    if (process->process.phase != ECL_PROCESS_INACTIVE) {
      FEerror("Cannot enable the running process ~A.", 1, process);
    }
    ok = 0;
    process->process.phase = ECL_PROCESS_BOOTING;

    process->process.parent = mp_current_process();
    process->process.trap_fpe_bits =
      process->process.parent->process.env->trap_fpe_bits;

    /* Link environment and process together */
    process_env = _ecl_alloc_env(the_env);
    process_env->own_process = process;
    process->process.env = process_env;

    /* Immediately list the process such that its environment is
     * marked by the gc when its contents are allocated */
    ecl_list_process(process);

    /* Now we can safely allocate memory for the environment contents
     * and store pointers to it in the environment */
    ecl_init_env(process_env);

    process_env->trap_fpe_bits = process->process.trap_fpe_bits;
    process_env->bindings_array = process->process.initial_bindings;
    process_env->thread_local_bindings_size = 
      process_env->bindings_array->vector.dim;
    process_env->thread_local_bindings =
      process_env->bindings_array->vector.self.t;

    ecl_disable_interrupts_env(the_env);
#ifdef ECL_WINDOWS_THREADS
    {
      HANDLE code;
      DWORD threadId;

      code = (HANDLE)CreateThread(NULL, 0, thread_entry_point, process, 0, &threadId);
      ok = (process->process.thread = code) != NULL;
    }
#else
    {
      int code;
      pthread_attr_t pthreadattr;

      pthread_attr_init(&pthreadattr);
      pthread_attr_setdetachstate(&pthreadattr, PTHREAD_CREATE_DETACHED);
      /*
       * Block all asynchronous signals until the thread is completely
       * set up. The synchronous signals SIGSEGV and SIGBUS are needed
       * by the gc and thus can't be blocked.
       */
#ifdef HAVE_SIGPROCMASK
      {
        sigset_t new, previous;
        sigfillset(&new);
        sigdelset(&new, SIGSEGV);
        sigdelset(&new, SIGBUS);
        pthread_sigmask(SIG_BLOCK, &new, &previous);
        code = pthread_create(&process->process.thread, &pthreadattr,
                              thread_entry_point, process);
        pthread_sigmask(SIG_SETMASK, &previous, NULL);
      }
#else
      code = pthread_create(&process->process.thread, &pthreadattr,
                            thread_entry_point, process);
#endif
      ok = (code == 0);
    }
#endif
    ecl_enable_interrupts_env(the_env);
  } ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT {
    if (!ok) {
      /* INV: interrupts are already disabled through thread safe
       * unwind-protect */
      ecl_unlist_process(process);
      process->process.phase = ECL_PROCESS_INACTIVE;
      /* Alert possible waiting processes. */
      ecl_cond_var_broadcast(&process->process.exit_barrier);
      process->process.env = NULL;
      if (process_env != NULL)
        _ecl_dealloc_env(process_env);
    }
    /* Unleash the thread */
    ecl_mutex_unlock(&process->process.start_stop_lock);
  } ECL_UNWIND_PROTECT_THREAD_SAFE_END;

  @(return (ok? process : ECL_NIL));
}

cl_object
mp_exit_process(void)
{
  /* We simply undo the whole of the frame stack. This brings up
     back to the thread entry point, going through all possible
     UNWIND-PROTECT.
  */
  const cl_env_ptr the_env = ecl_process_env();
  ecl_unwind(the_env, the_env->frs_org);
  /* Never reached */
}

cl_object
mp_all_processes(void)
{
  /* No race condition here because this list is never destructively
   * modified. When we add or remove processes, we create new lists. */
  @(return ecl_process_list());
}

cl_object
mp_process_name(cl_object process)
{
  assert_type_process(process);
  @(return process->process.name);
}

cl_object
mp_process_active_p(cl_object process)
{
  assert_type_process(process);
  @(return (process->process.phase? ECL_T : ECL_NIL));
}

cl_object
mp_process_whostate(cl_object process)
{
  assert_type_process(process);
  @(return (cl_core.null_string));
}

cl_object
mp_process_join(cl_object process)
{
  cl_env_ptr the_env = ecl_process_env();
  volatile cl_object values;

  assert_type_process(process);
  ECL_UNWIND_PROTECT_BEGIN(the_env) {
    ecl_mutex_lock(&process->process.start_stop_lock);
    while (process->process.phase != ECL_PROCESS_INACTIVE) {
      ecl_cond_var_wait(&process->process.exit_barrier, &process->process.start_stop_lock);
    }
    values = cl_values_list(process->process.exit_values);
  } ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT {
    ecl_mutex_unlock(&process->process.start_stop_lock);
  } ECL_UNWIND_PROTECT_THREAD_SAFE_END;
  return values;
}

cl_object
mp_process_run_function(cl_narg narg, cl_object name, cl_object function, ...)
{
  cl_object process;
  cl_object rest;
  ecl_va_list args;
  if (narg < 2)
    FEwrong_num_arguments(@[mp::process-run-function]);
  if (CONSP(name)) {
    process = cl_apply(2, @'mp::make-process', name);
  } else {
    process = mp_make_process(2, @':name', name);
  }
  ecl_va_start(args, function, narg, 2);
  rest = cl_grab_rest_args(args);
  ecl_va_end(args);
  cl_apply(4, @'mp::process-preset', process, function,
           rest);
  return mp_process_enable(process);
}

cl_object
mp_process_run_function_wait(cl_narg narg, ...)
{
  cl_object process;
  ecl_va_list args;
  ecl_va_start(args, narg, narg, 0);
  process = cl_apply(2, @'mp::process-run-function',
                     cl_grab_rest_args(args));
  if (!Null(process)) {
    while (process->process.phase < ECL_PROCESS_ACTIVE) {
      ecl_musleep(0.001);
    }
  }
  ecl_va_end(args);
  @(return process);
}

/*----------------------------------------------------------------------
 * INTERRUPTS
 */

#ifndef ECL_WINDOWS_THREADS
static cl_object
mp_get_sigmask(void)
{
  cl_object data = ecl_alloc_simple_vector(sizeof(sigset_t), ecl_aet_b8);
  sigset_t *mask_ptr = (sigset_t*)data->vector.self.b8;
  sigset_t no_signals;
  sigemptyset(&no_signals);
  if (pthread_sigmask(SIG_BLOCK, &no_signals, mask_ptr))
    FElibc_error("MP:GET-SIGMASK failed in a call to pthread_sigmask", 0);
  @(return data);
}

static cl_object
mp_set_sigmask(cl_object data)
{
  sigset_t *mask_ptr = (sigset_t*)data->vector.self.b8;
  if (pthread_sigmask(SIG_SETMASK, mask_ptr, NULL))
    FElibc_error("MP:SET-SIGMASK failed in a call to pthread_sigmask", 0);
  @(return data);
}
#endif

cl_object
mp_block_signals(void)
{
#ifdef ECL_WINDOWS_THREADS
  cl_env_ptr the_env = ecl_process_env();
  cl_object previous = ecl_cmp_symbol_value(the_env, @'ext::*interrupts-enabled*');
  ECL_SETQ(the_env, @'ext::*interrupts-enabled*', ECL_NIL);
  @(return previous);
#else
  cl_object previous = mp_get_sigmask();
  sigset_t all_signals;
  sigfillset(&all_signals);
  /* SIGSEGV or SIGBUS are needed by the gc in incremental mode and
   * can thus never be blocked */
  sigdelset(&all_signals, SIGSEGV);
  sigdelset(&all_signals, SIGBUS);
  if (pthread_sigmask(SIG_SETMASK, &all_signals, NULL))
    FElibc_error("MP:BLOCK-SIGNALS failed in a call to pthread_sigmask",0);
  @(return previous);
#endif
}

cl_object
mp_restore_signals(cl_object sigmask)
{
#ifdef ECL_WINDOWS_THREADS
  cl_env_ptr the_env = ecl_process_env();
  ECL_SETQ(the_env, @'ext::*interrupts-enabled*', sigmask);
  ecl_check_pending_interrupts(the_env);
  @(return sigmask);
#else
  return mp_set_sigmask(sigmask);
#endif
}

/* -- Initialization ------------------------------------------------ */

void
init_threads(cl_env_ptr env)
{
  cl_object process;
  ecl_thread_t main_thread;
  init_process();
  /* We have to set the environment before any allocation takes place,
   * so that the interrupt handling code works. */
  ecl_set_process_env(env);
  ecl_set_process_self(main_thread);
  process = ecl_alloc_object(t_process);
  process->process.phase = ECL_PROCESS_ACTIVE;
  process->process.name = @'si::top-level';
  process->process.function = ECL_NIL;
  process->process.args = ECL_NIL;
  process->process.thread = main_thread;
  process->process.env = env;
  process->process.woken_up = ECL_NIL;
  ecl_mutex_init(&process->process.start_stop_lock, TRUE);
  ecl_cond_var_init(&process->process.exit_barrier);

  env->own_process = process;
  {
    cl_object v = si_make_vector(ECL_T, /* Element type */
                                 ecl_make_fixnum(256), /* Size */
                                 ecl_make_fixnum(0), /* fill pointer */
                                 ECL_NIL, ECL_NIL, ECL_NIL);
    v->vector.self.t[0] = process;
    v->vector.fillp = 1;
    cl_core.processes = v;
  }
}
