/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * process.c - managing the process environment(s)
 *
 * Copyright (c) 2003 Juan Jose Garcia Ripoll
 * Copyright (c) 2023 Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>            /* includes ECL_WINDOWS_THREADS */
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>

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

/* -- Thread-local variables ------------------------------------------------ */

#ifdef ECL_THREADS
# ifdef ECL_WINDOWS_THREADS
#  define ecl_process_eq(t1, t2) (GetThreadId(t1) == GetThreadId(t2))
#  define ecl_set_process_self(var)             \
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
#  define ecl_process_key_t DWORD
#  define ecl_process_key_create(key) key = TlsAlloc()
#  define ecl_process_get_tls(key) TlsGetValue(key)
#  define ecl_process_set_tls(key,val) (TlsSetValue(key,val)!=0)
# else
#  define ecl_process_eq(t1, t2) (t1 == t2)
#  define ecl_set_process_self(var) (var = pthread_self())
#  define ecl_process_key_t static pthread_key_t
#  define ecl_process_key_create(key) pthread_key_create(&key, NULL)
#  define ecl_process_get_tls(key) pthread_getspecific(key)
#  define ecl_process_set_tls(key,val) (pthread_setspecific(key,val)==0)
# endif  /* ECL_WINDOWS_THREADS */

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

void
ecl_set_process_env(cl_env_ptr env)
{
  if(!ecl_process_set_tls(cl_env_key, env)) {
    ecl_thread_internal_error("pthread_setspecific() failed.");
  }
}
#else
/* The current global environment for single-threaded builds. */
cl_env_ptr cl_env_p = NULL;
#endif  /* ECL_THREADS */

/* -- Managing the collection of processes ---------------------------------- */

#ifdef ECL_THREADS

static void
add_env(cl_env_ptr the_env)
{
  ecl_mutex_lock(&ecl_core.processes_lock);
  if (ecl_core.nthreads >= ecl_core.sthreads) {
    cl_index ns = (1 + ecl_core.nthreads) + ecl_core.nthreads/2;
    ecl_core.threads = ecl_realloc(ecl_core.threads, ns*sizeof(cl_env_ptr));
    ecl_core.sthreads = ns;
  }
  ecl_core.threads[ecl_core.nthreads++] = the_env;
  ecl_mutex_unlock(&ecl_core.processes_lock);
}

static void
del_env(cl_env_ptr the_env)
{
  ecl_mutex_lock(&ecl_core.processes_lock);
  for(cl_index idx = 0, ndx = ecl_core.nthreads; idx < ndx; idx++) {
    if(ecl_core.threads[idx] == the_env) {
      do { ecl_core.threads[idx] = ecl_core.threads[idx+1]; }
      while (++idx <= ndx);
      ecl_core.nthreads--;
      break;
    }
  }
  ecl_mutex_unlock(&ecl_core.processes_lock);
}

/* Run a process in the current system thread. */
cl_env_ptr
ecl_adopt_cpu()
{
  struct cl_env_struct env_aux[1];
  struct ecl_interrupt_struct int_aux[1];
  cl_env_ptr the_env = ecl_process_env_unsafe();
  ecl_thread_t current;
  if (the_env != NULL)
    return the_env;
  /* Ensure that the thread is known to the GC. */
  /* FIXME this should be executed with hooks. */
#ifdef GBC_BOEHM
  if (GC_thread_is_registered() == 0) {
    struct GC_stack_base stack;
    GC_get_stack_base(&stack);
    GC_register_my_thread(&stack);
  }
#endif
  ecl_set_process_self(current);
  /* We need a fake env to allow for interrupts blocking and to set up frame
   * stacks or other stuff that is needed by ecl_init_env. Since the fake env is
   * allocated on the stack, we can safely store pointers to memory allocated by
   * the gc there. */
  memset(env_aux, 0, sizeof(*env_aux));
  env_aux->disable_interrupts = 1;
  env_aux->interrupt_struct = int_aux;
  env_aux->interrupt_struct->pending_interrupt = ECL_NIL;
  ecl_mutex_init(&env_aux->interrupt_struct->signal_queue_lock, FALSE);
  env_aux->interrupt_struct->signal_queue = ECL_NIL;
  ecl_set_process_env(env_aux);
  env_aux->thread = current;
  ecl_init_env(env_aux);

  /* Allocate, initialize and switch to the real environment. */
  the_env = _ecl_alloc_env(0);
  memcpy(the_env, env_aux, sizeof(*the_env));
  ecl_set_process_env(the_env);
  add_env(the_env);

  return the_env;
}

void
ecl_disown_cpu()
{
  cl_env_ptr the_env = ecl_process_env_unsafe();
  if (the_env == NULL)
    return;
  ecl_disable_interrupts_env(the_env);
  /* FIXME this should be part of dealloc. */
  ecl_clear_bignum_registers(the_env);
#ifdef ECL_WINDOWS_THREADS
  CloseHandle(the_env->thread);
#endif
  ecl_set_process_env(NULL);
  del_env(the_env);
  _ecl_dealloc_env(the_env);
  /* FIXME thsi should be executed with hooks. */
#ifdef GBC_BOEHM
  if (GC_thread_is_registered() == 1) {
    GC_unregister_my_thread();
  }
#endif
}

#ifdef ECL_WINDOWS_THREADS
static DWORD WINAPI
#else
static void *
#endif
thread_entry_point(void *ptr)
{
  cl_env_ptr the_env = ecl_cast_ptr(cl_env_ptr, ptr);
  cl_object process = the_env->own_process;
  /* Setup the environment for the execution of the thread. */
  ecl_set_process_env(the_env);
  ecl_cs_init(the_env);

  process->process.entry(0);

  /* This routine performs some cleanup before a thread is completely
   * killed. For instance, it has to remove the associated process object from
   * the list, an it has to dealloc some memory.
   *
   * NOTE: this cleanup does not provide enough "protection". In order to ensure
   * that all UNWIND-PROTECT forms are properly executed, never use the function
   * pthread_cancel() to kill a process, but rather use the lisp functions
   * mp_interrupt_process() and mp_process_kill(). */

  ecl_set_process_env(NULL);
  the_env->own_process = ECL_NIL;
  del_env(the_env);
#ifdef ECL_WINDOWS_THREADS
  CloseHandle(the_env->thread);
#endif
  _ecl_dealloc_env(the_env);

#ifdef ECL_WINDOWS_THREADS
  return 1;
#else
  return NULL;
#endif
}

/* Run a process in a new system thread. */
cl_env_ptr
ecl_spawn_cpu(cl_object process)
{
  cl_env_ptr the_env = ecl_process_env();
  cl_env_ptr new_env = NULL;
  int ok = 1;
  /* Allocate and initialize the new cpu env. */
  {
    new_env = _ecl_alloc_env(the_env);
    /* List the process such that its environment is marked by the GC when its
       contents are allocated. */
    add_env(new_env);
    /* Now we can safely allocate memory for the environment ocntents and store
       pointers to it in the environment. */
    ecl_init_env(new_env);
    /* Copy the parent env defaults. */
    new_env->trap_fpe_bits = the_env->trap_fpe_bits;
    new_env->own_process = process;
    new_env->bds_stack.tl_bindings = process->process.initial_bindings;
    new_env->bds_stack.tl_bindings_size = process->process.initial_bindings_size;
    process->process.env = new_env;
  }
  /* Spawn the thread */
  ecl_disable_interrupts_env(the_env);
#ifdef ECL_WINDOWS_THREADS
  {
    HANDLE code;
    DWORD threadId;

    code = (HANDLE)CreateThread(NULL, 0, thread_entry_point, new_env, 0, &threadId);
    new_env->thread = code;
    ok = code != NULL;
  }
#else /* ECL_WINDOWS_THREADS */
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
# ifdef HAVE_SIGPROCMASK
    {
      sigset_t new, previous;
      sigfillset(&new);
      sigdelset(&new, SIGSEGV);
      sigdelset(&new, SIGBUS);
      pthread_sigmask(SIG_BLOCK, &new, &previous);
      code = pthread_create(&new_env->thread, &pthreadattr,
                            thread_entry_point, new_env);
      pthread_sigmask(SIG_SETMASK, &previous, NULL);
    }
# else
    code = pthread_create(&new_env->thread, &pthreadattr,
                          thread_entry_point, new_env);
# endif
    ok = (code == 0);
  }
#endif /* ECL_WINDOWS_THREADS */
  /* Deal with the fallout of the thread creation. */
  if (!ok) {
    del_env(new_env);
    process->process.env = NULL;
    _ecl_dealloc_env(new_env);
  }
  ecl_enable_interrupts_env(the_env);
  return ok ? new_env : NULL;
}
#endif

/* -- Initialiation --------------------------------------------------------- */

void
init_process(void)
{
  cl_env_ptr the_env = ecl_core.first_env;
#ifdef ECL_THREADS
  ecl_thread_t main_thread;
  ecl_set_process_self(main_thread);
  the_env->thread = main_thread;
  ecl_process_key_create(cl_env_key);
  ecl_mutex_init(&ecl_core.processes_lock, 1);
  ecl_mutex_init(&ecl_core.global_lock, 1);
  ecl_mutex_init(&ecl_core.error_lock, 1);
  ecl_rwlock_init(&ecl_core.global_env_lock);
  ecl_core.threads = ecl_malloc(ecl_core.sthreads * sizeof(cl_env_ptr));
#endif
  ecl_set_process_env(the_env);
  the_env->default_sigmask = NULL;
  the_env->method_cache = NULL;
  the_env->slot_cache = NULL;
  the_env->interrupt_struct = NULL;
  the_env->disable_interrupts = 1;
}
