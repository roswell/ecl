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

/* -- Thread local bindings */
static void
init_tl_bindings(cl_object process, cl_env_ptr env)
{
#ifdef ECL_THREADS
  cl_index bindings_size;
  cl_object *bindings;
  if (Null(process) || Null(process->process.inherit_bindings_p)) {
    cl_index idx = 0, size = 256;
    bindings_size = size;
    bindings = (cl_object *)ecl_malloc(size*sizeof(cl_object*));
    for(idx=0; idx<256; idx++) {
      bindings[idx] = ECL_NO_TL_BINDING;
    }
  } else {
    cl_env_ptr parent_env = ecl_process_env();
    bindings_size = parent_env->bds_stack.tl_bindings_size;
    bindings = (cl_object *)ecl_malloc(bindings_size*sizeof(cl_object*));
    ecl_copy(bindings, parent_env->bds_stack.tl_bindings, bindings_size*sizeof(cl_object*));
  }
  env->bds_stack.tl_bindings_size = bindings_size;
  env->bds_stack.tl_bindings = bindings;
#endif
}


/* -- Managing the collection of processes ---------------------------------- */

#ifdef ECL_THREADS

static void
add_env(cl_env_ptr the_env)
{
  cl_object _env;
  ecl_mutex_lock(&ecl_core.processes_lock);
  _env = ecl_cast_ptr(cl_object,the_env);
  ecl_stack_push(ecl_core.threads, _env);
  ecl_mutex_unlock(&ecl_core.processes_lock);
}

static void
del_env(cl_env_ptr the_env)
{
  cl_object _env;
  ecl_mutex_lock(&ecl_core.processes_lock);
  _env = ecl_cast_ptr(cl_object,the_env);
  ecl_stack_del(ecl_core.threads, _env);
  ecl_mutex_unlock(&ecl_core.processes_lock);
}

static void
register_gc_thread()
{
#ifdef GBC_BOEHM
  if (GC_thread_is_registered() == 0) {
    struct GC_stack_base stack;
    GC_get_stack_base(&stack);
    GC_register_my_thread(&stack);
  }
#endif
}

static void
unregister_gc_thread()
{
#ifdef GBC_BOEHM
  if (GC_thread_is_registered() == 1) {
    GC_unregister_my_thread();
  }
#endif
}

/* Run a process in the current system thread. */
cl_env_ptr
ecl_adopt_cpu()
{
  cl_env_ptr the_env = ecl_process_env_unsafe();
  ecl_thread_t current;
  if (the_env != NULL)
    return the_env;
  register_gc_thread();
  ecl_set_process_self(current);
  the_env = _ecl_alloc_env(0);
  the_env->thread = current;
  ecl_set_process_env(the_env);
  ecl_init_env(the_env);
  add_env(the_env);
  init_tl_bindings(ECL_NIL, the_env);
  ecl_modules_init_cpu(the_env);

  return the_env;
}

void
ecl_disown_cpu()
{
  cl_env_ptr the_env = ecl_process_env_unsafe();
  if (the_env == NULL)
    return;
  ecl_modules_free_cpu(the_env);
#ifdef ECL_WINDOWS_THREADS
  CloseHandle(the_env->thread);
#endif
  del_env(the_env);
  _ecl_dealloc_env(the_env);
  unregister_gc_thread();
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
  ecl_modules_init_cpu(the_env);
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

  ecl_disable_interrupts_env(the_env);
  ecl_modules_free_cpu(the_env);
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
    init_tl_bindings(process, new_env);
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

/* -- Module definition (so meta!) ------------------------------------------ */
static cl_object
create_process(void)
{
#ifdef ECL_THREADS
  ecl_process_key_create(cl_env_key);
  ecl_mutex_init(&ecl_core.processes_lock, 1);
  ecl_mutex_init(&ecl_core.global_lock, 1);
  ecl_mutex_init(&ecl_core.error_lock, 1);
  ecl_rwlock_init(&ecl_core.global_env_lock);
  ecl_core.threads = ecl_make_stack(16);
#endif
  return ECL_NIL;
}

static cl_object
init_env_process(cl_env_ptr the_env)
{
#ifdef ECL_THREADS
  the_env->own_process = ECL_NIL;
#endif
  return ECL_NIL;
}

static cl_object
init_cpu_process(cl_env_ptr the_env)
{
  ecl_set_process_env(the_env);
  return ECL_NIL;
}

static cl_object
free_cpu_process(cl_env_ptr the_env)
{
  ecl_set_process_env(NULL);
  return ECL_NIL;
}

static cl_object
free_env_process(cl_env_ptr the_env)
{
#ifdef ECL_THREADS
  the_env->own_process = ECL_NIL;
#endif
  return ECL_NIL;
}

static cl_object
destroy_process(void)
{
  return ECL_NIL;
}

ecl_def_ct_base_string(str_process, "PROCESS", 7, static, const);

static struct ecl_module module_process = {
  .t = t_module,
  .name = str_process,
  .create = create_process,
  .enable = ecl_module_no_op,
  .init_env = init_env_process,
  .init_cpu = init_cpu_process,
  .free_cpu = free_cpu_process,
  .free_env = free_env_process,
  .disable = ecl_module_no_op,
  .destroy = destroy_process
};

cl_object ecl_module_process = (cl_object)&module_process;
