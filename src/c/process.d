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

#ifdef ECL_THREADS
/* -- Thread local bindings */
static void
init_tl_bindings(cl_object process, cl_env_ptr env)
{

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

}
#endif

/* -- Managing the collection of processes ---------------------------------- */

#ifdef ECL_THREADS

/* Run a process in the current system thread. */
cl_env_ptr
ecl_adopt_cpu()
{
  cl_env_ptr the_env = ecl_process_env_unsafe();
  if (the_env != NULL)
    return the_env;
  the_env = _ecl_alloc_env(0);
  ecl_set_process_env(the_env);
  the_env->own_process = ECL_NIL;
  ecl_modules_init_env(the_env);
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
  ecl_modules_free_env(the_env);
  _ecl_dealloc_env(the_env);
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
  /* Start the user routine */
  process->process.entry(0);
  ecl_disable_interrupts_env(the_env);
  ecl_disown_cpu();
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
  int code = 0;
  /* Allocate and initialize the new cpu env. */
  {
    new_env = _ecl_alloc_env(the_env);
    new_env->trap_fpe_bits = the_env->trap_fpe_bits;
    new_env->own_process = process;
    process->process.env = new_env;
    ecl_modules_init_env(new_env);
  }
  /* Spawn the thread */
  ecl_disable_interrupts_env(the_env);
#if !defined(ECL_WINDOWS_THREADS) && defined(HAVE_SIGPROCMASK)
  {
    /* Block all asynchronous signals until the thread is completely set up. The
     * synchronous signals SIGSEGV and SIGBUS are needed by the gc and and can't
     * be blocked. */
    sigset_t new, previous;
    sigfillset(&new);
    sigdelset(&new, SIGSEGV);
    sigdelset(&new, SIGBUS);
    ecl_sigmask(SIG_BLOCK, &new, &previous);
    code = ecl_thread_create(new_env, thread_entry_point);
    ecl_sigmask(SIG_SETMASK, &previous, NULL);
  }
#else
  code = ecl_thread_create(new_env, thread_entry_point);
#endif
  /* Deal with the fallout of the thread creation. */
  if (code != 0) {
    process->process.env = NULL;
    ecl_modules_free_env(new_env);
    _ecl_dealloc_env(new_env);
  }
  ecl_enable_interrupts_env(the_env);
  return code ? NULL : new_env;
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
  ecl_core.first_env->own_process = ECL_NIL;
#endif
  return ECL_NIL;
}

static cl_object
init_env_process(cl_env_ptr the_env)
{
#ifdef ECL_THREADS
  init_tl_bindings(the_env->own_process, the_env);
#endif
  return ECL_NIL;
}

static cl_object
init_cpu_process(cl_env_ptr the_env)
{
#ifdef ECL_THREADS
  ecl_thread_t main_thread;
  ecl_set_process_self(main_thread);
  the_env->thread = main_thread;
#endif
  ecl_set_process_env(the_env);
  return ECL_NIL;
}

static cl_object
free_cpu_process(cl_env_ptr the_env)
{
#ifdef ECL_WINDOWS_THREADS
  CloseHandle(the_env->thread);
#endif
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
