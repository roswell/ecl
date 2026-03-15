/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * lwp.c - light-weight processes and delimited continuations
 *
 * Copyright (c) 1990, Giuseppe Attardi.
 * Copyright (c) 2001, Juan Jose Garcia Ripoll.
 * Copyright (c) 2026, Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/internal.h>

# include <sys/mman.h>
# include <unistd.h>
#include <ucontext.h>

/* -- implementation notes -----------------------------------------------------
 *
 * Fist and foremost, ucontext.h has been deprecated in POSIX.1-2001 and removed
 * in POSIX.1-2008. That said glibc implements it, and musl libc has a separate
 * library called libucontext that implements the API. On Windows we can write a
 * similar library that utilizes Fibers. We should abstract platform details.
 *
 * - arguments passed to the function from `makecontext' are all ints, so it may
 *   be useful to us only to pass flags, and most notably not pointers (objects)
 *
 * - `getcontext' creates a new instance of context notably uc_link is not
 *   inherited from the current context (uc_link contains uinitialized data).
 *
 * - we must ensure that GC scans all stacks if we don't want to drop objects,
 *   or disable GC while we are in a multi-stack context
 *
 * -------------------------------------------------------------------------- */

static cl_object
ensure_cc(cl_env_ptr the_env)
{
  cl_object process = the_env->own_process;
  cl_object cc = process->process.cont;
  if(Null(cc)) {
    cc = process->process.cont = si_make_continuation(ECL_NIL);
    cc->cont.thread = si_make_thread(ECL_NIL);
  }
  return cc;
}


#define LWP_STACK_SIZE 8*1024

/* FIXME munmap in the finalizer. */
static void*
make_stack(size_t size)
{
  size_t page_size = sysconf(_SC_PAGESIZE);
  void *mem = mmap(NULL, size+page_size,
                   PROT_READ | PROT_WRITE,
                   MAP_ANON | MAP_PRIVATE, -1, 0);
  /* catch overflows. Note that we don't need it normally in ECL, because we do
     our own overflow checks, but this helps to illustrate and catch early when
     GC goes off rails because it doesn't work well with multiple stakck. */
  mprotect(mem, page_size, PROT_NONE);
  return mem+page_size;
}

static void
_lwp_entry(void)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object process = the_env->own_process;
  cl_object cc = process->process.cont;
  cl_object thread = cc->cont.thread;

  cl_funcall(2, thread->thread.fun, thread);
  _ecl_unexpected_return();
}

cl_object
si_make_thread(cl_object fun)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object o = ecl_alloc_object(t_thread);
  o->thread.fun = fun;
  o->thread.cont = ECL_NIL;
  ecl_return1(the_env, o);
}

cl_object
si_make_continuation(cl_object thread)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object o = ecl_alloc_object(t_cont);
  ucontext_t *uc = &o->cont.uc;
  o->cont.thread = thread;
  if(Null(thread)) {
    o->cont.env = the_env;
  } else {
    const cl_env_ptr new_env = _ecl_alloc_env(the_env);
    cl_object cpu = the_env->own_process;
    char *stack = o->cont.stack;
    /* Configure the environment. */
    new_env->trap_fpe_bits = the_env->trap_fpe_bits;
    new_env->own_process = cpu;
    ecl_modules_init_env(new_env);
    o->cont.env = new_env;
    /* Create the context. */
    getcontext(uc);
    uc->uc_stack.ss_sp = make_stack(LWP_STACK_SIZE);
    uc->uc_stack.ss_size = LWP_STACK_SIZE;
    uc->uc_link = NULL;
    makecontext(uc, (void(*)(void))_lwp_entry, 0);
  }
  ecl_return1(the_env, o);
}

cl_object
si_pass(cl_object cont)
{
  const cl_env_ptr the_env = ecl_process_env();
  const cl_env_ptr new_env = cont->cont.env;
  cl_object cc = ensure_cc(the_env);
  cl_object process = new_env->own_process;
  cl_object thread = cont->cont.thread;
  cl_object cc_thread = cc->cont.thread;
  ucontext_t *here, *next;
  ecl_cs_check(the_env, cc_thread);
  here = &cc->cont.uc;
  next = &cont->cont.uc;

  ecl_set_process_env(new_env);
  process->process.cont = cont;
  thread->thread.cont = cc;
  swapcontext(here, next);

  ecl_return1(the_env, cc_thread->thread.cont);
}

/* (si:pass (si:make-continuation (si:make-thread nil))) */
/* (si:pass (si:pass (si:make-continuation (si:make-thread nil)))) */
