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

static void
_lwp_entry(void)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object process = the_env->own_process;
  cl_object thread = process->process.function; /* kludge */
  cl_object cont = thread->thread.cont;
  cl_object cc = si_make_continuation(ECL_NIL);

  ucontext_t *top = &cc->cont.uc;
  ucontext_t *sub = &cont->cont.uc;

  /* top = &ret; */
  /* own = top->uc_link; */
  printf("YYY this is an entry!\n");
  ecl_print(@"ZZZ Hello!", ECL_T);
  printf("YYY yield\n");
  thread->thread.cont = cc;
  cc->cont.thread = thread;
  ecl_set_process_env(cont->cont.env);
  swapcontext(top, sub);
  printf("YYY continue brrt!\n");
  ecl_print(@"ZZZ Bonjur!!", ECL_T);
  printf("YYY return\n");
  ecl_set_process_env(cont->cont.env);
  setcontext(&cont->cont.uc);
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
    cpu->process.function = thread; /* kludge */
    new_env->trap_fpe_bits = the_env->trap_fpe_bits;
    new_env->own_process = cpu;
    ecl_modules_init_env(new_env);
    o->cont.env = new_env;
    /* Create the context. */
    getcontext(uc);
    uc->uc_stack.ss_sp = stack;
    uc->uc_stack.ss_size = 16*1024;
    uc->uc_link = NULL;
    makecontext(uc, (void(*)(void))_lwp_entry, 0);
  }
  ecl_return1(the_env, o);
}

cl_object
si_pass(cl_object cont)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object cc = si_make_continuation(ECL_NIL);;
  cl_object thread = cont->cont.thread;
  ucontext_t *top, *sub;

  top = &cc->cont.uc;
  sub = &cont->cont.uc;

  thread->thread.cont = cc;
  ecl_module_gc->module.disable();
  printf("XXX dispatch!\n");
  ecl_set_process_env(cont->cont.env);
  swapcontext(top, sub);
  printf("XXX returned!\n");
  ecl_module_gc->module.enable();
  printf("XXX finished!\n");

  ecl_return1(the_env, thread->thread.cont);
}

/* (si:pass (si:make-continuation (si:make-thread nil))) */
