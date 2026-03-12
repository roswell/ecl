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

ucontext_t ret;

static void
_lwp_entry(void)
{
  ucontext_t *top, *own;
  top = &ret;
  own = top->uc_link;
  printf("YYY this is an entry!\n");
  printf("YYY yield\n");
  swapcontext(own, top);
  printf("YYY continue brrt!\n");
  printf("YYY return\n");
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
  o->cont.thread = thread;
  ecl_return1(the_env, o);
}

cl_object
si_pass(cl_object cont)
{
  const cl_env_ptr the_env = ecl_process_env();
  ucontext_t *uc = &cont->cont.uc;
  char *stack = cont->cont.stack;
  getcontext(uc);

  printf("XXX setup!\n");
  uc->uc_stack.ss_sp = stack;
  uc->uc_stack.ss_size = sizeof(stack);
  uc->uc_link = &ret;
  ret.uc_link = uc;
  ecl_module_gc->module.disable();

  makecontext(uc, (void(*)(void))_lwp_entry, 0);
  printf("XXX dispatch!\n");

  swapcontext(&ret, uc);
  printf("XXX returned!\n");

  swapcontext(&ret, uc);
  printf("XXX finished!\n");

  ecl_module_gc->module.enable();
  ecl_return1(the_env, ECL_T);
}

/* (si:pass (si:make-continuation (si:make-thread nil))) */
