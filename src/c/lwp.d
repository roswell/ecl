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
