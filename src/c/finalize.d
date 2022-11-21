/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * finalize.c - object finalization
 *
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 * Copyright (c) 2022 Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/threads.h>
#include <ecl/internal.h>

cl_object
si_get_finalizer(cl_object o)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object output;
  ecl_disable_interrupts_env(the_env);
  output = ecl_set_finalizer_unprotected(o, ECL_NIL);
  ecl_set_finalizer_unprotected(o, output);
  ecl_enable_interrupts_env(the_env);
  ecl_return0(the_env);
}

cl_object
si_set_finalizer(cl_object o, cl_object finalizer)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_disable_interrupts_env(the_env);
  ecl_set_finalizer_unprotected(o, finalizer);
  ecl_enable_interrupts_env(the_env);
  ecl_return0(the_env);
}
