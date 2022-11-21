/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * garbage.c - garbage collector interface
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
#include <gc/gc_mark.h>

cl_object
si_gc(cl_narg narg, ...)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_collect_garbage();
  ecl_return0(the_env);
}

cl_object
si_gc_stats(cl_object enable)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object old_status;
  cl_object size1;
  cl_object size2;
  old_status = (cl_core.gc_stats == 0) ? ECL_NIL : ECL_T;

  if (cl_core.bytes_consed == ECL_NIL) {
    cl_core.bytes_consed = ecl_alloc_object(t_bignum);
    mpz_init2(ecl_bignum(cl_core.bytes_consed), 128);
    cl_core.gc_counter = ecl_alloc_object(t_bignum);
    mpz_init2(ecl_bignum(cl_core.gc_counter), 128);
  }

  /* We need fresh copies of the bignums */
  size1 = _ecl_big_register_copy(cl_core.bytes_consed);
  size2 = _ecl_big_register_copy(cl_core.gc_counter);

  if (enable == ECL_NIL) {
    cl_core.gc_stats = 0;
    mpz_set_ui(ecl_bignum(cl_core.bytes_consed), 0);
    mpz_set_ui(ecl_bignum(cl_core.gc_counter), 0);
  } else {
    cl_core.gc_stats = 1;
  }
  ecl_return3(the_env, size1, size2, old_status);
}
