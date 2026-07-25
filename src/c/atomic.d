/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * atomic.c - atomic operations
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/internal.h>

#ifdef ECL_THREADS

cl_object
ecl_atomic_get(cl_object *slot)
{
  cl_object old;
  do {
    old = (cl_object)ecl_atomic_load(slot);
  } while (!ecl_atomic_compare_and_swap_full(slot, old, ECL_NIL));
  return old;
}

cl_object
ecl_atomic_psh(cl_object *slot, cl_object cons)
{
  cl_object cdr;
  do {
    cdr = (cl_object)ecl_atomic_load(slot);
    ECL_RPLACD(cons, cdr);
  } while (!ecl_atomic_compare_and_swap_full(slot, cdr, cons));
  return cdr;
}

cl_object
ecl_atomic_pop(cl_object *slot)
{
  cl_object cons, rest;
  do {
    cons = (cl_object)ecl_atomic_load(slot);
    rest = CDR(cons);
  } while (!ecl_atomic_compare_and_swap_full(slot, cons, rest));
  return cons;
}

cl_index
ecl_atomic_index_incf(cl_index *slot)
{
  cl_index old;
  cl_index next;
  do {
    old = ecl_atomic_load(slot);
    next = old+1;
  } while (!ecl_atomic_compare_and_swap_full(slot, old, next));
  return next;
}

#endif /* ECL_THREADS */
