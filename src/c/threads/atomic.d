/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * atomic.d - atomic operations
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
#include "threads/ecl_atomics.h"

cl_object
ecl_atomic_get(cl_object *slot)
{
  cl_object old;
  do {
    old = (cl_object)AO_load((AO_t*)slot);
  } while (!AO_compare_and_swap_full((AO_t*)slot, (AO_t)old, (AO_t)ECL_NIL));
  return old;
}

void
ecl_atomic_push(cl_object *slot, cl_object c)
{
  cl_object cons = ecl_list1(c), car;
  do {
    car = (cl_object)AO_load((AO_t*)slot);
    ECL_RPLACD(cons, car);
  } while (!AO_compare_and_swap_full((AO_t*)slot, (AO_t)car, (AO_t)cons));
}

cl_object
ecl_atomic_pop(cl_object *slot)
{
  cl_object cons, rest;
  do {
    cons = (cl_object)AO_load((AO_t*)slot);
    rest = CDR(cons);
  } while (!AO_compare_and_swap_full((AO_t*)slot, (AO_t)cons, (AO_t)rest));
  return cons;
}

cl_index
ecl_atomic_index_incf(cl_index *slot)
{
  AO_t old;
  AO_t next;
  do {
    old = AO_load((AO_t*)slot);
    next = old+1;
  } while (!AO_compare_and_swap_full((AO_t*)slot, (AO_t)old, (AO_t)next));
  return (cl_index)next;
}

#endif /* ECL_THREADS */
