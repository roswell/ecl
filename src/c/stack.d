/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * stack.d - stack interface
 *
 * Copyright (c) 2022 Daniel Kochmański
 *
 */

#include <ecl/ecl.h>

static void
assert_type_stack(cl_object o)
{
  if (ecl_t_of(o) != t_stack)
    FEwrong_type_argument(@[si::stack], o);
}

cl_object
si_make_stack(cl_object size)
{
  cl_index ndx = ecl_to_size(size);
  cl_object stack = ecl_make_stack(ndx, 0);
  ecl_return1(ecl_process_env(), stack);
}

cl_object
si_stack_size(cl_object o)
{
  cl_object integer;
  assert_type_stack(o);
  integer = ecl_make_integer(o->stack.size);
  ecl_return1(ecl_process_env(), integer);
}

cl_object
si_stack_push(cl_object o, cl_object v)
{
  assert_type_stack(o);
  _ecl_stack_push(o, v);
  ecl_return1(ecl_process_env(), v);
}

cl_object
si_stack_drop(cl_object o, cl_object n)
{
  assert_type_stack(o);
  _ecl_stack_drop(o, ecl_to_size(n));
  ecl_return1(ecl_process_env(), si_stack_top(o));
}

cl_object
si_stack_pop(cl_object o)
{
  assert_type_stack(o);
  ecl_return1(ecl_process_env(), _ecl_stack_pop(o));
}

cl_object
si_stack_top(cl_object o)
{
  assert_type_stack(o);
  ecl_return1(ecl_process_env(), _ecl_stack_top(o));
}
