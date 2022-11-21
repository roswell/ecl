/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * weak_pointer.c - weak pointers
 *
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 * Copyright (c) 2022 Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>

cl_object
si_make_weak_pointer(cl_object o)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object pointer = ecl_alloc_weak_pointer(o);
  si_set_finalizer(pointer, ECL_T);
  ecl_return1(the_env, pointer);
}

cl_object
si_weak_pointer_value(cl_object o)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object value;
  if (ecl_unlikely(ecl_t_of(o) != t_weak_pointer)) {
    FEwrong_type_only_arg(@[ext::weak-pointer-value], o, @[ext::weak-pointer]);
  }
  value = ecl_weak_pointer_value(o);
  if (value) {
    ecl_return2(the_env, value, ECL_T);
  } else {
    ecl_return2(the_env, ECL_NIL, ECL_NIL);
  }
}
