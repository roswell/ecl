/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * weak_pointers.c - weak pointers
 *
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 * Copyright (c) 2022 Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>


cl_object
ecl_alloc_weak_pointer(cl_object o)
{
  const cl_env_ptr the_env = ecl_process_env();
  struct ecl_weak_pointer *obj;
  ecl_disable_interrupts_env(the_env);
  obj = GC_MALLOC_ATOMIC(sizeof(struct ecl_weak_pointer));
  ecl_enable_interrupts_env(the_env);
  obj->t = t_weak_pointer;
  obj->value = o;
  if (!ECL_IMMEDIATE(o)) {
    GC_GENERAL_REGISTER_DISAPPEARING_LINK((void**)&(obj->value), (void*)o);
    si_set_finalizer((cl_object)obj, ECL_T);
  }
  return (cl_object)obj;
}

static cl_object
ecl_weak_pointer_value(cl_object o)
{
  return ecl_weak_pointer(o);
}

cl_object
si_make_weak_pointer(cl_object o)
{
  cl_object pointer = ecl_alloc_weak_pointer(o);
  @(return pointer);
}

cl_object
si_weak_pointer_value(cl_object o)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object value;
  if (ecl_unlikely(ecl_t_of(o) != t_weak_pointer))
    FEwrong_type_only_arg(@[ext::weak-pointer-value], o,
                          @[ext::weak-pointer]);
  value = (cl_object)GC_call_with_alloc_lock((GC_fn_type)ecl_weak_pointer_value, o);
  if (value) {
    ecl_return2(the_env, value, ECL_T);
  } else {
    ecl_return2(the_env, ECL_NIL, ECL_NIL);
  }
}
