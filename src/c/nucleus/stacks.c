/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * stacks.c - lisp stacks
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 * Copyright (c) 2024 Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <signal.h>
#include <string.h>
#ifdef HAVE_SYS_RESOURCE_H
# include <sys/time.h>
# include <sys/resource.h>
#endif
#include <ecl/nucleus.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/stack-resize.h>

/* -- General purpose LISP stack -------------------------------------------- */

cl_object *
ecl_stack_init(cl_object self, cl_index size, cl_index margin)
{
  cl_index limit_size = size-2*margin;
  self->stack.t = t_stack;
  self->stack.size = size;
  self->stack.limit_size = limit_size;
  self->stack.org = (cl_object *)ecl_malloc(size * sizeof(cl_object));
  self->stack.top = self->stack.org;
  self->stack.limit = &self->stack.org[limit_size];
  return self->stack.top;
}

cl_object *
ecl_stack_resize(cl_object self, cl_index size, cl_index margin)
{
  cl_index top = self->stack.top - self->stack.org;
  cl_index limit_size = size-2*margin;
  cl_object *new_stack;
  new_stack = (cl_object *)ecl_realloc(self->stack.org, size * sizeof(cl_object));
  self->stack.size = size;
  self->stack.limit_size = limit_size;
  self->stack.org = new_stack;
  self->stack.top = self->stack.org + top;
  self->stack.limit = &self->stack.org[limit_size];
  return self->stack.top;
}

cl_index
ecl_stack_index(cl_object self) {
  return self->stack.top - self->stack.org;
}

cl_object *
ecl_stack_unwind(cl_object self, cl_index ndx)
{
  self->stack.top = self->stack.org + ndx;
  return self->stack.top;
}

cl_object *
ecl_stack_tangle(cl_object self, cl_object them)
{
  cl_index ndx = ecl_stack_index(them);
  ecl_copy(self->stack.org, them->stack.org, ndx);
  self->stack.top = self->stack.org + ndx;
  return self->stack.top;
}

cl_object
ecl_stack_push(cl_object self, cl_object elt)
{
  cl_object *new_top = self->stack.top;
  if (ecl_unlikely(new_top >= self->stack.limit)) {
    cl_index old_size = self->stack.size;
    cl_index margin = old_size - self->stack.limit_size;
    cl_index new_size = old_size + old_size/2;
    new_top = ecl_stack_resize(self, new_size, margin);
  }
  self->stack.top = new_top+1;
  *new_top = (elt);
  return self;
}

cl_object
ecl_stack_pop(cl_object self)
{
  if (ecl_unlikely(self->stack.top <= self->stack.org))
    ecl_internal_error("ecl_pop: stack underflow");
  return *(--(self->stack.top));
}

cl_object
ecl_stack_dup(cl_object self)
{
  cl_object elt;
  if (ecl_unlikely(self->stack.top <= self->stack.org))
    ecl_internal_error("ecl_dup: empty stack");
  elt = ECL_STACK_TOP(self);
  ecl_stack_push(self, elt);
  return elt;
}

cl_object
ecl_stack_del(cl_object self, cl_object elt)
{
  cl_index idx;
  cl_index ndx = ECL_STACK_NDX(self);
  cl_object *v = self->stack.org;
  for(idx = 0; idx < ndx; idx++) {
    if (v[idx] == elt) {
      do { v[idx] = v[idx+1]; } while (++idx <= ndx);
      ecl_stack_popu(self);
      break;
    }
  }
  return self;
}

/* Unsafe operations */

cl_object
ecl_stack_popu(cl_object self)
{
  return *(--(self->stack.top));
}

void
ecl_stack_grow(cl_object self, cl_index n)
{
  self->stack.top += n;
}

void
ecl_stack_drop(cl_object self, cl_index n)
{
  self->stack.top -= n;
}

/* -- Frame stack ------------------------------------------------------------ */

static void
frs_init(cl_env_ptr env)
{
  cl_index size, margin, limit_size;
  margin = ecl_option_values[ECL_OPT_FRAME_STACK_SAFETY_AREA];
  limit_size = ecl_option_values[ECL_OPT_FRAME_STACK_SIZE];
  size = limit_size + 2 * margin;
  env->frs_stack.org = (ecl_frame_ptr)ecl_malloc(size * sizeof(*env->frs_stack.org));
  env->frs_stack.top = env->frs_stack.org-1;
  env->frs_stack.limit = &env->frs_stack.org[limit_size];
  env->frs_stack.size = size;
  env->frs_stack.limit_size = limit_size;
}

/* -- Binding stack ---------------------------------------------------------- */

static void
bds_init(cl_env_ptr env)
{
  cl_index size, margin, limit_size;
  margin = ecl_option_values[ECL_OPT_BIND_STACK_SAFETY_AREA];
  limit_size = ecl_option_values[ECL_OPT_BIND_STACK_SIZE];
  size = limit_size + 2 * margin;
  env->bds_stack.org = (ecl_bds_ptr)ecl_malloc(size * sizeof(*env->bds_stack.org));
  env->bds_stack.top = env->bds_stack.org-1;
  env->bds_stack.limit = &env->bds_stack.org[limit_size];
  env->bds_stack.size = size;
  env->bds_stack.limit_size = limit_size;
}

/* --------------------------------------------------------------------------- */

void
init_early_stacks(cl_env_ptr env)
{
  frs_init(env);
  bds_init(env);
  /* ihs_init(env); */
  /* vms_init(env); */
}
