/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * stacks.d - runtime, binding, history and frame stacks
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
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

/* -- Bindings stack -------------------------------------------------------- */

static ecl_bds_ptr
get_bds_ptr(cl_object x)
{
  if (ECL_FIXNUMP(x)) {
    cl_env_ptr env = ecl_process_env();
    ecl_bds_ptr p = env->bds_stack.org + ecl_fixnum(x);
    if (env->bds_stack.org <= p && p <= env->bds_stack.top)
      return(p);
  }
  FEerror("~S is an illegal bds index.", 1, x);
}

cl_object
si_bds_top()
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, ecl_make_fixnum(env->bds_stack.top - env->bds_stack.org));
}

cl_object
si_bds_var(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, get_bds_ptr(arg)->symbol);
}

cl_object
si_bds_val(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  cl_object v = get_bds_ptr(arg)->value;
  ecl_return1(env, ((v == OBJNULL || v == ECL_NO_TL_BINDING)? ECL_UNBOUND : v));
}

/* -- Frame stack ----------------------------------------------------------- */

static ecl_frame_ptr
get_frame_ptr(cl_object x)
{
  if (ECL_FIXNUMP(x)) {
    cl_env_ptr env = ecl_process_env();
    ecl_frame_ptr p = env->frs_stack.org + ecl_fixnum(x);
    if (env->frs_stack.org <= p && p <= env->frs_stack.top)
      return p;
  }
  FEerror("~S is an illegal frs index.", 1, x);
}

cl_object
si_frs_top()
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, ecl_make_fixnum(env->frs_stack.top - env->frs_stack.org));
}

cl_object
si_frs_bds(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, ecl_make_fixnum(get_frame_ptr(arg)->frs_bds_ndx));
}

cl_object
si_frs_tag(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, get_frame_ptr(arg)->frs_val);
}

cl_object
si_frs_ihs(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, ecl_make_fixnum(get_frame_ptr(arg)->frs_ihs->index));
}

cl_object
si_sch_frs_base(cl_object fr, cl_object ihs)
{
  cl_env_ptr env = ecl_process_env();
  ecl_frame_ptr x;
  cl_index y = ecl_to_size(ihs);
  for (x = get_frame_ptr(fr);
       x <= env->frs_stack.top && x->frs_ihs->index < y;
       x++);
  ecl_return1(env, ((x > env->frs_stack.top)
                    ? ECL_NIL
                    : ecl_make_fixnum(x - env->frs_stack.org)));
}

/* -- Invocation stack ------------------------------------------------------ */

static ecl_ihs_ptr
get_ihs_ptr(cl_index n)
{
  cl_env_ptr env = ecl_process_env();
  ecl_ihs_ptr p = env->ihs_stack.top;
  if (n > p->index)
    FEerror("~D is an illegal IHS index.", 1, ecl_make_fixnum(n));
  while (n < p->index)
    p = p->next;
  return p;
}

cl_object
si_ihs_top(void)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, ecl_make_fixnum(env->ihs_stack.top->index));
}

cl_object
si_ihs_prev(cl_object x)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, cl_1M(x));
}

cl_object
si_ihs_next(cl_object x)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, cl_1P(x));
}

cl_object
si_ihs_bds(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, ecl_make_fixnum(get_ihs_ptr(ecl_to_size(arg))->bds));
}

cl_object
si_ihs_fun(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, get_ihs_ptr(ecl_to_size(arg))->function);
}

cl_object
si_ihs_lex(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, get_ihs_ptr(ecl_to_size(arg))->lex_env);
}

cl_object
si_ihs_lcl(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, get_ihs_ptr(ecl_to_size(arg))->lcl_env);
}

/* DEPRECATED backward compatibility with SWANK/SLYNK. --jd 2025-11-17 */
cl_object
si_ihs_env(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, get_ihs_ptr(ecl_to_size(arg))->lcl_env);
}

/* -- Lisp ops on stacks ---------------------------------------------------- */

cl_object
si_set_limit(cl_object type, cl_object limit)
{
  cl_env_ptr env = ecl_process_env();
  cl_index margin;
  if (type == @'ext::frame-stack') {
    cl_index current_size = env->frs_stack.top - env->frs_stack.org;
    cl_index request_size = ecl_to_size(limit);
    if(current_size > request_size)
      FEerror("Cannot shrink frame stack below ~D.", 1, limit);
    ecl_frs_set_limit(env, request_size);
  } else if (type == @'ext::binding-stack') {
    cl_index current_size = env->bds_stack.top - env->bds_stack.org;
    cl_index request_size = ecl_to_size(limit);
    if(current_size > request_size)
      FEerror("Cannot shrink binding stack below ~D.", 1, limit);
    ecl_bds_set_limit(env, request_size);
  } else if (type == @'ext::lisp-stack') {
    cl_index current_size = env->run_stack.top - env->run_stack.org;
    cl_index request_size = ecl_to_size(limit);
    if(current_size > request_size)
      FEerror("Cannot shrink lisp stack below ~D.", 1, limit);
    ecl_data_stack_set_limit(env, request_size);
  } else if (type == @'ext::c-stack') {
    cl_index the_size = ecl_to_size(limit);
    margin = ecl_option_values[ECL_OPT_C_STACK_SAFETY_AREA];
    ecl_cs_set_size(env, the_size + 2*margin);
  } else if (type == @'ext::heap-size') {
    /*
     * size_t can be larger than cl_index, and ecl_to_size()
     * creates a fixnum which is too small for size_t on 32-bit.
     */
    size_t the_size = (size_t)ecl_to_ulong(limit);
    _ecl_set_max_heap_size(the_size);
  }

  ecl_return1(env, si_get_limit(type));
}

cl_object
si_get_limit(cl_object type)
{
  cl_env_ptr env = ecl_process_env();
  cl_index output = 0;
  if (type == @'ext::frame-stack')
    output = env->frs_stack.limit_size;
  else if (type == @'ext::binding-stack')
    output = env->bds_stack.limit_size;
  else if (type == @'ext::lisp-stack')
    output = env->run_stack.limit_size;
  else if (type == @'ext::c-stack')
    output = env->c_stack.limit_size;
  else if (type == @'ext::heap-size') {
    /* size_t can be larger than cl_index */
    ecl_return1(env, ecl_make_unsigned_integer(ecl_core.max_heap_size));
  }

  ecl_return1(env, ecl_make_unsigned_integer(output));
}
