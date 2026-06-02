/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * gfun.d - dispatch for generic functions
 *
 * Copyright (c) 1990 Giuseppe Attardi
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <string.h>
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/cache.h>
#include "newhash.h"

static ecl_cache_ptr
gf_method_cache(cl_object gfun)
{
  return ecl_cast_ptr(ecl_cache_ptr,GFUN_HIST(gfun)->foreign.data);
}

static cl_object generic_function_dispatch_vararg(cl_narg, ...);

cl_object
FEnot_funcallable_fixed()
{
  cl_env_ptr env = ecl_process_env();
  cl_object fun = env->function;
  FEerror("Not a funcallable instance ~A.", 1, fun);
  @(return);
}

cl_object
FEnot_funcallable_vararg(cl_narg narg, ...)
{
  return FEnot_funcallable_fixed();
}

static cl_object
user_function_dispatch(cl_narg narg, ...)
{
  int i;
  cl_object output;
  cl_env_ptr env = ecl_process_env();
  cl_object fun = env->function;
  struct ecl_stack_frame frame_aux;
  const cl_object frame = ecl_stack_frame_open(env, (cl_object)&frame_aux, narg);
  ecl_va_list args; ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < narg; i++) {
    ECL_STACK_FRAME_SET(frame, i, ecl_va_arg(args));
  }
  fun = fun->instance.gfdef;
  output = ecl_apply_from_stack_frame(frame, fun);
  ecl_stack_frame_close(frame);
  ecl_va_end(args);
  return output;
}

cl_object
clos_set_funcallable_instance_function(cl_object x, cl_object function_or_t)
{
  if (ecl_unlikely(!ECL_FUNCALLABLE_P(x))) {
    FEwrong_type_nth_arg(@[clos::set-funcallable-instance-function],
                         1, x, @[clos::funcallable-standard-object]);
  }
  x->instance.gfdef = function_or_t;
  if (function_or_t == ECL_T) {
    x->instance.isgf = ECL_STANDARD_DISPATCH;
    x->instance.entry = generic_function_dispatch_vararg;
  } else if (function_or_t == @'standard-generic-function') {
    x->instance.isgf = ECL_RESTRICTED_DISPATCH;
    x->instance.entry = generic_function_dispatch_vararg;
  } else if (function_or_t == ECL_NIL) {
    x->instance.isgf = ECL_NULL_DISPATCH;
    x->instance.entry = FEnot_funcallable_vararg;
  } else if (function_or_t == @'clos::standard-optimized-reader-method') {
    x->instance.isgf = ECL_READER_DISPATCH;
    x->instance.entry = ecl_slot_reader_dispatch;
  } else if (function_or_t == @'clos::standard-optimized-writer-method') {
    x->instance.isgf = ECL_WRITER_DISPATCH;
    x->instance.entry = ecl_slot_writer_dispatch;
  } else if (Null(cl_functionp(function_or_t))) {
    FEwrong_type_argument(@'function', function_or_t);
  } else {
    x->instance.isgf = ECL_USER_DISPATCH;
    x->instance.entry = user_function_dispatch;
  }
  @(return x);
}

cl_object
si_funcallable_object_p(cl_object x)
{
  cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, (ECL_FUNCALLABLE_P(x) ? ECL_T : ECL_NIL));
}

static cl_index
vector_hash_key(cl_object *keys, cl_index size)
{
  cl_index c, n = size, a = GOLDEN_RATIO, b = GOLDEN_RATIO;
  for (c = 0; n >= 3; ) {
    c += (cl_index)keys[--n];
    b += (cl_index)keys[--n];
    a += (cl_index)keys[--n];
    mix(a, b, c);
  }
  switch (n) {
  case 2: b += (cl_index)keys[--n];
  case 1: a += (cl_index)keys[--n];
    c += size;
    mix(a,b,c);
  }
  return c;
}

static cl_index
fill_spec_vector(cl_object *keys, cl_index len, cl_object frame, cl_object gf)
{
  cl_object *args = ECL_STACK_FRAME_PTR(frame);
  cl_index narg = frame->frame.size;
  cl_object spec_how_list = GFUN_SPEC(gf);
  int spec_no = 0;
  loop_for_on_unsafe(spec_how_list) {
    cl_object spec_how = ECL_CONS_CAR(spec_how_list);
    cl_object spec_eql = ECL_CONS_CDR(spec_how);
    cl_object eql_spec;
    cl_object this_arg;
    unlikely_if (spec_no >= narg)
      FEwrong_num_arguments(gf);
    unlikely_if (spec_no >= len)
      ecl_internal_error("Too many arguments to fill_spec_vector().");
    unlikely_if (!ECL_LISTP(spec_eql))
      ecl_internal_error("Invalid GF specialization profile.");
    this_arg = args[spec_no];
    /* Need to differentiate between EQL specializers and class specializers,
       because the EQL value can be a class, and may clash with a class
       specializer.  Store the cons cell containing the EQL value. */
    if (!Null(eql_spec = ecl_memql(this_arg, spec_eql))) {
      keys[spec_no++] = eql_spec;
    } else {
      keys[spec_no++] = cl_class_of(this_arg);
    }
  } end_loop_for_on_unsafe(spec_how_list);
  return vector_hash_key(keys, spec_no);
}

static cl_object
frame_to_list(cl_object frame)
{
  cl_object arglist, *p;
  cl_object *base = ECL_STACK_FRAME_PTR(frame);
  for (p = base + frame->frame.size, arglist = ECL_NIL; p != base; ) {
    arglist = CONS(*(--p), arglist);
  }
  return arglist;
}

static cl_object
frame_to_classes(cl_object frame)
{
  cl_object arglist, *p;
  cl_object *base = ECL_STACK_FRAME_PTR(frame);
  for (p = base + frame->frame.size, arglist = ECL_NIL; p != base; ) {
    arglist = CONS(cl_class_of(*(--p)), arglist);
  }
  return arglist;
}

static cl_object
generic_compute_applicable_method(cl_env_ptr env, cl_object frame, cl_object gf)
{
  /* method not cached */
  cl_object methods = _ecl_funcall3(@'clos::compute-applicable-methods-using-classes',
                                    gf, frame_to_classes(frame));
  unlikely_if (Null(env->values[1])) {
    cl_object arglist = frame_to_list(frame);
    methods = _ecl_funcall3(@'compute-applicable-methods',
                            gf, arglist);
    unlikely_if (methods == ECL_NIL) {
      env->values[1] = ECL_NIL;
      return methods;
    }
  }
  methods = clos_compute_effective_method_function(gf, GFUN_COMB(gf), methods);
  env->values[1] = ECL_T;
  return methods;
}

static cl_object
restricted_compute_applicable_method(cl_env_ptr env, cl_object frame, cl_object gf)
{
  /* method not cached */
  cl_object arglist = frame_to_list(frame);
  cl_object methods = clos_std_compute_applicable_methods(gf, arglist);
  unlikely_if (methods == ECL_NIL) {
    env->values[1] = ECL_NIL;
    return methods;
  }
  methods = clos_std_compute_effective_method(gf, GFUN_COMB(gf), methods);
  env->values[1] = ECL_T;
  return methods;
}

static cl_object
compute_applicable_method(cl_env_ptr env, cl_object frame, cl_object gf)
{
  if (gf->instance.isgf == ECL_RESTRICTED_DISPATCH)
    return restricted_compute_applicable_method(env, frame, gf);
  else
    return generic_compute_applicable_method(env, frame, gf);
}

cl_object
_ecl_standard_dispatch(cl_object frame, cl_object gf)
{
  const cl_env_ptr env = frame->frame.env;
  ecl_cache_ptr cache = gf_method_cache(gf);
  ecl_cache_record_ptr e;
  cl_index key_length = cache->key_length, hash;
  cl_object func, keys[key_length];
  ECL_WITHOUT_INTERRUPTS_BEGIN(env) {
    hash = fill_spec_vector(keys, key_length, frame, gf);
    e = ecl_search_cache(cache, hash, keys, key_length);
    if (e->key != OBJNULL) {
      func = e->value;
    } else {
      /* The keys and the cache may change while we
       * compute the applicable methods. We must save
       * the keys and recompute the cache location if
       * it was filled. */
      func = compute_applicable_method(env, frame, gf);
      if (env->values[1] != ECL_NIL) {
        if (e->key != OBJNULL) {
          e = ecl_search_cache(cache, hash, keys, key_length);
        }
        e->key = ecl_cache_make_key(cache, keys);
        e->value = func;
      }
    }
  } ECL_WITHOUT_INTERRUPTS_END;
  if (func == ECL_NIL)
    func = cl_apply(3, @'no-applicable-method', gf, frame);
  else {
    func = _ecl_funcall3(func, frame, ECL_NIL);
  }
  return func;
}

static cl_object
generic_function_dispatch_vararg(cl_narg narg, ...)
{
  cl_object output;
  ECL_STACK_FRAME_VARARGS_BEGIN(narg, narg, frame) {
    output = _ecl_standard_dispatch(frame, frame->frame.env->function);
  } ECL_STACK_FRAME_VARARGS_END(frame);
  return output;
}


cl_object
si_clear_gfun_hash(cl_object gf)
{
  /*
   * This function clears the generic function call hashes selectively.
   */
  cl_env_ptr the_env = ecl_process_env();
  ecl_cache_ptr cache;
  cl_index spec_args = ecl_length(GFUN_SPEC(gf));
  cache = ecl_make_cache(spec_args, 4096);
  GFUN_HIST(gf) = ecl_make_foreign_data(ECL_NIL, 0, cache);
  ecl_return0(the_env);
}
