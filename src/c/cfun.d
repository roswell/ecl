/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * cfun.c - compiled functions
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <string.h>     /* for memmove() */

#include "cfun_dispatch.d"

cl_object
ecl_make_cfun(cl_objectfn_fixed c_function, cl_object name, cl_object cblock, int narg)
{
  cl_object cf;

  if (ecl_unlikely(narg < 0 || narg > ECL_C_ARGUMENTS_LIMIT)) {
    FEprogram_error("ecl_make_cfun: ~a", 1,
                    (narg < 0)
                    ? @"number of arguments must be greater than 0."
                    : @"function requires too many arguments.");
  }


  cf = ecl_alloc_object(t_cfunfixed);
  cf->cfunfixed.entry = fixed_dispatch_table[narg];
  cf->cfunfixed.entry_fixed = c_function;
  cf->cfunfixed.name = name;
  cf->cfunfixed.block = cblock;
  cf->cfunfixed.file = ECL_NIL;
  cf->cfunfixed.file_position = ecl_make_fixnum(-1);
  cf->cfunfixed.narg = narg;
  return cf;
}

cl_object
ecl_make_cfun_va(cl_objectfn c_function, cl_object name, cl_object cblock, int narg_fixed)
{
  cl_object cf;

  if (ecl_unlikely(narg_fixed < 0 || narg_fixed > ECL_C_ARGUMENTS_LIMIT)) {
    FEprogram_error("ecl_make_cfun_va: ~a", 1,
                    (narg_fixed < 0)
                    ? @"number of arguments must be greater than 0."
                    : @"function requires too many arguments.");
  }

  cf = ecl_alloc_object(t_cfun);
#ifdef ECL_C_COMPATIBLE_VARIADIC_DISPATCH
  if (narg_fixed == 0) {
    cf->cfun.entry = c_function;
  } else {
    cf->cfun.entry = variadic_dispatch_table[narg_fixed];
    cf->cfun.entry_variadic = c_function;
  }
#else
  cf->cfun.entry = c_function;
#endif
  cf->cfun.name = name;
  cf->cfun.block = cblock;
  cf->cfun.file = ECL_NIL;
  cf->cfun.file_position = ecl_make_fixnum(-1);
  cf->cfun.narg = narg_fixed;
  return cf;
}

cl_object
ecl_make_cclosure_va(cl_objectfn c_function, cl_object env, cl_object block, int narg_fixed)
{
  cl_object cc;

  if (ecl_unlikely(narg_fixed < 0 || narg_fixed > ECL_C_ARGUMENTS_LIMIT)) {
    FEprogram_error("ecl_make_cclosure_va: ~a", 1,
                    (narg_fixed < 0)
                    ? @"number of arguments must be greater than 0."
                    : @"function requires too many arguments.");
  }

  cc = ecl_alloc_object(t_cclosure);
#ifdef ECL_C_COMPATIBLE_VARIADIC_DISPATCH
  if (narg_fixed == 0) {
    cc->cclosure.entry = c_function;
  } else {
    cc->cclosure.entry = variadic_dispatch_table[narg_fixed];
    cc->cclosure.entry_variadic = c_function;
  }
#else
  cc->cclosure.entry = c_function;
#endif
  cc->cclosure.env = env;
  cc->cclosure.block = block;
  cc->cclosure.file = ECL_NIL;
  cc->cclosure.file_position = ecl_make_fixnum(-1);
  cc->cclosure.narg = narg_fixed;
  return cc;
}

void
ecl_def_c_function(cl_object sym, cl_objectfn_fixed c_function, int narg)
{
  si_fset(2, sym,
          ecl_make_cfun(c_function, sym, ecl_symbol_value(@'si::*cblock*'), narg));
}

void
ecl_def_c_macro(cl_object sym, cl_objectfn_fixed c_function, int narg)
{
  si_fset(3, sym,
          ecl_make_cfun(c_function, sym, ecl_symbol_value(@'si::*cblock*'), 2),
          ECL_T);
}

void
ecl_def_c_macro_va(cl_object sym, cl_objectfn c_function, int narg_fixed)
{
  si_fset(3, sym,
          ecl_make_cfun_va(c_function, sym, ecl_symbol_value(@'si::*cblock*'), narg_fixed),
          ECL_T);
}

void
ecl_def_c_function_va(cl_object sym, cl_objectfn c_function, int narg_fixed)
{
  si_fset(2, sym,
          ecl_make_cfun_va(c_function, sym, ecl_symbol_value(@'si::*cblock*'), narg_fixed));
}

cl_object
si_compiled_function_name(cl_object fun)
{
  cl_env_ptr the_env = ecl_process_env();
  cl_object output;

  switch(ecl_t_of(fun)) {
  case t_bclosure:
    fun = fun->bclosure.code;
  case t_bytecodes:
    output = fun->bytecodes.name; break;
  case t_cfun:
  case t_cfunfixed:
    output = fun->cfun.name; break;
  case t_cclosure:
    output = ECL_NIL; break;
  default:
    FEinvalid_function(fun);
  }
  ecl_return1(the_env, output);
}

cl_object
cl_function_lambda_expression(cl_object fun)
{
  cl_env_ptr the_env = ecl_process_env();
  cl_object output, name = ECL_NIL, lex = ECL_NIL;

  switch(ecl_t_of(fun)) {
  case t_bclosure:
    lex = fun->bclosure.lex;
    fun = fun->bclosure.code;
    /* fallthrough */
  case t_bytecodes:
    name = fun->bytecodes.name;
    output = fun->bytecodes.definition;
    if (name == ECL_NIL && output != ECL_NIL)
      output = cl_cons(@'lambda', output);
    else if (name != @'si::bytecodes' && output != ECL_NIL)
      output = @list*(3, @'ext::lambda-block', name, output);
    break;
  case t_cfun:
  case t_cfunfixed:
    name = fun->cfun.name;
    lex = ECL_NIL;
    output = ECL_NIL;
    break;
  case t_cclosure:
    name = ECL_NIL;
    lex = ECL_T;
    output = ECL_NIL;
    break;
  case t_instance:
    if (fun->instance.isgf) {
      name = ECL_NIL;
      lex = ECL_NIL;
      output = ECL_NIL;
      break;
    }
  default:
    FEinvalid_function(fun);
  }
  ecl_return3(the_env, output, lex, name);
}

cl_object
si_compiled_function_block(cl_object fun)
{
  cl_object output;

  switch(ecl_t_of(fun)) {
  case t_cfun:
    output = fun->cfun.block; break;
  case t_cfunfixed:
    output = fun->cfunfixed.block; break;
  case t_cclosure:
    output = fun->cclosure.block; break;
  default:
    FEerror("~S is not a C compiled function.", 1, fun);
  }
  @(return output);
}

cl_object
si_compiled_function_file(cl_object b)
{
  cl_env_ptr the_env = ecl_process_env();
 BEGIN:
  switch (ecl_t_of(b)) {
  case t_bclosure:
    b = b->bclosure.code;
    goto BEGIN;
  case t_bytecodes:
    ecl_return2(the_env, b->bytecodes.file, b->bytecodes.file_position);
  case t_cfun:
    ecl_return2(the_env, b->cfun.file, b->cfun.file_position);
  case t_cfunfixed:
    ecl_return2(the_env, b->cfunfixed.file, b->cfunfixed.file_position);
  case t_cclosure:
    ecl_return2(the_env, b->cclosure.file, b->cclosure.file_position);
  default:
    ecl_return2(the_env, ECL_NIL, ECL_NIL);
  }
}

void
ecl_set_function_source_file_info(cl_object b, cl_object source, cl_object position)
{
 BEGIN:
  switch (ecl_t_of(b)) {
  case t_bclosure:
    b = b->bclosure.code;
    goto BEGIN;
  case t_bytecodes:
    b->bytecodes.file = source;
    b->bytecodes.file_position = position;
    break;
  case t_cfun:
    b->cfun.file = source;
    b->cfun.file_position = position;
    break;
  case t_cfunfixed:
    b->cfunfixed.file = source;
    b->cfunfixed.file_position = position;
    break;
  case t_cclosure:
    b->cclosure.file = source;
    b->cclosure.file_position = position;
    break;
  default:
    FEerror("~S is not a compiled function.", 1, b);
  }
}

void
ecl_cmp_defmacro(cl_object fun)
{
  si_fset(3, fun->cfun.name, fun, ECL_T);
}

void
ecl_cmp_defun(cl_object fun)
{
  si_fset(2, fun->cfun.name, fun);
}
