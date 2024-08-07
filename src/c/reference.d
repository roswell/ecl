/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * reference.d - reference in Constants and Variables
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
#include <ecl/ecl-inl.h>

/* Symbol-function returns */
/*         function-closure                for function */
/*         (macro . function-closure)      for macros */
/*         special                         for special forms. */
cl_object
cl_symbol_function(cl_object sym)
{
  cl_object output;
  int type = ecl_symbol_type(sym);
  if (type & ecl_stp_special_form) {
    output = @'special';
  } else if (Null(sym)) {
    FEundefined_function(sym);
  } else if (type & ecl_stp_macro) {
    output = CONS(@'si::macro', sym->symbol.macfun);
  } else if (ECL_FBOUNDP(sym)) {
    output = ECL_SYM_FUN(sym);
  } else {
    FEundefined_function(sym);
  }
  @(return output);
}

cl_object
cl_fdefinition(cl_object fname)
{
  @(return ((ECL_SYMBOLP(fname))? cl_symbol_function(fname) : ecl_fdefinition(fname)));
}

cl_object
cl_fboundp(cl_object fname)
{
  if (Null(fname)) {
    @(return ECL_NIL);
  } else if (ECL_SYMBOLP(fname)) {
    @(return (((fname->symbol.stype & (ecl_stp_special_form | ecl_stp_macro))
               || ECL_FBOUNDP(fname))? ECL_T : ECL_NIL));
  } else if (LISTP(fname)) {
    if (CAR(fname) == @'setf') {
      cl_object sym = CDR(fname);
      if (CONSP(sym) && CDR(sym) == ECL_NIL) {
        cl_object pair;
        sym = CAR(sym);
        if (ECL_SYMBOLP(sym)) {
          pair = ecl_setf_definition(sym, ECL_NIL);
          @(return ecl_cdr(pair));
        }
      }
    }
  }
  FEinvalid_function_name(fname);
}

cl_object
ecl_fdefinition(cl_object fun)
{
  cl_type t = ecl_t_of(fun);
  cl_object output;

  if (t == t_symbol) {
    unlikely_if (!ECL_FBOUNDP(fun) ||
                 fun->symbol.stype & (ecl_stp_macro | ecl_stp_special_form))
      FEundefined_function(fun);
    if (fun->symbol.stype & ecl_stp_macro)
      output = fun->symbol.macfun;
    else if (fun->symbol.stype & ecl_stp_special_form)
      output = ECL_NIL;
    else
      output = ECL_SYM_FUN(fun);
  } else unlikely_if (Null(fun)) {
    FEundefined_function(fun);
  } else if (t == t_list) {
    cl_object sym = CDR(fun);
    unlikely_if (!CONSP(sym))
      FEinvalid_function_name(fun);
    if (CAR(fun) == @'setf') {
      unlikely_if (CDR(sym) != ECL_NIL)
        FEinvalid_function_name(fun);
      sym = CAR(sym);
      unlikely_if (ecl_t_of(sym) != t_symbol)
        FEinvalid_function_name(fun);
      output = ecl_setf_definition(sym, ECL_NIL);
      unlikely_if (Null(ecl_cdr(output)))
        FEundefined_function(fun);
      output = ECL_CONS_CAR(output);
    } else if (CAR(fun) == @'lambda') {
      return si_make_lambda(ECL_NIL, sym);
    } else if (CAR(fun) == @'ext::lambda-block') {
      return si_make_lambda(CAR(sym), CDR(sym));
    } else {
      FEinvalid_function_name(fun);
    }
  } else {
    FEinvalid_function_name(fun);
  }
  return output;
}

cl_object
si_coerce_to_function(cl_object fun)
{
  cl_type t = ecl_t_of(fun);
  if (!(t == t_cfun || t == t_cfunfixed || t == t_cclosure
        || t == t_bytecodes || t == t_bclosure
        || (t == t_instance && fun->instance.isgf))) {
    fun = ecl_fdefinition(fun);
  }
  @(return fun);
}

cl_object
cl_symbol_value(cl_object sym)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object value;
  if (Null(sym)) {
    value = sym;
  } else {
    if (ecl_unlikely(!ECL_SYMBOLP(sym))) {
      FEwrong_type_only_arg(@[symbol-value], sym, @[symbol]);
    }
    value = ECL_SYM_VAL(the_env, sym);
    if (ecl_unlikely(value == OBJNULL)) {
      FEunbound_variable(sym);
    }
  }
  @(return value);
}

bool
ecl_boundp(cl_env_ptr env, cl_object sym)
{
  if (Null(sym)) {
    return 1;
  } else {
    if (ecl_unlikely(!ECL_SYMBOLP(sym)))
      FEwrong_type_only_arg(@[boundp], sym, @[symbol]);
    return ECL_SYM_VAL(env, sym) != OBJNULL;
  }
}

cl_object
cl_boundp(cl_object sym)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_boundp(the_env,sym)? ECL_T : ECL_NIL);
}

cl_object
cl_special_operator_p(cl_object form)
{
  const cl_env_ptr the_env = ecl_process_env();
  int special = ecl_symbol_type(form) & ecl_stp_special_form;
  ecl_return1(the_env, special? ECL_T : ECL_NIL);
}
