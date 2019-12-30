/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * expt.d - exponentiate
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */


#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/impl/math_fenv.h>
#include <ecl/impl/math_dispatch.h>

#pragma STDC FENV_ACCESS ON

cl_fixnum
ecl_fixnum_expt(cl_fixnum x, cl_fixnum y)
{
  cl_fixnum z = 1;
  while (y > 0)
    if (y%2 == 0) {
      x *= x;
      y /= 2;
    } else {
      z *= x;
      --y;
    }
  return(z);
}

cl_object
cl_expt(cl_object x, cl_object y)
{
  @(return ecl_expt(x, y));
}

ecl_def_ct_single_float(singlefloat_one,1,static,const);
ecl_def_ct_double_float(doublefloat_one,1,static,const);
ecl_def_ct_long_float(longfloat_one,1,static,const);
#ifdef ECL_COMPLEX_FLOAT
ecl_def_ct_csfloat(csfloat_one,1,static,const);
ecl_def_ct_cdfloat(cdfloat_one,1,static,const);
ecl_def_ct_clfloat(clfloat_one,1,static,const);
#endif

static cl_object
expt_zero(cl_object x, cl_object y)
{
  cl_type ty, tx;
  cl_object z;
  ty = ecl_t_of(y);
  tx = ecl_t_of(x);
  if (ecl_unlikely(!ECL_NUMBER_TYPE_P(tx))) {
    FEwrong_type_nth_arg(@[expt], 1, x, @[number]);
  }
  /* INV: The most specific numeric types come first. */
  switch ((ty > tx)? ty : tx) {
  case t_fixnum:
  case t_bignum:
  case t_ratio:
    return ecl_make_fixnum(1);
  case t_singlefloat:
    return singlefloat_one;
  case t_doublefloat:
    return doublefloat_one;
  case t_longfloat:
    return longfloat_one;
  case t_complex:
    z = expt_zero((tx == t_complex)? x->gencomplex.real : x,
                  (ty == t_complex)? y->gencomplex.real : y);
    return ecl_make_complex(z, ecl_make_fixnum(0));
#ifdef ECL_COMPLEX_FLOAT
  case t_csfloat:
    return csfloat_one;
  case t_cdfloat:
    return cdfloat_one;
  case t_clfloat:
    return clfloat_one;
#endif
  default:
    /* We will never reach this */
    if (ty > tx)  FEwrong_type_nth_arg(@[expt], 1, x, @[number]);
    else          FEwrong_type_nth_arg(@[expt], 2, y, @[number]);
  }
}

static cl_object
ecl_expt_generic(cl_object x, cl_object y) {
  bool minusp = ecl_minusp(y);
  cl_object z = ecl_make_fixnum(1);
  if (minusp) {
    y = ecl_negate(y);
  }
  ECL_MATHERR_CLEAR;
  do {
    /* INV: ecl_integer_divide outputs an integer */
    if (!ecl_evenp(y)) {
      z = ecl_times(z, x);
    }
    y = ecl_integer_divide(y, ecl_make_fixnum(2));
    if (ecl_zerop(y)) {
      if (minusp) return ecl_divide(ecl_make_fixnum(1), z);
      else        return z;
    }
    x = ecl_times(x, x);
  } while (1);
  ECL_MATHERR_TEST;
}

static cl_object
ecl_expt_float(cl_object x, cl_object y) {
  cl_type
    tx = ecl_t_of(x),
    ty = ecl_t_of(y);
  switch((ty > tx) ? ty : tx) {
  case t_longfloat:
    return ecl_make_long_float
      (powl(ecl_to_long_double(x), ecl_to_long_double(y)));
  case t_doublefloat:
    return ecl_make_double_float
      (pow(ecl_to_double(x), ecl_to_double(y)));
  default:
    return ecl_make_single_float
      (powf(ecl_to_float(x), ecl_to_float(y)));
  }
}

#ifdef ECL_COMPLEX_FLOAT
static cl_object
ecl_expt_complex_float(cl_object x, cl_object y) {
  cl_type
    tx = ecl_t_of(x),
    ty = ecl_t_of(y);
  switch ((ty > tx)? ty : tx) {
  case t_clfloat:
  case t_longfloat:
    return ecl_make_clfloat
      (cpowl(ecl_to_clfloat(x), ecl_to_clfloat(y)));
  case t_cdfloat:
  case t_doublefloat:
    return ecl_make_cdfloat
      (cpow (ecl_to_cdfloat(x), ecl_to_cdfloat(y)));
  default:
    return ecl_make_csfloat
      (cpowf(ecl_to_csfloat(x), ecl_to_csfloat(y)));
  }
}
#endif

cl_object
ecl_expt(cl_object x, cl_object y)
{
  cl_type ty, tx;
  /* 0 ^ 0 -> 1 */
  if (ecl_unlikely(ecl_zerop(y))) {
    return expt_zero(x, y);
  }
  if (ecl_unlikely(ecl_zerop(x))) {
    /* 0 ^ -y = (1/0)^y */
    if (!ecl_plusp(cl_realpart(y))) {
      /* Normally we would have signalled FEdivision_by_zero, but fpe
         may be disabled and then we want to work on infinity. */
      return ecl_divide(ecl_make_fixnum(1), x);
    }
    /* 0 ^ +y = 0 */
    /* We call ecl_times to ensure the most contagious type.*/
    return ecl_times(x, y);
  }
  /* Here comes the order in which we handle cases: */
  /* -----------------------------------FIRST IF--- */
  /* rational ^ integer  -> rational                */
  /* complex  ^ integer  -> complex/rational        */
  /* float    ^ integer  -> float                   */
  /* cfloat   ^ integer  -> cfloat                  */
  /* ----------------------------------SECOND IF--- */
  /* number   ^ complex  -> cfloat                  */
  /* complex  ^ number   -> cfloat                  */
  /* negative ^ number   -> cfloat                  */
  /* ---------------------------------THIRD ELSE--- */
  /* positive ^ number   -> float                   */
  /* ---------------------------------------------- */
  /* x and y are already ensured to be a number by ecl_zerop above */
  ty = ecl_t_of(y);
  tx = ecl_t_of(x);
  if (ty == t_fixnum || ty == t_bignum) {
    switch (tx) {
    case t_fixnum:
    case t_bignum:
    case t_ratio:
    case t_complex:
      return ecl_expt_generic(x, y);
    case t_longfloat:
    case t_doublefloat:
    case t_singlefloat:
      return ecl_expt_float(x, y);
#ifdef ECL_COMPLEX_FLOAT
    case t_clfloat:
    case t_cdfloat:
    case t_csfloat:
      return ecl_expt_complex_float(x, y);
#endif
    default:
      ecl_internal_error("expt: unhandled switch branch.");
    }
  }
  if (ECL_COMPLEXP(y) || ECL_COMPLEXP(x) || ecl_minusp(x)) {
#ifdef ECL_COMPLEX_FLOAT
    return ecl_expt_complex_float(x, y);
#else
    /* We call expt_zero to ensure the most contagious type.*/
    cl_object z = ecl_log1(ecl_times(x, expt_zero(x, y)));
    z = ecl_times(z, y);
    z = ecl_exp(z);
    return z;
#endif
  }
  return ecl_expt_float(x, y);
}
