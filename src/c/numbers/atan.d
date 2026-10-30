/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * atan.d - Trascendental functions: arc tangent
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 * Copyright (c) 2016 Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/impl/math_fenv.h>

#pragma STDC FENV_ACCESS ON

ecl_def_ct_single_float(ecl_ct_flt_zero,0,static,const);
ecl_def_ct_single_float(ecl_ct_flt_one,1,static,const);
ecl_def_ct_single_float(ecl_ct_flt_two,2,static,const);

ecl_def_ct_complex(ecl_ct_imag_unit,ecl_ct_flt_zero,ecl_ct_flt_one,static,const);
ecl_def_ct_complex(ecl_ct_twice_imag_unit,ecl_ct_flt_zero,ecl_ct_flt_two,static,const);

cl_object
ecl_atan2(cl_object y, cl_object x)
{
  cl_object output;
  ECL_MATHERR_CLEAR;
  {
    int tx = ecl_t_of(x);
    int ty = ecl_t_of(y);
    if (tx < ty)
      tx = ty;
    if (tx == t_longfloat) {
      long double d = atan2l(ecl_to_long_double(y), ecl_to_long_double(x));
      output = ecl_make_long_float(d);
    } else {
      double dx = ecl_to_double(x);
      double dy = ecl_to_double(y);
      double dz = atan2(dy, dx);
      if (tx == t_doublefloat) {
        output = ecl_make_double_float(dz);
      } else {
        output = ecl_make_single_float(dz);
      }
    }
  }
  ECL_MATHERR_TEST;
  return output;
}

#if defined(ECL_COMPLEX_FLOAT)
cl_object
ecl_atan1(cl_object y)
{
  cl_object result;
  switch (ecl_t_of(y)) {
  case t_csfloat:
    result = ecl_alloc_object(t_csfloat);
    ECL_MATHERR_CLEAR;
    ecl_csfloat(result) = catanf(ecl_csfloat(y));
    ECL_MATHERR_TEST;
    return result;
  case t_cdfloat:
    result = ecl_alloc_object(t_cdfloat);
    ECL_MATHERR_CLEAR;
    ecl_cdfloat(result) = catan(ecl_cdfloat(y));
    ECL_MATHERR_TEST;
    return result;
  case t_clfloat:
    result = ecl_alloc_object(t_clfloat);
    ECL_MATHERR_CLEAR;
    ecl_clfloat(result) = catanl(ecl_clfloat(y));
    ECL_MATHERR_TEST;
    return result;
  case t_complex:
    result = ecl_alloc_object(t_csfloat);
    ECL_MATHERR_CLEAR;
    ecl_csfloat(result) = catanf(ecl_to_float(ecl_complex_real(y)) + I * ecl_to_float(ecl_complex_imag(y)));
    ECL_MATHERR_TEST;
    return result;
  default:
    return ecl_atan2(y, ecl_make_fixnum(1));
  }
}
#else
cl_object
ecl_atan1(cl_object y)
{
  if (ECL_COMPLEXP(y)) {
    /* The ANSI standard specifies atan as (log(1+iy) - log(1-iy))/(2i). */
    /* We use the equivalent formula log(1 + 2*i*y/(1-i*y))/(2*i).
     * This is more accurate for small values of y when using log1p.
     * We avoid potential inaccuracies around y = +I introduced by
     * this rewriting through reflecting inputs from the upper half
     * plane into the lower half plane and using atan(-y) = -atan(y).
     */
    bool reflect = FALSE;
    if (ecl_greater(ecl_complex_imag(y), ecl_make_fixnum(0))) {
      y = ecl_negate(y);
      reflect = TRUE;
    }
    cl_object z = ecl_times(ecl_ct_imag_unit, y);
    cl_object result = ecl_divide(ecl_log1p(ecl_divide(ecl_times(ecl_make_fixnum(2), z),
                                                       ecl_minus(ecl_make_fixnum(1), z))),
                                  ecl_ct_twice_imag_unit);
    /* Branch cut handling */
    cl_object r = ecl_complex_real(y);
    cl_object i = ecl_complex_imag(y);
    if (ecl_zerop(r) && ecl_lower(i,ecl_make_fixnum(-1)) && ecl_t_of(r) != t_fixnum && ecl_signbit(r)) {
      result = ecl_make_complex(ecl_times(ecl_make_fixnum(-1),ecl_complex_real(result)), ecl_complex_imag(result));
    }
    if (reflect) {
      result = ecl_negate(result);
    }
    return result;
  } else {
    return ecl_atan2(y, ecl_make_fixnum(1));
  }
}
#endif /* ECL_COMPLEX_FLOAT */

@(defun atan (x &optional (y OBJNULL))
  @
  /* INV: type check in ecl_atan1() & ecl_atan2() */
  /* FIXME ecl_atan1() and ecl_atan2() produce generic errors
     without recovery and function information. */
  if (y == OBJNULL) {
    @(return ecl_atan1(x));
  }
  @(return ecl_atan2(x, y));
  @)
