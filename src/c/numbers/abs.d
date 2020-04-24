/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * abs.d - absolute value
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <stdlib.h>
#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#include <ecl/internal.h>
#include <ecl/impl/math_dispatch.h>

cl_object
cl_abs(cl_object x)
{
  @(return ecl_abs(x));
}

static cl_object
ecl_abs_fixnum(cl_object x)
{
  return ecl_fixnum_minusp(x)? ecl_make_integer(-ecl_fixnum(x)) : x;
}

static cl_object
ecl_abs_bignum(cl_object x)
{
  return (_ecl_big_sign(x) < 0)? _ecl_big_negate(x) : x;
}

static cl_object
ecl_abs_rational(cl_object x)
{
  return (ecl_minusp(x->ratio.num))?
    ecl_make_ratio(ecl_negate(x->ratio.num), x->ratio.den) : x;
}

/* Example in ABS spec is a bit misleading because it says that

     (abs -0.0) ; -> -0.0

   but CLHS 1.4.3 states that the examples are not normative. */

static cl_object
ecl_abs_single_float(cl_object x)
{
  float f = ecl_single_float(x);
  f = fabsf(f);
  return ecl_make_single_float(f);
}

static cl_object
ecl_abs_double_float(cl_object x)
{
  double f = ecl_double_float(x);
  f = fabs(f);
  return ecl_make_double_float(f);
}

static cl_object
ecl_abs_long_float(cl_object x)
{
  long double f = ecl_long_float(x);
  f = fabsl(f);
  return ecl_make_long_float(f);
}

static cl_object
ecl_abs_complex(cl_object x)
{
  /* Compute sqrt(r*r + i*i) carefully to prevent overflow.
   * Assume |i| >= |r|. Then sqrt(i*i + r*r) = |i|*sqrt(1 +(r/i)^2).
   */
  cl_object r = ecl_abs(x->gencomplex.real);
  cl_object i = ecl_abs(x->gencomplex.imag);
  int comparison;
  comparison = ecl_number_compare(r, i);
  if (comparison == 0) {
    r = ecl_times(r, r);
    return ecl_sqrt(ecl_plus(r, r));
  } else {
    if (comparison > 0) {
      cl_object aux = i;
      i = r; r = aux;
    }
    r = ecl_divide(r, i);
    r = ecl_plus(ecl_make_fixnum(1), ecl_times(r, r));
    return ecl_times(cl_sqrt(r), i);
  }
}

#ifdef ECL_COMPLEX_FLOAT
static cl_object
ecl_abs_csfloat(cl_object x)
{
  float f = crealf(cabsf(ecl_csfloat(x)));
  x = ecl_make_single_float(f);
  return x;
}

static cl_object
ecl_abs_cdfloat(cl_object x)
{
  double f = creal(cabs(ecl_cdfloat(x)));
  x = ecl_make_double_float(f);
  return x;
}

static cl_object
ecl_abs_clfloat(cl_object x)
{
  long double f = creall(cabsl(ecl_clfloat(x)));
  x = ecl_make_long_float(f);
  return x;
}
#endif

MATH_DEF_DISPATCH1_NE(abs, @[abs], @[number],
                      ecl_abs_fixnum, ecl_abs_bignum, ecl_abs_rational,
                      ecl_abs_single_float, ecl_abs_double_float, ecl_abs_long_float,
                      ecl_abs_complex,
                      ecl_abs_csfloat, ecl_abs_cdfloat, ecl_abs_clfloat);
