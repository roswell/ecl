/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * sqrt.d - square root
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
#include <ecl/internal.h>
#include <ecl/impl/math_dispatch.h>

#pragma STDC FENV_ACCESS ON

cl_object
cl_sqrt(cl_object x)
{
  @(return ecl_sqrt(x));
}

static cl_object
ecl_sqrt_rational(cl_object x)
{
  if (ecl_minusp(x)) {
    x = ecl_sqrt_rational(ecl_negate(x));
    return ecl_make_complex(ecl_make_fixnum(0), x);
  } else {
    return ecl_make_single_float(sqrtf(ecl_to_float(x)));
  }
}

static cl_object
ecl_sqrt_single_float(cl_object x)
{
  float f = ecl_single_float(x);
  if (f < 0) {
    return ecl_make_complex(ecl_make_fixnum(0),
                            ecl_make_single_float(sqrtf(-f)));
  } else {
    return ecl_make_single_float(sqrtf(f));
  }
}

static cl_object
ecl_sqrt_double_float(cl_object x)
{
  double f = ecl_double_float(x);
  if (f < 0) {
    return ecl_make_complex(ecl_make_fixnum(0),
                            ecl_make_double_float(sqrt(-f)));
  } else {
    return ecl_make_double_float(sqrt(f));
  }
}

#ifdef ECL_LONG_FLOAT
static cl_object
ecl_sqrt_long_float(cl_object x)
{
  long double f = ecl_long_float(x);
  if (f < 0) {
    return ecl_make_complex(ecl_make_fixnum(0),
                            ecl_make_long_float(sqrtl(-f)));
  } else {
    return ecl_make_long_float(sqrtl(f));
  }
}
#endif

static cl_object
ecl_sqrt_complex(cl_object x)
{
  return ecl_expt(x, cl_core.plus_half);
}

MATH_DEF_DISPATCH1(sqrt, @[sqrt], @[number],
                   ecl_sqrt_rational, ecl_sqrt_rational, ecl_sqrt_rational,
                   ecl_sqrt_single_float, ecl_sqrt_double_float, ecl_sqrt_long_float,
                   ecl_sqrt_complex);
