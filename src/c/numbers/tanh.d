/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * tanh.d - trascendental functions: hyperbolic tangent
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
cl_tanh(cl_object x)
{
  @(return ecl_tanh(x));
}

static cl_object
ecl_tanh_rational(cl_object x)
{
  return ecl_make_single_float(tanhf(ecl_to_float(x)));
}

static cl_object
ecl_tanh_single_float(cl_object x)
{
  return ecl_make_single_float(tanhf(ecl_single_float(x)));
}

static cl_object
ecl_tanh_double_float(cl_object x)
{
  return ecl_make_double_float(tanh(ecl_double_float(x)));
}

static cl_object
ecl_tanh_long_float(cl_object x)
{
  return ecl_make_long_float(tanhl(ecl_long_float(x)));
}

static cl_object
ecl_tanh_complex(cl_object x)
{
  cl_object a = ecl_sinh(x);
  cl_object b = ecl_cosh(x);
  return ecl_divide(a, b);
}

#ifdef ECL_COMPLEX_FLOAT
static cl_object
ecl_tanh_csfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_csfloat);
  ecl_csfloat(result) = ctanhf(ecl_csfloat(x));
  return result;
}

static cl_object
ecl_tanh_cdfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_cdfloat);
  ecl_cdfloat(result) = ctanh(ecl_cdfloat(x));
  return result;
}

static cl_object
ecl_tanh_clfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_clfloat);
  ecl_clfloat(result) = ctanhl(ecl_clfloat(x));
  return result;
}
#endif

MATH_DEF_DISPATCH1(tanh, @[tanh], @[number],
                   ecl_tanh_rational, ecl_tanh_rational, ecl_tanh_rational,
                   ecl_tanh_single_float, ecl_tanh_double_float, ecl_tanh_long_float,
                   ecl_tanh_complex,
                   ecl_tanh_csfloat, ecl_tanh_cdfloat, ecl_tanh_clfloat);
