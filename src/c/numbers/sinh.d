/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * sinh.d - trascendental functions: hyperbolic sine
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
cl_sinh(cl_object x)
{
  @(return ecl_sinh(x));
}

static cl_object
ecl_sinh_rational(cl_object x)
{
  return ecl_make_single_float(sinhf(ecl_to_float(x)));
}

static cl_object
ecl_sinh_single_float(cl_object x)
{
  return ecl_make_single_float(sinhf(ecl_single_float(x)));
}

static cl_object
ecl_sinh_double_float(cl_object x)
{
  return ecl_make_double_float(sinh(ecl_double_float(x)));
}

static cl_object
ecl_sinh_long_float(cl_object x)
{
  return ecl_make_long_float(sinhl(ecl_long_float(x)));
}

static cl_object
ecl_sinh_complex(cl_object x)
{
  /*
    z = x + I y
    sinh(z) = (exp(z)-exp(-z))/2
    = (exp(x)*(cos(y)+Isin(y))-exp(-x)*(cos(y)-Isin(y)))/2
    = sinh(x)*cos(y) + Icosh(x)*sin(y);
  */
  cl_object dx = x->gencomplex.real;
  cl_object dy = x->gencomplex.imag;
  cl_object a = ecl_times(ecl_sinh(dx), ecl_cos(dy));
  cl_object b = ecl_times(ecl_cosh(dx), ecl_sin(dy));
  return ecl_make_complex(a, b);
}

#ifdef ECL_COMPLEX_FLOAT
static cl_object
ecl_sinh_csfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_csfloat);
  ecl_csfloat(result) = csinhf(ecl_csfloat(x));
  return result;
}

static cl_object
ecl_sinh_cdfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_cdfloat);
  ecl_cdfloat(result) = csinh(ecl_cdfloat(x));
  return result;
}

static cl_object
ecl_sinh_clfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_clfloat);
  ecl_clfloat(result) = csinhl(ecl_clfloat(x));
  return result;
}
#endif

MATH_DEF_DISPATCH1(sinh, @[sinh], @[number],
                   ecl_sinh_rational, ecl_sinh_rational, ecl_sinh_rational,
                   ecl_sinh_single_float, ecl_sinh_double_float, ecl_sinh_long_float,
                   ecl_sinh_complex,
                   ecl_sinh_csfloat, ecl_sinh_cdfloat, ecl_sinh_clfloat);
