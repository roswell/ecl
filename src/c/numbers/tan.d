/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * tan.d - trascendental functions: tangent
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
cl_tan(cl_object x)
{
  @(return ecl_tan(x));
}

static cl_object
ecl_tan_rational(cl_object x)
{
  return ecl_make_single_float(tanf(ecl_to_float(x)));
}

static cl_object
ecl_tan_single_float(cl_object x)
{
  return ecl_make_single_float(tanf(ecl_single_float(x)));
}

static cl_object
ecl_tan_double_float(cl_object x)
{
  return ecl_make_double_float(tan(ecl_double_float(x)));
}

static cl_object
ecl_tan_long_float(cl_object x)
{
  return ecl_make_long_float(tanl(ecl_long_float(x)));
}

static cl_object
ecl_tan_complex(cl_object x)
{
  cl_object a = ecl_sin(x);
  cl_object b = ecl_cos(x);
  return ecl_divide(a, b);
}

#ifdef ECL_COMPLEX_FLOAT
static cl_object
ecl_tan_csfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_csfloat);
  ecl_csfloat(result) = ctanf(ecl_csfloat(x));
  return result;
}

static cl_object
ecl_tan_cdfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_cdfloat);
  ecl_cdfloat(result) = ctan(ecl_cdfloat(x));
  return result;
}

static cl_object
ecl_tan_clfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_clfloat);
  ecl_clfloat(result) = ctanl(ecl_clfloat(x));
  return result;
}
#endif

MATH_DEF_DISPATCH1(tan, @[tan], @[number],
                   ecl_tan_rational, ecl_tan_rational, ecl_tan_rational,
                   ecl_tan_single_float, ecl_tan_double_float, ecl_tan_long_float,
                   ecl_tan_complex,
                   ecl_tan_csfloat, ecl_tan_cdfloat, ecl_tan_clfloat);
