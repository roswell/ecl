/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * cos.d - trascendental functions: cosine
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
cl_cos(cl_object x)
{
  @(return ecl_cos(x));
}

static cl_object
ecl_cos_rational(cl_object x)
{
  return ecl_make_single_float(cosf(ecl_to_float(x)));
}

static cl_object
ecl_cos_single_float(cl_object x)
{
  return ecl_make_single_float(cosf(ecl_single_float(x)));
}

static cl_object
ecl_cos_double_float(cl_object x)
{
  return ecl_make_double_float(cos(ecl_double_float(x)));
}

static cl_object
ecl_cos_long_float(cl_object x)
{
  return ecl_make_long_float(cosl(ecl_long_float(x)));
}

static cl_object
ecl_cos_complex(cl_object x)
{
  /* z = x + I y
     cos(z) = cosh(I z) = cosh(-y + I x)
  */
  cl_object dx = x->gencomplex.real;
  cl_object dy = x->gencomplex.imag;
  cl_object a = ecl_times(ecl_cos(dx), ecl_cosh(dy));
  cl_object b = ecl_times(ecl_negate(ecl_sin(dx)), ecl_sinh(dy));
  return ecl_make_complex(a, b);
}

#ifdef ECL_COMPLEX_FLOAT
static cl_object
ecl_cos_csfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_csfloat);
  ecl_csfloat(result) = ccosf(ecl_csfloat(x));
  return result;
}

static cl_object
ecl_cos_cdfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_cdfloat);
  ecl_cdfloat(result) = ccos(ecl_cdfloat(x));
  return result;
}

static cl_object
ecl_cos_clfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_clfloat);
  ecl_clfloat(result) = ccosl(ecl_clfloat(x));
  return result;
}
#endif

MATH_DEF_DISPATCH1(cos, @[cos], @[number],
                   ecl_cos_rational, ecl_cos_rational, ecl_cos_rational,
                   ecl_cos_single_float, ecl_cos_double_float, ecl_cos_long_float,
                   ecl_cos_complex,
                   ecl_cos_csfloat, ecl_cos_cdfloat, ecl_cos_clfloat);
