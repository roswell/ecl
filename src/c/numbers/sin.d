/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * sin.d - trascendental functions: sine
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
cl_sin(cl_object x)
{
  @(return ecl_sin(x));
}

static cl_object
ecl_sin_rational(cl_object x)
{
  return ecl_make_single_float(sinf(ecl_to_float(x)));
}

static cl_object
ecl_sin_single_float(cl_object x)
{
  return ecl_make_single_float(sinf(ecl_single_float(x)));
}

static cl_object
ecl_sin_double_float(cl_object x)
{
  return ecl_make_double_float(sin(ecl_double_float(x)));
}

static cl_object
ecl_sin_long_float(cl_object x)
{
  return ecl_make_long_float(sinl(ecl_long_float(x)));
}

static cl_object
ecl_sin_complex(cl_object x)
{
  /*
    z = x + I y
    z = x + I y
    sin(z) = sinh(I z) = sinh(-y + I x)
  */
  cl_object dx = x->gencomplex.real;
  cl_object dy = x->gencomplex.imag;
  cl_object a = ecl_times(ecl_sin(dx), ecl_cosh(dy));
  cl_object b = ecl_times(ecl_cos(dx), ecl_sinh(dy));
  return ecl_make_complex(a, b);
}

#ifdef ECL_COMPLEX_FLOAT
static cl_object
ecl_sin_csfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_csfloat);
  ecl_csfloat(result) = csinf(ecl_csfloat(x));
  return result;
}

static cl_object
ecl_sin_cdfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_cdfloat);
  ecl_cdfloat(result) = csin(ecl_cdfloat(x));
  return result;
}

static cl_object
ecl_sin_clfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_clfloat);
  ecl_clfloat(result) = csinl(ecl_clfloat(x));
  return result;
}
#endif

MATH_DEF_DISPATCH1(sin, @[sin], @[number],
                   ecl_sin_rational, ecl_sin_rational, ecl_sin_rational,
                   ecl_sin_single_float, ecl_sin_double_float, ecl_sin_long_float,
                   ecl_sin_complex,
                   ecl_sin_csfloat, ecl_sin_cdfloat, ecl_sin_clfloat);
