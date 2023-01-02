/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * eclog1.d - trascendental functions: log(x)
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
#include <complex.h>
#include <float.h>
#include <ecl/impl/math_dispatch.h>

#pragma STDC FENV_ACCESS ON

#ifndef ECL_COMPLEX_FLOAT
static cl_object
ecl_log1_complex_inner(cl_object r, cl_object i)
{
  cl_object a = ecl_abs(r);
  cl_object p = ecl_abs(i);
  int rel = ecl_number_compare(a, p);
  if (rel > 0) {
    cl_object aux = p;
    p = a; a = aux;
  } else if (rel == 0) {
    /* if a == p, 
     * log(sqrt(a^2+p^2)) = log(2a^2)/2
     */
    a = ecl_times(a, a);
    a = ecl_divide(ecl_log1(ecl_plus(a, a)), ecl_make_fixnum(2));
    goto OUTPUT;
  }
  /* For the real part of the output we use the formula
   *      log(sqrt(p^2 + a^2)) = log(sqrt(p^2*(1 + (a/p)^2)))
   *                           = log(p) + log(1 + (a/p)^2)/2; */
  a = ecl_divide(a, p);
  a = ecl_plus(ecl_divide(ecl_log1p(ecl_times(a,a)), ecl_make_fixnum(2)),
               ecl_log1(p));
 OUTPUT:
  p = ecl_atan2(i, r);
  return ecl_make_complex(a, p);
}
#endif

static cl_object
ecl_log1_bignum(cl_object x)
{
  cl_fixnum l = ecl_integer_length(x) - 1;
  cl_object r = ecl_make_ratio(x, ecl_ash(ecl_make_fixnum(1), l));
  float f = ecl_to_float(r);
  if (f < 0) {
#ifdef ECL_COMPLEX_FLOAT
    return ecl_make_csfloat(clogf(f) + l * logf(2.0));
#else
    return ecl_make_complex(ecl_make_single_float(logf(-f) + l * logf(2.0)), ecl_make_single_float(ECL_PI_D));
#endif
  } else {
    return ecl_make_single_float(logf(f) + l * logf(2.0));
  }
}

static cl_object
ecl_log1_simple(cl_object x)
{
  float f = ecl_to_float(x);
  if (f < 0) {
#ifdef ECL_COMPLEX_FLOAT
    return ecl_make_csfloat(clogf(f));
#else
    return ecl_make_complex(ecl_make_single_float(logf(-f)), ecl_make_single_float(ECL_PI_D));
#endif
  }
  return ecl_make_single_float(logf(f));
}

static cl_object
ecl_log1_ratio(cl_object x)
{
  cl_object num = x->ratio.num;
  cl_object den = x->ratio.den;
  cl_index lnum = ecl_integer_length(num);
  cl_index lden = ecl_integer_length(den);
  if ((lnum > lden) ? (lnum - lden >= FLT_MAX_EXP) : (lden - lnum >= -FLT_MIN_EXP)) {
    cl_object numlog = ecl_log1(num);
    cl_object denlog = ecl_log1(den);
    return ecl_minus(numlog, denlog);
  } else {
    return ecl_log1_simple(x);
  }
}

static cl_object
ecl_log1_single_float(cl_object x)
{
  float f = ecl_single_float(x);
  if (isnan(f)) return x;
  if (f < 0) {
#ifdef ECL_COMPLEX_FLOAT
    return ecl_make_csfloat(clogf(f));
#else
    return ecl_make_complex(ecl_make_single_float(logf(-f)), ecl_make_single_float(ECL_PI_D));
#endif
  }
  return ecl_make_single_float(logf(f));
}

static cl_object
ecl_log1_double_float(cl_object x)
{
  double f = ecl_double_float(x);
  if (isnan(f)) return x;
  if (f < 0) {
#ifdef ECL_COMPLEX_FLOAT
    return ecl_make_cdfloat(clog(f));
#else
    return ecl_make_complex(ecl_make_double_float(log(-f)), ecl_make_double_float(ECL_PI_D));
#endif
  }
  return ecl_make_double_float(log(f));
}

static cl_object
ecl_log1_long_float(cl_object x)
{
  long double f = ecl_long_float(x);
  if (isnan(f)) return x;
  if (f < 0) {
#ifdef ECL_COMPLEX_FLOAT
    return ecl_make_clfloat(clogl(f));
#else
    return ecl_make_complex(ecl_make_long_float(logl(-f)), ecl_make_long_float(ECL_PI_L));
#endif
  }
  return ecl_make_long_float(logl(f));
}

static cl_object
ecl_log1_complex(cl_object x)
{
#ifdef ECL_COMPLEX_FLOAT
  cl_object result = ecl_alloc_object(t_csfloat);
  float _Complex fc = ecl_to_float(x->gencomplex.real) + I * ecl_to_float(x->gencomplex.real);
  ecl_csfloat(result) = clogf(fc);
  return result;
#else
  return ecl_log1_complex_inner(x->gencomplex.real, x->gencomplex.imag);
#endif
}

#ifdef ECL_COMPLEX_FLOAT
static cl_object
ecl_log1_csfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_csfloat);
  ecl_csfloat(result) = clogf(ecl_csfloat(x));
  return result;
}

static cl_object
ecl_log1_cdfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_cdfloat);
  ecl_cdfloat(result) = clog(ecl_cdfloat(x));
  return result;
}

static cl_object
ecl_log1_clfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_clfloat);
  ecl_clfloat(result) = clogl(ecl_clfloat(x));
  return result;
}
#endif

MATH_DEF_DISPATCH1(log1, @[log], @[number],
                   ecl_log1_simple, ecl_log1_bignum, ecl_log1_ratio,
                   ecl_log1_single_float, ecl_log1_double_float, ecl_log1_long_float,
                   ecl_log1_complex,
                   ecl_log1_csfloat, ecl_log1_cdfloat, ecl_log1_clfloat);

cl_object
ecl_log2(cl_object x, cl_object y)
{
  return ecl_divide(ecl_log1(y), ecl_log1(x));
}

@(defun log (x &optional (y OBJNULL))
  @       /* INV: type check in ecl_log1() and ecl_log2() */
  if (y == OBJNULL) {
    @(return ecl_log1(x));
  }
  @(return ecl_log2(y, x))
  @)

cl_object
si_log1p(cl_object x)
{
  @(return ecl_log1p(x));
}

static cl_object
ecl_log1p_simple(cl_object x)
{
  float f = ecl_to_float(x);
  if (f < -1) {
#ifdef ECL_COMPLEX_FLOAT
    cl_object result = ecl_alloc_object(t_csfloat);
    ecl_csfloat(result) = clogf(1.0+f);
    return result;
#else
    return ecl_log1_complex_inner(ecl_one_plus(x), ecl_make_fixnum(0));
#endif
  }
  return ecl_make_single_float(log1pf(ecl_to_float(x)));
}

static cl_object
ecl_log1p_single_float(cl_object x)
{
  float f = ecl_single_float(x);
  if (isnan(f)) return x;
  if (f < -1) {
#ifdef ECL_COMPLEX_FLOAT
    cl_object result = ecl_alloc_object(t_csfloat);
    ecl_csfloat(result) = clogf(1+f);
    return result;
#else
    return ecl_log1_complex_inner(ecl_one_plus(x), ecl_make_fixnum(0));
#endif
  }
  return ecl_make_single_float(log1pf(f));
}

static cl_object
ecl_log1p_double_float(cl_object x)
{
  double f = ecl_double_float(x);
  if (isnan(f)) return x;
  if (f < -1) {
#ifdef ECL_COMPLEX_FLOAT
    cl_object result = ecl_alloc_object(t_cdfloat);
    ecl_cdfloat(result) = clog(1+f);
    return result;
#else
    return ecl_log1_complex_inner(ecl_one_plus(x), ecl_make_fixnum(0));
#endif
  }
  return ecl_make_double_float(log1p(f));
}

static cl_object
ecl_log1p_long_float(cl_object x)
{
  long double f = ecl_long_float(x);
  if (isnan(f)) return x;
  if (f < -1) {
#ifdef ECL_COMPLEX_FLOAT
    cl_object result = ecl_alloc_object(t_clfloat);
    ecl_clfloat(result) = clogl(1+f);
    return result;
#else
    return ecl_log1_complex_inner(ecl_one_plus(x), ecl_make_fixnum(0));
#endif
  }
  return ecl_make_long_float(log1pl(f));
}

static cl_object
ecl_log1p_complex(cl_object x)
{
#ifdef ECL_COMPLEX_FLOAT
  cl_object result = ecl_alloc_object(t_csfloat);
  float _Complex fc = ecl_to_float(x->gencomplex.real) + I * ecl_to_float(x->gencomplex.real);
  ecl_csfloat(result) = clogf(1+fc);
  return result;
#else
  return ecl_log1_complex_inner(ecl_one_plus(x), ecl_make_fixnum(0));
#endif
}

#ifdef ECL_COMPLEX_FLOAT
static cl_object
ecl_log1p_csfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_csfloat);
  ecl_csfloat(result) = clogf(1+ecl_csfloat(x));
  return result;
}

static cl_object
ecl_log1p_cdfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_cdfloat);
  ecl_cdfloat(result) = clog(1+ecl_cdfloat(x));
  return result;
}

static cl_object
ecl_log1p_clfloat(cl_object x)
{
  cl_object result = ecl_alloc_object(t_clfloat);
  ecl_clfloat(result) = clogl(1+ecl_clfloat(x));
  return result;
}
#endif

MATH_DEF_DISPATCH1(log1p, @[si::log1p], @[number],
                   ecl_log1p_simple, ecl_log1p_simple, ecl_log1p_simple,
                   ecl_log1p_single_float, ecl_log1p_double_float, ecl_log1p_long_float,
                   ecl_log1p_complex,
                   ecl_log1p_csfloat, ecl_log1p_cdfloat, ecl_log1p_clfloat);
