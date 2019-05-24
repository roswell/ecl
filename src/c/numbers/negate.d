/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * negate.d - trascendental functions: negateine
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/impl/math_dispatch.h>

static cl_object
ecl_negate_fix(cl_object x)
{
  return ecl_make_integer(-ecl_fixnum(x));
}

static cl_object
ecl_negate_big(cl_object x)
{
  return _ecl_big_negate(x);
}

static cl_object
ecl_negate_ratio(cl_object x)
{
  return ecl_make_ratio(ecl_negate(x->ratio.num), x->ratio.den);
}

static cl_object
ecl_negate_single_float(cl_object x)
{
  return ecl_make_single_float(-ecl_single_float(x));
}

static cl_object
ecl_negate_double_float(cl_object x)
{
  return ecl_make_double_float(-ecl_double_float(x));
}

static cl_object
ecl_negate_long_float(cl_object x)
{
  return ecl_make_long_float(-ecl_long_float(x));
}

static cl_object
ecl_negate_complex(cl_object x)
{
  return ecl_make_complex(ecl_negate(x->gencomplex.real),
                          ecl_negate(x->gencomplex.imag));
}

#ifdef ECL_COMPLEX_FLOAT
static cl_object
ecl_negate_csfloat(cl_object x)
{
  return ecl_make_csfloat(-ecl_csfloat(x));
}

static cl_object
ecl_negate_cdfloat(cl_object x)
{
  return ecl_make_cdfloat(-ecl_cdfloat(x));
}

static cl_object
ecl_negate_clfloat(cl_object x)
{
  return ecl_make_clfloat(-ecl_clfloat(x));
}
#endif

MATH_DEF_DISPATCH1_NE(negate, @[-], @[number],
                      ecl_negate_fix, ecl_negate_big, ecl_negate_ratio,
                      ecl_negate_single_float, ecl_negate_double_float,
                      ecl_negate_long_float,
                      ecl_negate_complex,
                      ecl_negate_csfloat, ecl_negate_cdfloat, ecl_negate_clfloat);
