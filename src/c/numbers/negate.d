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

#ifdef ECL_LONG_FLOAT
static cl_object
ecl_negate_long_float(cl_object x)
{
  return ecl_make_long_float(-ecl_long_float(x));
}
#endif

static cl_object
ecl_negate_complex(cl_object x)
{
  return ecl_make_complex(ecl_negate(x->complex.real),
                          ecl_negate(x->complex.imag));
}

MATH_DEF_DISPATCH1_NE(negate, @[-], @[number],
                      ecl_negate_fix, ecl_negate_big, ecl_negate_ratio,
                      ecl_negate_single_float, ecl_negate_double_float,
                      ecl_negate_long_float,
                      ecl_negate_complex);
