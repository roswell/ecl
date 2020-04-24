/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * one_minus.d - implementation of CL:1-
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

/* INV: FLT_MIN - 1 == FLT_MIN
 *      DBL_MIN - 1 == DBL_MIN
 *      LDBL_MIN - 1 == LDBL_MIN
 * (no ECL_MATHERR_TEST needed) */

static cl_object
ecl_one_minus_fix(cl_object x)
{
  if (x == ecl_make_fixnum(MOST_NEGATIVE_FIXNUM))
    return ecl_make_integer(MOST_NEGATIVE_FIXNUM-1);
  return (cl_object)((cl_fixnum)x - ((cl_fixnum)ecl_make_fixnum(1) - ECL_FIXNUM_TAG));
}

static cl_object
ecl_one_minus_big(cl_object x)
{
  return ecl_minus(x, ecl_make_fixnum(1));
}

static cl_object
ecl_one_minus_ratio(cl_object x)
{
  return ecl_make_ratio(ecl_minus(x->ratio.num,x->ratio.den), x->ratio.den);
}

static cl_object
ecl_one_minus_single_float(cl_object x)
{
  return ecl_make_single_float(ecl_single_float(x) - 1);
}

static cl_object
ecl_one_minus_double_float(cl_object x)
{
  return ecl_make_double_float(ecl_double_float(x) - 1);
}

static cl_object
ecl_one_minus_long_float(cl_object x)
{
  return ecl_make_long_float(ecl_long_float(x) - 1);
}

static cl_object
ecl_one_minus_complex(cl_object x)
{
  return ecl_make_complex(ecl_one_minus(x->gencomplex.real),
                          x->gencomplex.imag);
}

#ifdef ECL_COMPLEX_FLOAT
static cl_object
ecl_one_minus_csfloat(cl_object x)
{
  return ecl_make_csfloat(ecl_csfloat(x) - 1);
}

static cl_object
ecl_one_minus_cdfloat(cl_object x)
{
  return ecl_make_cdfloat(ecl_cdfloat(x) - 1);
}

static cl_object
ecl_one_minus_clfloat(cl_object x)
{
  return ecl_make_clfloat(ecl_clfloat(x) - 1);
}
#endif

MATH_DEF_DISPATCH1_NE(one_minus, @[1-], @[number],
                      ecl_one_minus_fix, ecl_one_minus_big, ecl_one_minus_ratio,
                      ecl_one_minus_single_float, ecl_one_minus_double_float,
                      ecl_one_minus_long_float,
                      ecl_one_minus_complex,
                      ecl_one_minus_csfloat, ecl_one_minus_cdfloat, ecl_one_minus_clfloat);

/*  (1- x)  */
cl_object
@1-(cl_object x)
{       /* INV: type check is in ecl_one_minus() */
  @(return ecl_one_minus(x));
}
