/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * plusp.d - implementation of CL:PLUSP
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

cl_object
cl_plusp(cl_object x)
{       /* INV: ecl_plusp()  checks type */
  @(return (ecl_plusp(x) ? ECL_T : ECL_NIL));
}

static int
ecl_plusp_fixnum(cl_object x)
{
  return ecl_fixnum_plusp(x);
}

static int
ecl_plusp_big(cl_object x)
{
  return _ecl_big_sign(x) > 0;
}

static int
ecl_plusp_ratio(cl_object x)
{
  return ecl_plusp(x->ratio.num);
}

static int
ecl_plusp_single_float(cl_object x)
{
  return ecl_single_float(x) > 0;
}

static int
ecl_plusp_double_float(cl_object x)
{
  return ecl_double_float(x) > 0;
}

#ifdef ECL_LONG_FLOAT
static int ecl_plusp_long_float(cl_object x)
{
  return ecl_long_float(x) > 0;
}
#endif

MATH_DEF_DISPATCH1_BOOL(plusp, @[plusp], @[real],
                        ecl_plusp_fixnum, ecl_plusp_big, ecl_plusp_ratio,
                        ecl_plusp_single_float, ecl_plusp_double_float,
                        ecl_plusp_long_float,
                        pluspfailed,
                        /* implementme*/ pluspfailed, pluspfailed, pluspfailed)
