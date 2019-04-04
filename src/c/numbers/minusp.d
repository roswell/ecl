/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * minusp.d - implementation of CL:MINUSP
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
cl_minusp(cl_object x)
{                               /* INV: ecl_minusp() checks type */
  @(return (ecl_minusp(x) ? ECL_T : ECL_NIL));
}

static int
ecl_minusp_fixnum(cl_object x)
{
  return ecl_fixnum_minusp(x);
}

static int
ecl_minusp_big(cl_object x)
{
  return _ecl_big_sign(x) < 0;
}

static int
ecl_minusp_ratio(cl_object x)
{
  return ecl_minusp(x->ratio.num);
}

static int
ecl_minusp_single_float(cl_object x)
{
  return ecl_single_float(x) < 0;
}

static int
ecl_minusp_double_float(cl_object x)
{
  return ecl_double_float(x) < 0;
}

#ifdef ECL_LONG_FLOAT
static int ecl_minusp_long_float(cl_object x)
{
  return ecl_long_float(x) < 0;
}
#endif

MATH_DEF_DISPATCH1_BOOL(minusp, @[minusp], @[real],
                        ecl_minusp_fixnum, ecl_minusp_big, ecl_minusp_ratio,
                        ecl_minusp_single_float, ecl_minusp_double_float,
                        ecl_minusp_long_float,
                        minuspfailed,
                        minuspfailed, minuspfailed, minuspfailed)
