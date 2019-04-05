/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * zerop.d - implementation of CL:ZEROP
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
cl_zerop(cl_object x)
{       /* INV: ecl_zerop() checks type */
  @(return (ecl_zerop(x) ? ECL_T : ECL_NIL));
}

static int
ecl_zerop_fixnum(cl_object x)
{
  return x == ecl_make_fixnum(0);
}

static int
ecl_zerop_ratio(cl_object x)
{
  return 0;
}

static int
ecl_zerop_single_float(cl_object x)
{
  return ecl_single_float(x) == 0;
}

static int
ecl_zerop_double_float(cl_object x)
{
  return ecl_double_float(x) == 0;
}

#ifdef ECL_LONG_FLOAT
static int ecl_zerop_long_float(cl_object x)
{
  return ecl_long_float(x) == 0;
}
#endif

static int
ecl_zerop_complex(cl_object x)
{
  return ecl_zerop(x->gencomplex.real) && ecl_zerop(x->gencomplex.imag);
}

MATH_DEF_DISPATCH1_BOOL(zerop, @[zerop], @[number],
                        ecl_zerop_fixnum, ecl_zerop_ratio, ecl_zerop_ratio,
                        ecl_zerop_single_float, ecl_zerop_double_float,
                        ecl_zerop_long_float,
                        ecl_zerop_complex)
