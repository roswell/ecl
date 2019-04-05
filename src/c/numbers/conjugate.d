/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * conjugate.d - trascendental functions: conjugateine
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
cl_conjugate(cl_object x)
{
  @(return ecl_conjugate(x));
}

static cl_object
ecl_conjugate_real(cl_object x)
{
  return x;
}

static cl_object
ecl_conjugate_complex(cl_object x)
{
  return ecl_make_complex(x->gencomplex.real, ecl_negate(x->gencomplex.imag));
}

MATH_DEF_DISPATCH1_NE(conjugate, @[conjugate], @[number],
                      ecl_conjugate_real, ecl_conjugate_real, ecl_conjugate_real,
                      ecl_conjugate_real, ecl_conjugate_real,
                      ecl_conjugate_real,
                      ecl_conjugate_complex);
