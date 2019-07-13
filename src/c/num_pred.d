/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * num_pred.d - predicates on numbers
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

int
ecl_oddp(cl_object x)
{
  if (ECL_FIXNUMP(x))
    return ecl_fixnum(x) & 1;
  unlikely_if (!ECL_BIGNUMP(x))
    FEwrong_type_only_arg(@[oddp], x, @[integer]);
  return _ecl_big_odd_p(x);
}

int
ecl_evenp(cl_object x)
{
  if (ECL_FIXNUMP(x))
    return ~ecl_fixnum(x) & 1;
  unlikely_if (!ECL_BIGNUMP(x))
    FEwrong_type_only_arg(@[evenp], x, @[integer]);
  return _ecl_big_even_p(x);
}

cl_object
cl_oddp(cl_object x)
{       /* INV: ecl_oddp() checks type */
  @(return (ecl_oddp(x) ? ECL_T : ECL_NIL));
}

cl_object
cl_evenp(cl_object x)
{       /* INV: ecl_evenp() checks_type */
  @(return (ecl_evenp(x) ? ECL_T : ECL_NIL));
}

cl_object
si_float_nan_p(cl_object x)
{
  @(return (ecl_float_nan_p(x)? ECL_T : ECL_NIL));
}

cl_object
si_float_infinity_p(cl_object x)
{
  @(return (ecl_float_infinity_p(x)? ECL_T : ECL_NIL));
}

bool
ecl_float_nan_p(cl_object x)
{
  switch (ecl_t_of(x)) {
  case t_singlefloat:
    return isnan(ecl_single_float(x));
  case t_doublefloat:
    return isnan(ecl_double_float(x));
  case t_longfloat:
    return isnan(ecl_long_float(x));
  default:
    return 0;
  }
}

bool
ecl_float_infinity_p(cl_object x)
{
  switch (ecl_t_of(x)) {
  case t_singlefloat:
    return isinf(ecl_single_float(x));
  case t_doublefloat:
    return isinf(ecl_double_float(x));
  case t_longfloat:
    return isinf(ecl_long_float(x));
  default:
    return 0;
  }
}
