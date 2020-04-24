/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * num_arith.d - arithmetic operations
 *
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/number.h>
#include <stdlib.h>
#include <ecl/impl/math_dispatch2.h>

cl_object
ecl_integer_divide(cl_object x, cl_object y)
{
  MATH_DISPATCH2_BEGIN(x,y) {
    CASE_FIXNUM_FIXNUM;
    if (y == ecl_make_fixnum(0)) {
      FEdivision_by_zero(x,y);
    }
    return ecl_make_fixnum(ecl_fixnum(x) / ecl_fixnum(y));
    CASE_FIXNUM_BIGNUM return _ecl_fix_divided_by_big(ecl_fixnum(x), y);
    CASE_BIGNUM_FIXNUM return _ecl_big_divided_by_fix(x, ecl_fixnum(y));
    CASE_BIGNUM_BIGNUM return _ecl_big_divided_by_big(x, y);
    CASE_UNKNOWN(@[round],x,y,@[integer]);
  }
  MATH_DISPATCH2_END;
}

@(defun gcd (&rest nums)
  cl_object gcd;
@
  if (narg == 0) {
    @(return ecl_make_fixnum(0));
  }
  /* INV: ecl_gcd() checks types */
  gcd = ecl_va_arg(nums);
  if (narg == 1) {
    assert_type_integer(gcd);
    @(return (ecl_minusp(gcd) ? ecl_negate(gcd) : gcd));
  }
  while (--narg)
    gcd = ecl_gcd(gcd, ecl_va_arg(nums));
  @(return gcd);
@)

cl_object
ecl_gcd(cl_object x, cl_object y)
{
  ECL_WITH_TEMP_BIGNUM(x_big,1);
  ECL_WITH_TEMP_BIGNUM(y_big,1);

  switch (ecl_t_of(x)) {
  case t_fixnum:
    _ecl_big_set_fixnum(x_big, ecl_fixnum(x));
    x = x_big;
  case t_bignum:
    break;
  default:
    FEwrong_type_nth_arg(@[gcd], 1, x, @[integer]);
  }
  switch (ecl_t_of(y)) {
  case t_fixnum:
    _ecl_big_set_fixnum(y_big, ecl_fixnum(y));
    y = y_big;
  case t_bignum:
    break;
  default:
    FEwrong_type_nth_arg(@[gcd], 2, y, @[integer]);
  }
  return _ecl_big_gcd(x, y);
}

@(defun lcm (&rest nums)
  cl_object lcm;
@
  if (narg == 0) {
    @(return ecl_make_fixnum(1));
  }
  /* INV: ecl_gcd() checks types. By placing `numi' before `lcm' in
     this call, we make sure that errors point to `numi' */
  lcm = ecl_va_arg(nums);
  assert_type_integer(lcm);
  while (narg-- > 1) {
    cl_object numi = ecl_va_arg(nums);
    cl_object t = ecl_times(lcm, numi);
    cl_object g = ecl_gcd(numi, lcm);
    if (g != ecl_make_fixnum(0))
      lcm = ecl_divide(t, g);
  }
  @(return (ecl_minusp(lcm) ? ecl_negate(lcm) : lcm));
@)
