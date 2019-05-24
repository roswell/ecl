/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * number_compare.d - number comparison
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */


#include <ecl/ecl.h>
#include <ecl/impl/math_dispatch2.h>
#include "numbers/float_fix_compare.d"

/*
 *
 * The value of ecl_number_compare(x, y) is
 *
 * -1      if      x < y
 *  0      if      x = y
 *  1      if      x > y.
 *
 * If x or y is not real, it fails.
*/
int
ecl_number_compare(cl_object x, cl_object y)
{
  double dx, dy;
  long double ldx, ldy;
 BEGIN:
  MATH_DISPATCH2_BEGIN(x,y) {
    /* rational x rational */
    CASE_FIXNUM_FIXNUM {
      cl_fixnum
        ix = ecl_fixnum(x),
        iy = ecl_fixnum(y);
      if (ix < iy) return -1;
      else         return (ix != iy);
    }
    /* INV: (= x y) can't be zero since fixnum != bignum */
    CASE_FIXNUM_BIGNUM { return _ecl_big_sign(y) < 0 ?  1 : -1; }
    CASE_BIGNUM_FIXNUM { return _ecl_big_sign(x) < 0 ? -1 :  1; }
    CASE_BIGNUM_BIGNUM { return _ecl_big_compare(x, y); }
    CASE_FIXNUM_RATIO;
    CASE_BIGNUM_RATIO { return ecl_number_compare(ecl_times(x, y->ratio.den), y->ratio.num); }
    CASE_RATIO_FIXNUM;
    CASE_RATIO_BIGNUM { return ecl_number_compare(x->ratio.num, ecl_times(y, x->ratio.den)); }
    CASE_RATIO_RATIO  { return ecl_number_compare(ecl_times(x->ratio.num, y->ratio.den),
                                                  ecl_times(y->ratio.num, x->ratio.den)); }
    /* float x fixnum */
    CASE_SINGLE_FLOAT_FIXNUM { return -double_fix_compare(ecl_fixnum(y), ecl_single_float(x)); }
    CASE_FIXNUM_SINGLE_FLOAT { return  double_fix_compare(ecl_fixnum(x), ecl_single_float(y)); }
    CASE_DOUBLE_FLOAT_FIXNUM { return -double_fix_compare(ecl_fixnum(y), ecl_double_float(x)); }
    CASE_FIXNUM_DOUBLE_FLOAT { return  double_fix_compare(ecl_fixnum(x), ecl_double_float(y)); }
    CASE_LONG_FLOAT_FIXNUM { return -long_double_fix_compare(ecl_fixnum(y), ecl_long_float(x)); }
    CASE_FIXNUM_LONG_FLOAT { return  long_double_fix_compare(ecl_fixnum(x), ecl_long_float(y)); }
    /* float x [bignum,ratio] */
    CASE_SINGLE_FLOAT_BIGNUM;
    CASE_SINGLE_FLOAT_RATIO;
    CASE_DOUBLE_FLOAT_BIGNUM;
    CASE_DOUBLE_FLOAT_RATIO;
    CASE_LONG_FLOAT_BIGNUM;
    CASE_LONG_FLOAT_RATIO;
    {
#ifdef ECL_IEEE_FP
      if (ecl_float_infinity_p(x))
        return ecl_number_compare(x, ecl_make_fixnum(0));
#endif
      x = cl_rational(x);
      goto BEGIN;
    }
    CASE_BIGNUM_SINGLE_FLOAT;
    CASE_RATIO_SINGLE_FLOAT;
    CASE_BIGNUM_DOUBLE_FLOAT;
    CASE_RATIO_DOUBLE_FLOAT;
    CASE_BIGNUM_LONG_FLOAT;
    CASE_RATIO_LONG_FLOAT;
    {
#ifdef ECL_IEEE_FP
      if (ecl_float_infinity_p(y))
        return ecl_number_compare(ecl_make_fixnum(0), y);
#endif
      y = cl_rational(y);
      goto BEGIN;
    }
    /* float x float */
    CASE_SINGLE_FLOAT_SINGLE_FLOAT {
      dx = ecl_single_float(x);
      dy = ecl_single_float(y);
      goto DOUBLEFLOAT;
    }
    CASE_SINGLE_FLOAT_DOUBLE_FLOAT {
      dx = ecl_single_float(x);
      dy = ecl_double_float(y);
      goto DOUBLEFLOAT;
    }
    CASE_DOUBLE_FLOAT_SINGLE_FLOAT {
      dx = ecl_double_float(x);
      dy = ecl_single_float(y);
      goto DOUBLEFLOAT;
    }
    CASE_DOUBLE_FLOAT_DOUBLE_FLOAT {
      dx = ecl_double_float(x);
      dy = ecl_double_float(y);
    DOUBLEFLOAT:
      if (dx == dy) return 0;
      else          return (dx < dy) ? -1 : 1;
    }
    CASE_SINGLE_FLOAT_LONG_FLOAT {
      ldx = ecl_single_float(x);
      ldy = ecl_long_float(y);
      goto LONGFLOAT;
    }
    CASE_LONG_FLOAT_SINGLE_FLOAT {
      ldx = ecl_long_float(x);
      ldy = ecl_single_float(y);
      goto LONGFLOAT;
    }
    CASE_DOUBLE_FLOAT_LONG_FLOAT  {
      ldx = ecl_double_float(x);
      ldy = ecl_long_float(y);
      goto LONGFLOAT;
    }
    CASE_LONG_FLOAT_DOUBLE_FLOAT  {
      ldx = ecl_long_float(x);
      ldy = ecl_double_float(y);
      goto LONGFLOAT;
    }
    CASE_LONG_FLOAT_LONG_FLOAT {
      ldx = ecl_long_float(x);
      ldy = ecl_long_float(y);
    LONGFLOAT:
      if (ldx == ldy) return 0;
      else            return (ldx < ldy) ? -1 : 1;
    }
    CASE_UNKNOWN(@[<],x,y,@[real]);
  }
  MATH_DISPATCH2_END;
}

static cl_object
monotonic(int s, int t, int narg, ecl_va_list nums)
{
  cl_object c, d;

  if (narg == 0) {
    FEwrong_num_arguments_anonym();
  }
  if (narg == 1) {
    c = ecl_va_arg(nums);
    if (ECL_REAL_TYPE_P(ecl_t_of(c))) {
      return1(ECL_T);
    }
    FEwrong_type_nth_arg(@[<], 1, c, @[real]);
  }
  /* INV: type check occurs in ecl_number_compare() */
  for (c = ecl_va_arg(nums); --narg; c = d) {
    d = ecl_va_arg(nums);
    if (s*ecl_number_compare(d, c) < t)
      return1(ECL_NIL);
  }
  return1(ECL_T);
}

#define MONOTONIC(i, j) (cl_narg narg, ...)                     \
  { ecl_va_list nums; ecl_va_start(nums, narg, narg, 0);        \
    cl_object result = monotonic(i, j, narg, nums);             \
    ecl_va_end(nums);                                           \
    return result; }

cl_object @<= MONOTONIC( 1, 0);
cl_object @>= MONOTONIC(-1, 0);
cl_object @<  MONOTONIC( 1, 1);
cl_object @>  MONOTONIC(-1, 1);
