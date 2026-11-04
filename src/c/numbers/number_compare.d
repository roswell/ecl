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

/* Helpers */

/* In Common Lisp, comparisons between floats and integers are performed via an
 * intermediate rationalization of the floating point number. In C, on the other
 * hand, the comparison is performed by converting the integer into a floating
 * point number. However, if the double type is too small this may lead to a
 * loss of precision and two numbers being told equal when, by Common Lisp
 * standards, would not. */
static int
compare_dfloat_fixnum(double d, cl_object nval)
{
  cl_fixnum n = ecl_fixnum(nval);
  if (d == (double)n) {
#if DBL_MANT_DIG >= ECL_FIXNUM_BITS
    return 0;
#else
    /* When we reach here, the double type has no significant decimal
     * part. However, as explained above, the double type is too small and
     * integers may coerce to the same double number giving a false
     * positive. Hence we perform the comparison in integer space. */
    cl_fixnum m = (cl_fixnum)d;
    if (n == m) {
      return 0;
    }
    return (m < n) ? -1 : +1;
#endif
  }
  return (d < (double)n) ? -1 : +1;
}

static int
compare_lfloat_fixnum(long double d, cl_object nval)
{
  cl_fixnum n = ecl_fixnum(nval);
  if (d == (long double)n) {
#if LDBL_MANT_DIG >= ECL_FIXNUM_BITS
    return 0;
#else
    cl_fixnum m = (cl_fixnum)d;
    if (n == m) {
      return 0;
    }
    return (m < n) ? -1 : +1;
#endif
  }
  return (d < (long double)n) ? -1 : +1;
}

static int
compare_ratio_integer(cl_object xval, cl_object yval)
{
  return ecl_number_compare(ecl_ratio_num(xval),
                            ecl_times(yval, ecl_ratio_den(xval)));
}

static int
compare_ratio_ratio(cl_object xval, cl_object yval)
{
  return ecl_number_compare(ecl_times(ecl_ratio_num(xval), ecl_ratio_den(yval)),
                            ecl_times(ecl_ratio_num(yval), ecl_ratio_den(xval)));
}

static int
compare_float_rational(cl_object xval, cl_object yval)
{
#ifdef ECL_IEEE_FP
  if (ecl_float_infinity_p(xval))
    return ecl_number_compare(xval, ecl_make_fixnum(0));
#endif
  return ecl_number_compare(cl_rational(xval), yval);
}

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
    CASE_FIXNUM_BIGNUM { return ecl_bigsgn(y) < 0 ?  1 : -1; }
    CASE_BIGNUM_FIXNUM { return ecl_bigsgn(x) < 0 ? -1 :  1; }
    CASE_BIGNUM_BIGNUM { return _ecl_big_compare(x, y); }
    CASE_FIXNUM_RATIO;
    CASE_BIGNUM_RATIO { return -compare_ratio_integer(y, x); }
    CASE_RATIO_FIXNUM;
    CASE_RATIO_BIGNUM { return +compare_ratio_integer(x, y); }
    CASE_RATIO_RATIO  { return compare_ratio_ratio(x, y); }
    /* float x fixnum */
    CASE_SINGLE_FLOAT_FIXNUM { return +compare_dfloat_fixnum(ecl_single_float(x), y); }
    CASE_FIXNUM_SINGLE_FLOAT { return -compare_dfloat_fixnum(ecl_single_float(y), x); }
    CASE_DOUBLE_FLOAT_FIXNUM { return +compare_dfloat_fixnum(ecl_double_float(x), y); }
    CASE_FIXNUM_DOUBLE_FLOAT { return -compare_dfloat_fixnum(ecl_double_float(y), x); }
    CASE_LONG_FLOAT_FIXNUM { return +compare_lfloat_fixnum(ecl_long_float(x), y); }
    CASE_FIXNUM_LONG_FLOAT { return -compare_lfloat_fixnum(ecl_long_float(y), x); }
    /* float x [bignum,ratio] */
    CASE_SINGLE_FLOAT_BIGNUM;
    CASE_SINGLE_FLOAT_RATIO;
    CASE_DOUBLE_FLOAT_BIGNUM;
    CASE_DOUBLE_FLOAT_RATIO;
    CASE_LONG_FLOAT_BIGNUM;
    CASE_LONG_FLOAT_RATIO { return compare_float_rational(x, y); }
    CASE_BIGNUM_SINGLE_FLOAT;
    CASE_RATIO_SINGLE_FLOAT;
    CASE_BIGNUM_DOUBLE_FLOAT;
    CASE_RATIO_DOUBLE_FLOAT;
    CASE_BIGNUM_LONG_FLOAT;
    CASE_RATIO_LONG_FLOAT { return -compare_float_rational(y, x); }
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
  c = ecl_va_arg(nums);
#ifdef ECL_IEEE_FP
  if (ecl_float_nan_p(c))
    return1(ECL_NIL);
#endif
  for (; --narg; c = d) {
    d = ecl_va_arg(nums);
    if (
#ifdef ECL_IEEE_FP
        ecl_float_nan_p(d) ||
#endif
        s*ecl_number_compare(d, c) < t)
      return1(ECL_NIL);
  }
  return1(ECL_T);
}

#define MONOTONIC(i, j) (cl_narg narg, ...)                     \
  { ecl_va_list nums; ecl_va_start(nums, narg, narg, 0);        \
    cl_object result = monotonic(i, j, narg, nums);             \
    ecl_va_end(nums);                                           \
    return result; }

cl_object @<= MONOTONIC( 1, 0)
cl_object @>= MONOTONIC(-1, 0)
cl_object @<  MONOTONIC( 1, 1)
cl_object @>  MONOTONIC(-1, 1)
