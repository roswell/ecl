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
#include <ecl/bithacks.h>
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

/* integer length of |n| without consing (abs n) */
static inline cl_fixnum
magnitude_length(cl_object num)
{
  if (!ECL_FIXNUMP(num))
    return _ecl_big_integer_length(num); /* already magnitude */
  {
    cl_fixnum val = ecl_fixnum(num);
    cl_fixnum len = ecl_fixnum_bit_length(val);
    if (val<0 && ecl_ctz(val) == len) len++;
    return len;
  }
}

static int
compare_ratio_integer(cl_object xval, cl_object yval)
{
  cl_object xnum = ecl_ratio_num(xval);
  cl_object xden = ecl_ratio_den(xval);
  /* Ratio and integer are never equal and ratio is never 0. */
  /* 0. Cheap test (sign). */
  int xsig = ecl_minusp(xnum) ? -1 : +1;
  int ysig = ecl_minusp(yval) ? -1 : (ecl_zerop(yval) ? 0 : +1);
  if(xsig!=ysig) return xsig<ysig ? -1 : +1;
  /* 1. Compare magnitudes. */
  {
    /* Note that the description below holds for positive integers. When numbers
       are negative, then the comparison direction is swapped. */
    /* ln=maglen(yval) -> yval in [2^(ln-1);2^ln). */
    /* la=maglen(xnum) -> xnum in [2^(la-1);2^la)  */
    /* lb=maglen(xden) -> xden in [2^(lb-1);2^lb)  */
    /* xnum/xden -> [2^(la-1-lb), 2^(la-lb+1)) -> [2^(la-lb-1), 2^(la-lb+1)) */
    /* --- */
    /* ln   <= (la-lb-1) -> xnum/xden > ynum */
    /* ln-1 >= (la-lb+1) -> xnum/xden < ynum */
    cl_fixnum alen = magnitude_length(xnum);
    cl_fixnum blen = ecl_integer_length(xden);
    cl_fixnum nlen = magnitude_length(yval);
    cl_fixnum xleft  = (alen-blen-1);
    cl_fixnum xright = (alen-blen+1);
    cl_fixnum yleft  = (nlen-1);
    cl_fixnum yright = nlen;
    if (yright <= xleft)  return xsig==+1 ? +1 : -1;
    if (yleft  >= xright) return xsig==+1 ? -1 : +1;
  }
  /* 2. fast path for fixnums (when __SIZEOF_INT128__ is defined). */
  /* 3. slow path using a bignum register. */
  {
    /* Since we operate strictly on bignums, there is no need to normalize to
       fixnum even when the result falls in a fixnum range. */
    int result;
    cl_object reg1 = _ecl_big_register1();
    cl_object reg2 = _ecl_big_register2();
    /* Initialize registers. */
    if (ECL_FIXNUMP(xnum)) _ecl_big_set_fix(reg1, ecl_fixnum(xnum));
    else                   _ecl_big_set(reg1, xnum);
    if (ECL_FIXNUMP(yval)) _ecl_big_set_fix(reg2, ecl_fixnum(yval));
    else                   _ecl_big_set(reg2, yval);
    /* INV denominator is always positive. */
    if (ECL_FIXNUMP(xden)) _ecl_big_mul_ui(reg2, reg2, ecl_fixnum(xden));
    else                   _ecl_big_mul(reg2, reg2, xden);
    result = _ecl_big_compare(reg1, reg2);
    _ecl_big_register_free(reg1);
    _ecl_big_register_free(reg2);
    return result;
  }
  /* Unoptimized version (never reached): */
  /* return ecl_number_compare(xnum, ecl_times(yval, xden)); */
}

static int
compare_ratio_ratio(cl_object xval, cl_object yval)
{
  cl_object xnum = ecl_ratio_num(xval);
  cl_object xden = ecl_ratio_den(xval);
  cl_object ynum = ecl_ratio_num(yval);
  cl_object yden = ecl_ratio_den(yval);
  /* Cheap tests: */
  int xsig = ecl_minusp(xnum) ? -1 : +1;
  int ysig = ecl_minusp(ynum) ? -1 : +1;
  if (xsig!=ysig) return xsig<ysig ? -1 : +1;
  /* Integer comparison is cheap. */
  if (ecl_number_equalp(xden, yden)) {
    if (xnum==ynum) return 0;
    else return ecl_number_compare(xnum, ynum);
  }
  /* 1. Compare magnitudes (c.f compare_ratio_integer) */
  {
    cl_fixnum alen = magnitude_length(xnum);
    cl_fixnum blen = ecl_integer_length(xden);
    cl_fixnum clen = magnitude_length(ynum);
    cl_fixnum dlen = ecl_integer_length(yden);
    cl_fixnum xleft  = (alen-blen-1);
    cl_fixnum xright = (alen-blen+1);
    cl_fixnum yleft  = (clen-dlen-1);
    cl_fixnum yright = (clen-dlen+1);
    if (yright <= xleft)  return xsig==+1 ? +1 : -1;
    if (yleft  >= xright) return xsig==+1 ? -1 : +1;
  }
  /* 2. fast path for fixnums (when __SIZEOF_INT128__ is defined). */
  /* 3. slow path using a bignum register. */
  {
    /* Since we operate strictly on bignums, there is no need to normalize to
       fixnum even when the result falls in a fixnum range. */
    int result;
    cl_object reg1 = _ecl_big_register1();
    cl_object reg2 = _ecl_big_register2();
    /* Initialize registers. */
    if (ECL_FIXNUMP(xnum)) _ecl_big_set_fix(reg1, ecl_fixnum(xnum));
    else                   _ecl_big_set(reg1, xnum);
    if (ECL_FIXNUMP(ynum)) _ecl_big_set_fix(reg2, ecl_fixnum(ynum));
    else                   _ecl_big_set(reg2, ynum);
    /* INV denominator is always positive. */
    if (ECL_FIXNUMP(yden)) _ecl_big_mul_ui(reg1, reg1, ecl_fixnum(yden));
    else                   _ecl_big_mul(reg1, reg1, yden);
    if (ECL_FIXNUMP(xden)) _ecl_big_mul_ui(reg2, reg2, ecl_fixnum(xden));
    else                   _ecl_big_mul(reg2, reg2, xden);
    result = _ecl_big_compare(reg1, reg2);
    _ecl_big_register_free(reg1);
    _ecl_big_register_free(reg2);
    return result;
  }
  /* Unoptimized version (never reached): */
  /* return ecl_number_compare(ecl_times(xnum, yden), */
  /*                           ecl_times(ynum, xden)); */
}

static int
compare_dfloat_rational(double dval, cl_object yval)
{
  int xsig, ysig;
  cl_object xval;
  /* Cheap tests: */
#ifdef ECL_IEEE_FP
  if (isinf(dval))
    return (dval<0) ? -1 : +1;
#endif
  if (dval==0)
    return ecl_minusp(yval) ? +1 : (ecl_zerop(yval) ? 0 : -1);
  xsig = signbit(dval) ? -1 : +1;
  ysig = ecl_minusp(yval) ? -1 : (ecl_zerop(yval) ? 0 : +1);
  if (xsig!=ysig) return xsig<ysig ? -1 : +1;
  /* 1. compare magnitudes */
  /* 2. dyadic comparison in a register */
  /* Unoptimized version: */
  xval = ecl_make_double_float(dval);
  return ecl_number_compare(cl_rational(xval), yval);
}

static int
compare_lfloat_rational(long double dval, cl_object yval)
{
  int xsig, ysig;
  cl_object xval;
  /* Cheap tests: */
#ifdef ECL_IEEE_FP
  if (isinf(dval))
    return (dval<0) ? -1 : +1;
#endif
  if (dval==0)
    return ecl_minusp(yval) ? +1 : (ecl_zerop(yval) ? 0 : -1);
  xsig = signbit(dval) ? -1 : +1;
  ysig = ecl_minusp(yval) ? -1 : (ecl_zerop(yval) ? 0 : +1);
  if (xsig!=ysig) return xsig<ysig ? -1 : +1;
  /* 1. compare magnitudes */
  /* 2. dyadic comparison in a register */
  /* Unoptimized version: */
  xval = ecl_make_long_float(dval);
  return ecl_number_compare(cl_rational(xval), yval);
}

/*
 *
 * The value of ecl_number_compare(x, y) is
 *
 * -n      if      x < y
 *  0      if      x = y
 * +n      if      x > y.
 *
 * If x or y is not real, it fails.
 * n is a positive int (c.f mpz_cmp ).
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
    CASE_LONG_FLOAT_FIXNUM   { return +compare_lfloat_fixnum(ecl_long_float(x), y); }
    CASE_FIXNUM_LONG_FLOAT   { return -compare_lfloat_fixnum(ecl_long_float(y), x); }
    /* float x [bignum,ratio] */
    CASE_SINGLE_FLOAT_BIGNUM;
    CASE_SINGLE_FLOAT_RATIO  { return +compare_dfloat_rational(ecl_single_float(x), y); }
    CASE_BIGNUM_SINGLE_FLOAT;
    CASE_RATIO_SINGLE_FLOAT  { return -compare_dfloat_rational(ecl_single_float(y), x); }
    CASE_DOUBLE_FLOAT_BIGNUM;
    CASE_DOUBLE_FLOAT_RATIO  { return +compare_dfloat_rational(ecl_double_float(x), y); }
    CASE_BIGNUM_DOUBLE_FLOAT;
    CASE_RATIO_DOUBLE_FLOAT  { return -compare_dfloat_rational(ecl_double_float(y), x); }
    CASE_LONG_FLOAT_BIGNUM;
    CASE_LONG_FLOAT_RATIO    { return +compare_lfloat_rational(ecl_long_float(x), y); }
    CASE_BIGNUM_LONG_FLOAT;
    CASE_RATIO_LONG_FLOAT    { return -compare_lfloat_rational(ecl_long_float(y), x); }
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
