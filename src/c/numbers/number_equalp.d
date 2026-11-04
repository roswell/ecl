/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * number_compare.c - number comparison and sorting
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <float.h>
#include <ecl/ecl.h>
#include <ecl/impl/math_dispatch2.h>

@(defun = (num &rest nums)
  int i;
  @
  /* INV: For >= 2 arguments, ecl_number_equalp() performs checks */
  if (!ECL_NUMBER_TYPE_P(ecl_t_of(num))) {
    FEwrong_type_nth_arg(@[=], 1, num, @[number]);
  }
  for (i = 1; i < narg; i++) {
    if (!ecl_number_equalp(num, ecl_va_arg(nums))) {
      @(return ECL_NIL);
    }
  }
  @(return ECL_T);
  @)

/* Helpers */

static int
equalp_dfloat_fixnum(double d, cl_object nval)
{
  cl_fixnum n = ecl_fixnum(nval);
  /* Rounding to IEEE floats is monotonic. This is why cast of a signed integer
     to double can be used to see whether it is smaller or bigger. */
  if ((double)n != d) {
    return 0;
  } else {
#if DBL_MANT_DIG >= ECL_FIXNUM_BITS
    return 1;
#else
    /* If mantissa is too small we may have false positives for equal. */
    return (n == (cl_fixnum)d) ? 1 : 0;
#endif
  }
}

static int
equalp_lfloat_fixnum(long double d, cl_object nval)
{
  cl_fixnum n = ecl_fixnum(nval);
  /* Rounding to IEEE floats is monotonic. This is why cast of a signed integer
     to double can be used to see whether it is smaller or bigger. */
  if ((long double)n != d) {
    return 0;
  } else {
#if LDBL_MANT_DIG >= ECL_FIXNUM_BITS
    return 1;
#else
    /* If mantissa is too small we may have false positives for equal. */
    return (n == (cl_fixnum)d) ? 1 : 0;
#endif
  }
}

/* float <> rational functions are inspired by equalp hashing defined in
   hash.d. They don't cons and operate on scratch registers when necessary. */

static int
equalp_dfloat_bignum(double dval, cl_object nval)
{
  if (isfinite(dval) && dval==trunc(dval)) {
    /* Cheap tests first (compare signs and ranges). */
    const double limit = ldexp(1.0, ECL_FIXNUM_BITS-ECL_TAG_BITS-1);
    int signum_d = signbit(dval)      ? -1 : +1;
    int signum_n = ecl_bigsgn(nval)<0 ? -1 : +1;
    if ((signum_d != signum_n) ||
        (dval < limit && dval >= -limit)) {
      return 0;
    } else {
      cl_object reg = _ecl_big_register2();
      int equalp;
      _ecl_big_set_d(reg, dval);
      equalp = (_ecl_big_compare(reg, nval) == 0);
      _ecl_big_register_free(reg);
      return equalp;
    }
  }
  return 0;
}

static int
equalp_lfloat_bignum(long double dval, cl_object nval)
{
  if (isfinite(dval) && dval==truncl(dval)) {
    /* Cheap tests first (compare signs and ranges). */
    const long double limit = ldexpl(1.0, ECL_FIXNUM_BITS-ECL_TAG_BITS-1);
    int signum_d = signbit(dval)      ? -1 : +1;
    int signum_n = ecl_bigsgn(nval)<0 ? -1 : +1;
    if ((signum_d != signum_n) ||
        (dval < limit && dval >= -limit)) {
      return 0;
    } else {
      /* GMP doesn't have setter from long double. We first compute the mantissa
         as a bignum and then multiply it by 2^exp in bignum representation . */
#if LDBL_MANT_DIG <= ECL_FIXNUM_BITS
      int exponent, equalp, shift;
      long double dsrc = (signum_d==-1) ? -dval : dval;
      cl_object reg = _ecl_big_register2();
      cl_index mantissa = ldexpl(frexpl(dsrc, &exponent), LDBL_MANT_DIG);
      shift=exponent-LDBL_MANT_DIG;
      _ecl_big_set_idx(reg, mantissa);
      if (shift<0) _ecl_big_div_2exp(reg, reg, -shift); /* overshot */
      else         _ecl_big_mul_2exp(reg, reg, +shift); /* undershot */
      if(signum_d==-1) _ecl_big_neg(reg, reg);
      equalp = (_ecl_big_compare(reg, nval) == 0);
      _ecl_big_register_free(reg);
      return equalp;
#else
      int iterator, exponent, equalp, shift;
      long double frac;
      cl_object reg = _ecl_big_register2();
      unsigned long int limb;
      frac = (signum_d==-1) ? frexpl(-dval, &exponent) : frexpl(+dval, &exponent);
      _ecl_big_set_ui(reg, 0);
      for (iterator=0; iterator<LDBL_MANT_DIG && frac; iterator+=LONG_BITS) {
        frac=ldexpl(frac, LONG_BITS);
        limb=(unsigned long int)frac;
        frac-=limb;
        _ecl_big_mul_2exp(reg, reg, LONG_BITS);
        _ecl_big_add_ui(reg, reg, limb);
      }
      shift = exponent-iterator;
      if (shift<0) _ecl_big_div_2exp(reg, reg, -shift); /* overshot */
      else         _ecl_big_mul_2exp(reg, reg, +shift); /* undershot */
      if (signum_d==-1) _ecl_big_neg(reg, reg);
      equalp = (_ecl_big_compare(reg, nval) == 0);
      _ecl_big_register_free(reg);
      return equalp;
#endif
    }
  }
  return 0;
}

static inline int
dyadic_p(cl_object den)
{
  if (ECL_FIXNUMP(den)) {
    cl_fixnum dfix = ecl_fixnum(den);
    /* INV dfix>=2 so (dfix-1)>=1. */
    return !(dfix & (dfix-1));
  } else {
    /* When bignum is a pow2, then integer_length == ctz+1 or (popcount==1) */
    return ((_ecl_big_ctz(den)+1) == _ecl_big_integer_length(den));
  }
}

static int
equalp_dfloat_ratio(double dval, cl_object nval)
{
  if (isfinite(dval) && dval!=trunc(dval)) {
    cl_object num = ecl_ratio_num(nval);
    cl_object den = ecl_ratio_den(nval);
    if (dyadic_p(den)) {
      /* See comment in equalp_hash_ratio to understand why we don't need abs:
         length = ecl_integer_length(ecl_abs(num));. */
      cl_fixnum length = (ECL_FIXNUMP(num)
                          ? ecl_integer_length(num)
                          : _ecl_big_integer_length(num));
      cl_fixnum exponent = -(ecl_integer_length(den)-1);
      if (length <= DBL_MANT_DIG && exponent >= DBL_MIN_EXP-DBL_MANT_DIG) {
        double value = (ECL_FIXNUMP(num) ? ecl_fixnum(num) : _ecl_big_get_d(num));
        value = ldexp(value, exponent);
        return value == dval;
      }
    }
  }
  return 0;
}

static int
equalp_lfloat_ratio(long double dval, cl_object nval)
{
  if (isfinite(dval) && dval!=truncl(dval)) {
    cl_object num = ecl_ratio_num(nval);
    cl_object den = ecl_ratio_den(nval);
    if (dyadic_p(den)) {
      /* See comment in equalp_hash_ratio to understand why we don't need abs:
         length = ecl_integer_length(ecl_abs(num));. */
      cl_fixnum length = (ECL_FIXNUMP(num)
                          ? ecl_integer_length(num)
                          : _ecl_big_integer_length(num));
      cl_fixnum exponent = -(ecl_integer_length(den)-1);
      if (length <= LDBL_MANT_DIG && exponent >= LDBL_MIN_EXP-LDBL_MANT_DIG) {
        long double value = (ECL_FIXNUMP(num) ? ecl_fixnum(num) : _ecl_big_get_lf(num));
        value = ldexpl(value, exponent);
        return value == dval;
      }
    }
  }
  return 0;
}

/* This variant is exact and it conses, included for completness. Not used. */
#if 0
static int
equalp_xfloat_rational(cl_object dval, cl_object nval)
{
  return ecl_number_equalp(cl_rational(dval), nval);
}
#endif

#ifdef ECL_COMPLEX_FLOAT
static int
equalp_dfloat_rational(double dval, cl_object nval)
{
  switch(ecl_t_of(nval)) {
  case t_fixnum: return equalp_dfloat_fixnum(dval, nval);
  case t_bignum: return equalp_dfloat_bignum(dval, nval);
  case t_ratio: return equalp_dfloat_ratio(dval, nval);
  default:
    ecl_internal_error("equalp_dfloat_rational: unexpected branch.");
  }
}

static int
equalp_lfloat_rational(long double dval, cl_object nval)
{
  switch(ecl_t_of(nval)) {
  case t_fixnum: return equalp_lfloat_fixnum(dval, nval);
  case t_bignum: return equalp_lfloat_bignum(dval, nval);
  case t_ratio: return equalp_lfloat_ratio(dval, nval);
  default:
    ecl_internal_error("equalp_lfloat_rational: unexpected branch.");
  }
}

static int
equalp_cdfloat_complex(double _Complex dval, cl_object nval) {
  return (equalp_dfloat_rational(creal(dval), ecl_complex_real(nval)) &&
          equalp_dfloat_rational(cimag(dval), ecl_complex_imag(nval)));
}

static int
equalp_clfloat_complex(long double _Complex dval, cl_object nval) {
  return (equalp_lfloat_rational(creall(dval), ecl_complex_real(nval)) &&
          equalp_lfloat_rational(cimagl(dval), ecl_complex_imag(nval)));
}
#endif

/* Returns 1 if both numbers compare to equal */
int
ecl_number_equalp(cl_object x, cl_object y)
{
  /* INV: (= fixnum bignum) => 0 */
  /* INV: (= fixnum ratio) => 0 */
  /* INV: (= bignum ratio) => 0 */
  MATH_DISPATCH2_BEGIN(x,y) {
    /* rational x rational */
    CASE_FIXNUM_FIXNUM {
      return x == y; }
    CASE_BIGNUM_BIGNUM {
      return _ecl_big_compare(x,y) == 0; }
    CASE_RATIO_RATIO {
      return (ecl_number_equalp(ecl_ratio_num(x), ecl_ratio_num(y)) &&
              ecl_number_equalp(ecl_ratio_den(x), ecl_ratio_den(y))); }
    CASE_FIXNUM_BIGNUM;
    CASE_FIXNUM_RATIO;
    CASE_BIGNUM_FIXNUM;
    CASE_BIGNUM_RATIO;
    CASE_RATIO_FIXNUM;
    CASE_RATIO_BIGNUM { return 0; }
    /* float x fixnum */
    CASE_FIXNUM_SINGLE_FLOAT { return equalp_dfloat_fixnum(ecl_single_float(y), x); }
    CASE_SINGLE_FLOAT_FIXNUM { return equalp_dfloat_fixnum(ecl_single_float(x), y); }
    CASE_FIXNUM_DOUBLE_FLOAT { return equalp_dfloat_fixnum(ecl_double_float(y), x); }
    CASE_DOUBLE_FLOAT_FIXNUM { return equalp_dfloat_fixnum(ecl_double_float(x), y); }
    CASE_FIXNUM_LONG_FLOAT   { return equalp_lfloat_fixnum(ecl_long_float(y), x); }
    CASE_LONG_FLOAT_FIXNUM   { return equalp_lfloat_fixnum(ecl_long_float(x), y); }
    /* float x bignum */
    CASE_BIGNUM_SINGLE_FLOAT { return equalp_dfloat_bignum(ecl_single_float(y), x); }
    CASE_SINGLE_FLOAT_BIGNUM { return equalp_dfloat_bignum(ecl_single_float(x), y); }
    CASE_BIGNUM_DOUBLE_FLOAT { return equalp_dfloat_bignum(ecl_double_float(y), x); }
    CASE_DOUBLE_FLOAT_BIGNUM { return equalp_dfloat_bignum(ecl_double_float(x), y); }
    CASE_BIGNUM_LONG_FLOAT   { return equalp_lfloat_bignum(ecl_long_float(y), x); }
    CASE_LONG_FLOAT_BIGNUM   { return equalp_lfloat_bignum(ecl_long_float(x), y); }
    /* float x ratio */
    CASE_RATIO_SINGLE_FLOAT { return equalp_dfloat_ratio(ecl_single_float(y), x); }
    CASE_SINGLE_FLOAT_RATIO { return equalp_dfloat_ratio(ecl_single_float(x), y); }
    CASE_RATIO_DOUBLE_FLOAT { return equalp_dfloat_ratio(ecl_double_float(y), x); }
    CASE_DOUBLE_FLOAT_RATIO { return equalp_dfloat_ratio(ecl_double_float(x), y); }
    CASE_RATIO_LONG_FLOAT   { return equalp_lfloat_ratio(ecl_long_float(y), x); }
    CASE_LONG_FLOAT_RATIO   { return equalp_lfloat_ratio(ecl_long_float(x), y); }
    /* float x float */
    CASE_SINGLE_FLOAT_SINGLE_FLOAT { return ecl_single_float(x) == ecl_single_float(y); }
    CASE_SINGLE_FLOAT_DOUBLE_FLOAT { return ecl_single_float(x) == ecl_double_float(y); }
    CASE_SINGLE_FLOAT_LONG_FLOAT   { return ecl_single_float(x) == ecl_long_float(y); }
    CASE_DOUBLE_FLOAT_SINGLE_FLOAT { return ecl_double_float(x) == ecl_single_float(y); }
    CASE_DOUBLE_FLOAT_DOUBLE_FLOAT { return ecl_double_float(x) == ecl_double_float(y); }
    CASE_DOUBLE_FLOAT_LONG_FLOAT   { return ecl_double_float(x) == ecl_long_float(y); }
    CASE_LONG_FLOAT_SINGLE_FLOAT   { return ecl_long_float(x) == ecl_single_float(y); }
    CASE_LONG_FLOAT_DOUBLE_FLOAT   { return ecl_long_float(x) == ecl_double_float(y); }
    CASE_LONG_FLOAT_LONG_FLOAT     { return ecl_long_float(x) == ecl_long_float(y); }
    /* complex x real ; c?float x real */
    CASE_COMPLEX_FIXNUM;
    CASE_COMPLEX_BIGNUM;
    CASE_COMPLEX_RATIO;
    CASE_COMPLEX_SINGLE_FLOAT;
    CASE_COMPLEX_DOUBLE_FLOAT;
#ifdef ECL_COMPLEX_FLOAT
    CASE_CSFLOAT_FIXNUM;
    CASE_CSFLOAT_BIGNUM;
    CASE_CSFLOAT_RATIO;
    CASE_CSFLOAT_SINGLE_FLOAT;
    CASE_CSFLOAT_DOUBLE_FLOAT;
    CASE_CDFLOAT_FIXNUM;
    CASE_CDFLOAT_BIGNUM;
    CASE_CDFLOAT_RATIO;
    CASE_CDFLOAT_SINGLE_FLOAT;
    CASE_CDFLOAT_DOUBLE_FLOAT;
    CASE_CLFLOAT_FIXNUM;
    CASE_CLFLOAT_BIGNUM;
    CASE_CLFLOAT_RATIO;
    CASE_CLFLOAT_SINGLE_FLOAT;
    CASE_CLFLOAT_DOUBLE_FLOAT;
#endif
    CASE_COMPLEX_LONG_FLOAT;
#ifdef ECL_COMPLEX_FLOAT
    CASE_CSFLOAT_LONG_FLOAT;
    CASE_CDFLOAT_LONG_FLOAT;
    CASE_CLFLOAT_LONG_FLOAT;
#endif
    {
      if (!ecl_zerop(cl_imagpart(x))) { return 0; }
      return ecl_number_equalp(cl_realpart(x), y);
    }
    CASE_FIXNUM_COMPLEX;
    CASE_BIGNUM_COMPLEX;
    CASE_RATIO_COMPLEX;
    CASE_SINGLE_FLOAT_COMPLEX;
    CASE_DOUBLE_FLOAT_COMPLEX;
#ifdef ECL_COMPLEX_FLOAT
    CASE_FIXNUM_CSFLOAT;
    CASE_BIGNUM_CSFLOAT;
    CASE_RATIO_CSFLOAT;
    CASE_SINGLE_FLOAT_CSFLOAT;
    CASE_DOUBLE_FLOAT_CSFLOAT;
    CASE_FIXNUM_CDFLOAT;
    CASE_BIGNUM_CDFLOAT;
    CASE_RATIO_CDFLOAT;
    CASE_SINGLE_FLOAT_CDFLOAT;
    CASE_DOUBLE_FLOAT_CDFLOAT;
    CASE_FIXNUM_CLFLOAT;
    CASE_BIGNUM_CLFLOAT;
    CASE_RATIO_CLFLOAT;
    CASE_SINGLE_FLOAT_CLFLOAT;
    CASE_DOUBLE_FLOAT_CLFLOAT;
#endif
    CASE_LONG_FLOAT_COMPLEX;
#ifdef ECL_COMPLEX_FLOAT
    CASE_LONG_FLOAT_CSFLOAT;
    CASE_LONG_FLOAT_CDFLOAT;
    CASE_LONG_FLOAT_CLFLOAT;
#endif
    {
      if (!ecl_zerop(cl_imagpart(y))) { return 0; }
      return ecl_number_equalp(cl_realpart(y), x);
    }
    /* complex x complex */
    CASE_COMPLEX_COMPLEX {
      return (ecl_number_equalp(ecl_complex_real(x), ecl_complex_real(y)) &&
              ecl_number_equalp(ecl_complex_imag(x), ecl_complex_imag(y)));
    }
#ifdef ECL_COMPLEX_FLOAT
    /* complex x c?float */
    CASE_COMPLEX_CSFLOAT { return equalp_cdfloat_complex(ecl_csfloat(y), x); }
    CASE_CSFLOAT_COMPLEX { return equalp_cdfloat_complex(ecl_csfloat(x), y); }
    CASE_COMPLEX_CDFLOAT { return equalp_cdfloat_complex(ecl_cdfloat(y), x); }
    CASE_CDFLOAT_COMPLEX { return equalp_cdfloat_complex(ecl_cdfloat(x), y); }
    CASE_COMPLEX_CLFLOAT { return equalp_clfloat_complex(ecl_clfloat(y), x); }
    CASE_CLFLOAT_COMPLEX { return equalp_clfloat_complex(ecl_clfloat(x), y); }
    /* c?float x c?float */
    CASE_CSFLOAT_CSFLOAT { return ecl_csfloat(x) == ecl_csfloat(y); }
    CASE_CSFLOAT_CDFLOAT { return ecl_csfloat(x) == ecl_cdfloat(y); }
    CASE_CDFLOAT_CSFLOAT { return ecl_cdfloat(x) == ecl_csfloat(y); }
    CASE_CDFLOAT_CDFLOAT { return ecl_cdfloat(x) == ecl_cdfloat(y); }
    CASE_CSFLOAT_CLFLOAT { return ecl_csfloat(x) == ecl_clfloat(y); }
    CASE_CLFLOAT_CSFLOAT { return ecl_clfloat(x) == ecl_csfloat(y); }
    CASE_CDFLOAT_CLFLOAT { return ecl_cdfloat(x) == ecl_clfloat(y); }
    CASE_CLFLOAT_CDFLOAT { return ecl_clfloat(x) == ecl_cdfloat(y); }
    CASE_CLFLOAT_CLFLOAT { return ecl_clfloat(x) == ecl_clfloat(y); }
#endif
    CASE_UNKNOWN(@[=],x,y,@[number]);
  }
  MATH_DISPATCH2_END;;
}

@(defun /= (&rest nums &aux numi)
  int i, j;
  @
  if (narg == 0)
  FEwrong_num_arguments_anonym();
  numi = ecl_va_arg(nums);
  for (i = 2; i<=narg; i++) {
    ecl_va_list numb;
    ecl_va_start(numb, narg, narg, 0);
    numi = ecl_va_arg(nums);
    for (j = 1; j<i; j++)
      if (ecl_number_equalp(numi, ecl_va_arg(numb))) {
        ecl_va_end(numb);
        @(return ECL_NIL);
      }
    ecl_va_end(numb);
  }
  @(return ECL_T);
  @)
