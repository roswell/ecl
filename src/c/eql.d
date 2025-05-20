/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/* aux.c - early routines */

/* -- imports --------------------------------------------------------------- */

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/external.h>

#include <string.h>

/*
 * EQL-comparison of floats. If we are using signed zeros and NaNs,
 * numeric comparison of floating points is not equivalent to bit-wise
 * equality. In particular every two NaNs always give false
 *      (= #1=(/ 0.0 0.0) #1#) => NIL
 * and signed zeros always compare equal
 *      (= 0 -0.0) => T
 * which is not the same as what EQL should return
 *      (EQL #1=(/ 0.0 0.0) #1#) => T
 *      (EQL 0 -0.0) => NIL
 *
 * Furthermore, we can not use bit comparisons because in some platforms
 * long double has unused bits that makes two long floats be = but not eql.
 */
#if !defined(ECL_SIGNED_ZERO) && !defined(ECL_IEEE_FP)
#define FLOAT_EQL(name, type)                           \
  static bool name(type a, type b) { return a == b; }
#else
#define FLOAT_EQL(name, type)                                           \
  static bool name(type a, type b) {                                    \
    if (a == b) return signbit(a) == signbit(b);                        \
    if (isnan(a) || isnan(b)) return isnan(a) && isnan(b);              \
    return 0;                                                           \
  }
#endif

FLOAT_EQL(float_eql, float);
FLOAT_EQL(double_eql, double);
FLOAT_EQL(long_double_eql, long double);
#undef FLOAT_EQL

/* To avoid linking GMP in nucleus we directly compare limbs. */
static bool
_bignum_eql(cl_object x, cl_object y)
{
  cl_fixnum size;
  size = ECL_BIGNUM_SIZE(x);
  if(size != ECL_BIGNUM_SIZE(y)) return 0;
  return !memcmp(ECL_BIGNUM_LIMBS(x), ECL_BIGNUM_LIMBS(y),
                 ((size>0) ? size : -size) * (ECL_BIGNUM_LIMB_BITS/8));
}

bool
ecl_eql(cl_object x, cl_object y)
{
  if (x == y)
    return TRUE;
  if (ECL_IMMEDIATE(x) || ECL_IMMEDIATE(y))
    return FALSE;
  if (x->d.t != y->d.t)
    return FALSE;
  switch (x->d.t) {
  case t_bignum:
    return _bignum_eql(x, y);
  case t_ratio:
    return (ecl_eql(x->ratio.num, y->ratio.num) &&
            ecl_eql(x->ratio.den, y->ratio.den));
  case t_singlefloat:
    return float_eql(ecl_single_float(x), ecl_single_float(y));
  case t_longfloat:
    return long_double_eql(ecl_long_float(x), ecl_long_float(y));
  case t_doublefloat:
    return double_eql(ecl_double_float(x), ecl_double_float(y));
  case t_complex:
    return (ecl_eql(x->gencomplex.real, y->gencomplex.real) &&
            ecl_eql(x->gencomplex.imag, y->gencomplex.imag));
#ifdef ECL_COMPLEX_FLOAT
  case t_csfloat:
    return (float_eql(crealf(ecl_csfloat(x)), crealf(ecl_csfloat(y))) &&
            float_eql(cimagf(ecl_csfloat(x)), cimagf(ecl_csfloat(y))));
  case t_cdfloat:
    return (double_eql(creal(ecl_cdfloat(x)), creal(ecl_cdfloat(y))) &&
            double_eql(cimag(ecl_cdfloat(x)), cimag(ecl_cdfloat(y))));
  case t_clfloat:
    return (long_double_eql(creall(ecl_clfloat(x)), creall(ecl_clfloat(y))) &&
            long_double_eql(cimagl(ecl_clfloat(x)), cimagl(ecl_clfloat(y))));
#endif
#ifdef ECL_SSE2
  case t_sse_pack:
    return !memcmp(x->sse.data.b8, y->sse.data.b8, 16);
#endif
  default:
    return FALSE;
  }
}
