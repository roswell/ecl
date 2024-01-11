/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
 * Copyright (c) 2010, Juan Jose Garcia Ripoll.
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/* math_dispatch.h -- fast dispatch for math functions */
#ifndef ECL_MATH_DISPATCH2_H
#define ECL_MATH_DISPATCH2_H

#include <ecl/internal.h> /* for unlikely_if */

#define MATH_DISPATCH2_LABEL(t1,t2) case ((t1)*(t_last_number+1)+(t2)):
#define MATH_DISPATCH2_BEGIN(x,y) {                                     \
    int tx = ecl_t_of(x), ty = ecl_t_of(y);                               \
    unlikely_if (ty > t_last_number) { goto DISPATCH2_ERROR; } \
    switch (tx * (t_last_number+1) + ty)
#define MATH_DISPATCH2_END } (void)0

#define CASE_CLFLOAT_FIXNUM \
    MATH_DISPATCH2_LABEL(t_clfloat,t_fixnum)
#define CASE_CLFLOAT_BIGNUM \
    MATH_DISPATCH2_LABEL(t_clfloat,t_bignum)
#define CASE_CLFLOAT_RATIO \
    MATH_DISPATCH2_LABEL(t_clfloat,t_ratio)
#define CASE_CLFLOAT_SINGLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_clfloat,t_singlefloat)
#define CASE_CLFLOAT_DOUBLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_clfloat,t_doublefloat)
#define CASE_CLFLOAT_LONG_FLOAT \
    MATH_DISPATCH2_LABEL(t_clfloat,t_longfloat)
#define CASE_CLFLOAT_COMPLEX \
    MATH_DISPATCH2_LABEL(t_clfloat,t_complex)
#define CASE_CLFLOAT_CSFLOAT \
    MATH_DISPATCH2_LABEL(t_clfloat,t_csfloat)
#define CASE_CLFLOAT_CDFLOAT \
    MATH_DISPATCH2_LABEL(t_clfloat,t_cdfloat)
#define CASE_CLFLOAT_CLFLOAT \
    MATH_DISPATCH2_LABEL(t_clfloat,t_clfloat)

#define CASE_CDFLOAT_FIXNUM \
    MATH_DISPATCH2_LABEL(t_cdfloat,t_fixnum)
#define CASE_CDFLOAT_BIGNUM \
    MATH_DISPATCH2_LABEL(t_cdfloat,t_bignum)
#define CASE_CDFLOAT_RATIO \
    MATH_DISPATCH2_LABEL(t_cdfloat,t_ratio)
#define CASE_CDFLOAT_SINGLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_cdfloat,t_singlefloat)
#define CASE_CDFLOAT_DOUBLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_cdfloat,t_doublefloat)
#define CASE_CDFLOAT_LONG_FLOAT \
    MATH_DISPATCH2_LABEL(t_cdfloat,t_longfloat)
#define CASE_CDFLOAT_COMPLEX \
    MATH_DISPATCH2_LABEL(t_cdfloat,t_complex)
#define CASE_CDFLOAT_CSFLOAT \
    MATH_DISPATCH2_LABEL(t_cdfloat,t_csfloat)
#define CASE_CDFLOAT_CDFLOAT \
    MATH_DISPATCH2_LABEL(t_cdfloat,t_cdfloat)
#define CASE_CDFLOAT_CLFLOAT \
    MATH_DISPATCH2_LABEL(t_cdfloat,t_clfloat)

#define CASE_CSFLOAT_FIXNUM \
    MATH_DISPATCH2_LABEL(t_csfloat,t_fixnum)
#define CASE_CSFLOAT_BIGNUM \
    MATH_DISPATCH2_LABEL(t_csfloat,t_bignum)
#define CASE_CSFLOAT_RATIO \
    MATH_DISPATCH2_LABEL(t_csfloat,t_ratio)
#define CASE_CSFLOAT_SINGLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_csfloat,t_singlefloat)
#define CASE_CSFLOAT_DOUBLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_csfloat,t_doublefloat)
#define CASE_CSFLOAT_LONG_FLOAT \
    MATH_DISPATCH2_LABEL(t_csfloat,t_longfloat)
#define CASE_CSFLOAT_COMPLEX \
    MATH_DISPATCH2_LABEL(t_csfloat,t_complex)
#define CASE_CSFLOAT_CSFLOAT \
    MATH_DISPATCH2_LABEL(t_csfloat,t_csfloat)
#define CASE_CSFLOAT_CDFLOAT \
    MATH_DISPATCH2_LABEL(t_csfloat,t_cdfloat)
#define CASE_CSFLOAT_CLFLOAT \
    MATH_DISPATCH2_LABEL(t_csfloat,t_clfloat)

#define CASE_COMPLEX_FIXNUM \
    MATH_DISPATCH2_LABEL(t_complex,t_fixnum)
#define CASE_COMPLEX_BIGNUM \
    MATH_DISPATCH2_LABEL(t_complex,t_bignum)
#define CASE_COMPLEX_RATIO \
    MATH_DISPATCH2_LABEL(t_complex,t_ratio)
#define CASE_COMPLEX_SINGLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_complex,t_singlefloat)
#define CASE_COMPLEX_DOUBLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_complex,t_doublefloat)
#define CASE_COMPLEX_LONG_FLOAT \
    MATH_DISPATCH2_LABEL(t_complex,t_longfloat)
#define CASE_COMPLEX_COMPLEX \
    MATH_DISPATCH2_LABEL(t_complex,t_complex)
#define CASE_COMPLEX_CSFLOAT \
    MATH_DISPATCH2_LABEL(t_complex,t_csfloat)
#define CASE_COMPLEX_CDFLOAT \
    MATH_DISPATCH2_LABEL(t_complex,t_cdfloat)
#define CASE_COMPLEX_CLFLOAT \
    MATH_DISPATCH2_LABEL(t_complex,t_clfloat)

#define CASE_LONG_FLOAT_FIXNUM \
    MATH_DISPATCH2_LABEL(t_longfloat,t_fixnum)
#define CASE_LONG_FLOAT_BIGNUM \
    MATH_DISPATCH2_LABEL(t_longfloat,t_bignum)
#define CASE_LONG_FLOAT_RATIO \
    MATH_DISPATCH2_LABEL(t_longfloat,t_ratio)
#define CASE_LONG_FLOAT_SINGLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_longfloat,t_singlefloat)
#define CASE_LONG_FLOAT_DOUBLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_longfloat,t_doublefloat)
#define CASE_LONG_FLOAT_LONG_FLOAT \
    MATH_DISPATCH2_LABEL(t_longfloat,t_longfloat)
#define CASE_LONG_FLOAT_COMPLEX \
    MATH_DISPATCH2_LABEL(t_longfloat,t_complex)
#define CASE_LONG_FLOAT_CSFLOAT                    \
    MATH_DISPATCH2_LABEL(t_longfloat,t_csfloat)
#define CASE_LONG_FLOAT_CDFLOAT \
    MATH_DISPATCH2_LABEL(t_longfloat,t_cdfloat)
#define CASE_LONG_FLOAT_CLFLOAT \
    MATH_DISPATCH2_LABEL(t_longfloat,t_clfloat)

#define CASE_DOUBLE_FLOAT_FIXNUM \
    MATH_DISPATCH2_LABEL(t_doublefloat,t_fixnum)
#define CASE_DOUBLE_FLOAT_BIGNUM \
    MATH_DISPATCH2_LABEL(t_doublefloat,t_bignum)
#define CASE_DOUBLE_FLOAT_RATIO \
    MATH_DISPATCH2_LABEL(t_doublefloat,t_ratio)
#define CASE_DOUBLE_FLOAT_SINGLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_doublefloat,t_singlefloat)
#define CASE_DOUBLE_FLOAT_DOUBLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_doublefloat,t_doublefloat)
#define CASE_DOUBLE_FLOAT_LONG_FLOAT \
    MATH_DISPATCH2_LABEL(t_doublefloat,t_longfloat)
#define CASE_DOUBLE_FLOAT_COMPLEX \
    MATH_DISPATCH2_LABEL(t_doublefloat,t_complex)
#define CASE_DOUBLE_FLOAT_CSFLOAT \
    MATH_DISPATCH2_LABEL(t_doublefloat,t_csfloat)
#define CASE_DOUBLE_FLOAT_CDFLOAT \
    MATH_DISPATCH2_LABEL(t_doublefloat,t_cdfloat)
#define CASE_DOUBLE_FLOAT_CLFLOAT \
    MATH_DISPATCH2_LABEL(t_doublefloat,t_clfloat)

#define CASE_SINGLE_FLOAT_FIXNUM \
    MATH_DISPATCH2_LABEL(t_singlefloat,t_fixnum)
#define CASE_SINGLE_FLOAT_BIGNUM \
    MATH_DISPATCH2_LABEL(t_singlefloat,t_bignum)
#define CASE_SINGLE_FLOAT_RATIO \
    MATH_DISPATCH2_LABEL(t_singlefloat,t_ratio)
#define CASE_SINGLE_FLOAT_SINGLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_singlefloat,t_singlefloat)
#define CASE_SINGLE_FLOAT_DOUBLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_singlefloat,t_doublefloat)
#define CASE_SINGLE_FLOAT_LONG_FLOAT \
    MATH_DISPATCH2_LABEL(t_singlefloat,t_longfloat)
#define CASE_SINGLE_FLOAT_COMPLEX \
    MATH_DISPATCH2_LABEL(t_singlefloat,t_complex)
#define CASE_SINGLE_FLOAT_CSFLOAT \
    MATH_DISPATCH2_LABEL(t_singlefloat,t_csfloat)
#define CASE_SINGLE_FLOAT_CDFLOAT \
    MATH_DISPATCH2_LABEL(t_singlefloat,t_cdfloat)
#define CASE_SINGLE_FLOAT_CLFLOAT \
    MATH_DISPATCH2_LABEL(t_singlefloat,t_clfloat)

#define CASE_RATIO_FIXNUM \
    MATH_DISPATCH2_LABEL(t_ratio,t_fixnum)
#define CASE_RATIO_BIGNUM \
    MATH_DISPATCH2_LABEL(t_ratio,t_bignum)
#define CASE_RATIO_RATIO \
    MATH_DISPATCH2_LABEL(t_ratio,t_ratio)
#define CASE_RATIO_SINGLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_ratio,t_singlefloat)
#define CASE_RATIO_DOUBLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_ratio,t_doublefloat)
#define CASE_RATIO_LONG_FLOAT \
    MATH_DISPATCH2_LABEL(t_ratio,t_longfloat)
#define CASE_RATIO_COMPLEX \
    MATH_DISPATCH2_LABEL(t_ratio,t_complex)
#define CASE_RATIO_CSFLOAT \
    MATH_DISPATCH2_LABEL(t_ratio,t_csfloat)
#define CASE_RATIO_CDFLOAT \
    MATH_DISPATCH2_LABEL(t_ratio,t_cdfloat)
#define CASE_RATIO_CLFLOAT \
    MATH_DISPATCH2_LABEL(t_ratio,t_clfloat)

#define CASE_BIGNUM_FIXNUM \
    MATH_DISPATCH2_LABEL(t_bignum,t_fixnum)
#define CASE_BIGNUM_BIGNUM \
    MATH_DISPATCH2_LABEL(t_bignum,t_bignum)
#define CASE_BIGNUM_RATIO \
    MATH_DISPATCH2_LABEL(t_bignum,t_ratio)
#define CASE_BIGNUM_SINGLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_bignum,t_singlefloat)
#define CASE_BIGNUM_DOUBLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_bignum,t_doublefloat)
#define CASE_BIGNUM_LONG_FLOAT \
    MATH_DISPATCH2_LABEL(t_bignum,t_longfloat)
#define CASE_BIGNUM_COMPLEX \
    MATH_DISPATCH2_LABEL(t_bignum,t_complex)
#define CASE_BIGNUM_CSFLOAT \
    MATH_DISPATCH2_LABEL(t_bignum,t_csfloat)
#define CASE_BIGNUM_CDFLOAT \
    MATH_DISPATCH2_LABEL(t_bignum,t_cdfloat)
#define CASE_BIGNUM_CLFLOAT \
    MATH_DISPATCH2_LABEL(t_bignum,t_clfloat)

#define CASE_FIXNUM_FIXNUM \
    MATH_DISPATCH2_LABEL(t_fixnum,t_fixnum)
#define CASE_FIXNUM_BIGNUM \
    MATH_DISPATCH2_LABEL(t_fixnum,t_bignum)
#define CASE_FIXNUM_RATIO \
    MATH_DISPATCH2_LABEL(t_fixnum,t_ratio)
#define CASE_FIXNUM_SINGLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_fixnum,t_singlefloat)
#define CASE_FIXNUM_DOUBLE_FLOAT \
    MATH_DISPATCH2_LABEL(t_fixnum,t_doublefloat)
#define CASE_FIXNUM_LONG_FLOAT \
    MATH_DISPATCH2_LABEL(t_fixnum,t_longfloat)
#define CASE_FIXNUM_COMPLEX \
    MATH_DISPATCH2_LABEL(t_fixnum,t_complex)
#define CASE_FIXNUM_CSFLOAT \
    MATH_DISPATCH2_LABEL(t_fixnum,t_csfloat)
#define CASE_FIXNUM_CDFLOAT \
    MATH_DISPATCH2_LABEL(t_fixnum,t_cdfloat)
#define CASE_FIXNUM_CLFLOAT \
    MATH_DISPATCH2_LABEL(t_fixnum,t_clfloat)

#define CASE_UNKNOWN(routine,x,y,type)                  \
    default: DISPATCH2_ERROR:                           \
    if (!ecl_numberp(x))                                \
        FEwrong_type_nth_arg(routine, 1, x, type);      \
    else                                                \
        FEwrong_type_nth_arg(routine, 2, y, type)

#endif /* ECL_MATH_DISPATCH2_H */
