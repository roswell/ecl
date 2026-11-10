/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
 * Copyright (c) 2010, Juan Jose Garcia Ripoll.
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/* math_dispatch.h -- fast dispatch for math functions */
#ifndef ECL_MATH_DISPATCH_H
#define ECL_MATH_DISPATCH_H

#include <ecl/internal.h> /* for unlikely_if */
#include <ecl/impl/math_fenv.h>

#define MATH_DEF_DISPATCH_FAILED(ret,name,id,type)      \
    static inline ret name##failed(cl_object x)         \
    {                                                   \
        FEwrong_type_only_arg(id, x, type);             \
    }                                                   \

#ifdef ECL_COMPLEX_FLOAT
#define MATH_DEF_DISPATCH1_NE(name,id,type,                             \
                              fix,big,ratio, sfloat,dfloat,lfloat,      \
                              complex,       csfloat,cdfloat,clfloat)   \
    MATH_DEF_DISPATCH_FAILED(cl_object,name,id,type)                    \
    cl_object ecl_##name(cl_object arg)                                 \
    { switch(ecl_t_of(arg)) {                                           \
        case t_fixnum: return fix(arg);                                 \
        case t_bignum: return big(arg);                                 \
        case t_ratio: return ratio(arg);                                \
        case t_singlefloat: return sfloat(arg);                         \
        case t_doublefloat: return dfloat(arg);                         \
        case t_longfloat: return lfloat(arg);                           \
        case t_complex: return complex(arg);                            \
        case t_csfloat: return csfloat(arg);                            \
        case t_cdfloat: return cdfloat(arg);                            \
        case t_clfloat: return clfloat(arg);                            \
        default: return name##failed(arg); }}

#define MATH_DEF_DISPATCH1_BOOL(name,id,type,                           \
                                fix,big,ratio, sfloat,dfloat,lfloat,    \
                                complex,       csfloat,cdfloat,clfloat) \
    MATH_DEF_DISPATCH_FAILED(int,name,id,type)                          \
    int ecl_##name(cl_object arg)                                       \
    { switch(ecl_t_of(arg)) {                                           \
        case t_fixnum: return fix(arg);                                 \
        case t_bignum: return big(arg);                                 \
        case t_ratio: return ratio(arg);                                \
        case t_singlefloat: return sfloat(arg);                         \
        case t_doublefloat: return dfloat(arg);                         \
        case t_longfloat: return lfloat(arg);                           \
        case t_complex: return complex(arg);                            \
        case t_csfloat: return csfloat(arg);                            \
        case t_cdfloat: return cdfloat(arg);                            \
        case t_clfloat: return clfloat(arg);                            \
        default: return name##failed(arg); }}

#else
#define MATH_DEF_DISPATCH1_NE(name,id,type,                             \
                              fix,big,ratio, sfloat,dfloat,lfloat,      \
                              complex,       csfloat,cdfloat,clfloat)   \
    MATH_DEF_DISPATCH_FAILED(cl_object,name,id,type)                    \
    cl_object ecl_##name(cl_object arg)                                 \
    { switch(ecl_t_of(arg)) {                                           \
        case t_fixnum: return fix(arg);                                 \
        case t_bignum: return big(arg);                                 \
        case t_ratio: return ratio(arg);                                \
        case t_singlefloat: return sfloat(arg);                         \
        case t_doublefloat: return dfloat(arg);                         \
        case t_longfloat: return lfloat(arg);                           \
        case t_complex: return complex(arg);                            \
        default: return name##failed(arg); }}

#define MATH_DEF_DISPATCH1_BOOL(name,id,type,                           \
                                fix,big,ratio, sfloat,dfloat,lfloat,    \
                                complex,       csfloat,cdfloat,clfloat) \
    MATH_DEF_DISPATCH_FAILED(int,name,id,type)                          \
    int ecl_##name(cl_object arg)                                       \
    { switch(ecl_t_of(arg)) {                                           \
        case t_fixnum: return fix(arg);                                 \
        case t_bignum: return big(arg);                                 \
        case t_ratio: return ratio(arg);                                \
        case t_singlefloat: return sfloat(arg);                         \
        case t_doublefloat: return dfloat(arg);                         \
        case t_longfloat: return lfloat(arg);                           \
        case t_complex: return complex(arg);                            \
        default: return name##failed(arg); }}
#endif

#define MATH_DEF_DISPATCH1(name,id,type,fix,big,ratio,                  \
                           single_float,double_float,long_float,        \
                           complex,csfloat,cdfloat,clfloat)             \
        MATH_DEF_DISPATCH1_NE(name##_ne,id,type,fix,big,ratio,          \
                              single_float,double_float,long_float,     \
                              complex,csfloat,cdfloat,clfloat)          \
    cl_object ecl_##name(cl_object arg)                                 \
    {                                                                   \
        cl_object out;                                                  \
        ECL_MATHERR_CLEAR;                                              \
        out = ecl_##name##_ne(arg);                                     \
        ECL_MATHERR_TEST;                                               \
        return out;                                                     \
    }

#endif /* ECL_MATH_DISPATCH_H */
