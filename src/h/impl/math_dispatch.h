/* -*- mode: c; c-basic-offset: 4 -*- */
/*
    math_dispatch.h -- fast dispatch for math functions
*/
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifndef ECL_MATH_DISPATCH_H
#define ECL_MATH_DISPATCH_H

#include <ecl/impl/math_fenv.h>

typedef cl_object (*math_one_arg_fn)(cl_object);

#ifdef ECL_LONG_FLOAT
#define MATH_LONG_DOUBLE(opt) opt,
#else
#define MATH_LONG_DOUBLE(opt)
#endif
#define MATH_DEF_DISPATCH1(name,id,type,rational,single_float,double_float,long_float,complex) \
    static cl_object name##failed(cl_object x) {                        \
        FEwrong_type_only_arg(id, x, type);                             \
    }                                                                   \
    static const math_one_arg_fn name##dispatch[t_complex+1]= {         \
        name##failed, /* t_start */                                     \
        name##failed, /* t_list */                                      \
        name##failed, /* t_character */                                 \
        rational, rational, rational, /* t_fixnum, bignum, ratio */     \
        single_float, double_float, /* t_singlefloat, t_doublefloat */  \
        MATH_LONG_DOUBLE(long_float) /* t_longfloat, optional */        \
        complex };                                                      \
    cl_object ecl_##name(cl_object arg)                                 \
    {                                                                   \
        int t = type_of(arg);                                           \
        if (t > t_complex) name##failed(arg);                           \
        ECL_MATHERR_CLEAR;                                              \
        arg = name##dispatch[t](arg);                                   \
        ECL_MATHERR_TEST;                                               \
        return arg;                                                     \
    }
#define MATH_DEF_DISPATCH1_NE(name,id,type,rational,single_float,double_float,long_float,complex) \
    static cl_object name##failed(cl_object x) {                        \
        FEwrong_type_only_arg(id, x, type);                             \
    }                                                                   \
    static const math_one_arg_fn name##dispatch[t_complex+1]= {         \
        name##failed, /* t_start */                                     \
        name##failed, /* t_list */                                      \
        name##failed, /* t_character */                                 \
        rational, rational, rational, /* t_fixnum, bignum, ratio */     \
        single_float, double_float, /* t_singlefloat, t_doublefloat */  \
        MATH_LONG_DOUBLE(long_float) /* t_longfloat, optional */        \
        complex };                                                      \
    cl_object ecl_##name(cl_object arg)                                 \
    {                                                                   \
        int t = type_of(arg);                                           \
        if (t > t_complex) name##failed(arg);                           \
        arg = name##dispatch[t](arg);                                   \
        return arg;                                                     \
    }
#endif /* ECL_MATH_DISPATCH_H */
