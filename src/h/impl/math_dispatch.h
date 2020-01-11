/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

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

#include <ecl/internal.h> /* for unlikely_if */
#include <ecl/impl/math_fenv.h>

typedef cl_object (*math_one_arg_fn)(cl_object);

#ifdef ECL_COMPLEX_FLOAT
#define MATH_CFLOAT(c1,c2,c3) c1, c2, c3
#else
#define MATH_CFLOAT(c1,c2,c3)
#endif

#define MATH_DEF_DISPATCH1_NE(name,id,type,fix,big,ratio,               \
                              single_float,double_float,long_float,     \
                              complex,csfloat,cdfloat,clfloat)          \
    static cl_object name##failed(cl_object x) {                        \
        FEwrong_type_only_arg(id, x, type);                             \
    }                                                                   \
    static const math_one_arg_fn name##dispatch[t_last_number+1]= {     \
        name##failed,                 /* t_start */                     \
        name##failed,                 /* t_list */                      \
        name##failed,                 /* t_character */                 \
        fix, big, ratio,              /* t_fixnum, t_bignum, t_ratio */ \
        single_float,                 /* t_singlefloat */               \
        double_float,                 /* t_doublefloat */               \
        long_float,                   /* t_longfloat */                 \
        complex,                      /* t_complex */                   \
        MATH_CFLOAT(csfloat,cdfloat,clfloat) /* t_c?float */ };         \
    cl_object ecl_##name(cl_object arg)                                 \
    {                                                                   \
        int t = ECL_IMMEDIATE(arg);                                     \
        if (t == 0) {                                                   \
            t = arg->d.t;                                               \
            unlikely_if (t > t_last_number) return name##failed(arg);   \
        }                                                               \
        return name##dispatch[t](arg);                                  \
    }
#define MATH_DEF_DISPATCH1(name,id,type,fix,big,ratio,                  \
                           single_float,double_float,long_float,        \
                           complex,csfloat,cdfloat,clfloat)             \
        MATH_DEF_DISPATCH1_NE(name##_ne,id,type,fix,big,ratio,          \
                              single_float,double_float,long_float,     \
                              complex,csfloat,cdfloat,clfloat)          \
    cl_object ecl_##name(cl_object arg)                                 \
    {                                                                   \
        cl_object out;                                                  \
        out = ecl_##name##_ne(arg);                                     \
        return out;                                                     \
    }

typedef int (*math_one_arg_bool_fn)(cl_object);
#define MATH_DEF_DISPATCH1_BOOL(name,id,type,fix,big,ratio,             \
                                single_float,double_float,long_float,   \
                                complex,csfloat,cdfloat,clfloat)        \
    static int name##failed(cl_object x) {                              \
        FEwrong_type_only_arg(id, x, type);                             \
    }                                                                   \
    static const math_one_arg_bool_fn name##dispatch[t_last_number+1]= {\
        name##failed,                 /* t_start */                     \
        name##failed,                 /* t_list */                      \
        name##failed,                 /* t_character */                 \
        fix, big, ratio,              /* t_fixnum, t_bignum, t_ratio */ \
        single_float,                 /* t_singlefloat */               \
        double_float,                 /* t_doublefloat */               \
        long_float,                   /* t_longfloat */                 \
        complex,                      /* t_complex */                   \
        MATH_CFLOAT(csfloat,cdfloat,clfloat) /* t_c?float */ };         \
    int ecl_##name(cl_object arg)                                       \
    {                                                                   \
        int t = ECL_IMMEDIATE(arg);                                     \
        if (t == 0) {                                                   \
            t = arg->d.t;                                               \
            unlikely_if (t > t_last_number) return name##failed(arg);   \
        }                                                               \
        return name##dispatch[t](arg);                                  \
    }
#endif /* ECL_MATH_DISPATCH_H */
