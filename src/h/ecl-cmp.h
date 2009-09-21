/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    ecl-cmp.h  -- Include file for compiled code.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or modify it under
    the terms of the GNU General Library Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    See file '../Copyright' for full details.
*/

#define _WINSOCKAPI_
#include <ecl/ecl.h>
#include <math.h> /* for inline mathematics */

#undef cs_check
#define	cs_check \
	if ((int *)(&narg) < cs_limit) \
		cs_overflow()

#define TRAMPOLINK(narg, vv, lk, cblock) \
	cl_va_list args; cl_va_start(args, narg, narg, 0); \
	return(_ecl_link_call(vv, (cl_objectfn *)lk, cblock, narg, args))

#define ecl_def_ct_base_string(name,chars,len,static,const)     \
        static const struct ecl_base_string name ## data = {    \
                (int8_t)t_base_string, 0, aet_bc, 0,            \
                Cnil, (cl_index)(len), (cl_index)(len),         \
                (ecl_base_char*)(chars) };                      \
        static const cl_object name = (cl_object)(& name ## data)

#define ecl_def_ct_single_float(name,f,static,const)            \
        static const struct ecl_doublefloat name ## data = {    \
                (int8_t)t_singlefloat, 0, 0, 0,                 \
                (float)(f) };                                   \
        static const cl_object name = (cl_object)(& name ## data)

#define ecl_def_ct_double_float(name,f,static,const)            \
        static const struct ecl_singlefloat name ## data = {    \
                (int8_t)t_doublefloat, 0, 0, 0,                 \
                (double)(f) };                                  \
        static const cl_object name = (cl_object)(& name ## data)

#define ecl_def_ct_vector(name,type,raw,len,static,const)               \
        static const struct ecl_vector name ## data = {                 \
                (int8_t)t_vector, 0, (type), 0,                         \
                Cnil, (cl_index)(len), (cl_index)(len),                 \
                (ecl_base_char*)(raw), 0 };                             \
        static const cl_object name = (cl_object)(& name ## data)
        
enum ecl_locative_type {
        _ecl_object_loc = 0,
        _ecl_fixnum_loc,
        _ecl_base_char_loc,
        _ecl_uni_char_loc,
        _ecl_float_loc,
        _ecl_double_loc
};

struct ecl_var_debug_info {
        const char *name;
        uint8_t type;
};
