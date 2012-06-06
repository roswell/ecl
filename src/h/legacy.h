/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    legacy.h -- Legacy macros, functions and names.
*/
/*
    Copyright (c) 2011, Juan Jose Garcia-Ripoll

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#if !defined(ECL_LEGACY_H) && !defined(ECL_NO_LEGACY)
#define ECL_LEGACY_H

/*
 * LEGACY
 */
#define CHARACTERP(o)		ECL_CHARACTERP(o)
#define BASE_CHAR_P(o)		ECL_BASE_CHAR_P(o)
#define BASE_CHAR_CODE_P(o)	ECL_BASE_CHAR_CODE_P(o)
#define CODE_CHAR(o)		ECL_CODE_CHAR(o)
#define CHAR_CODE(o)		ECL_CHAR_CODE(o)
#define REAL_TYPE(t)		ECL_REAL_TYPE_P(t)
#define IMMEDIATE(o)		ECL_IMMEDIATE(o)
#define IMMEDIATE_TAG		ECL_IMMEDIATE_TAG

#define FIXNUM_TAG		t_fixnum
#define MAKE_FIXNUM(n)		ecl_make_fixnum(n)
#define FIXNUM_MINUSP(n)	ecl_fixnum_minusp(n)
#define FIXNUM_PLUSP(n)		ecl_fixnum_plusp(n)
#define	fix(o)			ecl_fix(o)
#define FIXNUMP(o)		ECL_FINXUMP(o)

#define make_shortfloat(x) ecl_make_shortfloat(x);
#define cl_def_c_function_va(sym,function) ecl_def_c_function_va(sym,function)
#define cl_def_c_function(sym,function,narg) ecl_def_c_function(sym,function,narg)
#define cl_def_c_macro(sym,function,narg) {                     \
                int n = (narg);                                 \
                if (n < 0)                                      \
                        ecl_def_c_macro_va((sym),(function));   \
                else                                            \
                        ecl_def_c_macro((sym),(function),n); }
#define cl_make_cfun(fun,name,block,narg) ecl_make_cfun(fun,name,block,narg)
#define cl_make_cfun_va(fun,name,block) ecl_make_cfun_va(fun,name,block)
#define cl_make_cclosure_va(fun,name,block) ecl_make_cclosure_va(fun,name,block)
#define si_bc_file(o) si_compiled_function_file(o)
#define ARRAYP ECL_ARRAYP
#define VECTORP ECL_VECTORP
#define ARRAY_TYPE(t) (t >= t_array && t <= t_bitvector)
#define c_string_to_object ecl_read_from_cstring

#define big_register0_get _ecl_big_register0
#define big_register1_get _ecl_big_register1
#define big_register2_get _ecl_big_register2
#define big_register_free _ecl_big_register_free
#define big_register_copy _ecl_big_register_copy
#define big_register_normalize _ecl_big_register_normalize
/* #define big_copy _ecl_big_copy  Has disappeared */
/* #define big_to_double Has disappeared */

#define cl_alloc_simple_base_string ecl_alloc_simple_base_string
#define cl_alloc_adjustable_base_string ecl_alloc_adjustable_base_string
#define cl_alloc_simple_extended_string ecl_alloc_simple_extended_string

#define ecl_search_hash _ecl_gethash

#define read_VV ecl_init_module

#endif /* !ECL_LEGACY_H && !ECL_NO_LEGACY */

#define make_simple_base_string(s) ecl_make_simple_base_string((s),-1)
#define make_constant_base_string(s) ecl_make_simple_base_string((char *)(s),-1)

