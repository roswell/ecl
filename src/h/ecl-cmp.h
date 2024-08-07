/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
 * Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
 * Copyright (c) 1990, Giuseppe Attardi.
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/* ecl-cmp.h  -- Include file for compiled code. */

#ifndef ECL_CMP_H
#define ECL_CMP_H

#ifndef __CYGWIN__
/* Recent versions of cygwin do not define fd_set when WINSOCKAPI is
 * defined */
#define _WINSOCKAPI_
#endif /* __CYGWIN__ */
#if defined(__clang__)
/* Disable a couple of clang's more annoying diagnostics */
#pragma clang diagnostic ignored "-Wreturn-type"
#pragma clang diagnostic ignored "-Wunused-value"
#pragma clang diagnostic ignored "-Wparentheses-equality"
#elif defined(_MSC_VER)
#pragma warning(disable:4715) //not all control paths return a value
#pragma warning(disable:4716) //must return a value
#endif
#include <ecl/ecl.h>
#include <math.h> /* for inline mathematics */
#include <ecl/ecl-inl.h>

enum ecl_locative_type {
        _ecl_object_loc = 0,
        _ecl_fixnum_loc,
        _ecl_base_char_loc,
        _ecl_uni_char_loc,
        _ecl_float_loc,
        _ecl_double_loc,
        _ecl_long_double_loc
#ifdef ECL_COMPLEX_FLOAT
        , _ecl_csfloat_loc
        , _ecl_cdfloat_loc
        , _ecl_clfloat_loc
#endif
#ifdef ECL_SSE2
        , _ecl_int_sse_pack_loc
        , _ecl_float_sse_pack_loc
        , _ecl_double_sse_pack_loc
#endif
};

struct ecl_var_debug_info {
        const char *name;
        uint8_t type;
};

#define _ecl_check_narg(n) \
        do { if (ecl_unlikely(narg != (n))) FEwrong_num_arguments_anonym();} while(0)

#endif /* ECL_CMP_H */
