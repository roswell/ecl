/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
 * Copyright (c) 2010, Juan Jose Garcia Ripoll.
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/* math_msvc_fenv.h -- fake fenv.h using Microsoft Visual C++ */
#ifndef ECL_MATH_FENV_MSVC_H
#define ECL_MATH_FENV_MSVC_H

#ifndef HAVE_FEENABLEEXCEPT
# define HAVE_FEENABLEEXCEPT
#endif
#ifndef HAVE_FENV_H
# define HAVE_FENV_H
#endif

#include <float.h>

#if defined(_MSC_VER)
# define FE_DIVBYZERO _EM_ZERODIVIDE
# define FE_OVERFLOW  _EM_OVERFLOW
# define FE_UNDERFLOW _EM_UNDERFLOW
# define FE_INVALID   _EM_INVALID
# define FE_INEXACT   _EM_INEXACT
typedef int fenv_t;
#else
# ifdef _MCW_EM
#  define MCW_EM _MCW_EM
# else
#  define MCW_EM 0x0008001F
# endif
# define fenv_t int
#endif

#define feenableexcept(bits) do { \
    int cw = _controlfp(0,0); cw &= ~(bits); _controlfp(cw,MCW_EM); } while(0)
#define fedisableexcept(bits) do { \
    int cw = _controlfp(0,0); cw |= (bits); _controlfp(cw,MCW_EM); } while(0)
#define feholdexcept(bits) do { \
    *(bits) = _controlfp(0,0); _controlfp(0xffffffff, MCW_EM); } while(0)
#define fegetenv(bits) do { *(bits) = _controlfp(0,0); } while (0)
#define fesetenv(bits) do { _controlfp(*(bits), MCW_EM); } while (0)
#define feupdateenv(bits) fesetenv(bits)
#define feclearexcept(bits) _clearfp()
#define fetestexcept(bits) (_clearfp() & (bits))

#endif /* !ECL_MATH_FENV_MSVC_H */
