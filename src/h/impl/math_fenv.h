/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
    math_fenv.h -- inlined versions of fenv.h
*/
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#ifndef ECL_MATH_FENV_H
#define ECL_MATH_FENV_H

/*
 * ECL admits two ways to process floating point errors
 *   - Based on hardware exceptions
 *   - Based on explicit checks
 *
 * In the hardware exception model we simply expect the floating point
 * unit to complain about a computation. In order for this to happen
 * we may need two things:
 *
 *   - Activate explicitely signaling of exceptions
 *   - Insert explicit checks for exceptions
 *
 * The first task is achieved using feenableexcept() or an equivalent
 * function. The second task is only needed on some platforms where
 * exceptions are activated by one floating point computation but are
 * only signaled with the _next_ floating point instruction (Read x86
 * processors)
 *
 * The second model is more portable and safer and it is based on
 * using the C99 routines in fenv.h or equivalent functions. In this
 * model hardware exceptions are never triggered and instead we
 * surround the computation with explicit checks for problems.
 */

#if defined(HAVE_FEENABLEEXCEPT) && !defined(_GNU_SOURCE)
# define _GNU_SOURCE
#endif
#ifdef HAVE_FENV_H
# include <fenv.h>
#endif
#if defined(ECL_MS_WINDOWS_HOST)
# include <ecl/impl/math_fenv_msvc.h>
#endif

#ifdef HAVE_FENV_H
# define ECL_WITHOUT_FPE_BEGIN do { fenv_t env; feholdexcept(&env);
# define ECL_WITHOUT_FPE_END        fesetenv(&env); } while (0)
#else
# define FE_INVALID 1
# define FE_DIVBYZERO 2
# define FE_INEXACT 0
# define FE_OVERFLOW 3
# define FE_UNDERFLOW 0
# define ECL_WITHOUT_FPE_BEGIN
# define ECL_WITHOUT_FPE_END
# define feclearexcept(x)
#endif /* !HAVE_FENV_H */

#ifndef FE_ALL_EXCEPT
# define FE_ALL_EXCEPT FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW | FE_INVALID
#endif

#if defined(HAVE_FENV_H) && !defined(ECL_AVOID_FPE_H)
# if defined(HAVE_FEENABLEEXCEPT)
#  define ECL_WITH_LISP_FPE_BEGIN do {                   \
        fenv_t __fenv;                                   \
        fegetenv(&__fenv);                               \
        cl_env_ptr __the_env = ecl_process_env_unsafe(); \
        if (__the_env) {                                 \
                int bits = __the_env->trap_fpe_bits;     \
                fedisableexcept(FE_ALL_EXCEPT & ~bits);  \
                feenableexcept(FE_ALL_EXCEPT & bits);    \
        }                                                \
        feclearexcept(FE_ALL_EXCEPT);
# else
#  define ECL_WITH_LISP_FPE_BEGIN do {                   \
        fenv_t __fenv;                                   \
        fegetenv(&__fenv);                               \
        feclearexcept(FE_ALL_EXCEPT);
# endif
# define ECL_WITH_LISP_FPE_END \
        fesetenv(&__fenv); } while (0)
#else
# define ECL_WITH_LISP_FPE_BEGIN do {
# define ECL_WITH_LISP_FPE_END } while (0)
#endif

#if defined(HAVE_FENV_H) && defined(ECL_IEEE_FP) && !defined(HAVE_FEENABLEEXCEPT) && !defined(ECL_AVOID_FPE_H)
# define ECL_USED_EXCEPTIONS (FE_DIVBYZERO|FE_INVALID|FE_OVERFLOW|FE_UNDERFLOW)
# define ECL_MATHERR_CLEAR feclearexcept(FE_ALL_EXCEPT)
# define ECL_MATHERR_TEST do {                                         \
        int bits = fetestexcept(ECL_USED_EXCEPTIONS);                  \
        unlikely_if (bits) {                                           \
                bits &= ecl_process_env()->trap_fpe_bits;              \
                if (bits) ecl_deliver_fpe(bits);                       \
        }                                                              \
        } while(0)
#else
# define ECL_MATHERR_CLEAR
# define ECL_MATHERR_TEST
#endif

extern void ecl_deliver_fpe(int flags);

#endif /* !ECL_MATH_FENV_H */
