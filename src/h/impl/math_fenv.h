/* -*- mode: c; c-basic-offset: 4 -*- */
/*
    math_fenv.h -- inlined versions of fenv.h
*/
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#if !defined(ECL_MATH_FENV_H) && defined(HAVE_FENV_H)
#define ECL_MATH_FENV_H

#if defined(__APPLE__) && defined(__amd64__)
#define feclearexcept myfeclearexcept
static inline void myfeclearexcept(int flags)
{
    int aux;
    int f = ~(0x3d);
    __asm__ (
    "fnclex  \n\t"
    "stmxcsr %0\n\t"
    "andl    %1,%0\n\t"
    "ldmxcsr %0\n\t"
    : "=m"(aux) : "a"(f));
}
#define fetestexcept myfetestexcept
static inline int myfetestexcept(cl_fixnum flags)
{
    cl_fixnum output = (flags & 0x3d);
    int sw;
    __asm__ (
    "fnstsw  %0\n\t"
    "movzwl  %0,%%eax\n\t"
    "stmxcsr %0\n\t"
    "orl     %0,%%eax\n\t"
    "and     %%rax,%1\n\t"
    : "=m"(sw), "=d"(output) : "d"(output) : "%rax");
    return output;
}
#endif /* __APPLE__ && __amd64__ */

#endif /* !ECL_MATH_FENV_H && HAVE_FENV_H */
