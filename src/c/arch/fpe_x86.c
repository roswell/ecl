/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
 * fpe_x86.c -- Nonportable component of the floating point code
 *
 * Copyright (c) 2005, Juan Jose Garcia Ripoll.
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/* See fpe_none.c for a description. */

#ifdef _MSC_VER
# ifdef _WIN64
#  error "This file shouldn't have been included!"
# else
#  define ecl_detect_fpe() __asm fwait
# endif
#endif

#ifdef __GNUC__
#define ecl_detect_fpe() asm("fwait")
#endif
