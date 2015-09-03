/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
    fpe_x86.c -- Nonportable component of the floating point code
*/
/*
    Copyright (c) 2005, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/*
 * See fpe_none.c for a description
 */

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
