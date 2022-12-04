/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
    page.h  -- Page macros.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifdef __cplusplus
extern "C" {
#endif

/*****************************
 * BOEHM's GARBAGE COLLECTOR *
 *****************************/

#ifdef GBC_BOEHM
#ifdef ECL_THREADS
#define GC_THREADS
#endif

extern struct typemanager {
        const char *tm_name;
        cl_index tm_size;
} tm_table[(int)t_end];

#define tm_of(t)        (&tm_table[(int)(t)])
#endif

/*******************************
 * SYMBOLS & KEYWORDS DATABASE *
 *******************************/

#ifdef __cplusplus
}
#endif
