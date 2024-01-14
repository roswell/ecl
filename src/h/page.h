/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
 * Copyright (c) 1990, Giuseppe Attardi.
 *
 * See file 'LICENSE' for the copyright details.
 *
 */
/* page.h  -- Page macros. */
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
