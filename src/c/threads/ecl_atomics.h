/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * ecl_atomics.h - alternative definitions for atomic operations
 *
 * Copyright (c) 2012 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#ifndef ECL_ATOMICS_H
#define AO_ASSUME_WINDOWS98
#include <ecl/internal.h>

#if !defined(AO_HAVE_compare_and_swap_full)
# error "ECL needs AO_compare_and_swap_full or an equivalent"
#endif
#if !defined(AO_HAVE_compare_and_swap)
# error "ECL needs AO_compare_and_swap or an equivalent"
#endif
#if !defined(AO_HAVE_fetch_and_add1)
# error "Cannot implement mailboxs without AO_fetch_and_add1"
#endif

#endif /* ECL_ATOMICS_H */
