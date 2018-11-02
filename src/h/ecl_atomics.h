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
#define ECL_ATOMICS_H

#ifdef ECL_THREADS

# define AO_REQUIRE_CAS
# define AO_ASSUME_WINDOWS98
# ifdef ECL_LIBATOMIC_OPS_H
#  include <ecl/atomic_ops.h>
# else
#  include <atomic_ops.h>
# endif

# if !defined(AO_HAVE_compare_and_swap_full)
#  error "ECL needs AO_compare_and_swap_full or an equivalent"
# endif
# if !defined(AO_HAVE_compare_and_swap)
#  error "ECL needs AO_compare_and_swap or an equivalent"
# endif
# if !defined(AO_HAVE_fetch_and_add1)
#  error "Cannot implement mailboxs without AO_fetch_and_add1"
# endif
# if !defined(AO_HAVE_fetch_and_add)
#  error "ECL needs AO_fetch_and_add or an equivalent"
# endif
# if !defined(AO_HAVE_fetch_compare_and_swap)
#  error "ECL needs AO_fetch_compare_and_swap or an equivalent"
# endif

static inline cl_object
ecl_compare_and_swap(cl_object *slot, cl_object old_obj, cl_object new_obj)
{
  return (cl_object)AO_fetch_compare_and_swap((AO_t*)slot, (AO_t)old, (AO_t)new);
}

/* Atomic increment of fixnums: If we don't care about overflows, we
   can add two fixnums together by adding a normal fixnum to another
   fixnum which has its tag bits set to zero. */
static inline cl_object
ecl_atomic_incf(cl_object *slot, cl_object increment)
{
  if (ecl_unlikely(!ECL_FIXNUMP(increment)))
    FEtype_error_fixnum(increment);
  return (cl_object)AO_fetch_and_add((AO_t*)slot, (AO_t)increment & ~(AO_t)ECL_IMMEDIATE_TAG);
}

static inline cl_object
ecl_atomic_incf_by_fixnum(cl_object *slot, cl_fixnum increment)
{
  /* INV: AO_t is an unsigned integer, hence shifts will always zero fill */
  return (cl_object)AO_fetch_and_add((AO_t*)slot, (AO_t)increment << ECL_TAG_BITS);
}

#else  /* ECL_THREADS */

# define AO_load(x) (x)
# define AO_store(x,y) ((x)=(y))
# define AO_nop_full()

#endif  /* ECL_THREADS */

#endif /* ECL_ATOMICS_H */
