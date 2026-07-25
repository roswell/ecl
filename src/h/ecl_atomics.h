/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * ecl_atomics.h - alternative definitions for atomic operations
 *
 * Copyright (c) 2012 Juan Jose Garcia Ripoll
 * Copyright (c) 2026 Marius Gerbershagen
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#ifndef ECL_ATOMICS_H
#define ECL_ATOMICS_H

#ifdef ECL_THREADS

# ifdef ECL_USE_STD_ATOMIC
#  include <stdatomic.h>

#  define ecl_atomic_thread_fence() __atomic_thread_fence(__ATOMIC_SEQ_CST)
#  define ecl_atomic_fetch_and_add(p, v) __atomic_fetch_add(p, v, __ATOMIC_RELAXED)
#  define ecl_atomic_load(p) __atomic_load_n(p, __ATOMIC_RELAXED)
#  define ecl_atomic_load_acquire(p) __atomic_load_n(p, __ATOMIC_ACQUIRE)
#  define ecl_atomic_store(p, v) __atomic_store_n(p, v, __ATOMIC_RELAXED)
#  define ecl_atomic_store_release(p, v) __atomic_store_n(p, v, __ATOMIC_RELEASE)
#  define ecl_atomic_compare_and_swap(p, ov, nv) __atomic_compare_exchange_n(p, &ov, nv, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED)
#  define ecl_atomic_compare_and_swap_full(p, ov, nv) __atomic_compare_exchange_n(p, &ov, nv, 0, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)
#  define ecl_atomic_fetch_compare_and_swap(p, ov, nv) (__atomic_compare_exchange_n(p, &ov, nv, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED),ov)

# else  /* ECL_USE_STD_ATOMIC */

#  define AO_REQUIRE_CAS
#  define AO_ASSUME_WINDOWS98
#  ifdef ECL_LIBATOMIC_OPS_H
#   include <ecl/atomic_ops.h>
#  else
#   include <atomic_ops.h>
#  endif

#  if !defined(AO_HAVE_compare_and_swap_full)
#   error "ECL needs AO_compare_and_swap_full or an equivalent"
#  endif
#  if !defined(AO_HAVE_compare_and_swap)
#   error "ECL needs AO_compare_and_swap or an equivalent"
#  endif
#  if !defined(AO_HAVE_fetch_and_add)
#   error "ECL needs AO_fetch_and_add or an equivalent"
#  endif
#  if !defined(AO_HAVE_fetch_compare_and_swap)
#   error "ECL needs AO_fetch_compare_and_swap or an equivalent"
#  endif

#  define ecl_atomic_thread_fence() AO_nop_full()
#  define ecl_atomic_fetch_and_add(p, v) AO_fetch_and_add((AO_t*)(p), (AO_t)(v))
#  define ecl_atomic_load(p) AO_load((AO_t*)(p))
#  define ecl_atomic_load_acquire(p) AO_load_acquire((AO_t*)(p))
#  define ecl_atomic_store(p, v) AO_store((AO_t*)(p), (AO_t)(v))
#  define ecl_atomic_store_release(p, v) AO_store_release((AO_t*)(p), (AO_t)(v))
#  define ecl_atomic_compare_and_swap(p, ov, nv) AO_compare_and_swap((AO_t*)(p), (AO_t)(ov), (AO_t)(nv))
#  define ecl_atomic_compare_and_swap_full(p, ov, nv) AO_compare_and_swap_full((AO_t*)(p), (AO_t)(ov), (AO_t)(nv))
#  define ecl_atomic_fetch_compare_and_swap(p, ov, nv) AO_fetch_compare_and_swap((AO_t*)(p), (AO_t)(ov), (AO_t)(nv))

# endif  /* ECL_USE_STD_ATOMIC */

/* The functions below form part of the external API. */

static inline cl_object
ecl_compare_and_swap(cl_object *slot, cl_object old_val, cl_object new_val)
{
  return (cl_object)ecl_atomic_fetch_compare_and_swap(slot, old_val, new_val);
}

/* Atomic increment of fixnums: If we don't care about overflows, we
   can add two fixnums together by adding a normal fixnum to another
   fixnum which has its tag bits set to zero. */
static inline cl_object
ecl_atomic_incf(cl_object *slot, cl_object increment)
{
  if (ecl_unlikely(!ECL_FIXNUMP(increment)))
    FEtype_error_fixnum(increment);
  return (cl_object)ecl_atomic_fetch_and_add(slot, (cl_index)increment & ~(cl_index)ECL_IMMEDIATE_TAG);
}

static inline cl_object
ecl_atomic_incf_by_fixnum(cl_object *slot, cl_fixnum increment)
{
  /* INV: cl_index is an unsigned integer, hence shifts will always zero fill */
  return (cl_object)ecl_atomic_fetch_and_add(slot, (cl_index)increment << ECL_TAG_BITS);
}

#else  /* ECL_THREADS */

# define ecl_atomic_load(x) (x)
# define ecl_atomic_store(x,y) ((x)=(y))
# define ecl_atomic_thread_fence()

#endif  /* ECL_THREADS */

#endif /* ECL_ATOMICS_H */
