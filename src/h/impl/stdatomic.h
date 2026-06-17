/*
 * Copyright (c) 2017 Ivan Maidanski
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

/* This is a private ECL header which provides an implementation of     */
/* libatomic_ops subset primitives sufficient for ECL assuming that GCC */
/* atomic intrinsics are available (and have correct implementation).   */

/* Originally based on GC's gc_atomic_ops.h, adapted for ECL's needs    */

#ifndef ECL_STDATOMIC_H
#define ECL_STDATOMIC_H

  typedef unsigned long AO_t;

# define AO_INLINE static __inline

# define AO_nop_full() __atomic_thread_fence(__ATOMIC_SEQ_CST)
# define AO_HAVE_nop_full

# define AO_fetch_and_add(p, v) __atomic_fetch_add(p, v, __ATOMIC_RELAXED)
# define AO_HAVE_fetch_and_add

# define AO_load(p) __atomic_load_n(p, __ATOMIC_RELAXED)
# define AO_HAVE_load
# define AO_load_acquire(p) __atomic_load_n(p, __ATOMIC_ACQUIRE)
# define AO_HAVE_load_acquire

# define AO_store(p, v) __atomic_store_n(p, v, __ATOMIC_RELAXED)
# define AO_HAVE_store
# define AO_store_release(p, v) __atomic_store_n(p, v, __ATOMIC_RELEASE)
# define AO_HAVE_store_release

# ifdef AO_REQUIRE_CAS
    AO_INLINE int
    AO_compare_and_swap(volatile AO_t *p, AO_t ov, AO_t nv)
    {
      return (int)__atomic_compare_exchange_n(p, &ov, nv, 0,
                                        __ATOMIC_RELAXED, __ATOMIC_RELAXED);
    }
#   define AO_HAVE_compare_and_swap

    AO_INLINE int
    AO_compare_and_swap_full(volatile AO_t *p, AO_t ov, AO_t nv)
    {
      return (int)__atomic_compare_exchange_n(p, &ov, nv, 0,
                                        __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE);
    }
#   define AO_HAVE_compare_and_swap_full

    AO_INLINE AO_t
    AO_fetch_compare_and_swap(volatile AO_t *p, AO_t ov, AO_t nv)
    {
      (void)__atomic_compare_exchange_n(p, &ov, nv, 0,
                                        __ATOMIC_RELAXED, __ATOMIC_RELAXED);
      return ov;
    }
#   define AO_HAVE_fetch_compare_and_swap
# endif

#endif /* ECL_STDATOMIC_H */
