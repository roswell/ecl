/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    big.c -- Bignum routines.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <string.h>
#include <ecl/ecl.h>
#include <ecl/internal.h>

/* 
 * Using GMP multiple precision integers.
 */

void
_ecl_big_register_free(cl_object x)
{
        /* We only need to free the integer when it gets too large */
        if (x->big.big_dim > 3 * ECL_BIG_REGISTER_SIZE) {
                mpz_realloc2(x->big.big_num, ECL_BIG_REGISTER_SIZE * GMP_LIMB_BITS);
        }
}

static cl_object
_ecl_big_copy(cl_object old)
{
        cl_fixnum size = old->big.big_size;
        cl_index dim = (size < 0)? (-size) : size;
        cl_index bytes = dim * sizeof(mp_limb_t);
        cl_object new_big = ecl_alloc_compact_object(t_bignum, bytes);
        new_big->big.big_limbs = ECL_COMPACT_OBJECT_EXTRA(new_big);
        new_big->big.big_size = size;
        new_big->big.big_dim = dim;
        memcpy(new_big->big.big_limbs, old->big.big_limbs, bytes);
        return new_big;
}

cl_object
_ecl_big_register_copy(cl_object old)
{
        cl_object new_big = _ecl_big_copy(old);
        _ecl_big_register_free(old);
	return new_big;
}

cl_object
_ecl_big_register_normalize(cl_object x)
{
	int s = x->big.big_size;
	if (s == 0)
                return(MAKE_FIXNUM(0));
	if (s == 1) {
                mp_limb_t y = x->big.big_limbs[0];
                if (y <= MOST_POSITIVE_FIXNUM)
                        return MAKE_FIXNUM(y);
	} else if (s == -1) {
                mp_limb_t y = x->big.big_limbs[0];
                if (y <= -MOST_NEGATIVE_FIXNUM)
                        return MAKE_FIXNUM(-y);
	}
	return _ecl_big_register_copy(x);
}

#if ECL_LONG_BITSS < FIXNUM_BITS
# undef _ecl_big_set_fixnum
# undef _ecl_big_set_index
# if GMP_LIMB_BITS >= FIXNUM_BITS
cl_object
_ecl_big_set_fixnum(cl_object x, cl_fixnum f)
{
        if (f == 0) {
                mpz_set_si(x->big.big_num, 0);
        } else if (f > 0) {
                x->big.big_size = 1;
                x->big.big_limbs[0] = f;
        } else if (f < 0) {
                x->big.big_size = -1;
                x->big.big_limbs[0] = -f;
        }
}

cl_object
_ecl_big_set_index(cl_object x, cl_index f)
{
        if (f == 0) {
                mpz_set_si(x->big.big_num, 0);
        } else if (f > 0) {
                x->big.big_size = 1;
                x->big.big_limbs[0] = f;
        } else if (f < 0) {
                x->big.big_size = -1;
                x->big.big_limbs[0] = -f;
        }
}

# else
#  error "ECL cannot build with GMP when both long and mp_limb_t are smaller than cl_fixnum"
# endif
#endif /* ECL_LONG_BITS >= FIXNUM_BITS */

static void *
mp_alloc(size_t size)
{
        return ecl_alloc_atomic_align(size, sizeof(mp_limb_t));
}

static void *
mp_realloc(void *ptr, size_t osize, size_t nsize)
{
	mp_limb_t *p = ecl_alloc_atomic_align(nsize, sizeof(mp_limb_t));
	memcpy(p, ptr, (osize < nsize)? osize : nsize);
        ecl_dealloc(ptr);
	return p;
}

static void
mp_free(void *ptr, size_t size)
{
        ecl_dealloc(ptr);
}

void
init_big()
{
        if (ecl_get_option(ECL_OPT_SET_GMP_MEMORY_FUNCTIONS))
                mp_set_memory_functions(mp_alloc, mp_realloc, mp_free);
}
