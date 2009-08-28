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
 * Using GMP multiple precision integers:
 *
 * typedef struct
 * {
 *  long int alloc;		// Number of limbs allocated and pointed
 *				//   to by the D field.
 *  long int size;		// abs(SIZE) is the number of limbs
 *				//   the last field points to.  If SIZE
 *				//   is negative this is a negative number.
 *  unsigned long int *d;	// Pointer to the limbs,
 *				//   d[0] is the least significative.
 * } MP_INT;
 *
 * typedef unsigned long int	mp_limb_t;
 *
 */

cl_object
big_register0_get(void)
{
        cl_object output = cl_env.big_register[0];
        output->big.big_limbs = cl_env.big_register_limbs[0];
	output->big.big_size = 0;
        output->big.big_dim = BIGNUM_REGISTER_SIZE;
	return output;
}

cl_object
big_register1_get(void)
{
        cl_object output = cl_env.big_register[1];
        output->big.big_limbs = cl_env.big_register_limbs[1];
	output->big.big_size = 0;
        output->big.big_dim = BIGNUM_REGISTER_SIZE;
	return output;
}

cl_object
big_register2_get(void)
{
        cl_object output = cl_env.big_register[2];
        output->big.big_limbs = cl_env.big_register_limbs[2];
	output->big.big_size = 0;
        output->big.big_dim = BIGNUM_REGISTER_SIZE;
	return output;
}

void
big_register_free(cl_object x)
{
        /* We only need to free the integer when it has been reallocated */
        if (x->big.big_dim > BIGNUM_REGISTER_SIZE) {
                mpz_clear(x->big.big_num);
        }
}

cl_object
big_copy(cl_object old)
{
	cl_object new_big = ecl_alloc_object(t_bignum);
        cl_index dim, bytes;
        new_big->big.big_size = old->big.big_size;
        new_big->big.big_dim = dim = old->big.big_dim;
        bytes = dim * sizeof(mp_limb_t);
        new_big->big.big_limbs = ecl_alloc_atomic(bytes);
        memcpy(new_big->big.big_limbs, old->big.big_limbs, bytes);
        return new_big;
}

cl_object
big_register_copy(cl_object old)
{
        cl_object new_big = big_copy(old);
        big_register_free(old);
	return new_big;
}

cl_object
big_register_normalize(cl_object x)
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
	return big_register_copy(x);
}

cl_object
bignum1(cl_fixnum val)
{
        cl_object aux = big_register0_get();
        mpz_init_set_si(aux->big.big_num, val);
        return big_register_copy(aux);
}

/*
	big_zerop(x) tells whether bignum x is zero or not.

#define big_zerop(x)	(mp_size(x->big.big_num) == 0)
*/

/*
	big_sign(x) returns
		something < 0	if x < 0
		0		if x = 0
		something > 0	if x > 0.

#define big_sign(x)	(x->big.big_size)
*/

/*
	big_compare(x, y) returns
		-1	if x < y
		0	if x = y
		1	if x > y.

#define big_compare(x, y)	mpz_cmp(x->big.big_num, y->big.big_num)
*/

static void *
mp_alloc(size_t size)
{
	return ecl_alloc_atomic_align(size, sizeof(mp_limb_t));
}

static void *
mp_realloc(void *ptr, size_t osize, size_t nsize)
{
	void *p = ecl_alloc_atomic_align(nsize, sizeof(mp_limb_t));
	memcpy(p, ptr, osize);
	return p;
}

static void
mp_free(void *ptr, size_t size)
{
	char *x = ptr;
	if (x < (char *)(cl_env.big_register_limbs) ||
	    x > (char *)(cl_env.big_register_limbs+2))
		ecl_dealloc(x);
}

void init_big_registers(cl_env_ptr env)
{
	int i;
	for (i = 0; i < 3; i++) {
		env->big_register[i] = ecl_alloc_object(t_bignum);
	}
}

void
init_big(cl_env_ptr env)
{
	init_big_registers(env);
        if (ecl_get_option(ECL_OPT_SET_GMP_MEMORY_FUNCTIONS))
                mp_set_memory_functions(mp_alloc, mp_realloc, mp_free);
}
