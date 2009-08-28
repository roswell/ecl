/* -*- mode: c; c-basic-offset: 8 -*- */
/* 
    big_ll.c -- Bignum emulation with long long.
 */
/*
    Copyright (c) 2005, Maciek Pasternacki.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <ecl/internal.h>

void
_ecl_big_register_free(cl_object x) {}

cl_object
_ecl_big_register_copy(cl_object old)
{
	cl_object new_big = ecl_alloc_object(t_bignum);
        new_big->big.big_num = old->big.big_num;
	return new_big;
}

cl_object
_ecl_big_register_normalize(cl_object x)
{
	if (x->big.big_num == 0ll)
                return(MAKE_FIXNUM(0));
        if (x->big.big_num <= MOST_POSITIVE_FIXNUM && x->big.big_num >= MOST_NEGATIVE_FIXNUM)
                return(MAKE_FIXNUM(x->big.big_num));
	return _ecl_big_register_copy(x);
}

static cl_object
big_alloc(int size)
{
	volatile cl_object x = ecl_alloc_object(t_bignum);
	if (size <= 0)
		ecl_internal_error("negative or zero size for bignum in big_alloc");
	x->big.big_num = 0ll;
	return x;
}

static cl_object
_ecl_big_copy(cl_object x)
{
	volatile cl_object y = ecl_alloc_object(t_bignum);
        y->big.big_num = x->big.big_num;
	return y;
}

cl_object
_ecl_big_gcd(cl_object gcd, cl_object x, cl_object y)
{
        big_num_t i = x->big.big_num, j = y->big.big_num;
        while ( 1 ) {
                big_num_t k;
                if ( i<j ) {
                        k = i;
                        i = j;
                        j = k;
                }
                if ( j == 0 ) {
                        gcd->big.big_num = k;
                        break;
                }
                k = i % j;
                i = j;
                j = k;
        }
}

int
_ecl_big_num_t_sgn(big_num_t x)
{
	return ( x == (big_num_t)0 ) ? 0 : (x < (big_num_t)0) ? -1 : 1;
}

void
init_big(void)
{
}
