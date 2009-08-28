/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    number.h  -- GMP interface.
*/
/*
    Copyright (c) 1995, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#define ECL_BIG_REGISTER_SIZE	32
#ifdef WITH_GMP
#define big_init2(x,size)	mpz_init2((x)->big.big_num,(size)*GMP_LIMB_BITS)
#define _ecl_big_clear(x)	mpz_clear((x)->big.big_num)
#define big_set(x,y)		mpz_set((x)->big.big_num,(y)->big.big_num)
#define big_odd_p(x)		((mpz_get_ui(x->big.big_num) & 1) != 0)
#define big_even_p(x)		((mpz_get_ui(x->big.big_num) & 1) == 0)
#define big_zerop(x)		((x)->big.big_size == 0)
#define big_sign(x)		((x)->big.big_size)
#define big_compare(x, y)	mpz_cmp(x->big.big_num, y->big.big_num)
#define big_complement(z, x)	mpz_neg((z)->big.big_num,(x)->big.big_num)
#define big_add(z, x, y)	mpz_add((z)->big.big_num,(x)->big.big_num,(y)->big.big_num)
#define big_sub(z, x, y)	mpz_sub((z)->big.big_num,(x)->big.big_num,(y)->big.big_num)
#define big_mul(z, x, y)	mpz_mul((z)->big.big_num,(x)->big.big_num,(y)->big.big_num)
#define big_add_ui(z, x, i)	mpz_add_ui(z->big.big_num, x->big.big_num, i)
#define big_sub_ui(z, x, i)	mpz_sub_ui(z->big.big_num, x->big.big_num, i)
#define big_mul_ui(z, x, y)	mpz_mul_ui((z)->big.big_num,(x)->big.big_num,(y))
#define big_mul_si(z, x, y)	mpz_mul_si((z)->big.big_num,(x)->big.big_num,(y))
#define big_set_ui(x, i)	mpz_set_ui(x->big.big_num, (unsigned long int)i)
#define big_set_si(x, i)	mpz_set_si(x->big.big_num, (long int)i)
#define big_to_double(x)	mpz_get_d(x->big.big_num)
#define big_to_long(x)		mpz_get_si(x->big.big_num)
#define big_to_ulong(x)		mpz_get_ui(x->big.big_num)
#define big_cmp_si(x,y)		mpz_cmp_si((x)->big.big_num,(y))
#define big_tdiv_q(q, x, y)	mpz_tdiv_q((q)->big.big_num,(x)->big.big_num,(y)->big.big_num)
#define big_tdiv_q_ui(q, x, y)	mpz_tdiv_q_ui((q)->big.big_num, (x)->big.big_num, (y))
#define big_set_d(x, d)		mpz_set_d((x)->big.big_num, (d))
#else  /* WITH_GMP */
#define big_init2(x,size)	((x)->big.big_num=0)
#define big_clear(x)		mpz_clear((x)->big.big_num)
#define big_set(x,y)		((x)->big.big_num = (y)->big.big_num)
extern int _ecl_big_num_t_sgn(big_num_t x);
#define big_odd_p(x)		((int)((x)->big.big_num&1) != 0)
#define big_even_p(x)		((int)((x)->big.big_num&1) == 0)
#define big_zerop(x)		((x)->big.big_num == (big_num_t)0)
#define big_sign(x)		_ecl_big_num_t_sgn((x)->big.big_num)
#define big_compare(x,y)	_ecl_big_num_t_sgn((x)->big.big_num - (y)->big.big_num)
#define big_complement(z, x)	((z)->big.big_num = -((x)->big.big_num))
#define big_add(z, x, y)	(z)->big.big_num = (x)->big.big_num+(y)->big.big_num
#define big_sub(z, x, y)	(z)->big.big_num = (x)->big.big_num-(y)->big.big_num
#define big_mul(z, x, y)	(z)->big.big_num = (x)->big.big_num*(y)->big.big_num
#define big_add_ui(z, x, y)	((z)->big.big_num = (x)->big.big_num+(unsigned long)(y))
#define big_sub_ui(z, x, y)	((z)->big.big_num = (x)->big.big_num-(unsigned long)(y))
#define big_mul_ui(z, x, y)	((x)->big.big_num = (x)->big.big_num*(unsigned long)(y))
#define big_mul_si(z, x, y)	(z)->big.big_num = (x)->big.big_num*(y)
#define big_set_ui(x, i)	((x)->big.big_num = ((big_num_t)((unsigned long int)i)))
#define big_set_si(x, i)	((x)->big.big_num = ((big_num_t)((long int)i)))
#define big_to_double(x)	((double)((x)->big.big_num))
#define big_to_long(x)		((long int)((x)->big.big_num))
#define big_to_ulong(x)		((unsigned long int)((x)->big.big_num))
#define big_cmp_si(x, y)	((x)->big.big_num!=(y))
#define big_tdiv_q(q, x, y)	((q)->big.big_num = (x)->big.big_num / (y)->big.big_num)
#define big_tdiv_q_ui(q, x, y)	((q)->big.big_num = (x)->big.big_num / (y))
#define big_set_d(x, d)		((x)->big.big_num = (big_num_t)(d))
#endif /* WITH_GMP */
