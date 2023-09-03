/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

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

#ifndef ECL_NUMBER_H
#define ECL_NUMBER_H

#ifdef __cplusplus
extern "C" {
#endif

#define ECL_BIG_REGISTER_SIZE   32

#define ECL_WITH_TEMP_BIGNUM(name,n)                                    \
        mp_limb_t name##data[n];                                        \
        volatile struct ecl_bignum name##aux;                           \
        const cl_object name = (name##aux.value->_mp_alloc = n,       \
                                name##aux.value->_mp_size = 0,        \
                                name##aux.value->_mp_d = name##data,  \
                                (cl_object)(&name##aux))

#define ECL_BIGNUM_DIM(x)       (ecl_bignum(x)->_mp_alloc) /* number of allocated limbs */
#define ECL_BIGNUM_SIZE(x)      (ecl_bignum(x)->_mp_size)  /* number of limbs in use times sign of the bignum */
#define ECL_BIGNUM_LIMBS(x)     (ecl_bignum(x)->_mp_d)     /* pointer to array of allocated limbs */

/* Bignum internal protocol */
extern ECL_API cl_object _ecl_big_set_fixnum(cl_object x, cl_fixnum f);
extern ECL_API cl_object _ecl_big_set_index(cl_object x, cl_index f);
extern ECL_API cl_fixnum _ecl_big_get_fixnum(cl_object x);
extern ECL_API cl_index _ecl_big_get_index(cl_object x);
extern ECL_API long double _ecl_big_to_long_double(cl_object x);
typedef void (*_ecl_big_binary_op)(cl_object out, cl_object o1, cl_object o2);
extern ECL_API _ecl_big_binary_op _ecl_big_boole_operator(int op);

#if ECL_LONG_BITS >= ECL_FIXNUM_BITS
#define _ecl_big_set_fixnum(x, f) mpz_set_si(ecl_bignum(x),(f))
#define _ecl_big_set_index(x, f)  mpz_set_ui(ecl_bignum(x),(f))
#endif
#define _ecl_big_init2(x,size)      mpz_init2(ecl_bignum(x),(size)*GMP_LIMB_BITS)
#define _ecl_big_realloc2(x,size)   mpz_realloc2(ecl_bignum(x),(size)*GMP_LIMB_BITS)
#define _ecl_big_clear(x)           mpz_clear(ecl_bignum(x))
#define _ecl_big_set(x,y)           mpz_set(ecl_bignum(x),ecl_bignum(y))
#define _ecl_big_odd_p(x)           ((mpz_get_ui(ecl_bignum(x)) & 1) != 0)
#define _ecl_big_even_p(x)          ((mpz_get_ui(ecl_bignum(x)) & 1) == 0)
#define _ecl_big_zerop(x)           (ECL_BIGNUM_SIZE(x) == 0)
#define _ecl_big_sign(x)            ECL_BIGNUM_SIZE(x)
#define _ecl_big_compare(x, y)      mpz_cmp(ecl_bignum(x),ecl_bignum(y))
#define _ecl_big_complement(z, x)   mpz_neg(ecl_bignum(z),ecl_bignum(x))
#define _ecl_big_add(z, x, y)       mpz_add(ecl_bignum(z),ecl_bignum(x),ecl_bignum(y))
#define _ecl_big_sub(z, x, y)       mpz_sub(ecl_bignum(z),ecl_bignum(x),ecl_bignum(y))
#define _ecl_big_mul(z, x, y)       mpz_mul(ecl_bignum(z),ecl_bignum(x),ecl_bignum(y))
#define _ecl_big_add_ui(z, x, i)    mpz_add_ui(ecl_bignum(z),ecl_bignum(x),(i))
#define _ecl_big_sub_ui(z, x, i)    mpz_sub_ui(ecl_bignum(z),ecl_bignum(x),(i))
#define _ecl_big_mul_ui(z, x, y)    mpz_mul_ui(ecl_bignum(z),ecl_bignum(x),(y))
#define _ecl_big_div_ui(z, x, y)    mpz_div_ui(ecl_bignum(z),ecl_bignum(x),(y))
#define _ecl_big_mul_si(z, x, y)    mpz_mul_si(ecl_bignum(z),ecl_bignum(x),(y))
#define _ecl_big_set_ui(x, i)       mpz_set_ui(ecl_bignum(x),(unsigned long int)i)
#define _ecl_big_set_si(x, i)       mpz_set_si(ecl_bignum(x),(long int)i)
#define _ecl_big_to_double(x)       mpz_get_d(ecl_bignum(x))
#define _ecl_big_to_long(x)         mpz_get_si(ecl_bignum(x))
#define _ecl_big_to_ulong(x)        mpz_get_ui(ecl_bignum(x))
#define _ecl_big_cmp_si(x,y)        mpz_cmp_si(ecl_bignum(x),(y))
#define _ecl_big_tdiv_q(q, x, y)    mpz_tdiv_q(ecl_bignum(q),ecl_bignum(x),ecl_bignum(y))
#define _ecl_big_tdiv_q_ui(q, x, y) mpz_tdiv_q_ui(ecl_bignum(q),ecl_bignum(x), (y))
#define _ecl_big_set_d(x, d)        mpz_set_d(ecl_bignum(x),(d))


#if ECL_CAN_INLINE
static ECL_INLINE cl_fixnum
ecl_to_fix(cl_object f)
{
        if (ecl_unlikely(!ECL_FIXNUMP(f)))
                FEtype_error_fixnum(f);
        return ecl_fixnum(f);
}

static ECL_INLINE cl_index
ecl_to_size(cl_object f)
{
        cl_fixnum aux = 0;
        if (ecl_unlikely(!ECL_FIXNUMP(f) || ((aux = ecl_fixnum(f)) < 0)))
                FEtype_error_size(f);
        return aux;
}
#else
extern ECL_API cl_fixnum ecl_fixnum_value(cl_object f);
extern ECL_API cl_index ecl_to_size(cl_object f);
#endif

#ifdef __cplusplus
}
#endif

#endif /* ECL_NUMBER_H */
