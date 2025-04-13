/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
 * Copyright (c) 1995, Giuseppe Attardi.
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/* number.h  -- INTEGER interface. */
#ifndef ECL_NUMBER_H
#define ECL_NUMBER_H

#ifdef __cplusplus
extern "C" {
#endif

/* Bignum internal protocol */

/* Allocate a number on the stack. */
#define ECL_WITH_TEMP_BIGNUM(name,n)                                    \
        mp_limb_t name##data[n];                                        \
        volatile struct ecl_bignum name##aux;                           \
        const cl_object name = (name##aux.value->_mp_alloc = n,         \
                                name##aux.value->_mp_size = 0,          \
                                name##aux.value->_mp_d = name##data,    \
                                (cl_object)(&name##aux))

#define ECL_BIGNUM_LIMB_BITS GMP_LIMB_BITS
#define ECL_BIGNUM_DIM(x)    (ecl_bignum(x)->_mp_alloc) /* number of allocated limbs */
#define ECL_BIGNUM_SIZE(x)   (ecl_bignum(x)->_mp_size)  /* number of limbs in use times sign of the bignum */
#define ECL_BIGNUM_USIZE(x)  (mpz_size(ecl_bignum(x)))  /* number of limbs */
#define ECL_BIGNUM_LIMBS(x)  (ecl_bignum(x)->_mp_d)     /* pointer to array of allocated limbs */

typedef void (*_ecl_big_binary_op)(cl_object out, cl_object o1, cl_object o2);
extern ECL_API _ecl_big_binary_op _ecl_big_boole_operator(int op);

/* Type conversion (setters mutate the bignum structure). */
#if ECL_LONG_BITS >= ECL_FIXNUM_BITS
# define _ecl_big_set_index(x, f)  mpz_set_ui(ecl_bignum(x),(f))
# define _ecl_big_set_fixnum(x, f) mpz_set_si(ecl_bignum(x),(f))
# define _ecl_big_get_index(x)     mpz_get_ui(ecl_bignum(x))
# define _ecl_big_get_fixnum(x)    mpz_get_si(ecl_bignum(x))
#elif GMP_LIMB_BITS >= ECL_FIXNUM_BITS
# define ECL_GMP_FIXNUM_TO_LIMBS
extern ECL_API cl_index _ecl_big_get_idx(cl_object x);
extern ECL_API cl_fixnum _ecl_big_get_fix(cl_object x);
extern ECL_API void _ecl_big_set_idx(cl_object x, cl_index y);
extern ECL_API void _ecl_big_set_fix(cl_object x, cl_fixnum y);
# define _ecl_big_set_index(x, f)  _ecl_big_set_idx((x),(f))
# define _ecl_big_get_index(x)     _ecl_big_get_idx((x))
# define _ecl_big_set_fixnum(x, f) _ecl_big_set_fix((x),(f))
# define _ecl_big_get_fixnum(x)    _ecl_big_get_fix((x))
#else
# error "ECL cannot build with GMP when both long and mp_limb_t are smaller than cl_fixnum"
#endif

#define _ecl_big_set_ui(x, i)       mpz_set_ui(ecl_bignum(x),(unsigned long int)i)
#define _ecl_big_set_si(x, i)       mpz_set_si(ecl_bignum(x),(long int)i)
#define _ecl_big_set_d(x, d)        mpz_set_d(ecl_bignum(x),(d))

#define _ecl_big_get_ui(x)          mpz_get_ui(ecl_bignum(x))
#define _ecl_big_get_si(x)          mpz_get_si(ecl_bignum(x))
#define _ecl_big_get_d(x)           mpz_get_d(ecl_bignum(x))
#define _ecl_big_get_str(buf,b,x)   mpz_get_str(buf,-b,ecl_bignum(x))

#define _ecl_big_to_ulong(x)        _ecl_big_get_ui(x)
#define _ecl_big_to_long(x)         _ecl_big_get_si(x)
#define _ecl_big_to_double(x)       _ecl_big_get_d(x)

extern ECL_API long double _ecl_big_get_lf(cl_object x);
#define _ecl_big_to_long_double(x) _ecl_big_get_lf((x))

/* Type conversion predicates */
#define _ecl_big_fits_ui(x) mpz_fits_ulong_p(ecl_bignum(x))
#define _ecl_big_fits_si(x) mpz_fits_slong_p(ecl_bignum(x))

/* Predicates */
#define _ecl_big_odd_p(x)           ((mpz_get_ui(ecl_bignum(x)) & 1) != 0)
#define _ecl_big_even_p(x)          ((mpz_get_ui(ecl_bignum(x)) & 1) == 0)
#define _ecl_big_sign(x)            mpz_sgn(ecl_bignum(x))
#define _ecl_big_compare(x, y)      mpz_cmp(ecl_bignum(x),ecl_bignum(y))

/* Bit fiddling */
#define _ecl_big_popcount(x)        mpz_popcount(ecl_bignum(x))
#define _ecl_big_1com(x,y)          mpz_com(ecl_bignum(x),ecl_bignum(y))
#define _ecl_big_tstbit(x,n)        mpz_tstbit(ecl_bignum(x),n)
#define _ecl_big_bits(x)            mpz_sizeinbase(ecl_bignum(x),2)
#define _ecl_big_sizeinbase(x,n)    mpz_sizeinbase(ecl_bignum(x),n)

#define _ecl_big_div_2exp(z,y,i)    mpz_div_2exp(ecl_bignum(z), ecl_bignum(y), i)
#define _ecl_big_mul_2exp(z,y,i)    mpz_mul_2exp(ecl_bignum(z), ecl_bignum(y), i);
#define _ecl_big_fdiv_q_2exp(z,x,i) mpz_fdiv_q_2exp(ecl_bignum(z), ecl_bignum(x), i);
#define _ecl_big_fdiv_r_2exp(z,x,i) mpz_fdiv_r_2exp(ecl_bignum(z), ecl_bignum(x), i);

/* Arithmetic operations that are used for parsing numbers (and in big.d) */
#define _ecl_big_mul(z, x, y)       mpz_mul(ecl_bignum(z),ecl_bignum(x),ecl_bignum(y))
#define _ecl_big_mul_ui(z, x, y)    mpz_mul_ui(ecl_bignum(z),ecl_bignum(x),(y))
#define _ecl_big_add_ui(z, x, i)    mpz_add_ui(ecl_bignum(z),ecl_bignum(x),(i))
#define _ecl_big_neg(z, x)          mpz_neg(ecl_bignum(z),ecl_bignum(x))
#define _ecl_big_complement(z, x)   _ecl_big_neg(z,x)

/* Other number operations (not bignum) */
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
