/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
    num_rand.c  -- Random numbers.
n*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2015, Daniel Kochmański.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <ecl/internal.h>
#include <fcntl.h>
#if !defined(ECL_MS_WINDOWS_HOST)
# include <unistd.h>
#endif
#if !defined(_MSC_VER) && !defined(__MINGW32__)
# include <sys/stat.h>
/* it isn't pulled in by fcntl.h */
#endif

/*
 * Mersenne-Twister random number generator
 */

#if ECL_FIXNUM_BITS > 32
/*
 * 64 bit version
 */

/* Period parameters */
#define MT_N 312
#define MT_M 156
#define MATRIX_A 0xB5026F5AA96619E9ULL   /* constant vector a */
#define UPPER_MASK 0xFFFFFFFF80000000ULL /* most significant 33 bits */
#define LOWER_MASK 0x7FFFFFFFULL         /* least significant 31 bits */
#define ulong uint64_t

static cl_object
init_genrand(ulong seed)
{
        cl_object array = ecl_alloc_simple_vector((MT_N + 1), ecl_aet_b64);
        ulong *mt = array->vector.self.b64;
        mt[0] = seed;
        for (int j=1; j<MT_N; j++) 
                mt[j] =  (6364136223846793005ULL * (mt[j-1] ^ (mt[j-1] >> 62)) + j);

        mt[MT_N] = MT_N+1;
        return array;
}

static ulong
generate_int64(cl_object state)
{
        static  mag01[2]={0x0UL, MATRIX_A};
        ulong y;
        ulong *mt = state->vector.self.b64;

        if (mt[MT_N] >= MT_N) {
                /* refresh data */
                int kk;
                for (kk=0; kk < (MT_N - MT_M); kk++) {
                        y = (mt[kk] & UPPER_MASK) | (mt[kk+1] & LOWER_MASK);
                        mt[kk] = mt[kk + MT_M] ^ (y >> 1) ^ mag01[y & 0x1ULL];
                }
                for (; kk < (MT_N - 1); kk++) {
                        y = (mt[kk] & UPPER_MASK) | (mt[kk+1] & LOWER_MASK);
                        mt[kk] = mt[kk+(MT_M-MT_N)] ^ (y >> 1) ^ mag01[y & 0x1ULL];
                }
                y = (mt[MT_N-1] & UPPER_MASK) | (mt[0] & LOWER_MASK);
                mt[MT_N-1] = mt[MT_M-1] ^ (y >> 1) ^ mag01[y & 0x1ULL];
                mt[MT_N] = 0;
        }
        /* get random 64 bit num */
        y = mt[mt[MT_N]++];
        /* Tempering */
        y ^= (y >> 29) & 0x5555555555555555ULL;
        y ^= (y << 17) & 0x71D67FFFEDA60000ULL;
        y ^= (y << 37) & 0xFFF7EEE000000000ULL;
        y ^= (y >> 43);

        return y;
}

static double
generate_double(cl_object state)
{
        return (generate_int64(state) >> 11) * (1.0 / 9007199254740991.0);
}

static mp_limb_t
generate_limb(cl_object state)
{
#if GMP_LIMB_BITS <= 32
        return generate_int64(state);
#else
# if GMP_LIMB_BITS <= 64
        return generate_int64(state);
# else
#  if GMP_LIMB_BITS <= 128
        mp_limb_t high = generate_int64(state);
        return (high << 64) | generate_int64(state);
#  endif
# endif
#endif
}
#else
/*
 * 32 bit version
 */

/* Period parameters */
#define MT_N 624
#define MT_M 397
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UPPER_MASK 0x80000000UL /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffUL /* least significant r bits */
#define ulong uint32_t

static cl_object
init_genrand(ulong seed)
{
        cl_object array = ecl_alloc_simple_vector((MT_N + 1), ecl_aet_b32);
        ulong *mt = array->vector.self.b32;
        mt[0] = seed;
        for (int j=1; j < MT_N; j++)
                mt[j] = (1812433253UL * (mt[j-1] ^ (mt[j-1] >> 30)) + j);

        mt[MT_N] = MT_N+1;
        return array;
}

static ulong
generate_int32(cl_object state)
{
        static  mag01[2]={0x0UL, MATRIX_A};
        ulong y;
        ulong *mt = state->vector.self.b32;
        if (mt[MT_N] >= MT_N) {
                /* refresh data */
                int kk;
                for (kk=0; kk < (MT_N - MT_M); kk++) {
                        y = (mt[kk] & UPPER_MASK) | (mt[kk+1] & LOWER_MASK);
                        mt[kk] = mt[kk + MT_M] ^ (y >> 1) ^ mag01[y & 0x1UL];
                }
                for (; kk < (MT_N - 1); kk++) {
                        y = (mt[kk] & UPPER_MASK) | (mt[kk+1] & LOWER_MASK);
                        mt[kk] = mt[kk+(MT_M-MT_N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
                }
                y = (mt[MT_N-1] & UPPER_MASK) | (mt[0] & LOWER_MASK);
                mt[MT_N-1] = mt[MT_M-1] ^ (y >> 1) ^ mag01[y & 0x1UL];
                mt[MT_N] = 0;
        }
        /* get random 32 bit num */
        y = mt[mt[MT_N]++];
        /* Tempering */
        y ^= (y >> 11);
        y ^= (y << 7) & 0x9d2c5680UL;
        y ^= (y << 15) & 0xefc60000UL;
        y ^= (y >> 18);
        return y;
}

static double
generate_double(cl_object state)
{
        return generate_int32(state) * (1.0 / 4294967296.0);
}

static mp_limb_t
generate_limb(cl_object state)
{
#if GMP_LIMB_BITS <= 32
        return generate_int32(state);
#else
# if GMP_LIMB_BITS <= 64
        mp_limb_t high = generate_int32(state);
        return (high << 32) | generate_int32(state);
# else
#  if GMP_LIMB_BITS <= 128
        mp_limb_t word0 = generate_int32(state);
        mp_limb_t word1 = generate_int32(state);
        mp_limb_t word2 = generate_int32(state);
        mp_limb_t word3 = generate_int32(state);
        return (word3 << 96) | (word3 << 64) | (word1 << 32) || word0;
#  endif
# endif
#endif
}
#endif

cl_object
init_random_state(void)
{
        ulong seed;
#if !defined(ECL_MS_WINDOWS_HOST)
        /* fopen() might read full 4kB blocks and discard
         * a lot of entropy, so use open() */
        int file_handler = open("/dev/urandom", O_RDONLY);
        if (file_handler != -1) {
                read(file_handler, &seed, sizeof(ulong));
                close(file_handler);
        } else
#endif
        {
                /* cant get urandom, use crappy source */
                /* and/or fill rest of area */
                seed = (rand() + time(0));
        }

        return init_genrand(seed);
}

static cl_object
random_integer(cl_object limit, cl_object state)
{
        cl_index bit_length = ecl_integer_length(limit);
        cl_object buffer;
        if (bit_length <= ECL_FIXNUM_BITS)
                bit_length = ECL_FIXNUM_BITS;
        buffer = ecl_ash(ecl_make_fixnum(1), bit_length);
        for (bit_length = mpz_size(buffer->big.big_num); bit_length; ) {
                ECL_BIGNUM_LIMBS(buffer)[--bit_length] =
                        generate_limb(state);
        }
        return cl_mod(buffer, limit);
}

static cl_object
rando(cl_object x, cl_object rs)
{
        cl_object z;
        if (!ecl_plusp(x)) {
                goto ERROR;
        }
        switch (ecl_t_of(x)) {
        case t_fixnum:
#if ECL_FIXNUM_BITS <= 32
                z = ecl_make_fixnum(generate_int32(rs->random.value) % ecl_fixnum(x));
                break;
#endif
        case t_bignum:
                z = random_integer(x, rs->random.value);
                break;
        case t_singlefloat:
                z = ecl_make_single_float(ecl_single_float(x) *
                                         (float)generate_double(rs->random.value));
                break;
        case t_doublefloat:
                z = ecl_make_double_float(ecl_double_float(x) *
                                         generate_double(rs->random.value));
                break;
#ifdef ECL_LONG_FLOAT
        case t_longfloat:
                z = ecl_make_long_float(ecl_long_float(x) *
                                       (long double)generate_double(rs->random.value));
                break;
#endif
        default: ERROR: {
                const char *type = "(OR (INTEGER (0) *) (FLOAT (0) *))";
                FEwrong_type_nth_arg(@[random],1,x, ecl_read_from_cstring(type));
        }
        }
        return z;
}

cl_object
ecl_make_random_state(cl_object rs)
{
        cl_object z = ecl_alloc_object(t_random);
        if (rs == ECL_T) {
                z->random.value = init_random_state();
                return z;
        }

        if (Null(rs))
                rs = ecl_symbol_value(@'*random-state*');

        switch (ecl_t_of(rs)) {
        case t_random:
                z->random.value = cl_copy_seq(rs->random.value);
                break;
        case t_vector:
                z->random.value = cl_copy_seq(rs);
                break;
        case t_fixnum:
                /* XXX: If we'll decide to use 64-bit algorithm for
                   appropriate platforms then this will be replaced
                   with ecl_to_ulong_long from number.d, which takes
                   widest available type (32 or 64 bit)
                   automatically. */
                z->random.value = init_genrand(ecl_fixnum(rs));
                break;
        default: {
                const char *type
                        = "(OR RANDOM-STATE (SIMPLE-VECTOR *) (INTEGER 0 *))";
                FEwrong_type_only_arg(@[make-random-state], rs,
                                      ecl_read_from_cstring(type));
        }
        }

        return(z);
}

@(defun random (x &optional (rs ecl_symbol_value(@'*random-state*')))
@
        rs = ecl_check_cl_type(@'random', rs, t_random);
        @(return rando(x, rs));
@)

@(defun make_random_state (&optional (rs ECL_NIL))
@
        @(return ecl_make_random_state(rs))
@)

cl_object
cl_random_state_p(cl_object x)
{
        @(return (ECL_RANDOM_STATE_P(x) ? ECL_T : ECL_NIL))
}
