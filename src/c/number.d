/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    number.c -- Numeric constants.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#include <float.h>
#include <limits.h>
#include <signal.h>
#ifdef HAVE_FENV_H
# define _GNU_SOURCE
# include <fenv.h>
#endif
#include <float.h>
#define ECL_DEFINE_FENV_CONSTANTS
#include <ecl/internal.h>

#if defined(ECL_IEEE_FP) && defined(HAVE_FEENABLEEXCEPT)
/*
 * We are using IEEE arithmetics and can rely on FPE exceptions
 * to be raised when invalid operations are performed.
 */
# define DO_DETECT_FPE(f) ecl_detect_fpe()
#else
/*
 * Either we can not rely on C signals or we do not want IEEE NaNs and
 * infinities. The first case typically happens for instance under OS
 * X, where the status of the FPE control word is changed by
 * printf. We have two alternatives.
 */
# ifdef ECL_IEEE_FP
#  if defined(HAVE_FENV_H) && !defined(ECL_AVOID_FENV_H)
#   define DO_DETECT_FPE(f)                                             \
        if (isnan(f) || !isfinite(f))                                   \
                ecl_deliver_fpe();
#  else
#   define DO_DETECT_FPE(f)                                             \
	if (isnan(f)) {                                                 \
                if (ecl_process_env()->trap_fpe_bits & FE_INVALID)      \
                        cl_error(1, @'floating-point-invalid-operation'); \
	} else if (!isfinite(f)) {                                      \
                if (ecl_process_env()->trap_fpe_bits & FE_DIVBYZERO)    \
                        cl_error(1, @'division-by-zero');               \
	}
#  endif
# else
#  define DO_DETECT_FPE(f)                                              \
	if (isnan(f)) {                                                 \
                cl_error(1, @'floating-point-invalid-operation');       \
	} else if (!isfinite(f)) {                                      \
                cl_error(1, @'division-by-zero');                       \
	}
# endif
#endif

cl_fixnum
fixint(cl_object x)
{
	if (FIXNUMP(x))
		return fix(x);
	if (type_of(x) == t_bignum) {
#ifdef WITH_GMP
		if (mpz_fits_slong_p(x->big.big_num)) {
			return mpz_get_si(x->big.big_num);
		}
#else  /* WITH_GMP */
                if ( !((cl_fixnum)x->big.big_num < x->big.big_num) )
                        return (cl_fixnum)x->big.big_num;
#endif /* WITH_GMP */
	}
	FEwrong_type_argument(@'fixnum', x);
}

cl_index
fixnnint(cl_object x)
{
	if (FIXNUMP(x)) {
		cl_fixnum i = fix(x);
		if (i >= 0)
			return i;
	} else if (type_of(x) == t_bignum) {
#ifdef WITH_GMP
		if (mpz_fits_ulong_p(x->big.big_num)) {
			return mpz_get_ui(x->big.big_num);
		}
#else  /* WITH_GMP */
                if ( x->big.big_num >= 0
                     && !((cl_fixnum)x->big.big_num < x->big.big_num) )
                        return (cl_fixnum)x->big.big_num;
#endif /* WITH_GMP */
	}
	cl_error(9, @'simple-type-error', @':format-control',
		    make_constant_base_string("Not a non-negative fixnum ~S"),
		    @':format-arguments', cl_list(1,x),
		    @':expected-type', cl_list(3, @'integer', MAKE_FIXNUM(0), MAKE_FIXNUM(MOST_POSITIVE_FIXNUM)),
		    @':datum', x);
}

cl_fixnum
ecl_fixnum_in_range(cl_object fun, const char *what, cl_object value,
		    cl_fixnum min, cl_fixnum max)
{
	do {
		if (FIXNUMP(value)) {
			cl_fixnum output = fix(value);
			if ((min <= output) && (output <= max)) {
				return output;
			}
		}
		value = ecl_type_error(fun, what, value,
				       cl_list(3,@'integer',MAKE_FIXNUM(min),
					       MAKE_FIXNUM(max)));
	} while(1);
}

cl_object
ecl_make_integer(cl_fixnum l)
{
	if (l > MOST_POSITIVE_FIXNUM || l < MOST_NEGATIVE_FIXNUM) {
                cl_object z = _ecl_big_register0();
                _ecl_big_set_si(z, l);
                return _ecl_big_register_copy(z);
	}
	return MAKE_FIXNUM(l);
}

cl_object
ecl_make_unsigned_integer(cl_index l)
{
	if (l > MOST_POSITIVE_FIXNUM) {
                cl_object z = _ecl_big_register0();
                _ecl_big_set_ui(z, l);
                return _ecl_big_register_copy(z);
	}
	return MAKE_FIXNUM(l);
}

ecl_uint8_t
ecl_to_uint8_t(cl_object x) {
        do {
                if (FIXNUMP(x)) {
                        cl_fixnum y = fix(x);
                        if (y >= 0 && y < 256) {
                                return (uint8_t)y;
                        }
                }
		x = ecl_type_error(@'coerce', "variable", x,
                                   cl_list(3,@'integer',MAKE_FIXNUM(-128),
                                           MAKE_FIXNUM(127)));
        } while(1);
}

ecl_int8_t
ecl_to_int8_t(cl_object x) {
        do {
                if (FIXNUMP(x)) {
                        cl_fixnum y = fix(x);
                        if (y >= -128 && y <= 127) {
                                return (int8_t)y;
                        }
                }
		x = ecl_type_error(@'coerce', "variable", x,
                                   cl_list(3,@'integer',MAKE_FIXNUM(-128),
                                           MAKE_FIXNUM(127)));
        } while(1);
}

#if FIXNUM_BITS < 32
# error "Unsupported platform with cl_fixnum < ecl_uint32_t"
#endif

#ifdef ecl_uint16_t
ecl_uint16_t
ecl_to_uint16_t(cl_object x) {
        do {
                if (FIXNUMP(x)) {
                        cl_fixnum y = fix(x);
                        if (y >= 0 && y <= 0xFFFFL) {
                                return (ecl_uint16_t)y;
                        }
                }
		x = ecl_type_error(@'coerce', "variable", x,
                                   cl_list(3,@'integer',MAKE_FIXNUM(0),
                                           MAKE_FIXNUM(0xFFFFL)));
        } while(1);
}

ecl_int16_t
ecl_to_int16_t(cl_object x) {
        do {
                if (FIXNUMP(x)) {
                        cl_fixnum y = fix(x);
                        if (y >= -0x8000 && y <= 0x7FFF) {
                                return (ecl_int16_t)y;
                        }
                }
		x = ecl_type_error(@'coerce', "variable", x,
                                   cl_list(3,@'integer',MAKE_FIXNUM(-0x8000),
                                           MAKE_FIXNUM(0x7FFF)));
        } while(1);
}
#endif /* ecl_uint16_t */

#if defined(ecl_uint32_t) && (FIXNUM_BITS > 32)
ecl_uint32_t
ecl_to_uint32_t(cl_object x) {
        do {
                if (FIXNUMP(x)) {
                        cl_fixnum y = fix(x);
                        if (y >= 0 && y <= 0xFFFFFFFFUL) {
                                return (ecl_uint32_t)y;
                        }
                }
		x = ecl_type_error(@'coerce', "variable", x,
                                   cl_list(3,@'integer',MAKE_FIXNUM(0),
                                           ecl_make_unsigned_integer(0xFFFFFFFFUL)));
        } while(1);
}

ecl_int32_t
ecl_to_int32_t(cl_object x) {
        do {
                if (FIXNUMP(x)) {
                        cl_fixnum y = fix(x);
                        if (y >= -0x80000000L && y <= 0x7FFFFFFFL) {
                                return (ecl_int32_t)y;
                        }
                }
		x = ecl_type_error(@'coerce', "variable", x,
                                   cl_list(3,@'integer',
                                           ecl_make_integer(-0x80000000L),
                                           ecl_make_integer(0x7FFFFFFFL)));
        } while(1);
}
#endif /* ecl_uint32_t */

#if defined(ecl_uint64_t) && (FIXNUM_BITS < 64)
# if !defined(WITH_GMP) || FIXNUM_BITS != 32
#  error "Cannot handle ecl_uint64_t without GMP or 32/64 bits integer"
# endif
ecl_uint64_t
ecl_to_uint64_t(cl_object x) {
        do {
                if (!ecl_minusp(x)) {
                        if (FIXNUMP(x)) {
                                return (ecl_uint64_t)fix(x);
                        } else if (type_of(x) != t_bignum) {
                                (void)0;
                        } else if (mpz_fits_ulong_p(x->big.big_num)) {
                                return (ecl_uint64_t)mpz_get_ui(x->big.big_num);
                        } else {
                                cl_object copy = _ecl_big_register0();
                                mpz_fdiv_q_2exp(copy->big.big_num, x->big.big_num, 32);
                                if (mpz_fits_ulong_p(copy->big.big_num)) {
                                        volatile ecl_uint64_t output;
                                        output = (ecl_uint64_t)mpz_get_ui(copy->big.big_num);
                                        output = (output << 32) +
                                                (ecl_uint64_t)mpz_get_ui(x->big.big_num);
                                        return output;
                                }
                        }
                }
		x = ecl_type_error(@'coerce', "variable", x,
                                   cl_list(3,@'integer',MAKE_FIXNUM(0),
                                           ecl_one_minus(ecl_ash(MAKE_FIXNUM(1), 64))));
        } while(1);
}

ecl_int64_t
ecl_to_int64_t(cl_object x) {
        do {
                if (FIXNUMP(x)) {
                        return (ecl_int64_t)fix(x);
                } else if (type_of(x) != t_bignum) {
                        (void)0;
                } else if (mpz_fits_slong_p(x->big.big_num)) {
                        return (ecl_int64_t)mpz_get_si(x->big.big_num);
                } else {
                        cl_object copy = _ecl_big_register0();
                        mpz_fdiv_q_2exp(copy->big.big_num, x->big.big_num, 32);
                        if (mpz_fits_slong_p(copy->big.big_num)) {
                                ecl_int64_t output;
                                output = (ecl_int64_t)mpz_get_si(copy->big.big_num);
                                mpz_fdiv_r_2exp(copy->big.big_num, x->big.big_num, 32);
                                return (output << 32) + mpz_get_ui(copy->big.big_num);
                        }
                }
		x = ecl_type_error(@'coerce', "variable", x,
                                   cl_list(3,@'integer',
                                           ecl_negate(ecl_ash(MAKE_FIXNUM(1), 63)),
                                           ecl_one_minus(ecl_ash(MAKE_FIXNUM(1), 63))));
        } while(1);
}

cl_object
ecl_make_uint64_t(ecl_uint64_t i)
{
        if (i <= MOST_POSITIVE_FIXNUM) {
                return MAKE_FIXNUM(i);
        } else if (i <= ~(ecl_uint32_t)0) {
                return ecl_make_uint32_t(i);
        } else {
                cl_object aux = ecl_make_uint32_t(i >> 32);
                return cl_logior(2, ecl_ash(aux, 32),
                                 ecl_make_uint32_t((ecl_uint32_t)i));
        }
}

cl_object
ecl_make_int64_t(ecl_int64_t i)
{
        if (i >= MOST_NEGATIVE_FIXNUM && i <= MOST_POSITIVE_FIXNUM) {
                return MAKE_FIXNUM(i);
        } else {
                cl_object aux = ecl_make_int32_t(i >> 32);
                return cl_logior(2, ecl_ash(aux, 32), ecl_make_uint32_t((ecl_uint32_t)i));
        }
}
#endif /* ecl_uint64_t */

#if defined(ecl_ulong_long_t)
# if defined(ecl_uint32_t) && ECL_LONG_LONG_BITS == 32
ecl_ulong_long_t
ecl_to_unsigned_long_long(cl_object x) {
        return (ecl_ulong_long_t)ecl_to_uint32_t(x);
}

ecl_long_long_t
ecl_to_long_long(cl_object x) {
        return (ecl_long_long_t)ecl_to_int32_t(x);
}
cl_object
ecl_make_unsigned_long_long(ecl_ulong_long_t i) {
        return ecl_make_uint32_t(i);
}
cl_object
ecl_make_long_long(ecl_long_long_t i) {
        return ecl_make_int32_t(i);
}
# else
#  if defined(ecl_uint64_t) && ECL_LONG_LONG_BITS == 64
ecl_ulong_long_t
ecl_to_unsigned_long_long(cl_object x) {
        return (ecl_ulong_long_t)ecl_to_uint64_t(x);
}
ecl_long_long_t
ecl_to_long_long(cl_object x) {
        return (ecl_long_long_t)ecl_to_int64_t(x);
}
cl_object
ecl_make_unsigned_long_long(ecl_ulong_long_t i) {
        return ecl_make_uint64_t(i);
}
cl_object
ecl_make_long_long(ecl_long_long_t i) {
        return ecl_make_int64_t(i);
}
#  else
#  if !defined(WITH_GMP)
#   error "Cannot handle ecl_ulong_long_t without GMP"
#  endif
ecl_ulong_long_t
ecl_to_unsigned_long_long(cl_object x) {
        do {
                if (!ecl_minusp(x)) {
                        if (FIXNUMP(x)) {
                                return (ecl_ulong_long_t)fix(x);
                        } else if (type_of(x) != t_bignum) {
                                (void)0;
                        } else if (mpz_fits_ulong_p(x->big.big_num)) {
                                return (ecl_ulong_long_t)mpz_get_ui(x->big.big_num);
                        } else {
                                cl_object copy = _ecl_big_register0();
                                int i = ECL_LONG_LONG_BITS - FIXNUM_BITS;
                                mpz_fdiv_q_2exp(copy->bit.big_num, x->big.big_num, i);
                                if (mpz_fits_ulong_p(copy->big.big_num)) {
                                        volatile ecl_ulong_long_t output;
                                        output = mpz_get_ui(copy->big.big_num);
                                        for (i -= FIXNUM_BITS; i; i-= FIXNUM_BITS) {
                                                output = (output << FIXNUM_BITS);
                                                output += mpz_get_ui(x->big.big_num);
                                        }
                                        return output;
                                }
                        }
                }
		x = ecl_type_error(@'coerce', "variable", x,
                                   cl_list(3,@'integer',MAKE_FIXNUM(0),
                                           ecl_one_minus(ecl_ash(MAKE_FIXNUM(1),
                                                                 ECL_LONG_LONG_BITS))));
        } while(1);
}

ecl_long_long_t
ecl_to_long_long(cl_object x)
{
        do {
                if (FIXNUMP(x)) {
                        return (ecl_long_long_t)fix(x);
                } else if (type_of(x) != t_bignum) {
                        (void)0;
                } else if (mpz_fits_slong_p(x->big.big_num)) {
                        return (ecl_long_long_t)mpz_get_si(x->big.big_num);
                } else {
                        cl_object copy = _ecl_big_register0();
                        int i = ECL_LONG_LONG_BITS - FIXNUM_BITS;
                        mpz_fdiv_q_2exp(copy->bit.big_num, x->big.big_num, i);
                        if (mpz_fits_ulong_p(copy->big.big_num)) {
                                volatile ecl_long_long_t output;
                                output = mpz_get_si(copy->big.big_num);
                                for (i -= FIXNUM_BITS; i; i-= FIXNUM_BITS) {
                                        output = (output << FIXNUM_BITS);
                                        output += mpz_get_ui(x->big.big_num);
                                }
                                return output;
                        }
                }
		x = ecl_type_error(@'coerce', "variable", x,
                                   cl_list(3,@'integer',
                                           ecl_negate(ecl_ash(MAKE_FIXNUM(1), ECL_LONG_LONG_BITS-1)),
                                           ecl_one_minus(ecl_ash(MAKE_FIXNUM(1), ECL_LONG_LONG_BITS-1))));
        } while(1);
}

cl_object
ecl_make_unsigned_long_long(ecl_ulong_long_t i)
{
        if (i <= MOST_POSITIVE_FIXNUM) {
                return MAKE_FIXNUM(i);
        } else if (i <= ~(ecl_uint32_t)0) {
                return ecl_make_uint32_t(i);
        } else {
                cl_object aux = ecl_make_uint32_t(i >> 32);
                return cl_logior(2, ecl_ash(aux, 32),
                                 ecl_make_uint32_t((ecl_uint32_t)i));
        }
}

cl_object
ecl_make_long_long(ecl_long_long_t i)
{
        if (i >= MOST_NEGATIVE_FIXNUM && i <= MOST_POSITIVE_FIXNUM) {
                return MAKE_FIXNUM(i);
        } else {
                cl_object aux = ecl_make_int32_t(i >> 32);
                return cl_logior(2, ecl_ash(aux, 32), ecl_make_uint32_t((ecl_uint32_t)i));
        }
}
#  endif
# endif
#endif /* ecl_ulong_long_t */

cl_object
ecl_make_ratio(cl_object num, cl_object den)
{
	cl_object g, r;

	/* INV: the arguments NUM & DEN are integers */
	if (den == MAKE_FIXNUM(0))
		FEdivision_by_zero(num, den);
	if (num == MAKE_FIXNUM(0) || den == MAKE_FIXNUM(1))
		return(num);
	if (ecl_minusp(den)) {
		num = ecl_negate(num);
		den = ecl_negate(den);
	}
	g = ecl_gcd(num, den);
        if (g != MAKE_FIXNUM(1)) {
                num = ecl_integer_divide(num, g);
                den = ecl_integer_divide(den, g);
        }
	if (den == MAKE_FIXNUM(1))
		return num;
	if (den == MAKE_FIXNUM(-1))
		return ecl_negate(num);
	r = ecl_alloc_object(t_ratio);
	r->ratio.num = num;
	r->ratio.den = den;
	return(r);
}

#if defined(HAVE_FENV_H) && !defined(ECL_AVOID_FENV_H)
void
ecl_deliver_fpe(void)
{
        cl_env_ptr env = ecl_process_env();
        int bits = env->trap_fpe_bits;
        if (fetestexcept(env->trap_fpe_bits)) {
                cl_object condition;
		if (fetestexcept(bits & FE_DIVBYZERO))
			condition = @'division-by-zero';
		else if (fetestexcept(bits & FE_INVALID))
			condition = @'floating-point-invalid-operation';
		else if (fetestexcept(bits & FE_OVERFLOW))
			condition = @'floating-point-overflow';
		else if (fetestexcept(bits & FE_UNDERFLOW))
			condition = @'floating-point-underflow';
		else if (fetestexcept(bits & FE_INEXACT))
			condition = @'floating-point-inexact';
                else
                        condition = @'arithmetic-error';
                feclearexcept(FE_ALL_EXCEPT);
		cl_error(1, condition);
        }
        feclearexcept(FE_ALL_EXCEPT);
}
#endif

cl_object
ecl_make_singlefloat(float f)
{
	cl_object x;

	DO_DETECT_FPE(f);
	if (f == (float)0.0) {
#if defined(ECL_SIGNED_ZERO)
		if (signbit(f))
                        return cl_core.singlefloat_minus_zero;
#endif
                return cl_core.singlefloat_zero;
	}
	x = ecl_alloc_object(t_singlefloat);
	sf(x) = f;
	return(x);
}

cl_object
ecl_make_doublefloat(double f)
{
	cl_object x;

	DO_DETECT_FPE(f);
	if (f == (double)0.0) {
#if defined(ECL_SIGNED_ZERO)
		if (signbit(f))
                        return cl_core.doublefloat_minus_zero;
#endif
                return cl_core.doublefloat_zero;
	}
	x = ecl_alloc_object(t_doublefloat);
	df(x) = f;
	return(x);
}

#ifdef ECL_LONG_FLOAT
cl_object
ecl_make_longfloat(long double f)
{
	cl_object x;

	DO_DETECT_FPE(f);
	if (f == (long double)0.0) {
#if defined(ECL_SIGNED_ZERO)
		if (signbit(f))
                        return cl_core.longfloat_minus_zero;
#endif
                return cl_core.longfloat_zero;
	}
	x = ecl_alloc_object(t_longfloat);
	x->longfloat.value = f;
	return x;
}
#endif

cl_object
ecl_make_complex(cl_object r, cl_object i)
{
	cl_object c;
	cl_type ti;
 AGAIN:
	ti = type_of(i);
	/* Both R and I are promoted to a common type */
	switch (type_of(r)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		switch (ti) {
		case t_fixnum:
			if (i == MAKE_FIXNUM(0))
				return(r);
		case t_bignum:
		case t_ratio:
			break;
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			r = make_shortfloat((float)ecl_to_double(r));
			break;
#endif
		case t_singlefloat:
			r = ecl_make_singlefloat((float)ecl_to_double(r));
			break;
		case t_doublefloat:
			r = ecl_make_doublefloat(ecl_to_double(r));
			break;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			r = ecl_make_longfloat(ecl_to_double(r));
			break;
#endif
		default:
			i = ecl_type_error(@'complex',"imaginary part", i, @'real');
			goto AGAIN;
		}
		break;
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		switch (ti) {
		case t_fixnum:
		case t_bignum:
		case t_ratio:
			i = make_shortfloat((float)ecl_to_double(i));
		case t_shortfloat:
			break;
		case t_singlefloat:
			r = ecl_make_singlefloat((float)ecl_short_float(r));
			break;
		case t_doublefloat:
			r = ecl_make_doublefloat((double)ecl_short_float(r));
			break;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			r = ecl_make_longfloat((long double)ecl_short_float(r));
			break;
#endif
		default:
			i = ecl_type_error(@'complex',"imaginary part", i, @'real');
			goto AGAIN;
		}
		break;
#endif
	case t_singlefloat:
		switch (ti) {
		case t_fixnum:
		case t_bignum:
		case t_ratio:
			i = ecl_make_singlefloat((float)ecl_to_double(i));
			break;
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			i = ecl_make_singlefloat(ecl_short_float(i));
			break;
#endif
		case t_singlefloat:
			break;
		case t_doublefloat:
			r = ecl_make_doublefloat((double)(sf(r)));
			break;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			r = ecl_make_longfloat((long double)sf(r));
			break;
#endif
		default:
			i = ecl_type_error(@'complex',"imaginary part", i, @'real');
			goto AGAIN;
		}
		break;
	case t_doublefloat:
		switch (ti) {
		case t_fixnum:
		case t_bignum:
		case t_ratio:
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
#endif
		case t_singlefloat:
			i = ecl_make_doublefloat(ecl_to_double(i));
		case t_doublefloat:
			break;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			r = ecl_make_longfloat((long double)df(r));
			break;
#endif
		default:
			i = ecl_type_error(@'complex',"imaginary part", i, @'real');
			goto AGAIN;
		}
		break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		if (ti != t_longfloat)
			i = ecl_make_longfloat((long double)ecl_to_double(i));
		break;
#endif
	default:
		r = ecl_type_error(@'complex',"real part", r, @'real');
		goto AGAIN;

	}
	c = ecl_alloc_object(t_complex);
	c->complex.real = r;
	c->complex.imag = i;
	return(c);
}

#ifdef WITH_GMP
static cl_object
into_bignum(cl_object bignum, cl_object integer)
{
        if (FIXNUMP(integer)) {
                mpz_set_si(bignum->big.big_num, fix(integer));
        } else {
                mpz_set(bignum->big.big_num, integer->big.big_num);
        }
        return bignum;
}

static cl_fixnum
remove_zeros(cl_object *integer)
{
        cl_object buffer = into_bignum(_ecl_big_register0(), *integer);
        unsigned long den_twos = mpz_scan1(buffer->big.big_num, 0);
        if (den_twos < ULONG_MAX) {
                mpz_div_2exp(buffer->big.big_num, buffer->big.big_num, den_twos);
                *integer = _ecl_big_register_normalize(buffer);
                return -den_twos;
        } else {
                _ecl_big_register_free(buffer);
                return 0;
        }
}

static cl_object
prepare_ratio_to_float(cl_object num, cl_object den, int digits, cl_fixnum *scaleout)
{
        /* We have to cook our own routine because GMP does not round.
         * The recipe is simple: we multiply the numberator by a large
         * enough number so that the division by the denominator fits
         * the floating point number. The result is scaled back by the
         * appropriate exponent.
         */
        /* Scale down the denominator, eliminating the zeros
         * so that we have smaller operands.
         */
        cl_fixnum scale = remove_zeros(&den);
        cl_fixnum num_size, den_size, delta;
        num_size = ecl_integer_length(num);
        den_size = ecl_integer_length(den);
        delta = den_size - num_size;
        scale -= delta;
        num = ecl_ash(num, digits + delta + 1);
        do {
                cl_object fraction = ecl_truncate2(num, den);
                cl_object rem = VALUES(1);
                cl_fixnum len = ecl_integer_length(fraction);
                if ((len - digits) == 1) {
                        if (ecl_oddp(fraction)) {
                                cl_object one = ecl_minusp(num)?
                                        MAKE_FIXNUM(-1) :
                                        MAKE_FIXNUM(1);
                                if (rem == MAKE_FIXNUM(0)) {
                                        rem = cl_logand(2, fraction, MAKE_FIXNUM(2));
                                        if (rem != MAKE_FIXNUM(0))
                                                fraction = ecl_plus(fraction, one);
                                } else {
                                        fraction = ecl_plus(fraction, one);
                                }
                        }
                        *scaleout = scale - (digits + 1);
                        return fraction;
                }
                num = ecl_ash(num, -1);
                scale++;
                --delta;
        } while (1);
}
#endif /* WITH_GMP */

static float
ratio_to_float(cl_object num, cl_object den)
{
#ifdef WITH_GMP
        cl_fixnum scale;
        cl_object bits = prepare_ratio_to_float(num, den, FLT_MANT_DIG, &scale);
        float output = ecl_to_double(bits);
        return ldexpf(output, scale);
#else
        return (float)(FIXNUMP(num) ? fix(num) : num->big.big_num) /
                (float)(FIXNUMP(den) ? fix(den) : den->big.big_num);
#endif
}

static double
ratio_to_double(cl_object num, cl_object den)
{
#ifdef WITH_GMP
        cl_fixnum scale;
        cl_object bits = prepare_ratio_to_float(num, den, DBL_MANT_DIG, &scale);
        double output = ecl_to_double(bits);
        return ldexp(output, scale);
#else
        return (double)(FIXNUMP(num) ? fix(num) : num->big.big_num) /
                (double)(FIXNUMP(den) ? fix(den) : den->big.big_num);
#endif
}

#ifdef ECL_LONG_FLOAT
static long double
ratio_to_long_double(cl_object num, cl_object den)
{
#ifdef WITH_GMP
        cl_fixnum scale;
        cl_object bits = prepare_ratio_to_float(num, den, LDBL_MANT_DIG, &scale);
        long double output = ecl_to_long_double(bits);
        return ldexpl(output, scale);
#else
        return (long double)(FIXNUMP(num) ? fix(num) : num->big.big_num) /
                (long double)(FIXNUMP(den) ? fix(den) : den->big.big_num);
#endif
}
#endif /* ECL_LONG_FLOAT */

double
ecl_to_double(cl_object x)
{
	switch(type_of(x)) {
	case t_fixnum:
		return((double)(fix(x)));
	case t_bignum:
		return _ecl_big_to_double(x);
	case t_ratio:
                return ratio_to_double(x->ratio.num, x->ratio.den);
#ifdef ECL_SHORT_FLOAT
	case t_singlefloat:
		return ecl_short_float(x);
#endif
	case t_singlefloat:
		return (double)sf(x);
	case t_doublefloat:
		return(df(x));
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return (double)ecl_long_float(x);
#endif
	default:
		FEtype_error_real(x);
	}
}

#ifdef ECL_LONG_FLOAT
long double
ecl_to_long_double(cl_object x)
{
	switch(type_of(x)) {
	case t_fixnum:
		return (long double)fix(x);
	case t_bignum: {
                long double output = 0;
                int i, l = mpz_size(x->big.big_num), exp = 0;
                for (i = 0; i < l; i++) {
                        output += mpz_getlimbn(x->big.big_num, i);
                        output = ldexpl(output, -GMP_LIMB_BITS);
                }
                output = ldexpl(output, l * GMP_LIMB_BITS);
                return (mpz_sgn(x->big.big_num) < 0) ? -output : output;
        }
	case t_ratio:
                return ratio_to_long_double(x->ratio.num, x->ratio.den);
#ifdef ECL_SHORT_FLOAT
	case t_singlefloat:
		return ecl_short_float(x);
#endif
	case t_singlefloat:
		return (long double)sf(x);
	case t_doublefloat:
		return (long double)df(x);
	case t_longfloat:
		return ecl_long_float(x);
	default:
		FEtype_error_real(x);
	}
}
#endif

cl_object
cl_rational(cl_object x)
{
	double d;
 AGAIN:
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		break;
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		d = ecl_short_float(x);
		goto GO_ON;
#endif
	case t_singlefloat:
		d = sf(x);
		goto GO_ON;
	case t_doublefloat:
		d = df(x);
	GO_ON:	if (d == 0) {
			x = MAKE_FIXNUM(0);
		} else {
			int e;
			d = frexp(d, &e);
			e -= DBL_MANT_DIG;
			x = double_to_integer(ldexp(d, DBL_MANT_DIG));
                        if (e != 0) {
                                x = ecl_times(cl_expt(MAKE_FIXNUM(FLT_RADIX),
                                                      MAKE_FIXNUM(e)),
                                              x);
                        }
		}
		break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double d = ecl_long_float(x);
		if (d == 0) {
			x = MAKE_FIXNUM(0);
		} else {
			int e;
			d = frexpl(d, &e);
			e -= LDBL_MANT_DIG;
                        d = ldexpl(d, LDBL_MANT_DIG);
			x = long_double_to_integer(d);
			if (e != 0) {
				x = ecl_times(cl_expt(MAKE_FIXNUM(FLT_RADIX),
                                                      MAKE_FIXNUM(e)),
                                              x);
			}
		}
		break;
	}
#endif
	default:
		x = ecl_type_error(@'rational',"argument",x,@'number');
		goto AGAIN;
	}
	@(return x)
}

#ifdef ECL_LONG_FLOAT
cl_object
long_double_to_integer(long double d0)
{
        const int fb = FIXNUM_BITS - 3;
        int e;
        long double d = frexpl(d0, &e);
        if (e <= fb) {
                return MAKE_FIXNUM((cl_fixnum)d0);
        } else if (e > LDBL_MANT_DIG) {
                return ecl_ash(long_double_to_integer(ldexp(d, LDBL_MANT_DIG)),
                               e - LDBL_MANT_DIG);
        } else {
                long double d1 = floorl(d = ldexpl(d, fb));
                int newe = e - fb;
                cl_object o = ecl_ash(long_double_to_integer(d1), newe);
                long double d2 = ldexpl(d - d1, newe);
                if (d2) o = ecl_plus(o, long_double_to_integer(d2));
                return o;
        }
}
#endif

cl_object
double_to_integer(double d)
{
	if (d <= MOST_POSITIVE_FIXNUM && d >= MOST_NEGATIVE_FIXNUM)
		return MAKE_FIXNUM((cl_fixnum)d);
	else {
                cl_object z = _ecl_big_register0();
                _ecl_big_set_d(z, d);
                return _ecl_big_register_copy(z);
	}
}

cl_object
float_to_integer(float d)
{
	if (d <= MOST_POSITIVE_FIXNUM && d >= MOST_NEGATIVE_FIXNUM)
		return MAKE_FIXNUM((cl_fixnum)d);
	else {
                cl_object z = _ecl_big_register0();
                _ecl_big_set_d(z, d);
                return _ecl_big_register_copy(z);
	}
}

void
init_number(void)
{
	cl_object num;

	num = ecl_make_singlefloat(FLT_MAX);
	ECL_SET(@'MOST-POSITIVE-SHORT-FLOAT', num);
	ECL_SET(@'MOST-POSITIVE-SINGLE-FLOAT', num);

	num = ecl_make_singlefloat(-FLT_MAX);
	ECL_SET(@'MOST-NEGATIVE-SHORT-FLOAT', num);
	ECL_SET(@'MOST-NEGATIVE-SINGLE-FLOAT', num);

	num = ecl_make_singlefloat(FLT_MIN);
	ECL_SET(@'LEAST-POSITIVE-SHORT-FLOAT', num);
	ECL_SET(@'LEAST-POSITIVE-SINGLE-FLOAT', num);
	ECL_SET(@'LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT', num);
	ECL_SET(@'LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT', num);

	num = ecl_make_singlefloat(-FLT_MIN);
	ECL_SET(@'LEAST-NEGATIVE-SHORT-FLOAT', num);
	ECL_SET(@'LEAST-NEGATIVE-SINGLE-FLOAT', num);
	ECL_SET(@'LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT', num);
	ECL_SET(@'LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT', num);

	num = ecl_make_doublefloat(DBL_MAX);
	ECL_SET(@'MOST-POSITIVE-DOUBLE-FLOAT', num);
#ifdef ECL_LONG_FLOAT
	num = ecl_make_longfloat(LDBL_MAX);
#endif
	ECL_SET(@'MOST-POSITIVE-LONG-FLOAT', num);

	num = ecl_make_doublefloat(-DBL_MAX);
	ECL_SET(@'MOST-NEGATIVE-DOUBLE-FLOAT', num);
#ifdef ECL_LONG_FLOAT
	num = ecl_make_longfloat(-LDBL_MAX);
#endif
	ECL_SET(@'MOST-NEGATIVE-LONG-FLOAT', num);

	num = ecl_make_doublefloat(DBL_MIN);
	ECL_SET(@'LEAST-POSITIVE-DOUBLE-FLOAT', num);
	ECL_SET(@'LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT', num);
#ifdef ECL_LONG_FLOAT
	num = ecl_make_longfloat(LDBL_MIN);
#endif
	ECL_SET(@'LEAST-POSITIVE-LONG-FLOAT', num);
	ECL_SET(@'LEAST-POSITIVE-NORMALIZED-LONG-FLOAT', num);

	num = ecl_make_doublefloat(-DBL_MIN);
	ECL_SET(@'LEAST-NEGATIVE-DOUBLE-FLOAT', num);
	ECL_SET(@'LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT', num);
#ifdef ECL_LONG_FLOAT
	num = ecl_make_longfloat(-LDBL_MIN);
#endif
	ECL_SET(@'LEAST-NEGATIVE-LONG-FLOAT', num);
	ECL_SET(@'LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT', num);

 	cl_core.singlefloat_zero = ecl_alloc_object(t_singlefloat);
 	sf(cl_core.singlefloat_zero) = (float)0;
 	cl_core.doublefloat_zero = ecl_alloc_object(t_doublefloat);
 	df(cl_core.doublefloat_zero) = (double)0;
#ifdef ECL_LONG_FLOAT
 	cl_core.longfloat_zero = ecl_alloc_object(t_longfloat);
 	cl_core.longfloat_zero->longfloat.value = (long double)0;
#endif
#ifdef ECL_SIGNED_ZERO
 	cl_core.singlefloat_minus_zero = ecl_alloc_object(t_singlefloat);
 	sf(cl_core.singlefloat_minus_zero) = (float)-0.0;
 	cl_core.doublefloat_minus_zero = ecl_alloc_object(t_doublefloat);
 	df(cl_core.doublefloat_minus_zero) = (double)-0.0;
# ifdef ECL_LONG_FLOAT
 	cl_core.longfloat_minus_zero = ecl_alloc_object(t_longfloat);
 	cl_core.longfloat_minus_zero->longfloat.value = (long double)-0.0;
# endif
#else
        cl_core.singlefloat_minus_zero = cl_core.singlefloat_zero;
        cl_core.doublefloat_minus_zero = cl_core.doublefloat_zero;
# ifdef ECL_LONG_FLOAT
        cl_core.longfloat_minus_zero = cl_core.longfloat_zero;
# endif
#endif
	cl_core.plus_half = ecl_make_ratio(MAKE_FIXNUM(1), MAKE_FIXNUM(2));
	cl_core.minus_half = ecl_make_ratio(MAKE_FIXNUM(-1), MAKE_FIXNUM(2));
	cl_core.imag_unit =
	    ecl_make_complex(ecl_make_singlefloat(0.0), ecl_make_singlefloat(1.0));
	cl_core.minus_imag_unit =
	    ecl_make_complex(ecl_make_singlefloat(0.0), ecl_make_singlefloat(-1.0));
	cl_core.imag_two =
	    ecl_make_complex(ecl_make_singlefloat(0.0), ecl_make_singlefloat(2.0));

#ifdef ECL_LONG_FLOAT
	ECL_SET(@'pi', ecl_make_longfloat((long double)ECL_PI_L));
#else
	ECL_SET(@'pi', ecl_make_doublefloat((double)ECL_PI_D));
#endif
        ECL_SET(@'*random-state*', ecl_make_random_state(Ct));
}
