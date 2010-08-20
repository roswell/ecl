/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    num_sfun.c  -- Trascendental functions.
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
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>

#ifndef HAVE_LOG1P
double
log1p(double x)
{
	double u = 1.0 + x;
	if (u == 1) {
		return 0.0;
	} else {
		return (log(u) * x)/(u - 1.0);
	}
}
#endif

#ifndef HAVE_LOG1PF
float
log1pf(float x)
{
	float u = (float)1 + x;
	if (u == 1) {
		return (float)0;
	} else {
		return (logf(u) * x)/(u - (float)1);
	}
}
#endif

#if !defined(HAVE_LOG1PL) && defined(ECL_LONG_FLOAT)
long double
log1pl(long double x)
{
	long double u = (long double)1 + x;
	if (u == 1) {
		return (long double)1;
	} else {
		return (logl(u) * x)/(u - (long double)1);
	}
}
#endif

cl_object
ecl_abs(cl_object x)
{
	if (!ECL_COMPLEXP(x)) {
		if (ecl_minusp(x)) {
			x = ecl_negate(x);
		}
	} else {
		/* Compute sqrt(r*r + i*i) carefully to prevent overflow.
		 * Assume |i| >= |r|. Then sqrt(i*i + r*r) = |i|*sqrt(1 +(r/i)^2).
		 */
		cl_object r = x->complex.real;
		cl_object i = x->complex.imag;
		int comparison;
		if (ecl_minusp(r)) r = ecl_negate(r);
		if (ecl_minusp(i)) i = ecl_negate(i);
		comparison = ecl_number_compare(r, i);
		if (comparison == 0) {
			r = ecl_times(r, r);
			x = cl_sqrt(ecl_plus(r, r));
		} else {
			if (comparison > 0) {
				cl_object aux = i;
				i = r; r = aux;
			}
			r = ecl_divide(r, i);
			r = ecl_plus(MAKE_FIXNUM(1), ecl_times(r, r));
			x = ecl_times(cl_sqrt(r), i);
		}
	}
	return x;
}

cl_object
cl_abs(cl_object x)
{
	@(return ecl_abs(x))
}

cl_fixnum
ecl_fixnum_expt(cl_fixnum x, cl_fixnum y)
{
	cl_fixnum z = 1;
	while (y > 0)
		if (y%2 == 0) {
			x *= x;
			y /= 2;
		} else {
			z *= x;
			--y;
		}
	return(z);
}

cl_object
cl_exp(cl_object x)
{
        @(return ecl_exp(x));
}

cl_object
ecl_exp(cl_object x)
{
	cl_object output;
        ECL_MATHERR_CLEAR;
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		output = ecl_make_singlefloat(expf(number_to_float(x))); break;
	case t_singlefloat:
		output = ecl_make_singlefloat(expf(sf(x))); break;
	case t_doublefloat:
		output = ecl_make_doublefloat(exp(df(x))); break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		output = ecl_make_longfloat(expl(ecl_long_float(x))); break;
#endif
	case t_complex: {
		cl_object y, y1;

		y = x->complex.imag;
		output = ecl_exp(x->complex.real);
		y1 = ecl_cos(y);
		y = ecl_sin(y);
		y = ecl_make_complex(y1, y);
		output = ecl_times(output, y);
		break;
	}
	default:
		FEwrong_type_only_arg(@[exp], x, @[number]);
	}
        ECL_MATHERR_TEST;
	return output;
}

cl_object
cl_expt(cl_object x, cl_object y)
{
        @(return ecl_expt(x, y));
}

ecl_def_ct_single_float(singlefloat_one,1,static,const);
ecl_def_ct_double_float(doublefloat_one,1,static,const);
#ifdef ECL_LONG_FLOAT
ecl_def_ct_long_float(longfloat_one,1,static,const);
#endif

static cl_object
expt_zero(cl_object x, cl_object y)
{
	cl_type ty, tx;
	cl_object z;
        ty = type_of(y);
        tx = type_of(x);
        if (ecl_unlikely(!ECL_NUMBER_TYPE_P(tx))) {
                FEwrong_type_nth_arg(@[expt], 1, x, @[number]);
	}
        /* INV: The most specific numeric types come first. */
        switch ((ty > tx)? ty : tx) {
        case t_fixnum:
        case t_bignum:
        case t_ratio:
                return MAKE_FIXNUM(1);
        case t_singlefloat:
                return singlefloat_one;
        case t_doublefloat:
                return doublefloat_one;
#ifdef ECL_LONG_FLOAT
        case t_longfloat:
                return longfloat_one;
#endif
        case t_complex:
                z = expt_zero((tx == t_complex)? x->complex.real : x,
                              (ty == t_complex)? y->complex.real : y);
                return ecl_make_complex(z, MAKE_FIXNUM(0));
        default:
                /* We will never reach this */
                (void)0;
        }
}

cl_object
ecl_expt(cl_object x, cl_object y)
{
	cl_type ty, tx;
	cl_object z;
	if (ecl_unlikely(ecl_zerop(y))) {
                return expt_zero(x, y);
	}
        ty = type_of(y);
        tx = type_of(x);
        if (ecl_unlikely(!ECL_NUMBER_TYPE_P(tx))) {
                FEwrong_type_nth_arg(@[expt], 1, x, @[number]);
	}
        if (ecl_zerop(x)) {
		z = ecl_times(x, y);
		if (!ecl_plusp(ty==t_complex?y->complex.real:y))
			z = ecl_divide(MAKE_FIXNUM(1), z);
	} else if (ty != t_fixnum && ty != t_bignum) {
                /* The following could be just
                   z = ecl_log1(x);
                   however, Maxima expects EXPT to have double accuracy
                   when the first argument is integer and the second
                   is double-float */
		z = ecl_log1(ecl_times(x, expt_zero(x, y)));
		z = ecl_times(z, y);
		z = ecl_exp(z);
	} else if (ecl_minusp(y)) {
		z = ecl_negate(y);
		z = ecl_expt(x, z);
		z = ecl_divide(MAKE_FIXNUM(1), z);
	} else {
		z = MAKE_FIXNUM(1);
		do {
			/* INV: ecl_integer_divide outputs an integer */
			if (!ecl_evenp(y))
				z = ecl_times(z, x);
			y = ecl_integer_divide(y, MAKE_FIXNUM(2));
			if (ecl_zerop(y)) break;
			x = ecl_times(x, x);
		} while (1);
	}
	return z;
}

static cl_object
ecl_log1_complex(cl_object r, cl_object i)
{
	cl_object a = ecl_abs(r);
	cl_object p = ecl_abs(i);
	int rel = ecl_number_compare(a, p);
	if (rel > 0) {
		cl_object aux = p;
		p = a; a = aux;
	} else if (rel == 0) {
		/* if a == p, 
		 * log(sqrt(a^2+p^2)) = log(2a^2)/2
		 */
		a = ecl_times(a, a);
		a = ecl_divide(ecl_log1(ecl_plus(a, a)), MAKE_FIXNUM(2));
		goto OUTPUT;
	}
	/* For the real part of the output we use the formula
	 *	log(sqrt(p^2 + a^2)) = log(sqrt(p^2*(1 + (a/p)^2)))
	 *			     = log(p) + log(1 + (a/p)^2)/2; */
	a = ecl_divide(a, p);
	a = ecl_plus(ecl_divide(ecl_log1p(ecl_times(a,a)), MAKE_FIXNUM(2)),
		     ecl_log1(p));
 OUTPUT:
	p = ecl_atan2(i, r);
	return ecl_make_complex(a, p);
}

cl_object
ecl_log1(cl_object x)
{
        cl_object output;
        ECL_MATHERR_CLEAR;
	switch (type_of(x)) {
	case t_fixnum:
	case t_ratio: {
		float f = number_to_float(x);
		if (f < 0) goto COMPLEX;
		output = ecl_make_singlefloat(logf(number_to_float(x)));
                break;
	}
        case t_bignum: {
                if (ecl_minusp(x)) {
                        goto COMPLEX;
                } else {
                        cl_fixnum l = ecl_integer_length(x) - 1;
                        cl_object r = ecl_make_ratio(x, ecl_ash(MAKE_FIXNUM(1), l));
                        float d = logf(number_to_float(r)) + l * logf(2.0);
                        output = ecl_make_singlefloat(d);
                }
                break;
        }
	case t_singlefloat: {
		float f = sf(x);
		if (isnan(f)) goto ISNAN;
		if (f < 0) goto COMPLEX;
		output = ecl_make_singlefloat(logf(f));
                break;
	}
	case t_doublefloat: {
		double f = df(x);
		if (isnan(f)) goto ISNAN;
		if (f < 0) goto COMPLEX;
		output = ecl_make_doublefloat(log(f));
                break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double f = ecl_long_float(x);
		if (isnan(f)) goto ISNAN;
		if (f < 0) goto COMPLEX;
		output = ecl_make_longfloat(logl(f));
                break;
	}
#endif
	case t_complex:
		output = ecl_log1_complex(x->complex.real, x->complex.imag);
		break;
	ISNAN:
		output = x;
		break;
	COMPLEX:
		output = ecl_log1_complex(x, MAKE_FIXNUM(0));
		break;
	default:
                FEwrong_type_nth_arg(@[log], 1, x, @[number]);
	}
        ECL_MATHERR_TEST;
        return output;
}

cl_object
ecl_log2(cl_object x, cl_object y)
{
	return ecl_divide(ecl_log1(y), ecl_log1(x));
}

cl_object
ecl_log1p(cl_object x)
{
        cl_object output;
        ECL_MATHERR_CLEAR;
	switch (type_of(x)) {
	case t_fixnum:
	case t_ratio: {
		float f = number_to_float(x);
		if (f < -1) goto COMPLEX;
		output = ecl_make_singlefloat(log1pf(number_to_float(x)));
                break;
	}
	case t_bignum:
                return ecl_log1(ecl_one_plus(x));
	case t_singlefloat: {
		float f = sf(x);
		if (isnan(f)) goto ISNAN;
		if (f < -1) goto COMPLEX;
		output = ecl_make_singlefloat(log1pf(f));
                break;
	}
	case t_doublefloat: {
		double f = df(x);
		if (isnan(f)) goto ISNAN;
		if (f < -1) goto COMPLEX;
		output = ecl_make_doublefloat(log1p(f));
                break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double f = ecl_long_float(x);
		if (isnan(f)) goto ISNAN;
		if (f < -1) goto COMPLEX;
		output = ecl_make_longfloat(log1pl(f));
                break;
	}
#endif
	case t_complex:
		output = ecl_log1(ecl_plus(MAKE_FIXNUM(1), x));
		break;
	ISNAN:
		output = x;
		break;
	COMPLEX:
		output = ecl_log1_complex(ecl_plus(x, MAKE_FIXNUM(1)),
					  MAKE_FIXNUM(0));
		break;
	default:
                FEwrong_type_only_arg(@[log], x, @[number]);
        }
        ECL_MATHERR_TEST;
        return output;
}

cl_object
si_log1p(cl_object x)
{
	@(return ecl_log1p(x));
}

cl_object
cl_sqrt(cl_object x)
{
        @(return ecl_sqrt(x));
}

cl_object
ecl_sqrt(cl_object x)
{
	cl_object z;
	cl_type tx;
        ECL_MATHERR_CLEAR;
	tx = type_of(x);
	if (ecl_unlikely(!ECL_NUMBER_TYPE_P(tx))) {
                FEwrong_type_only_arg(@[sqrt], x, @[number]);
	}
	if (tx == t_complex) {
		z = cl_core.plus_half;
		z = ecl_expt(x, z);
	} else if (ecl_minusp(x)) {
		z = ecl_make_complex(MAKE_FIXNUM(0), ecl_sqrt(ecl_negate(x)));
	} else switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		z = ecl_make_singlefloat(sqrtf(number_to_float(x))); break;
	case t_singlefloat:
		z = ecl_make_singlefloat(sqrtf(sf(x))); break;
	case t_doublefloat:
		z = ecl_make_doublefloat(sqrt(df(x))); break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		z = ecl_make_longfloat(sqrtl(ecl_long_float(x))); break;
#endif
	default:
		/* Never reaches this */
		(void)0;
	}
        ECL_MATHERR_TEST;
	return z;
}

static double
ecl_atan2_double(double y, double x)
{
        if (signbit(x)) {
                if (signbit(y)) {
			return -ECL_PI_D + atan(-y / -x);
                } else if (y == 0) {
			return ECL_PI_D;
                } else {
			return ECL_PI_D - atan(y / -x);
		}
	} else if (x == 0) {
                if (signbit(y)) {
			return -ECL_PI2_D;
                } else if (y == 0) {
                        return x / y;  /* Produces a NaN */
		} else {
			return ECL_PI2_D;
		}
	} else {
                if (signbit(y)) {
                        return -atan(-y / x);
                } else if (y == 0) {
                        return (double)0;
                } else {
                        return atan(y / x);
		}
	}
}

#ifdef ECL_LONG_FLOAT
static long double
ecl_atan2_long_double(long double y, long double x)
{
        if (signbit(x)) {
                if (signbit(y)) {
			return -ECL_PI_L + atanl(-y / -x);
                } else if (y == 0) {
			return ECL_PI_L;
                } else {
			return ECL_PI_L - atanl(y / -x);
		}
	} else if (x == 0) {
                if (signbit(y)) {
			return -ECL_PI2_L;
                } else if (y == 0) {
                        return x / y;  /* Produces a NaN */
		} else {
			return ECL_PI2_L;
		}
	} else {
                if (signbit(y)) {
                        return -atanl(-y / x);
                } else if (y == 0) {
                        return (long double)0;
                } else {
                        return atanl(y / x);
		}
	}
}
#endif

cl_object
ecl_atan2(cl_object y, cl_object x)
{
        cl_object output;
        ECL_MATHERR_CLEAR;
        {
#ifdef ECL_LONG_FLOAT
	int tx = type_of(x);
	int ty = type_of(y);
	if (tx < ty)
		tx = ty;
	if (tx == t_longfloat) {
                long double d = ecl_atan2_long_double(ecl_to_long_double(y),
                                                      ecl_to_long_double(x));
		output = ecl_make_longfloat(d);
	} else {
		double dx = ecl_to_double(x);
		double dy = ecl_to_double(y);
		double dz = ecl_atan2_double(dy, dx);
		if (tx == t_doublefloat) {
			output = ecl_make_doublefloat(dz);
		} else {
			output = ecl_make_singlefloat(dz);
		}
	}
#else
	double dy = ecl_to_double(y);
	double dx = ecl_to_double(x);
	double dz = ecl_atan2_double(dy, dx);
	if (ECL_DOUBLE_FLOAT_P(x) || ECL_DOUBLE_FLOAT_P(y)) {
		output = ecl_make_doublefloat(dz);
	} else {
		output = ecl_make_singlefloat(dz);
	}
#endif
        }
        ECL_MATHERR_TEST;
        return output;
}

cl_object
ecl_atan1(cl_object y)
{
	if (ECL_COMPLEXP(y)) {
#if 0 /* ANSI states it should be this first part */
		cl_object z = ecl_times(cl_core.imag_unit, y);
		z = ecl_plus(ecl_log1(ecl_one_plus(z)),
			     ecl_log1(ecl_minus(MAKE_FIXNUM(1), z)));
		z = ecl_divide(z, ecl_times(MAKE_FIXNUM(2),
					    cl_core.imag_unit));
#else
		cl_object z1, z = ecl_times(cl_core.imag_unit, y);
		z = ecl_one_plus(z);
		z1 = ecl_times(y, y);
		z1 = ecl_one_plus(z1);
		z1 = ecl_sqrt(z1);
		z = ecl_divide(z, z1);
		z = ecl_log1(z);
		z = ecl_times(cl_core.minus_imag_unit, z);
#endif /* ANSI */
		return z;
	} else {
		return ecl_atan2(y, MAKE_FIXNUM(1));
	}
}

cl_object
cl_sin(cl_object x)
{
        @(return ecl_sin(x));
}

cl_object
ecl_sin(cl_object x)
{
	cl_object output;
        ECL_MATHERR_CLEAR;
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		output = ecl_make_singlefloat(sinf(number_to_float(x))); break;
	case t_singlefloat:
		output = ecl_make_singlefloat(sinf(sf(x))); break;
	case t_doublefloat:
		output = ecl_make_doublefloat(sin(df(x))); break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		output = ecl_make_longfloat(sinl(ecl_long_float(x))); break;
#endif
	case t_complex: {
		/*
		  z = x + I y
		  z = x + I y
		  sin(z) = sinh(I z) = sinh(-y + I x)
		*/
		cl_object dx = x->complex.real;
		cl_object dy = x->complex.imag;
		cl_object a = ecl_times(ecl_sin(dx), ecl_cosh(dy));
		cl_object b = ecl_times(ecl_cos(dx), ecl_sinh(dy));
		output = ecl_make_complex(a, b);
		break;
	}
	default:
                FEwrong_type_only_arg(@[sin], x, @[number]);
	}
        ECL_MATHERR_TEST;
        return output;
}

cl_object
cl_cos(cl_object x)
{
        @(return ecl_cos(x));
}

cl_object
ecl_cos(cl_object x)
{
	cl_object output;
        ECL_MATHERR_CLEAR;
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		output = ecl_make_singlefloat(cosf(number_to_float(x))); break;
	case t_singlefloat:
		output = ecl_make_singlefloat(cosf(sf(x))); break;
	case t_doublefloat:
		output = ecl_make_doublefloat(cos(df(x))); break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		output = ecl_make_longfloat(cosl(ecl_long_float(x))); break;
#endif
	case t_complex: {
		/*
		  z = x + I y
		  cos(z) = cosh(I z) = cosh(-y + I x)
		*/
		cl_object dx = x->complex.real;
		cl_object dy = x->complex.imag;
		cl_object a = ecl_times(ecl_cos(dx), ecl_cosh(dy));
		cl_object b = ecl_times(ecl_negate(ecl_sin(dx)), ecl_sinh(dy));
		output = ecl_make_complex(a, b);
		break;
	}
	default:
		FEwrong_type_only_arg(@[cos], x, @[number]);
	}
        ECL_MATHERR_TEST;
        return output;
}

/*
 * As of 2006-10-13 I found this bug in GLIBC's tanf, which overflows
 * when the argument is pi/4. It is 2008 and this has not yet been
 * solved. Not only that, but if we use tan() on float, GCC automatically
 * and stupidly forces the use of tanf().
 */
#if defined(__amd64__) && defined(__GLIBC__)
static double safe_tanf(double x) { return tan(x); }
#else
# define safe_tanf(x) tanf(x)
#endif

cl_object
cl_tan(cl_object x)
{
        @(return ecl_tan(x));
}

cl_object
ecl_tan(cl_object x)
{
	cl_object output;
        ECL_MATHERR_CLEAR;
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		output = ecl_make_singlefloat(safe_tanf(number_to_float(x))); break;
	case t_singlefloat:
		output = ecl_make_singlefloat(safe_tanf(sf(x))); break;
	case t_doublefloat:
		output = ecl_make_doublefloat(tan(df(x))); break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		output = ecl_make_longfloat(tanl(ecl_long_float(x))); break;
#endif
	case t_complex: {
		cl_object a = ecl_sin(x);
		cl_object b = ecl_cos(x);
		output = ecl_divide(a, b);
		break;
	}
	default:
                FEwrong_type_only_arg(@[tan], x, @[number]);
	}
        ECL_MATHERR_TEST;
	return output;
}

cl_object
cl_sinh(cl_object x)
{
        @(return ecl_sinh(x));
}

cl_object
ecl_sinh(cl_object x)
{
	cl_object output;
        ECL_MATHERR_CLEAR;
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		output = ecl_make_singlefloat(sinhf(number_to_float(x))); break;
	case t_singlefloat:
		output = ecl_make_singlefloat(sinhf(sf(x))); break;
	case t_doublefloat:
		output = ecl_make_doublefloat(sinh(df(x))); break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		output = ecl_make_longfloat(sinhl(ecl_long_float(x))); break;
#endif
	case t_complex: {
		/*
		  z = x + I y
		  sinh(z) = (exp(z)-exp(-z))/2
		          = (exp(x)*(cos(y)+Isin(y))-exp(-x)*(cos(y)-Isin(y)))/2
			  = sinh(x)*cos(y) + Icosh(x)*sin(y);
		*/
		cl_object dx = x->complex.real;
		cl_object dy = x->complex.imag;
		cl_object a = ecl_times(ecl_sinh(dx), ecl_cos(dy));
		cl_object b = ecl_times(ecl_cosh(dx), ecl_sin(dy));
		output = ecl_make_complex(a, b);
		break;
	}
	default:
                FEwrong_type_only_arg(@[sinh], x, @[number]);
	}
        ECL_MATHERR_TEST;
	return output;
}

cl_object
cl_cosh(cl_object x)
{
        @(return ecl_cosh(x));
}

cl_object
ecl_cosh(cl_object x)
{
	cl_object output;
        ECL_MATHERR_CLEAR;
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		output = ecl_make_singlefloat(coshf(number_to_float(x))); break;
	case t_singlefloat:
		output = ecl_make_singlefloat(coshf(sf(x))); break;
	case t_doublefloat:
		output = ecl_make_doublefloat(cosh(df(x))); break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		output = ecl_make_longfloat(coshl(ecl_long_float(x))); break;
#endif
	case t_complex: {
		/*
		  z = x + I y
		  cosh(z) = (exp(z)+exp(-z))/2
		          = (exp(x)*(cos(y)+Isin(y))+exp(-x)*(cos(y)-Isin(y)))/2
			  = cosh(x)*cos(y) + Isinh(x)*sin(y);
		*/
		cl_object dx = x->complex.real;
		cl_object dy = x->complex.imag;
		cl_object a = ecl_times(ecl_cosh(dx), ecl_cos(dy));
		cl_object b = ecl_times(ecl_sinh(dx), ecl_sin(dy));
		output = ecl_make_complex(a, b);
		break;
	}
	default:
                FEwrong_type_only_arg(@[cosh], x, @[number]);
	}
        ECL_MATHERR_TEST;
	return output;
}

cl_object
cl_tanh(cl_object x)
{
        @(return ecl_tanh(x));
}

cl_object
ecl_tanh(cl_object x)
{
	cl_object output;
        ECL_MATHERR_CLEAR;
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		output = ecl_make_singlefloat(tanhf(number_to_float(x))); break;
	case t_singlefloat:
		output = ecl_make_singlefloat(tanhf(sf(x))); break;
	case t_doublefloat:
		output = ecl_make_doublefloat(tanh(df(x))); break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		output = ecl_make_longfloat(tanhl(ecl_long_float(x))); break;
#endif
	case t_complex: {
		cl_object a = ecl_sinh(x);
		cl_object b = ecl_cosh(x);
		output = ecl_divide(a, b);
		break;
	}
	default:
                FEwrong_type_only_arg(@[tanh], x, @[number]);
	}
        ECL_MATHERR_TEST;
	return output;
}

@(defun log (x &optional (y OBJNULL))
@	/* INV: type check in ecl_log1() and ecl_log2() */
	if (y == OBJNULL)
		@(return ecl_log1(x))
	@(return ecl_log2(y, x))
@)

@(defun atan (x &optional (y OBJNULL))
@	/* INV: type check in ecl_atan() & ecl_atan2() */
	/* FIXME ecl_atan() and ecl_atan2() produce generic errors
	   without recovery and function information. */
	if (y == OBJNULL)
		@(return ecl_atan1(x))
	@(return ecl_atan2(x, y))
@)
