/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    num_arith.c  -- Arithmetic operations
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

#include <ecl/ecl.h>
#include <ecl/number.h>
#include <stdlib.h>

#pragma fenv_access on

/*  (*		)  */

@(defun * (&rest nums)
	cl_object prod = MAKE_FIXNUM(1);
@
	/* INV: type check in ecl_times() */
	while (narg--)
		prod = ecl_times(prod, cl_va_arg(nums));
	@(return prod)
@)

cl_object
ecl_times(cl_object x, cl_object y)
{
	cl_object z, z1;

	switch (type_of(x)) {
	case t_fixnum:
		switch (type_of(y)) {
		case t_fixnum:
			return _ecl_fix_times_fix(fix(x),fix(y));
		case t_bignum:
			return _ecl_big_times_fix(y, fix(x));
		case t_ratio:
			z = ecl_times(x, y->ratio.num);
			return ecl_make_ratio(z, y->ratio.den);
		case t_singlefloat:
			return ecl_make_singlefloat(fix(x) * sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(fix(x) * df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(fix(x) * ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[*], 2, y, @[number]);
		}
	case t_bignum:
		switch (type_of(y)) {
		case t_fixnum:
			return _ecl_big_times_fix(x, fix(y));
		case t_bignum:
			return _ecl_big_times_big(x, y);
		case t_ratio:
			z = ecl_times(x, y->ratio.num);
			return ecl_make_ratio(z, y->ratio.den);
		case t_singlefloat:
			return ecl_make_singlefloat(ecl_to_double(x) * sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_to_double(x) * df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(ecl_to_double(x) * ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[*], 2, y, @[number]);
		}
	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			z = ecl_times(x->ratio.num, y);
			return ecl_make_ratio(z, x->ratio.den);
		case t_ratio:
			z = ecl_times(x->ratio.num,y->ratio.num);
			z1 = ecl_times(x->ratio.den,y->ratio.den);
			return ecl_make_ratio(z, z1);
		case t_singlefloat:
			return ecl_make_singlefloat(ecl_to_double(x) * sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_to_double(x) * df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(ecl_to_double(x) * ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[*], 2, y, @[number]);
		}
	case t_singlefloat: {
		float fx = sf(x);
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_singlefloat(fx * fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_singlefloat(fx * ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_singlefloat(fx * sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(fx * df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(fx * ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[*], 2, y, @[number]);
		}
	}
	case t_doublefloat: {
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_doublefloat(df(x) * fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_doublefloat(df(x) * ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_doublefloat(df(x) * sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(df(x) * df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(df(x) * ecl_long_float(y));
#endif
		case t_complex: {
		COMPLEX: /* INV: x is real, y is complex */
			return ecl_make_complex(ecl_times(x, y->complex.real),
                                                ecl_times(x, y->complex.imag));
		}
		default:
			FEwrong_type_nth_arg(@[*], 2, y, @[number]);
		}
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double lx = ecl_long_float(x);
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_longfloat(lx * fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_longfloat(lx * ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_longfloat(lx * sf(y));
		case t_doublefloat:
			return ecl_make_longfloat(lx * df(y));
		case t_longfloat:
			return ecl_make_longfloat(lx * ecl_long_float(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[*], 2, y, @[number]);
		}
	}
#endif
	case t_complex:
	{
		cl_object z11, z12, z21, z22;

		if (type_of(y) != t_complex) {
			cl_object aux = x;
			x = y; y = aux;
			goto COMPLEX;
		}
		z11 = ecl_times(x->complex.real, y->complex.real);
		z12 = ecl_times(x->complex.imag, y->complex.imag);
		z21 = ecl_times(x->complex.imag, y->complex.real);
		z22 = ecl_times(x->complex.real, y->complex.imag);
		return ecl_make_complex(ecl_minus(z11, z12), ecl_plus(z21, z22));
	}
	default:
		FEwrong_type_nth_arg(@[*], 1, x, @[number]);
	}
}

/* (+          )   */
@(defun + (&rest nums)
	cl_object sum = MAKE_FIXNUM(0);
@
	/* INV: type check is in ecl_plus() */
	while (narg--)
		sum = ecl_plus(sum, cl_va_arg(nums));
	@(return sum)
@)

cl_object
ecl_plus(cl_object x, cl_object y)
{
	cl_fixnum i, j;
	cl_object z, z1;

	switch (type_of(x)) {
	case t_fixnum:
	        switch (type_of(y)) {
		case t_fixnum:
                        return ecl_make_integer(fix(x) + fix(y));
		case t_bignum:
                        return _ecl_big_plus_fix(y, fix(x));
		case t_ratio:
			z = ecl_times(x, y->ratio.den);
			z = ecl_plus(z, y->ratio.num);
			return ecl_make_ratio(z, y->ratio.den);
		case t_singlefloat:
			return ecl_make_singlefloat(fix(x) + sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(fix(x) + df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(fix(x) + ecl_long_float(y));
#endif
		case t_complex:
		COMPLEX: /* INV: x is real, y is complex */
			return ecl_make_complex(ecl_plus(x, y->complex.real),
					    y->complex.imag);
		default:
			FEwrong_type_nth_arg(@[+], 2, y, @[number]);
		}
	case t_bignum:
		switch (type_of(y)) {
		case t_fixnum:
                        return _ecl_big_plus_fix(x, fix(y));
		case t_bignum:
                        return _ecl_big_plus_big(x, y);
		case t_ratio:
			z = ecl_times(x, y->ratio.den);
			z = ecl_plus(z, y->ratio.num);
			return ecl_make_ratio(z, y->ratio.den);
		case t_singlefloat:
			return ecl_make_singlefloat(ecl_to_double(x) + sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_to_double(x) + df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(ecl_to_double(x) + ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[+], 2, y, @[number]);
		}
	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			z = ecl_times(x->ratio.den, y);
			z = ecl_plus(x->ratio.num, z);
			return ecl_make_ratio(z, x->ratio.den);
		case t_ratio:
			z1 = ecl_times(x->ratio.num,y->ratio.den);
			z = ecl_times(x->ratio.den,y->ratio.num);
			z = ecl_plus(z1, z);
			z1 = ecl_times(x->ratio.den,y->ratio.den);
			return ecl_make_ratio(z, z1);
		case t_singlefloat:
			return ecl_make_singlefloat(ecl_to_double(x) + sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_to_double(x) + df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(ecl_to_double(x) + ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[+], 2, y, @[number]);
		}
	case t_singlefloat:
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_singlefloat(sf(x) + fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_singlefloat(sf(x) + ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_singlefloat(sf(x) + sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(sf(x) + df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(sf(x) + ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[+], 2, y, @[number]);
		}
	case t_doublefloat:
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_doublefloat(df(x) + fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_doublefloat(df(x) + ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_doublefloat(df(x) + sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(df(x) + df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(df(x) + ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[+], 2, y, @[number]);
		}
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_longfloat(ecl_long_float(x) + fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_longfloat(ecl_long_float(x) + ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_longfloat(ecl_long_float(x) + sf(y));
		case t_doublefloat:
			return ecl_make_longfloat(ecl_long_float(x) + df(y));
		case t_longfloat:
			return ecl_make_longfloat(ecl_long_float(x) + ecl_long_float(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[+], 2, y, @[number]);
		}
#endif
	case t_complex:
		if (type_of(y) != t_complex) {
			cl_object aux = x;
			x = y; y = aux;
			goto COMPLEX;
		}
		z = ecl_plus(x->complex.real, y->complex.real);
		z1 = ecl_plus(x->complex.imag, y->complex.imag);
		return ecl_make_complex(z, z1);
	default:
		FEwrong_type_nth_arg(@[+], 1, x, @[number]);
	}
}

/*  (-		)  */
@(defun - (num &rest nums)
	cl_object diff;
@
	/* INV: argument type check in number_{negate,minus}() */
	if (narg == 1)
		@(return ecl_negate(num))
	for (diff = num;  --narg; )
		diff = ecl_minus(diff, cl_va_arg(nums));
	@(return diff)
@)

cl_object
ecl_minus(cl_object x, cl_object y)
{
	cl_fixnum i, j, k;
	cl_object z, z1;

	switch (type_of(x)) {
	case t_fixnum:
		switch(type_of(y)) {
		case t_fixnum:
                        return ecl_make_integer(fix(x) - fix(y));
		case t_bignum:
                        return _ecl_fix_minus_big(fix(x), y);
		case t_ratio:
			z = ecl_times(x, y->ratio.den);
			z = ecl_minus(z, y->ratio.num);
			return ecl_make_ratio(z, y->ratio.den);
		case t_singlefloat:
			return ecl_make_singlefloat(fix(x) - sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(fix(x) - df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(fix(x) - ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[-], 2, y, @[number]);
		}
	case t_bignum:
		switch (type_of(y)) {
		case t_fixnum:
                        return _ecl_big_plus_fix(x, -fix(y));
		case t_bignum:
                        return _ecl_big_minus_big(x, y);
		case t_ratio:
			z = ecl_times(x, y->ratio.den);
			z = ecl_minus(z, y->ratio.num);
			return ecl_make_ratio(z, y->ratio.den);
		case t_singlefloat:
			return ecl_make_singlefloat(ecl_to_double(x) - sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_to_double(x) - df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(ecl_to_double(x) - ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[-], 2, y, @[number]);
		}
	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			z = ecl_times(x->ratio.den, y);
			z = ecl_minus(x->ratio.num, z);
			return ecl_make_ratio(z, x->ratio.den);
		case t_ratio:
			z = ecl_times(x->ratio.num,y->ratio.den);
			z1 = ecl_times(x->ratio.den,y->ratio.num);
			z = ecl_minus(z, z1);
			z1 = ecl_times(x->ratio.den,y->ratio.den);
			return ecl_make_ratio(z, z1);
		case t_singlefloat:
			return ecl_make_singlefloat(ecl_to_double(x) - sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_to_double(x) - df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(ecl_to_double(x) - ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[-], 2, y, @[number]);
		}
	case t_singlefloat:
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_singlefloat(sf(x) - fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_singlefloat(sf(x) - ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_singlefloat(sf(x) - sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(sf(x) - df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(sf(x) - ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[-], 2, y, @[number]);
		}
	case t_doublefloat:
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_doublefloat(df(x) - fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_doublefloat(df(x) - ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_doublefloat(df(x) - sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(df(x) - df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(df(x) - ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[-], 2, y, @[number]);
		}
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_longfloat(ecl_long_float(x) - fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_longfloat(ecl_long_float(x) - ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_longfloat(ecl_long_float(x) - sf(y));
		case t_doublefloat:
			return ecl_make_longfloat(ecl_long_float(x) - df(y));
		case t_longfloat:
			return ecl_make_longfloat(ecl_long_float(x) - ecl_long_float(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[-], 2, y, @[number]);
		}
#endif
	COMPLEX:
		return ecl_make_complex(ecl_minus(x, y->complex.real),
                                        ecl_negate(y->complex.imag));
	case t_complex:
		if (type_of(y) != t_complex) {
			z = ecl_minus(x->complex.real, y);
			z1 = x->complex.imag;
		} else {
			z = ecl_minus(x->complex.real, y->complex.real);
			z1 = ecl_minus(x->complex.imag, y->complex.imag);
		}
		return ecl_make_complex(z, z1);
	default:
		FEwrong_type_nth_arg(@[-], 1, x, @[number]);
	}
}

/*  (/		)  */
@(defun / (num &rest nums)
@
	/* INV: type check is in ecl_divide() */
	if (narg == 0)
		FEwrong_num_arguments(@[/]);
	if (narg == 1)
		@(return ecl_divide(MAKE_FIXNUM(1), num))
	while (--narg)
		num = ecl_divide(num, cl_va_arg(nums));
	@(return num)
@)

cl_object
ecl_divide(cl_object x, cl_object y)
{
	cl_object z, z1, z2;

	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
		switch (type_of(y)) {
		case t_fixnum:
			if (y == MAKE_FIXNUM(0))
				FEdivision_by_zero(x, y);
		case t_bignum:
			if (ecl_minusp(y) == TRUE) {
				x = ecl_negate(x);
				y = ecl_negate(y);
			}
			return ecl_make_ratio(x, y);
		case t_ratio:
			z = ecl_times(x, y->ratio.den);
			return ecl_make_ratio(z, y->ratio.num);
		case t_singlefloat:
			return ecl_make_singlefloat(ecl_to_double(x) / sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_to_double(x) / df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(ecl_to_double(x) / ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[/], 2, y, @[number]);
		}
	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
			if (y == MAKE_FIXNUM(0))
				FEdivision_by_zero(x, y);
		case t_bignum:
			z = ecl_times(x->ratio.den, y);
			return ecl_make_ratio(x->ratio.num, z);
		case t_ratio:
			z = ecl_times(x->ratio.num,y->ratio.den);
			z1 = ecl_times(x->ratio.den,y->ratio.num);
			return ecl_make_ratio(z, z1);
		case t_singlefloat:
			return ecl_make_singlefloat(ecl_to_double(x) / sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_to_double(x) / df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(ecl_to_double(x) / ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[/], 2, y, @[number]);
		}
	case t_singlefloat:
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_singlefloat(sf(x) / fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_singlefloat(sf(x) / ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_singlefloat(sf(x) / sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(sf(x) / df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(sf(x) / ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[/], 2, y, @[number]);
		}
	case t_doublefloat:
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_doublefloat(df(x) / fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_doublefloat(df(x) / ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_doublefloat(df(x) / sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(df(x) / df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_longfloat(df(x) / ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[/], 2, y, @[number]);
		}
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_longfloat(ecl_long_float(x) / fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_longfloat(ecl_long_float(x) / ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_longfloat(ecl_long_float(x) / sf(y));
		case t_doublefloat:
			return ecl_make_longfloat(ecl_long_float(x) / df(y));
		case t_longfloat:
			return ecl_make_longfloat(ecl_long_float(x) / ecl_long_float(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[/], 2, y, @[number]);
		}
#endif
	case t_complex:
		if (type_of(y) != t_complex) {
			z1 = ecl_divide(x->complex.real, y);
			z2 = ecl_divide(x->complex.imag, y);
			return ecl_make_complex(z1, z2);
		} else if (1) {
			/* #C(z1 z2) = #C(xr xi) * #C(yr -yi) */
			z1 = ecl_plus(ecl_times(x->complex.real, y->complex.real),
					 ecl_times(x->complex.imag, y->complex.imag));
			z2 = ecl_minus(ecl_times(x->complex.imag, y->complex.real),
					  ecl_times(x->complex.real, y->complex.imag));
		} else {
		COMPLEX: /* INV: x is real, y is complex */
			/* #C(z1 z2) = x * #C(yr -yi) */
			z1 = ecl_times(x, y->complex.real);
			z2 = ecl_negate(ecl_times(x, y->complex.imag));
		}
		z  = ecl_plus(ecl_times(y->complex.real, y->complex.real),
				 ecl_times(y->complex.imag, y->complex.imag));
		z  = ecl_make_complex(ecl_divide(z1, z), ecl_divide(z2, z));
		return(z);
	default:
		FEwrong_type_nth_arg(@[/], 1, x, @[number]);
	}
}

cl_object
ecl_integer_divide(cl_object x, cl_object y)
{
	cl_type tx, ty;

	tx = type_of(x);
	ty = type_of(y);
	if (tx == t_fixnum) {
 		if (ty == t_fixnum) {
			if (y == MAKE_FIXNUM(0))
				FEdivision_by_zero(x, y);
			return MAKE_FIXNUM(fix(x) / fix(y));
		} else if (ty == t_bignum) {
                        return _ecl_fix_divided_by_big(fix(x), y);
		} else {
                        FEwrong_type_nth_arg(@[round], 2, y, @[integer]);
                }
	}
	if (tx == t_bignum) {
		if (ty == t_bignum) {
			return _ecl_big_divided_by_big(x, y);
		} else if (ty == t_fixnum) {
                        return _ecl_big_divided_by_fix(x, fix(y));
		} else {
                        FEwrong_type_nth_arg(@[round], 2, y, @[integer]);
		}
	}
        FEwrong_type_nth_arg(@[round], 1, x, @[integer]);
}

@(defun gcd (&rest nums)
	cl_object gcd;
@
	if (narg == 0)
		@(return MAKE_FIXNUM(0))
	/* INV: ecl_gcd() checks types */
	gcd = cl_va_arg(nums);
	if (narg == 1) {
		assert_type_integer(gcd);
		@(return (ecl_minusp(gcd) ? ecl_negate(gcd) : gcd))
	}
	while (--narg)
		gcd = ecl_gcd(gcd, cl_va_arg(nums));
	@(return gcd)
@)

cl_object
ecl_gcd(cl_object x, cl_object y)
{
	cl_object gcd;
        ECL_WITH_TEMP_BIGNUM(x_big,1);
        ECL_WITH_TEMP_BIGNUM(y_big,1);

	switch (type_of(x)) {
	case t_fixnum:
                _ecl_big_set_fixnum(x_big, fix(x));
                x = x_big;
	case t_bignum:
		break;
	default:
		FEwrong_type_nth_arg(@[gcd], 1, x, @[integer]);
	}
	switch (type_of(y)) {
	case t_fixnum:
                _ecl_big_set_fixnum(y_big, fix(y));
                y = y_big;
	case t_bignum:
                break;
	default:
		FEwrong_type_nth_arg(@[gcd], 2, y, @[integer]);
        }
        return _ecl_big_gcd(x, y);
}

@(defun lcm (&rest nums)
	cl_object lcm;
@
	if (narg == 0)
		@(return MAKE_FIXNUM(1))
	/* INV: ecl_gcd() checks types. By placing `numi' before `lcm' in
	   this call, we make sure that errors point to `numi' */
	lcm = cl_va_arg(nums);
	assert_type_integer(lcm);
	while (narg-- > 1) {
		cl_object numi = cl_va_arg(nums);
		cl_object t = ecl_times(lcm, numi);
		cl_object g = ecl_gcd(numi, lcm);
		if (g != MAKE_FIXNUM(0))
			lcm = ecl_divide(t, g);
	}
	@(return (ecl_minusp(lcm) ? ecl_negate(lcm) : lcm))
@)
