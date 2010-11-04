/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    times.d  -- Implementation of CL:*
*/
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <ecl/impl/math_dispatch2.h>

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
