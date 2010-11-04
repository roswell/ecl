/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    divde.d  -- Implementation of CL:/
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
