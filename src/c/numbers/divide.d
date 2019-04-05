/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * divde.d  - implementation of CL:/
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/impl/math_dispatch2.h>

@(defun / (num &rest nums)
  @
  /* INV: type check is in ecl_divide() */
  if (narg == 0)
  FEwrong_num_arguments(@[/]);
  if (narg == 1) {
    @(return ecl_divide(ecl_make_fixnum(1), num));
  }
  while (--narg)
        num = ecl_divide(num, ecl_va_arg(nums));
  @(return num);
  @)

static cl_object
complex_divide(cl_object ar, cl_object ai, cl_object br, cl_object bi)
{
  /* #C(z1 z2) = #C(xr xi) * #C(yr -yi) */
  cl_object z1 = ecl_plus(ecl_times(ar, br), ecl_times(ai, bi));
  cl_object z2 = ecl_minus(ecl_times(ai, br), ecl_times(ar, bi));
  cl_object absB = ecl_plus(ecl_times(br, br), ecl_times(bi, bi));
  return ecl_make_complex(ecl_divide(z1, absB), ecl_divide(z2, absB));
}

cl_object
ecl_divide(cl_object x, cl_object y)
{
  MATH_DISPATCH2_BEGIN(x,y)
    {
      CASE_FIXNUM_FIXNUM;
      CASE_BIGNUM_FIXNUM {
        if (y == ecl_make_fixnum(0))
          FEdivision_by_zero(x, y);
      }
      CASE_FIXNUM_BIGNUM;
      CASE_BIGNUM_BIGNUM {
        return ecl_make_ratio(x, y);
      }
      CASE_FIXNUM_RATIO;
      CASE_BIGNUM_RATIO {
        return ecl_make_ratio(ecl_times(x, y->ratio.den),
                              y->ratio.num);
      }
      CASE_FIXNUM_SINGLE_FLOAT {
        return ecl_make_single_float(ecl_fixnum(x) / ecl_single_float(y));
      }
      CASE_FIXNUM_DOUBLE_FLOAT {
        return ecl_make_double_float(ecl_fixnum(x) / ecl_double_float(y));
      }
      CASE_BIGNUM_SINGLE_FLOAT;
      CASE_RATIO_SINGLE_FLOAT {
        return ecl_make_single_float(ecl_to_float(x) / ecl_single_float(y));
      }
      CASE_BIGNUM_DOUBLE_FLOAT;
      CASE_RATIO_DOUBLE_FLOAT {
        return ecl_make_double_float(ecl_to_double(x) / ecl_double_float(y));
      }
      CASE_RATIO_FIXNUM {
        if (y == ecl_make_fixnum(0)) {
          FEdivision_by_zero(x,y);
        }
      }
      CASE_RATIO_BIGNUM {
        cl_object z = ecl_times(x->ratio.den, y);
        return ecl_make_ratio(x->ratio.num, z);
      }
      CASE_RATIO_RATIO {
        cl_object num = ecl_times(x->ratio.num,y->ratio.den);
        cl_object den = ecl_times(x->ratio.den,y->ratio.num);
        return ecl_make_ratio(num, den);
      }
      CASE_SINGLE_FLOAT_FIXNUM {
        return ecl_make_single_float(ecl_single_float(x) / ecl_fixnum(y));
      }
      CASE_SINGLE_FLOAT_BIGNUM;
      CASE_SINGLE_FLOAT_RATIO {
        return ecl_make_single_float(ecl_single_float(x) / ecl_to_float(y));
      }
      CASE_SINGLE_FLOAT_SINGLE_FLOAT {
        return ecl_make_single_float(ecl_single_float(x) / ecl_single_float(y));
      }
      CASE_SINGLE_FLOAT_DOUBLE_FLOAT {
        return ecl_make_double_float(ecl_single_float(x) / ecl_double_float(y));
      }
      CASE_DOUBLE_FLOAT_FIXNUM {
        return ecl_make_double_float(ecl_double_float(x) / ecl_fixnum(y));
      }
      CASE_DOUBLE_FLOAT_BIGNUM;
      CASE_DOUBLE_FLOAT_RATIO {
        return ecl_make_double_float(ecl_double_float(x) / ecl_to_double(y));
      }
      CASE_DOUBLE_FLOAT_SINGLE_FLOAT {
        return ecl_make_double_float(ecl_double_float(x) / ecl_single_float(y));
      }
      CASE_DOUBLE_FLOAT_DOUBLE_FLOAT {
        return ecl_make_double_float(ecl_double_float(x) / ecl_double_float(y));
      }
#ifdef ECL_LONG_FLOAT
      CASE_FIXNUM_LONG_FLOAT {
        return ecl_make_long_float(ecl_fixnum(x) / ecl_long_float(y));
      }
      CASE_BIGNUM_LONG_FLOAT;
      CASE_RATIO_LONG_FLOAT {
        return ecl_make_long_float(ecl_to_long_double(x) / ecl_long_float(y));
      }
      CASE_SINGLE_FLOAT_LONG_FLOAT {
        return ecl_make_long_float(ecl_single_float(x) / ecl_long_float(y));
      }
      CASE_DOUBLE_FLOAT_LONG_FLOAT {
        return ecl_make_long_float(ecl_double_float(x) / ecl_long_float(y));
      }
      CASE_LONG_FLOAT_FIXNUM {
        return ecl_make_long_float(ecl_long_float(x) / ecl_fixnum(y));
      }
      CASE_LONG_FLOAT_BIGNUM;
      CASE_LONG_FLOAT_RATIO {
        return ecl_make_long_float(ecl_long_float(x) / ecl_to_long_double(y));
      }
      CASE_LONG_FLOAT_SINGLE_FLOAT {
        return ecl_make_long_float(ecl_long_float(x) / ecl_single_float(y));
      }
      CASE_LONG_FLOAT_DOUBLE_FLOAT {
        return ecl_make_long_float(ecl_long_float(x) / ecl_double_float(y));
      }
      CASE_LONG_FLOAT_LONG_FLOAT {
        return ecl_make_long_float(ecl_long_float(x) / ecl_long_float(y));
      }
      CASE_LONG_FLOAT_COMPLEX {
        goto COMPLEX_Y;
      }
      CASE_COMPLEX_LONG_FLOAT;  {
        goto COMPLEX_X;
      }
#endif
      CASE_COMPLEX_FIXNUM;
      CASE_COMPLEX_BIGNUM;
      CASE_COMPLEX_RATIO;
      CASE_COMPLEX_SINGLE_FLOAT;
      CASE_COMPLEX_DOUBLE_FLOAT; COMPLEX_X: {
        return ecl_make_complex(ecl_divide(x->gencomplex.real, y),
                                ecl_divide(x->gencomplex.imag, y));
      }
      CASE_BIGNUM_COMPLEX;
      CASE_RATIO_COMPLEX;
      CASE_SINGLE_FLOAT_COMPLEX;
      CASE_DOUBLE_FLOAT_COMPLEX;
      CASE_FIXNUM_COMPLEX {
      COMPLEX_Y:
        return complex_divide(x, ecl_make_fixnum(0), y->gencomplex.real, y->gencomplex.imag);
      }
      CASE_COMPLEX_COMPLEX {
        return complex_divide(x->gencomplex.real, x->gencomplex.imag,
                              y->gencomplex.real, y->gencomplex.imag);
      }
      CASE_UNKNOWN(@[/],x,y,@[number]);
    }
  MATH_DISPATCH2_END;
}
