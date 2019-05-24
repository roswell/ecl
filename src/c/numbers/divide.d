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
#ifdef ECL_COMPLEX_FLOAT
      /* upgraded type csfloat */
      CASE_CSFLOAT_FIXNUM;
      CASE_CSFLOAT_BIGNUM;
      CASE_CSFLOAT_RATIO;
      CASE_CSFLOAT_SINGLE_FLOAT;
      CASE_CSFLOAT_COMPLEX;

      CASE_FIXNUM_CSFLOAT;
      CASE_BIGNUM_CSFLOAT;
      CASE_RATIO_CSFLOAT;
      CASE_SINGLE_FLOAT_CSFLOAT;
      CASE_COMPLEX_CSFLOAT;
      CASE_CSFLOAT_CSFLOAT {
        cl_object aux = ecl_alloc_object(t_csfloat);
        ecl_csfloat(aux) = ecl_to_csfloat(x) / ecl_to_csfloat(y);
        return aux;
      }
      /* upgraded type cdfloat */
      CASE_CSFLOAT_DOUBLE_FLOAT;
      CASE_DOUBLE_FLOAT_CSFLOAT;

      CASE_CDFLOAT_FIXNUM;
      CASE_CDFLOAT_BIGNUM;
      CASE_CDFLOAT_RATIO;
      CASE_CDFLOAT_SINGLE_FLOAT;
      CASE_CDFLOAT_DOUBLE_FLOAT;
      CASE_CDFLOAT_COMPLEX;
      CASE_CDFLOAT_CSFLOAT;

      CASE_FIXNUM_CDFLOAT;
      CASE_BIGNUM_CDFLOAT;
      CASE_RATIO_CDFLOAT;
      CASE_SINGLE_FLOAT_CDFLOAT;
      CASE_DOUBLE_FLOAT_CDFLOAT;
      CASE_COMPLEX_CDFLOAT;
      CASE_CSFLOAT_CDFLOAT;
      CASE_CDFLOAT_CDFLOAT {
        cl_object aux = ecl_alloc_object(t_cdfloat);
        ecl_cdfloat(aux) = ecl_to_cdfloat(x) / ecl_to_cdfloat(y);
        return aux;
      }
      /* upgraded type clfloat */
      CASE_CSFLOAT_LONG_FLOAT;
      CASE_LONG_FLOAT_CSFLOAT;
      CASE_CDFLOAT_LONG_FLOAT;
      CASE_LONG_FLOAT_CDFLOAT;

      CASE_CLFLOAT_FIXNUM;
      CASE_CLFLOAT_BIGNUM;
      CASE_CLFLOAT_RATIO;
      CASE_CLFLOAT_SINGLE_FLOAT;
      CASE_CLFLOAT_DOUBLE_FLOAT;
      CASE_CLFLOAT_LONG_FLOAT;
      CASE_CLFLOAT_COMPLEX;
      CASE_CLFLOAT_CSFLOAT;
      CASE_CLFLOAT_CDFLOAT;

      CASE_FIXNUM_CLFLOAT;
      CASE_BIGNUM_CLFLOAT;
      CASE_RATIO_CLFLOAT;
      CASE_SINGLE_FLOAT_CLFLOAT;
      CASE_DOUBLE_FLOAT_CLFLOAT;
      CASE_LONG_FLOAT_CLFLOAT;
      CASE_COMPLEX_CLFLOAT;
      CASE_CSFLOAT_CLFLOAT;
      CASE_CDFLOAT_CLFLOAT;
      CASE_CLFLOAT_CLFLOAT {
        cl_object aux = ecl_alloc_object(t_clfloat);
        ecl_clfloat(aux) = ecl_to_clfloat(x) / ecl_to_clfloat(y);
        return aux;
      }
#endif
      CASE_UNKNOWN(@[/],x,y,@[number]);
    }
  MATH_DISPATCH2_END;
}
