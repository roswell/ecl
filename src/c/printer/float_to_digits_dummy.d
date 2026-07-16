/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * float_to_digits_dummy.d: floating point fixint printer functions
 *
 * Copyright (c) 2026 Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#define ECL_INCLUDE_MATH_H
#include <float.h>
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>

/* This is a crude approximation that does not rely on bignums. It is used
   currently as a stub, so that midnums do not overflow during compilation.
   SI:FLOAT-TO-DIGITS produces scale and digits, it doesn't care about the sign.

   Schubfach (Raffaello Giulietti, 2019) is faster, exact and does not rely on
   bignums either. Unlike DragonBox it work fine floats with any precision. We
   should use that in the final implementation. -- jd 2026-07-14 */
cl_object
si_float_to_digits(cl_object digits, cl_object number, cl_object position,
                   cl_object relativep)
{
  const char *dchar = "0123456789";
  double num, num_q, num_r, helper;
  cl_fixnum k = 0, idx, digit, count, sig_digits;
  char *digits_array;
  switch (ecl_t_of(number)) {
  case t_singlefloat:
    num = ecl_single_float(number);
    sig_digits = FLT_DIG;
    break;
  case t_doublefloat:
    num = ecl_double_float(number);
    sig_digits = DBL_DIG;
    break;
  case t_longfloat:
    num = (double)ecl_long_float(number);
    sig_digits = DBL_DIG;
    break;
  default:
    FEwrong_type_argument(@[float], number);
  }
  num = fabs(num);
  /* Edge case -- num=0. */
  if (num==0) {
    if (Null(digits))
      digits = si_make_vector(@'base-char', ecl_make_fixnum(10),
                              ECL_T /* adjustable */,
                              ecl_make_fixnum(0) /* fill pointer */,
                              ECL_NIL /* displacement */,
                              ECL_NIL /* displ. offset */);
    k=0;
    ecl_string_push_extend(digits, '0');
    @(return ecl_make_fixnum(k) digits);
  }
  /* Find the magnitude (scale) of a number. */
  num_q = trunc(num);
  num_r = fmod(num, 1.0);
  if (num>=1) {
    helper = num_q;
    while (helper>=1) {
      k++;
      helper /= 10;
    }
  } else {
    helper = num_r;
    while (helper<1) {
      k--;
      helper *= 10;
    }
    k++;
  }
  /* Ensure the buffer. */
  if (Null(digits))
    digits = si_make_vector(@'base-char', ecl_make_fixnum(10),
                            ECL_T /* adjustable */,
                            ecl_make_fixnum(0) /* fill pointer */,
                            ECL_NIL /* displacement */,
                            ECL_NIL /* displ. offset */);
  if (k>0) {
    digits = ecl_extend_vector(digits, k);
    digits_array = digits->base_string.self;
    digits->base_string.fillp = k;
  }
  count = 0;
  /* Write down the integral part. */
  helper = num_q;
  for(idx=k; idx>0 && count < sig_digits; idx--) {
    digit = (cl_fixnum)fmod(helper, 10.0);
    if      (digit<0) digit=0;
    else if (digit>9) digit=9;
    helper /= 10;
    digits_array[idx-1] = dchar[digit];
    count++;
  }
  for(; idx>0; idx--) digits_array[idx-1] = '0';
  /* Write down the fractional part. */
  helper = num_r;
  for(idx=k; helper > 0.0 && count < sig_digits; idx++) {
    helper *= 10;
    digit = (cl_fixnum)trunc(helper);
    if      (digit<0) digit=0;
    else if (digit>9) digit=9;
    helper = fmod(helper, 1.0);
    if(idx>=0) {
      ecl_string_push_extend(digits, dchar[digit]);
      count++;
    }
  }
  @(return ecl_make_fixnum(k) digits);
}
