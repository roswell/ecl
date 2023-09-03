/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * big_ll.c - bignum emulation with long long
 *
 * Copyright (c) 2005 Maciek Pasternacki
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/internal.h>

void
_ecl_big_register_free(cl_object x) {}

cl_object
_ecl_big_register_copy(cl_object old)
{
  cl_object new = ecl_alloc_object(t_bignum);
  ecl_bignum(new) = ecl_bignum(old);
  return new;
}

static cl_object
big_normalize(cl_object x)
{
  if (ecl_bignum(x) == 0ll)
    return(ecl_make_fixnum(0));
  if (ECL_NEGATIVE_FIXNUM <= ecl_bignum(x) && ecl_bignum(x) <= MOST_POSITIVE_FIXNUM)
    return(ecl_make_fixnum(ecl_bignum(x)));
  return x;
}

cl_object
_ecl_big_register_normalize(cl_object x)
{
  if (ecl_bignum(x) == 0ll)
    return(ecl_make_fixnum(0));
  if (ECL_NEGATIVE_FIXNUM <= ecl_bignum(x) && ecl_bignum(x) <= MOST_POSITIVE_FIXNUM)
    return(ecl_make_fixnum(ecl_bignum(x)));
  return _ecl_big_register_copy(x);
}

static cl_object
big_alloc(int size)
{
  volatile cl_object x = ecl_alloc_object(t_bignum);
  if (size <= 0)
    ecl_internal_error("negative or zero size for bignum in big_alloc");
  ecl_bignum(x) = 0ll;
  return x;
}

static cl_object
_ecl_big_copy(cl_object x)
{
  volatile cl_object y = ecl_alloc_object(t_bignum);
  ecl_bignum(y) = ecl_bignum(x);
  return y;
}

cl_object
_ecl_big_gcd(cl_object x, cl_object y)
{
  big_num_t i = ecl_bignum(x), j = ecl_bignum(y);
  cl_object gcd = ecl_alloc_object(t_bignum);
  while ( 1 ) {
    big_num_t k;
    if ( i<j ) {
      k = i;
      i = j;
      j = k;
    }
    if ( j == 0 ) {
      ecl_bignum(gcd) = k;
      return gcd;
    }
    k = i % j;
    i = j;
    j = k;
  }
}

int
_ecl_big_num_t_sgn(big_num_t x)
{
  return ( x == (big_num_t)0 ) ? 0 : (x < (big_num_t)0) ? -1 : 1;
}

cl_object
_ecl_big_times_big(cl_object x, cl_object y)
{
  cl_object z = ecl_alloc_object(t_bignum);
  ecl_bignum(z) = ecl_bignum(x) * ecl_bignum(y);
  return z;
}

cl_object
_ecl_big_times_fix(cl_object x, cl_fixnum y)
{
  cl_object z = ecl_alloc_object(t_bignum);
  ecl_bignum(z) = ecl_bignum(x) * y;
  return big_normalize(z);
}

cl_object
_ecl_big_plus_big(cl_object x, cl_object y)
{
  cl_object z = ecl_alloc_object(t_bignum);
  ecl_bignum(z) = ecl_bignum(x) + ecl_bignum(y);
  return z;
}

cl_object
_ecl_big_plus_fix(cl_object x, cl_fixnum y)
{
  cl_object z = ecl_alloc_object(t_bignum);
  ecl_bignum(z) = ecl_bignum(x) + y;
  return big_normalize(z);
}

cl_object
_ecl_fix_times_fix(cl_fixnum x, cl_fixnum y)
{
  cl_object z = ecl_alloc_object(t_bignum);
  ecl_bignum(z) = x * y;
  return big_normalize(z);
}

cl_object
_ecl_big_ceiling(cl_object a, cl_object b, cl_object *pr)
{
  cl_object q = ecl_alloc_object(t_bignum);
  cl_object r = ecl_alloc_object(t_bignum);
  ecl_bignum(q) = ecl_bignum(x) / ecl_bignum(y);
  ecl_bignum(r) = ecl_bignum(x) % ecl_bignum(y);
  *pr = big_normalize(r);
  return big_normalize(q);
}

cl_object
_ecl_big_floor(cl_object a, cl_object b, cl_object *pr)
{
  cl_object q = ecl_alloc_object(t_bignum);
  cl_object r = ecl_alloc_object(t_bignum);
  ecl_bignum(q) = ecl_bignum(x) / ecl_bignum(y);
  ecl_bignum(r) = ecl_bignum(x) % ecl_bignum(y);
  *pr = big_normalize(r);
  return big_normalize(q);
}

cl_object
_ecl_big_negate(cl_object x)
{
  cl_object z = ecl_alloc_object(t_bignum);
  ecl_bignum(z) = - ecl_bignum(x);
  return z;
}

void
init_big(void)
{
}
