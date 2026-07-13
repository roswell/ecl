/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * midnum.c - bignum with fixed precision
 *
 * Copyright (c) 2005 Maciek Pasternacki
 * Copyright (c) 2026 Daniel Kochmański
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
  if (MOST_NEGATIVE_FIXNUM <= ecl_bignum(x) && ecl_bignum(x) <= MOST_POSITIVE_FIXNUM)
    return(ecl_make_fixnum(ecl_bignum(x)));
  return x;
}

cl_object
_ecl_big_register_normalize(cl_object x)
{
  if (ecl_bignum(x) == 0ll)
    return(ecl_make_fixnum(0));
  if (MOST_NEGATIVE_FIXNUM <= ecl_bignum(x) && ecl_bignum(x) <= MOST_POSITIVE_FIXNUM)
    return(ecl_make_fixnum(ecl_bignum(x)));
  return _ecl_big_register_copy(x);
}

cl_object
_ecl_big_gcd(cl_object x, cl_object y)
{
  big_num_t i = ecl_bignum(x), j = ecl_bignum(y);
  cl_object z = _ecl_big_register0();
  if (i<0) i=-i;
  if (j<0) j=-j;
  while ( 1 ) {
    big_num_t k;
    if ( i<j ) {
      k = i;
      i = j;
      j = k;
    }
    if ( j == 0 ) {
      ecl_bignum(z) = i;
      return _ecl_big_register_normalize(z);
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
_ecl_big_ceiling(cl_object x, cl_object y, cl_object *pr)
{
  cl_object q = ecl_alloc_object(t_bignum);
  cl_object r = ecl_alloc_object(t_bignum);
  ecl_bignum(q) = ecl_bignum(x) / ecl_bignum(y);
  ecl_bignum(r) = ecl_bignum(x) % ecl_bignum(y);
  *pr = big_normalize(r);
  return big_normalize(q);
}

cl_object
_ecl_big_floor(cl_object x, cl_object y, cl_object *pr)
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

/* missing interfaces */
void
ecl_init_bignum_registers(cl_env_ptr env)
{
  int i;
  for (i = 0; i < ECL_BIGNUM_REGISTER_NUMBER; i++) {
    cl_object x = ecl_alloc_object(t_bignum);
    env->big_register[i] = x;
  }
}

void
ecl_clear_bignum_registers(cl_env_ptr env)
{
  int i;
  for (i = 0; i < ECL_BIGNUM_REGISTER_NUMBER; i++) {
    ecl_bignum(env->big_register[i]) = 0;
    /* _ecl_big_clear(env->big_register[i]); */
  }
}

cl_fixnum
fixint(cl_object x)
{
  if (ECL_FIXNUMP(x))
    return ecl_fixnum(x);
  if (ECL_BIGNUMP(x)) {
    ecl_internal_error("implement fixint");
  }
  /* FIXME this error is not correct, cl_fixnum range is bigger. */
  FEwrong_type_argument(@[fixnum], x);
}

cl_index
fixnnint(cl_object x)
{
  if (ECL_FIXNUMP(x)) {
    cl_fixnum i = ecl_fixnum(x);
    if (i >= 0)
      return i;
  } else if (ECL_BIGNUMP(x)) {
    ecl_internal_error("implement fixnint");
  }
  /* FIXME this error is not correct, cl_index range is bigger. */
  FEwrong_type_argument(cl_list(3, @'integer', ecl_make_fixnum(0),
                                ecl_make_fixnum(MOST_POSITIVE_FIXNUM)),
                        x);
}

cl_object
_ecl_big_divided_by_big(cl_object x, cl_object y)
{
  cl_object z = ecl_alloc_object(t_bignum);
  ecl_bignum(z) = ecl_bignum(x) / ecl_bignum(y);
  return big_normalize(z);
}

cl_object
_ecl_big_divided_by_fix(cl_object x, cl_fixnum y)
{
  cl_object z = ecl_alloc_object(t_bignum);
  ecl_bignum(z) = ecl_bignum(x) / y;
  return big_normalize(z);
}

cl_object
_ecl_fix_divided_by_big(cl_fixnum x, cl_object y)
{
  cl_object z = ecl_alloc_object(t_bignum);
  ecl_bignum(z) = x / ecl_bignum(y);
  return big_normalize(z);
}

cl_object
_ecl_fix_minus_big(cl_fixnum x, cl_object y)
{
  cl_object z = ecl_alloc_object(t_bignum);
  ecl_bignum(z) = x - ecl_bignum(y);
  return big_normalize(z);
}

cl_object
_ecl_big_minus_big(cl_object x, cl_object y)
{
  cl_object z = ecl_alloc_object(t_bignum);
  ecl_bignum(z) = ecl_bignum(x) - ecl_bignum(y);
  return big_normalize(z);
}

static void
mid_bool_noop(cl_object op, cl_object x, cl_object y)
{
  ecl_internal_error("implement me!");
}

static _ecl_big_binary_op bignum_operations[16] = {
  mid_bool_noop,                /* ECL_BOOLCLR */
  mid_bool_noop,                /* ECL_BOOLAND */
  mid_bool_noop,                /* ECL_BOOLANDC2 */
  mid_bool_noop,                /* ECL_BOOL1 */
  mid_bool_noop,                /* ECL_BOOLANDC1 */
  mid_bool_noop,                /* ECL_BOOL2 */
  mid_bool_noop,                /* ECL_BOOLXOR */
  mid_bool_noop,                /* ECL_BOOLIOR */
  mid_bool_noop,                /* ECL_BOOLNOR */
  mid_bool_noop,                /* ECL_BOOLEQV */
  mid_bool_noop,                /* ECL_BOOLC2 */
  mid_bool_noop,                /* ECL_BOOLORC2 */
  mid_bool_noop,                /* ECL_BOOLC1 */
  mid_bool_noop,                /* ECL_BOOLORC1 */
  mid_bool_noop,                /* ECL_BOOLNAND */
  mid_bool_noop};               /* ECL_BOOLSET */

_ecl_big_binary_op
_ecl_big_boole_operator(int op)
{
  unlikely_if((op < 0) || (op >= 16)) {
    ecl_internal_error("_ecl_big_boole_operator passed an invalid operator");
  }
  return bignum_operations[op];
}
