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

/* Revisions

   [rev 20050919 mp] initial revision (implement big_ll)
   [rev 20260714 jd] make big_ll work (address bitrot)
   [rev 20260716 jd] avoid UB and handle overflows

   FIXME implement stubs and incorrect functions:
   coerce: fixnnint, fixint,
   access: _ecl_big_{get,set}_{idx,fix,ui,si,d,lf}
   mixing: _ecl_big_{mul,add}_ui

   FIXME implement a version using two limbs [intmax_t uintmax_t]. Necessary
   when we want to be able to represent full range of standard C ints.

   FIXME I've stubbed a naive implementation of float_to_digits that does not
   rely on bignums because printing floats were unstable due to overflows.

   NOTES representing a midnum as [sign, uint] is not practical, because boole
   operations require 2-complement. That's why using a signed number works best.
   It would be nice to provide "school-book" complete fallback implementation.

   -- jd 2026-07-14 */

#include <ecl/ecl.h>
#include <ecl/internal.h>

#define MOST_NEGATIVE_BIGNUM LLONG_MIN
#define MOST_POSITIVE_BIGNUM LLONG_MAX
#define BIGNUM_BITS ECL_LONG_LONG_BITS

/* Unsigned variant is used in mul_exp for negative xnum. Casting is done after
   ensuring that big_num_t won't overflow. */

typedef signed   long long sbig_num_t;
typedef unsigned long long ubig_num_t;

static void
storage_exhausted (void) {
  cl_error(1, @'ext::storage-exhausted');
}

/* Initialization */
void init_big(void) {}

/* Registers */
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
  sbig_num_t xnum = ecl_bignum(x);
  if (MOST_NEGATIVE_FIXNUM <= xnum && xnum <= MOST_POSITIVE_FIXNUM)
    return ecl_make_fixnum(xnum);
  return x;
}

cl_object
_ecl_big_register_normalize(cl_object x)
{
  cl_object z = big_normalize(x);
  return x==z ? _ecl_big_register_copy(z) : z;
}

int
_ecl_big_compare(cl_object x, cl_object y) {
  sbig_num_t ix = ecl_bignum(x), iy = ecl_bignum(y);
  if (ix < iy) return -1;
  else         return (ix != iy);
}

/* Math  */

cl_object
_ecl_big_gcd(cl_object x, cl_object y)
{
  sbig_num_t xnum = ecl_bignum(x);
  sbig_num_t ynum = ecl_bignum(y);
  cl_object z = _ecl_big_register0();

  ubig_num_t ux = (xnum < 0) ? -(ubig_num_t)xnum : xnum;
  ubig_num_t uy = (ynum < 0) ? -(ubig_num_t)ynum : ynum;

  while (uy != 0) {
    ubig_num_t rem = ux % uy;
    ux = uy;
    uy = rem;
  }

  /* This happens only for x=y=MOST_NEGATIVE_BIGNUM. */
  if (ux > (ubig_num_t)MOST_POSITIVE_BIGNUM)
    storage_exhausted();

  ecl_bignum(z) = (sbig_num_t)ux;
  return _ecl_big_register_normalize(z);
}

/* Bit shifts */

void
_ecl_big_div_2exp(cl_object z, cl_object x, cl_index bits)
{
  sbig_num_t xnum = ecl_bignum(x);
  if (xnum == 0 || bits >= BIGNUM_BITS) {
    /* Right-shift floor towards negative infinity. */
    ecl_bignum(z) = (xnum<0) ? -1 : 0;
  } else {
    /* Right-shifting negative integers is implementation defined. */
    if (xnum<0) ecl_bignum(z) = ~(~xnum >> bits);
    else        ecl_bignum(z) =    xnum >> bits;
  }
}

void
_ecl_big_mul_2exp(cl_object z, cl_object x, cl_index bits)
{
  sbig_num_t xnum = ecl_bignum(x);
  /* Right-shifting negative integers is implementation defined.
     That's why we cast limits to ubig_num_t. */
  const ubig_num_t positive_limit = MOST_POSITIVE_BIGNUM;
  const ubig_num_t negative_limit = MOST_NEGATIVE_BIGNUM;
  if (xnum == 0) {
    ecl_bignum(z) = 0;
    return;
  }
  if (bits >= BIGNUM_BITS)
    storage_exhausted();
  if ((xnum > 0 && (ubig_num_t)xnum >   (positive_limit >> bits)) ||
      (xnum < 0 && (ubig_num_t)xnum <= ~(negative_limit >> bits)) )
    storage_exhausted();
  /* Left-shifting negative integers is UB. We've already checked that the
     result will fit in sbig_num_t, so we can safely cast it to unsigned.*/
  ecl_bignum(z) = (ubig_num_t)xnum << bits;
}

/* Negation */

void
_ecl_big_neg(cl_object z, cl_object x)
{
  sbig_num_t xnum = ecl_bignum(x);
  /* For a minimal signed int value num=-num is UB. */
  if (xnum == MOST_NEGATIVE_BIGNUM) storage_exhausted();
  ecl_bignum(z) = -xnum;
}

cl_object
_ecl_big_negate(cl_object x)
{
  cl_object z = ecl_alloc_object(t_bignum);
  _ecl_big_neg(z, x);
  return big_normalize(z);
}

/* Division
   INV the second argument is never 0 (ensured by callers) */

void
_ecl_big_div(cl_object z, cl_object x, cl_object y)
{
  sbig_num_t xnum = ecl_bignum(x);
  sbig_num_t ynum = ecl_bignum(y);
  /* For a minimal signed int value num/-1 is UB. */
  if (xnum == MOST_NEGATIVE_BIGNUM && ynum==-1)
    storage_exhausted();
  ecl_bignum(z) = xnum / ynum;
}


cl_object
_ecl_big_divided_by_big(cl_object x, cl_object y)
{
  cl_object z = ecl_alloc_object(t_bignum);
  _ecl_big_div(z, x, y);
  return big_normalize(z);
}

cl_object
_ecl_big_divided_by_fix(cl_object x, cl_fixnum ynum)
{
  cl_object z = ecl_alloc_object(t_bignum);
  cl_object y = _ecl_big_register0();
  _ecl_big_set_fix(y, ynum);
  _ecl_big_div(z, x, y);
  return big_normalize(z);
}

cl_object
_ecl_fix_divided_by_big(cl_fixnum xnum, cl_object y)
{
  cl_object z = ecl_alloc_object(t_bignum);
  cl_object x = _ecl_big_register0();
  _ecl_big_set_fix(x, xnum);
  _ecl_big_div(z, x, y);
  return big_normalize(z);
}


/* Multiplication */

static sbig_num_t
_int_mul_int(sbig_num_t xnum, sbig_num_t ynum)
{
  if (xnum!=0 && ynum!=0) {
    if ((xnum>0 && ynum>0 && xnum > MOST_POSITIVE_BIGNUM / ynum) ||
        (xnum<0 && ynum<0 && xnum < MOST_POSITIVE_BIGNUM / ynum) ||
        /* xnum>0, so we avoid UB  MOST_NEGATIVE_BIGNUM/-1. */
        (xnum>0 && ynum<0 && ynum < MOST_NEGATIVE_BIGNUM / xnum) ||
        (xnum<0 && ynum>0 && xnum < MOST_NEGATIVE_BIGNUM / ynum))
      storage_exhausted();
  }
  return xnum * ynum;
}

void
_ecl_big_mul(cl_object z, cl_object x, cl_object y)
{
  sbig_num_t xnum = ecl_bignum(x);
  sbig_num_t ynum = ecl_bignum(y);
  ecl_bignum(z) = _int_mul_int(xnum, ynum);
}

void
_ecl_big_mul_ui(cl_object z, cl_object x, unsigned long int ynum)
{
  /* KLUDGE ecl_parse_integer calls this function with z=reg0. */
  cl_object y = _ecl_big_register1();
  _ecl_big_set_ui(y, ynum);
  _ecl_big_mul(z, x, y);
}

cl_object
_ecl_big_times_big(cl_object x, cl_object y)
{
  cl_object z = ecl_alloc_object(t_bignum);
  _ecl_big_mul(z, x, y);
  /* bignum x bignum is always bignum - no need for big_normalize. */
  return z;
}

cl_object
_ecl_big_times_fix(cl_object x, cl_fixnum ynum)
{
  cl_object z = ecl_alloc_object(t_bignum);
  cl_object y = _ecl_big_register0();
  _ecl_big_set_fix(y, ynum);
  _ecl_big_mul(z, x, y);
  return big_normalize(z);
}

cl_object
_ecl_fix_times_fix(cl_fixnum xnum, cl_fixnum ynum)
{
  cl_object z = ecl_alloc_object(t_bignum);
  cl_object x = _ecl_big_register0();
  cl_object y = _ecl_big_register1();
  _ecl_big_set_fix(x, xnum);
  _ecl_big_set_fix(y, ynum);
  _ecl_big_mul(z, x, y);
  return big_normalize(z);
}

/* Addition */

static sbig_num_t
_int_add_int(sbig_num_t xnum, sbig_num_t ynum)
{
  if ((xnum > 0 && ynum > 0 && xnum > MOST_POSITIVE_BIGNUM - ynum) ||
      (xnum < 0 && ynum < 0 && xnum < MOST_NEGATIVE_BIGNUM - ynum))
    storage_exhausted();
  return xnum+ynum;
}

void
_ecl_big_add(cl_object z, cl_object x, cl_object y)
{
  sbig_num_t xnum = ecl_bignum(x);
  sbig_num_t ynum = ecl_bignum(y);
  ecl_bignum(z) = _int_add_int(xnum, ynum);
}

void
_ecl_big_add_ui(cl_object z, cl_object x, unsigned long int ynum)
{
  /* KLUDGE ecl_parse_integer calls this function with z=reg0. */
  cl_object y = _ecl_big_register1();
  _ecl_big_set_ui(y, ynum);
  _ecl_big_add(z, x, y);
}

cl_object
_ecl_big_plus_big(cl_object x, cl_object y)
{
  cl_object z = ecl_alloc_object(t_bignum);
  _ecl_big_add(z, x, y);
  return big_normalize(z);
}

cl_object
_ecl_big_plus_fix(cl_object x, cl_fixnum ynum)
{
  cl_object y = _ecl_big_register0();
  _ecl_big_set_fix(y, ynum);
  return _ecl_big_plus_big(x, y);
}

/* Subtraction */

static sbig_num_t
_int_sub_int(sbig_num_t xnum, sbig_num_t ynum)
{
  if ((xnum > 0 && ynum < 0 && xnum > MOST_POSITIVE_BIGNUM + ynum) ||
      (xnum < 0 && ynum > 0 && xnum < MOST_NEGATIVE_BIGNUM + ynum))
    storage_exhausted();
  return xnum-ynum;
}

static void
_ecl_big_sub(cl_object z, cl_object x, cl_object y)
{
  sbig_num_t xnum = ecl_bignum(x);
  sbig_num_t ynum = ecl_bignum(y);
  ecl_bignum(z) = _int_sub_int(xnum, ynum);
}

cl_object
_ecl_big_minus_big(cl_object x, cl_object y)
{
  cl_object z = ecl_alloc_object(t_bignum);
  _ecl_big_sub(z, x, y);
  return big_normalize(z);
}

cl_object
_ecl_fix_minus_big(cl_fixnum xnum, cl_object y)
{
  cl_object x = _ecl_big_register0();
  _ecl_big_set_fix(x, xnum);
  return _ecl_big_minus_big(x, y);
}

/* Modulo
   INV the second argument is never 0 (ensured by callers)

   TRUNCATE truncates quotient towards 0
   CEILING  truncates quotient towards +infinity
   FLOOR    truncates quotient towards -infinity

   quotient * divisor + remainder = number
*/

static void
_big_div_qr(cl_object q, cl_object r, cl_object x, cl_object y)
{
  sbig_num_t xnum = ecl_bignum(x);
  sbig_num_t ynum = ecl_bignum(y);
  /* For a minimal signed int value num%-1 is UB. */
  if (xnum == MOST_NEGATIVE_BIGNUM && ynum==-1)
    storage_exhausted();
  ecl_bignum(q) = xnum / ynum;  /* quotient */
  ecl_bignum(r) = xnum % ynum;  /* remainder */
}

static cl_object
_ecl_big_truncate(cl_object x, cl_object y, cl_object *pr)
{
  cl_object q = _ecl_big_register0();
  cl_object r = _ecl_big_register1();
  _big_div_qr(q, r, x, y);
  *pr = _ecl_big_register_normalize(r);
  return _ecl_big_register_normalize(q);
}

cl_object
_ecl_big_ceiling(cl_object x, cl_object y, cl_object *pr)
{
  cl_object q = _ecl_big_register0();
  cl_object r = _ecl_big_register1();
  _big_div_qr(q, r, x, y);
  /* adjust -> +inf */
  {
    sbig_num_t rnum = ecl_bignum(r);
    int8_t xsig = (ecl_bignum(x)<0) ? -1 : 0;
    int8_t ysig = (ecl_bignum(y)<0) ? -1 : 0;
    if (rnum != 0 && (xsig == ysig)) {
      cl_object h = _ecl_big_register2();
      _ecl_big_set_ui(h, 1);
      _ecl_big_add(q, q, h);
      _ecl_big_sub(r, r, y);
    }
  }
  *pr = _ecl_big_register_normalize(r);
  return _ecl_big_register_normalize(q);
}

/* truncate towards -infinity */
cl_object
_ecl_big_floor(cl_object x, cl_object y, cl_object *pr)
{
  cl_object q = _ecl_big_register0();
  cl_object r = _ecl_big_register1();
  _big_div_qr(q, r, x, y);
  /* adjust -> -inf */
  {
    sbig_num_t rnum = ecl_bignum(r);
    int8_t xsig = (ecl_bignum(x)<0) ? -1 : 0;
    int8_t ysig = (ecl_bignum(y)<0) ? -1 : 0;
    if (rnum != 0 && (xsig != ysig)) {
      cl_object h = _ecl_big_register2();
      _ecl_big_set_ui(h, 1);
      _ecl_big_sub(q, q, h);
      _ecl_big_add(r, r, y);
    }
  }
  *pr = _ecl_big_register_normalize(r);
  return _ecl_big_register_normalize(q);
}

/* Boole */
static void
mid_bool_clr(cl_object z, cl_object x, cl_object y)
{ ecl_bignum(z) = 0; }

static void
mid_bool_set(cl_object z, cl_object x, cl_object y)
{ ecl_bignum(z) = -1; }

static void
mid_bool_1(cl_object z, cl_object x, cl_object y)
{ ecl_bignum(z) = ecl_bignum(x); }

static void
mid_bool_2(cl_object z, cl_object x, cl_object y)
{ ecl_bignum(z) = ecl_bignum(y); }

static void
mid_bool_c1(cl_object z, cl_object x, cl_object y)
{ ecl_bignum(z) = ~ecl_bignum(x); }

static void
mid_bool_c2(cl_object z, cl_object x, cl_object y)
{ ecl_bignum(z) = ~ecl_bignum(y); }

static void
mid_bool_and(cl_object z, cl_object x, cl_object y)
{ ecl_bignum(z) = ecl_bignum(x) & ecl_bignum(y); }

static void
mid_bool_andc1(cl_object z, cl_object x, cl_object y)
{ ecl_bignum(z) = (~ecl_bignum(x)) & ecl_bignum(y); }

static void
mid_bool_andc2(cl_object z, cl_object x, cl_object y)
{ ecl_bignum(z) = ecl_bignum(x) & (~ecl_bignum(y)); }

static void
mid_bool_nand(cl_object z, cl_object x, cl_object y)
{ ecl_bignum(z) = ~(ecl_bignum(x) & ecl_bignum(y)); }

static void
mid_bool_ior(cl_object z, cl_object x, cl_object y)
{ ecl_bignum(z) = ecl_bignum(x) | ecl_bignum(y); }

static void
mid_bool_orc1(cl_object z, cl_object x, cl_object y)
{ ecl_bignum(z) = (~ecl_bignum(x)) | ecl_bignum(y); }

static void
mid_bool_orc2(cl_object z, cl_object x, cl_object y)
{ ecl_bignum(z) = ecl_bignum(x) | (~ecl_bignum(y)); }

static void
mid_bool_nor(cl_object z, cl_object x, cl_object y)
{ ecl_bignum(z) = ~(ecl_bignum(x) | ecl_bignum(y)); }

static void
mid_bool_xor(cl_object z, cl_object x, cl_object y)
{ ecl_bignum(z) = ecl_bignum(x) ^ ecl_bignum(y); }

static void                     /* xnor */
mid_bool_eqv(cl_object z, cl_object x, cl_object y)
{ ecl_bignum(z) = ~(ecl_bignum(x) ^ ecl_bignum(y)); }

static const _ecl_big_binary_op bignum_operations[16] = {
  mid_bool_clr,                 /* ECL_BOOLCLR */
  mid_bool_and,                 /* ECL_BOOLAND */
  mid_bool_andc2,               /* ECL_BOOLANDC2 */
  mid_bool_1,                   /* ECL_BOOL1 */
  mid_bool_andc1,               /* ECL_BOOLANDC1 */
  mid_bool_2,                   /* ECL_BOOL2 */
  mid_bool_xor,                 /* ECL_BOOLXOR */
  mid_bool_ior,                 /* ECL_BOOLIOR */
  mid_bool_nor,                 /* ECL_BOOLNOR */
  mid_bool_eqv,                 /* ECL_BOOLEQV */
  mid_bool_c2,                  /* ECL_BOOLC2 */
  mid_bool_orc2,                /* ECL_BOOLORC2 */
  mid_bool_c1,                  /* ECL_BOOLC1 */
  mid_bool_orc1,                /* ECL_BOOLORC1 */
  mid_bool_nand,                /* ECL_BOOLNAND */
  mid_bool_set};                /* ECL_BOOLSET */

_ecl_big_binary_op
_ecl_big_boole_operator(int op)
{
  unlikely_if((op < 0) || (op >= 16)) {
    ecl_internal_error("_ecl_big_boole_operator passed an invalid operator");
  }
  return bignum_operations[op];
}

/* Coerce */

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

/* Bit fiddling */

cl_fixnum
_ecl_big_count_bits(cl_object x) {
  cl_fixnum count;
  sbig_num_t i = ecl_bignum(x);
  sbig_num_t j = (i < 0) ? ~i : i;
  for(count=0 ; j ; j >>=1) if (j & 1) count++;
  return count;
}

cl_fixnum
_ecl_big_integer_length(cl_object x) {
  cl_fixnum count;
  sbig_num_t i = ecl_bignum(x);
  sbig_num_t j = (i < 0) ? ~i : i;
  for(count=0 ; j ; j >>=1) count++;
  return count;
}

/* This function returns number of digits (no sign nor \0).  */
cl_fixnum
_ecl_big_sizeinbase(cl_object x, int base)
{
  sbig_num_t num = ecl_bignum(x);
  cl_fixnum result=0;
  do {
    result++;
    num = num / base;
  } while(num);
  return result;
}

/* Printing */

void
_ecl_big_get_str(char *buf, cl_index n, cl_object x, int base)
{
  const char *bb = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  sbig_num_t xnum = ecl_bignum(x);
  ubig_num_t ynum = (xnum<0) ? -(ubig_num_t)xnum : xnum;
  cl_index idx=n;
  if (xnum<0) {
    buf[0]='-';
    idx++;
  }
  buf[idx--] = '\0';
  do {
    buf[idx--] = bb[ynum%base];
    ynum = ynum / base;
  } while(ynum);
}
