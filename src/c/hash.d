/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * hash.d - hashing and hash tables
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/stacks.h>
#include <ecl/xxhash.h>
#include <ecl/bithacks.h>
#include <ecl/htable.h>

typedef uint64_t cl_hashkey;
typedef ecl_xxh3_state cl_hashstate;
typedef ecl_htable *cl_htable;
typedef ecl_hentry *cl_hentry;

#define hash_reset(s) ecl_xxh3_reset(s)
#define hash_digest(s) ecl_xxh3_digest(s)

#define hash_data(s,x,l) ecl_xxh3_update((s),(uint8_t *)(x),(l))
#define hash_only(x,l)   ecl_xxh3_64((uint8_t *)(x),(l))
#define hash_once(x)     ecl_xxh3_64((uint8_t *)&(x),sizeof(x))
#if ECL_FIXNUM_BITS <= 32
# define hash_word(s,x) ecl_xxh3_update32((s),(uint32_t)(x))
#else
# define hash_word(s,x) ecl_xxh3_update64((s),(uint64_t)(x))
#endif

#define hash_to_fixnum(x) \
  ecl_make_fixnum((cl_fixnum)((x) & (uint64_t)MOST_POSITIVE_FIXNUM))

#define SYMBOL_NAME(x) (Null(x)? ECL_NIL_SYMBOL->symbol.name : (x)->symbol.name)


/* Hashing */

static void
_hash_eql(cl_hashstate *state, cl_object x)
{
  switch (ecl_t_of(x)) {
  case t_bignum:
    hash_data(state,
              ECL_BIGNUM_LIMBS(x),
              ECL_BIGNUM_USIZE(x) * sizeof(ecl_limb_t));
    return;
  case t_ratio:
    _hash_eql(state, ecl_ratio_num(x));
    _hash_eql(state, ecl_ratio_den(x));
    return;
  case t_singlefloat:
    hash_data(state, &ecl_single_float(x), sizeof(ecl_single_float(x)));
    return;
  case t_doublefloat:
    hash_data(state, &ecl_double_float(x), sizeof(ecl_double_float(x)));
    return;
  case t_longfloat: {
    /* We coerce to double because long double has extra bits that
     * give rise to different hash key and are not meaningful. */
    struct { double mantissa; int exponent; int sign; } aux;
    aux.mantissa = frexpl(ecl_long_float(x), &aux.exponent);
    aux.sign = (ecl_long_float(x) < 0)? -1: 1;
    hash_data(state, &aux, sizeof(aux));
    return;
  }
  case t_complex:
    _hash_eql(state, ecl_complex_real(x));
    _hash_eql(state, ecl_complex_imag(x));
    return;
#ifdef ECL_COMPLEX_FLOAT
  case t_csfloat:
    hash_data(state, &ecl_csfloat(x), sizeof(ecl_csfloat(x)));
    return;
  case t_cdfloat:
    hash_data(state, &ecl_cdfloat(x), sizeof(ecl_cdfloat(x)));
    return;
  case t_clfloat: {
    /* We coerce to _Complex double because _Complex long double has
     * extra bits that give rise to different hash key and are not
     * meaningful. */
    struct {
      double mantissa1, mantissa2;
      int exponent1, exponent2;
      int sign1, sign2; } aux;
    long double realpart = creall(ecl_clfloat(x));
    long double imagpart = cimagl(ecl_clfloat(x));
    aux.mantissa1 = frexpl(realpart, &aux.exponent1);
    aux.mantissa2 = frexpl(imagpart, &aux.exponent2);
    aux.sign1 = (realpart < 0)? -1: 1;
    aux.sign2 = (imagpart < 0)? -1: 1;
    hash_data(state, &aux, sizeof(aux));
    return;
  }
#endif
  case t_character:
    hash_word(state, x);
    return;
#ifdef ECL_SSE2
  case t_sse_pack:
    hash_data(state, ecl_sse_data(x).b8, 16);
    return;
#endif
  default:
    hash_word(state, x);
    return;
  }
}

#ifdef ECL_UNICODE
static inline void
hash_base_string(cl_hashstate *h, cl_object x)
{
  const cl_index length = x->base_string.fillp;
  const ecl_base_char *self = x->base_string.self;
  ecl_character buffer[256];
  cl_index index = 0, jdx;
  while (index < length) {
    cl_index take = length-index;
    if (take > 256) take=256;
    for (jdx=0; jdx<take; jdx++) {
      buffer[jdx] = self[index+jdx];
    }
    hash_data(h, buffer, take*sizeof(ecl_character));
    index += take;
  }
}

static inline void
hash_full_string(cl_hashstate *h, cl_object x)
{
  const cl_index length = x->string.fillp;
  const ecl_character *self = x->string.self;
  hash_data(h, self, length*sizeof(ecl_character));
}
#endif

static void
_hash_equal(cl_hashstate *h, cl_object x, int depth)
{
  switch (ecl_t_of(x)) {
  case t_list:
    if (Null(x)) {
      hash_word(h, ECL_NIL);
      return;
    }
    if (--depth <= 0) {
      return;
    } else {
      _hash_equal(h, ECL_CONS_CAR(x), depth);
      _hash_equal(h, ECL_CONS_CDR(x), depth);
      return;
    }
#ifdef ECL_UNICODE
  case t_base_string:
    hash_base_string(h, x);
    return;
  case t_string:
    hash_full_string(h, x);
    return;
#else
  case t_base_string:
    hash_data(h, x->base_string.self, x->base_string.fillp * sizeof(ecl_base_char));
    return;
#endif
  case t_pathname:
    _hash_equal(h, x->pathname.directory, 256);
    _hash_equal(h, x->pathname.name, 0);
    _hash_equal(h, x->pathname.type, 0);
    _hash_equal(h, x->pathname.host, 0);
    _hash_equal(h, x->pathname.device, 0);
    _hash_equal(h, x->pathname.version, 0);
    return;
  case t_bitvector:
    /* Notice that we may round out some bits. We must do this
     * because the fill pointer may be set in the middle of a byte.
     * If so, the extra bits _must_ _not_ take part in the hash,
     * because otherwise two bit arrays which are EQUAL might
     * have different hash keys. */
    hash_data(h, x->vector.self.bc, x->vector.fillp / 8);
    return;
  case t_random: {
    cl_object array = x->random.value;
    size_t elt_size = (array->array.elttype == ecl_aet_b32
                       ? sizeof(uint32_t)
                       : sizeof(uint64_t));
    hash_data(h, array->vector.self.b8, array->vector.dim * elt_size);
    return;
  }
#ifdef ECL_SIGNED_ZERO
    /* According to 3.2.4.2.2 Definition of Similarity two numbers are
       "similar" if they are of the same type and represent the same
       mathematical value. -- jd 2019-05-06*/
  case t_singlefloat: {
    float f = ecl_single_float(x);
    if (f == 0.0) f = 0.0;
    hash_data(h, &f, sizeof(f));
    return;
  }
  case t_doublefloat: {
    double f = ecl_double_float(x);
    if (f == 0.0) f = 0.0;
    hash_data(h, &f, sizeof(f));
    return;
  }
  case t_longfloat: {
    /* We coerce to double because long double has extra bits
     * that give rise to different hash key and are not
     * meaningful */
    struct { double mantissa; int exponent; int sign; } aux;
    aux.mantissa = frexpl(ecl_long_float(x), &aux.exponent);
    aux.sign = (ecl_long_float(x) < 0)? -1: 1;
    if (aux.mantissa == 0.0) aux.mantissa = 0.0;
    hash_data(h, &aux, sizeof(aux));
    return;
  }
  case t_complex: {
    _hash_equal(h, ecl_complex_real(x), depth);
    _hash_equal(h, ecl_complex_imag(x), depth);
    return;
  }
# ifdef ECL_COMPLEX_FLOAT
  case t_csfloat: {
    _Complex float f = ecl_csfloat(x);
    if (crealf(f) == 0.0) f = 0.0 + I * cimagf(f);
    if (cimagf(f) == 0.0) f = crealf(f) + I * 0.0;
    hash_data(h, &f, sizeof(f));
    return;
  }
  case t_cdfloat: {
    _Complex double f = ecl_cdfloat(x);
    if (creal(f) == 0.0) f = 0.0 + I * cimag(f);
    if (cimag(f) == 0.0) f = creal(f) + I * 0.0;
    hash_data(h, &f, sizeof(f));
    return;
  }
  case t_clfloat: {
    /* We coerce to _Complex double because _Complex long double has
     * extra bits that give rise to different hash key and are not
     * meaningful. */
    struct {
      double mantissa1, mantissa2;
      int exponent1, exponent2;
      int sign1, sign2; } aux;
    long double realpart = creall(ecl_clfloat(x));
    long double imagpart = cimagl(ecl_clfloat(x));
    aux.mantissa1 = frexpl(realpart, &aux.exponent1);
    aux.mantissa2 = frexpl(imagpart, &aux.exponent2);
    aux.sign1 = (realpart < 0)? -1: 1;
    aux.sign2 = (imagpart < 0)? -1: 1;
    if (aux.mantissa1 == 0.0) aux.mantissa1 = 0.0;
    if (aux.mantissa2 == 0.0) aux.mantissa2 = 0.0;
    hash_data(h, &aux, sizeof(aux));
    return ;
  }
# endif
#endif
  default:
    _hash_eql(h, x);
    return;
  }
}

/* To ensure consistent hashing of numbers under the predicate EQUALP we use a
   canonical dyadic representation in form (SIGN ODD-INTEGER TWO-EXPONENT) and
   simplify from that.

   Canonical rule:

   1. Dyadic rationals are hashed as a tuple (sign, mantissa, exponent)
   2. Other numbers are hashed depending on their type (they are disjoint)

   Implementation rule:

   1. Integer values representable  as FIXNUM         -> (hash fixnum)
   2. Integer values representable  as LONG-FLOAT     -> (hash dyadic)
   3. Integer values exceeding LONG-FLOAT precision   -> (hash bignum)
   4. Dyadic fraction values representable as double  -> (hash double)
   5. Dyadic fraction values representable as lfloat  -> (hash lfloat)
   6. Other numbers are hashed depending on their type (they are disjoint)

   This way we reduce significantly consing required to bring together bignums
   and floats, hashing of and floats is deterministic and typical values, that
   is fixnums and floats, are always on the fast path.

   -- jd 2026-10-15 */

static inline void
equalp_hash_fixnum_data(cl_hashstate *h, cl_object x) {
  hash_word(h, ecl_fixnum(x));
}

#define equalp_hash_fixnum(h,x) equalp_hash_fixnum_data(h,x)

static inline void
equalp_hash_bignum_data(cl_hashstate *h, cl_object x) {
  hash_word(h, ecl_bigsgn(x));
  hash_data(h,
            ECL_BIGNUM_LIMBS(x),
            ECL_BIGNUM_USIZE(x) * sizeof(ecl_limb_t));
}

static void
equalp_hash_integer(cl_hashstate *h, cl_object x)
{
  if (ECL_FIXNUMP(x)) {
    equalp_hash_fixnum_data(h, x);
  } else {
    equalp_hash_bignum_data(h, x);
  }
}

static inline void
equalp_hash_and_free_bignum_register(cl_hashstate *h, cl_object mantissa) {
  /* INV this function assumes that mantissa is already positive. The bignum may
     not be normalized. We free the register after hashing. */
  if (ECL_BIGNUM_SIZE(mantissa) == 1) {
    ecl_limb_t y = ECL_BIGNUM_LIMBS(mantissa)[0];
    if (y <= MOST_POSITIVE_FIXNUM) {
      hash_word(h, y);
      goto DONE;
    }
  }
  equalp_hash_bignum_data(h, mantissa);
 DONE:
  _ecl_big_register_free(mantissa);
}

/* When hashing bignums we see whether they fit in LONG-FLOAT and only then we
   cast it to a dyadic form. Otherwise we directly hash limbs. */
static void
equalp_hash_bignum(cl_hashstate *h, cl_object x) {
  cl_fixnum length = _ecl_big_integer_length(x);
  cl_fixnum exponent = _ecl_big_ctz(x);
  if (length-exponent<=LDBL_MANT_DIG && length<=LDBL_MAX_EXP) {
    /* To avoid allocating fresh bignums by ABS and ASH we opencode this:
       cl_object mantissa = ecl_ash(ecl_abs(x), -exponent); */
    cl_object reg = _ecl_big_register2();
    cl_fixnum signum = ecl_bigsgn(x);
    if (signum<0) {
      _ecl_big_neg(reg, x);
      _ecl_big_div_2exp(reg, reg, exponent);
    }
    else {
      _ecl_big_div_2exp(reg, x, exponent);
    }
    hash_word(h, 8*exponent+signum);
    equalp_hash_and_free_bignum_register(h, reg);
  } else {
    equalp_hash_bignum_data(h, x);
  }
}

/* INV value is guaranteed to be finite and != 0. */
/* These two functions are an example why CL macros are the best. */
#define DECODE_SIGNUM(value,signum) \
  do { if (signbit(value)) { signum=-1; value=-value; } } while(0)

static inline void
equalp_hash_dfloat(cl_hashstate *h, double value)
{
#if DBL_MANT_DIG <= ECL_FIXNUM_BITS
  int ctz, exponent, signum=1;
  cl_index mantissa;
  DECODE_SIGNUM(value, signum);
  value = ldexp(frexp(value, &exponent), DBL_MANT_DIG);
  exponent -= DBL_MANT_DIG;
  mantissa = (cl_index)value;
  ctz = ecl_ctz(mantissa);
  exponent += ctz;
  mantissa >>= ctz;
  /* binary64 mantissa is 53 bits so it is always <= MOST_POSITIVE_FIXNUM, but
     we can imagine an exotic C implementation where double is binary80. */
  if (mantissa <= MOST_POSITIVE_FIXNUM) {
    hash_word(h, 8*exponent+signum);
    hash_word(h, mantissa);
  } else {
    cl_object reg = _ecl_big_register2();
    _ecl_big_set_idx(reg, mantissa);
    hash_word(h, 8*exponent+signum);
    equalp_hash_bignum_data(h, reg);
    _ecl_big_register_free(reg);
  }
#else  /* Happens for binary64 float and 32bit fixnum (i.e i686). */
  const int fixnum_bits = ECL_FIXNUM_BITS-ECL_TAG_BITS-1;
  int ctz=0, exponent, signum=1;
  cl_object mantissa = _ecl_big_register2();
  cl_fixnum helper, iterator;
  DECODE_SIGNUM(value, signum);
  value = frexp(value, &exponent);
  _ecl_big_set_ui(mantissa, 0);
  for(iterator=0; iterator<DBL_MANT_DIG && value; iterator+=fixnum_bits) {
    value = ldexp(value, fixnum_bits);
    helper = (cl_fixnum)value;
    value -= helper;
    _ecl_big_mul_2exp(mantissa, mantissa, fixnum_bits);
    _ecl_big_add_ui(mantissa, mantissa, helper);
    /* When this chunk contains only 0s, then we extend trailing zeros by
       fixnum_bits (chunk's size). Otherwise we start counting again. */
    ctz = !helper ? ctz+fixnum_bits : ecl_ctz(helper);
  }
  /* ctz is a trailing 0 count for the whole mantissa. iterator denotes how many
     bits we have shifted the mantissa. */
  exponent += ctz - iterator;
  _ecl_big_div_2exp(mantissa, mantissa, ctz);
  hash_word(h, 8*exponent+signum);
  equalp_hash_and_free_bignum_register(h, mantissa);
#endif
}

static inline void
equalp_hash_lfloat(cl_hashstate *h, long double value)
{
#if LDBL_MANT_DIG <= ECL_FIXNUM_BITS
  int ctz, exponent, signum=1;
  cl_index mantissa;
  DECODE_SIGNUM(value, signum);
  value = ldexpl(frexpl(value, &exponent), LDBL_MANT_DIG);
  exponent -= LDBL_MANT_DIG;
  mantissa = (cl_index)value;
  ctz = ecl_ctz(mantissa);
  exponent += ctz;
  mantissa >>= ctz;
  /* binary80 mantissa is 64 bits so it can falsify <= MOST_POSITIVE_FIXNUM. */
  if (mantissa <= MOST_POSITIVE_FIXNUM) {
    hash_word(h, 8*exponent+signum);
    hash_word(h, mantissa);
  } else {
    cl_object reg = _ecl_big_register2();
    _ecl_big_set_idx(reg, mantissa);
    hash_word(h, 8*exponent+signum);
    equalp_hash_bignum_data(h, reg);
    _ecl_big_register_free(reg);
  }
#else /* Happens for 128bit floats (i.e aarch64). */
  const int fixnum_bits = ECL_FIXNUM_BITS-ECL_TAG_BITS-1;
  int ctz=0, exponent, signum=1;
  cl_object mantissa = _ecl_big_register2();
  cl_fixnum helper, iterator;
  DECODE_SIGNUM(value, signum);
  value = frexpl(value, &exponent);
  _ecl_big_set_ui(mantissa, 0);
  for(iterator=0; iterator<LDBL_MANT_DIG && value; iterator+=fixnum_bits) {
    value = ldexpl(value, fixnum_bits);
    helper = (cl_fixnum)value;
    value -= helper;
    _ecl_big_mul_2exp(mantissa, mantissa, fixnum_bits);
    _ecl_big_add_ui(mantissa, mantissa, helper);
    /* When this chunk contains only 0s, then we extend trailing zeros by
       fixnum_bits (chunk's size). Otherwise we start counting again. */
    ctz = !helper ? ctz+fixnum_bits : ecl_ctz(helper);
  }
  /* ctz is a trailing 0 count for the whole mantissa. iterator denotes how many
     bits we have shifted the mantissa. */
  exponent += ctz - iterator;
  _ecl_big_div_2exp(mantissa, mantissa, ctz);
  hash_word(h, 8*exponent+signum);
  equalp_hash_and_free_bignum_register(h, mantissa);
#endif
}
#undef DECODE_SIGNUM

#define equalp_hash_single_float(h,v) equalp_hash_double_float((h),(double)(v))

static void
equalp_hash_double_float(cl_hashstate *h, double value) {
  if (isfinite(value) && (value==trunc(value))) {
    const double limit = ldexp(1.0, ECL_FIXNUM_BITS-ECL_TAG_BITS-1);
    if (value < limit && value >= -limit) {
      hash_word(h, (cl_fixnum)value);
      return;
    }
    equalp_hash_dfloat(h, value);
    return;
  }
  /* Fall-through. */
  hash_data(h, &value, sizeof(value));
}

/* We need to ensure, that when long double can be represented as double without
   loss of precision, that is value == (double)value, to hash it as double. */
static void
equalp_hash_long_float(cl_hashstate *h, long double value) {
  /* Hash as double. */
  if (!isfinite(value)
      || (fabsl(value) <= DBL_MAX && value==(double)value) ) {
    equalp_hash_double_float(h, (double)value);
    return;
  }
  /* Hash as fixnum. */
  {
    const long double limit = ldexpl(1.0L, ECL_FIXNUM_BITS-ECL_TAG_BITS-1);
    if ((value==truncl(value)) && (value < limit) && value >= -limit) {
      hash_word(h, (cl_fixnum)value);
      return;
    }
  }
  /* Hash as long double in dyadic form -- this is both for compatibility with
     bignums and to avoid hashing padding bits on x87. */
  equalp_hash_lfloat(h, value);
}

static void
equalp_hash_ratio(cl_hashstate *h, cl_object x) {
  /* When the ratio is dyadic and within long-float precision, then we cast it
     to long double for consistent hashes. Otherwise we hash ratio as is. */
  cl_object num = ecl_ratio_num(x);
  cl_object den = ecl_ratio_den(x);
  /* Ratio is dyadic when denominator is a power of two. To avoid bignum
     arithmetic we provide an optimized path for each subtype of integer.
     ecl_zerop(ecl_boole(ECL_BOOLAND, den, ecl_one_minus(den))); */
  bool dyadic_p;
  if (ECL_FIXNUMP(den)) {
    /* Denominator is always positive, so fixnump(x) -> fixnump(x-1) */
    cl_fixnum dfix = ecl_fixnum(den);
    dyadic_p = !(dfix & (dfix-1));
  } else {
    /* When bignum is a pow2, then integer_length == ctz+1 or (popcount==1) */
    dyadic_p = ((_ecl_big_ctz(den)+1) == _ecl_big_integer_length(den));
  }
  if (dyadic_p) {
    /* Normally we would call here
       cl_fixnum length = ecl_integer_length(ecl_abs(num));

       But! Numerator is normalized to an odd integer because denominator is a
       power of two. INTEGER-LENGTH returns different values for X and -X only
       when X is a power of two, and the only odd power of two is X=1.

       (INTEGER-LENGTH -1) == 0 and 0 <= LDBL_MANT_DIG
       (INTEGER-LENGTH +1) == 1 and 1 <= LDBL_MANT_DIG

       This is why we don't need to call ABS on the numerator. QED. */
    cl_fixnum length = (ECL_FIXNUMP(num)
                        ? ecl_integer_length(num)
                        : _ecl_big_integer_length(num));
    cl_fixnum exponent = -(ecl_integer_length(den)-1);
    if (length <= LDBL_MANT_DIG && exponent >= LDBL_MIN_EXP-LDBL_MANT_DIG) {
      /* Generic way to do this is: hash_long_float(h, ecl_to_long_double(x));
         that path recomputes mantissa and exponent from ratio using a generic
         arithmetic. We already have everything ready. */
      long double value = (ECL_FIXNUMP(num)
                           ? ecl_fixnum(num)
                           : _ecl_big_get_lf(num));
      value = ldexpl(value, exponent);
      equalp_hash_long_float(h, value);
      return;
    }
  }
  /* Fall-through. */
  hash_word(h, t_ratio);
  equalp_hash_integer(h, num);
  equalp_hash_integer(h, den);
}

static void
_hash_equalp(cl_hashstate *h, cl_object x, int depth)
{
  cl_index i, len;
  switch (ecl_t_of(x)) {
  case t_character:
    hash_word(h, ECL_CODE_CHAR(ecl_char_upcase(ECL_CHAR_CODE(x))));
    return;
  case t_list:
    if (Null(x)) {
      hash_word(h, ECL_NIL);
      return;
    }
    if (--depth <= 0) {
      return;
    } else {
      _hash_equalp(h, ECL_CONS_CAR(x), depth);
      _hash_equalp(h, ECL_CONS_CDR(x), depth);
      return;
    }
#ifdef ECL_UNICODE
  case t_string:
#endif
  case t_base_string:
  case t_vector:
  case t_bitvector:
    len = x->vector.fillp;
    goto SCAN;
  case t_array:
    len = x->vector.dim;
  SCAN:
    if (--depth > 0) {
      for (i = 0; i < len; i++) {
        _hash_equalp(h, ecl_aref_unsafe(x, i), depth);
      }
    }
    return;
  case t_fixnum:
    equalp_hash_fixnum(h, x);
    return;
  case t_bignum:
    equalp_hash_bignum(h, x);
    return;
  case t_ratio: {
    equalp_hash_ratio(h, x);
    return;
  }
  case t_singlefloat:
    equalp_hash_single_float(h, ecl_single_float(x));
    return;
  case t_doublefloat:
    equalp_hash_double_float(h, ecl_double_float(x));
    return;
  case t_longfloat:
    equalp_hash_long_float(h, ecl_long_float(x));
    return;
  case t_complex:
    hash_word(h, t_complex);
    _hash_equalp(h, ecl_complex_real(x), 0);
    _hash_equalp(h, ecl_complex_imag(x), 0);
    return;
#ifdef ECL_COMPLEX_FLOAT
  case t_csfloat: {
    _Complex float v = ecl_csfloat(x);
    float real = crealf(v);
    float imag = cimagf(v);
    if (imag == 0) {
      equalp_hash_single_float(h, real);
    } else {
      hash_word(h, t_complex);
      equalp_hash_single_float(h, real);
      equalp_hash_single_float(h, imag);
    }
    return;
  }
  case t_cdfloat: {
    _Complex double v = ecl_cdfloat(x);
    double real = creal(v);
    double imag = cimag(v);
    if (imag == 0) {
      equalp_hash_double_float(h, real);
    } else {
      hash_word(h, t_complex);
      equalp_hash_double_float(h, real);
      equalp_hash_double_float(h, imag);
    }
    return;
  }
  case t_clfloat: {
    _Complex long double v = ecl_clfloat(x);
    long double real = creall(v);
    long double imag = cimagl(v);
    if (imag == 0) {
      equalp_hash_long_float(h, real);
    } else {
      hash_word(h, t_complex);
      equalp_hash_long_float(h, real);
      equalp_hash_long_float(h, imag);
    }
    return;
  }
#endif
  case t_structure:
    /* FIXME! structures are currently implemented as instances.
       INV enum values t_structure === t_instance */
    if (si_structurep(x)) {
      len = ECL_STRUCT_LENGTH(x);
      cl_object *s = ECL_STRUCT_SLOTS(x);
      hash_word(h, ECL_STRUCT_TYPE(x));
      if (--depth > 0) {
        for (i=0; i<len; i++)
          _hash_equalp(h, s[i], depth);
      }
      return;
    }
    hash_word(h, x);
    return;
  case t_hashtable:
    /* We excercise only test here, because the number of entries may change in
       weak hash tables. We also don't want to add individual items, because
       that may get costly fast for EQUALP keys. -- jd 2026-09-23 */
    hash_word(h, x->hash.test);
    return;
  default:
    _hash_equal(h, x, depth);
    return;
  }
}

/* Internal top-level interface to hashing functions. */

static inline cl_hashkey
_ecl_hash_eq(cl_object key) {
  return hash_once(key);
}

static inline cl_hashkey
_ecl_hash_eql(cl_object key) {
  /* Some types that may be hashed in one-shot are optimized here. */
  switch(ecl_t_of(key)) {
  case t_symbol:
  case t_fixnum:
  case t_character:
    return hash_once(key);
  default: {
    cl_hashstate state;
    hash_reset(&state);
    _hash_eql(&state, key);
    return hash_digest(&state);
  }
  }
}

static inline cl_hashkey
_ecl_hash_equal(cl_object key) {
  /* Some types that may be hashed in one-shot are optimized here. */
 AGAIN:
  switch(ecl_t_of(key)) {
#ifdef ECL_UNICODE
  case t_string:
    return hash_only(key->string.self,
                     key->string.fillp * sizeof(ecl_character));
#else
  case t_base_string:
    return hash_only(key->base_string.self,
                     key->base_string.fillp * sizeof(ecl_base_char));
#endif
  case t_symbol:
  case t_fixnum:
  case t_character:
    return hash_once(key);
  default: {
    cl_hashstate state;
    hash_reset(&state);
    _hash_equal(&state, key, 3);
    return hash_digest(&state);
  }
  }
}

static inline cl_hashkey
_ecl_hash_equalp(cl_object key) {
  /* INV don't try to make fixnums faster by hashing whole object with the tag,
     because (EQUALP 1 1.0) is T. -- jd 2026-09-24 */
  cl_hashstate state;
  hash_reset(&state);
  _hash_equalp(&state, key, 3);
  return hash_digest(&state);
}

@(defun si::hash-eql (&rest args)
@
  cl_hashkey h;
  if (narg == 1) {
    h = _ecl_hash_eql(ecl_va_arg(args));
  } else {
    cl_hashstate state;
    hash_reset(&state);
    for (; narg; narg--) {
      cl_object o = ecl_va_arg(args);
      _hash_eql(&state, o);
    }
    h = hash_digest(&state);
  }
  /* We truncate the hash to fit in a fixnum. */
  @(return hash_to_fixnum(h));
@)

@(defun si::hash-equal (&rest args)
@
  cl_hashkey h;
  if (narg == 1) {
    h = _ecl_hash_equal(ecl_va_arg(args));
  } else {
    cl_hashstate state;
    hash_reset(&state);
    for (; narg; narg--) {
      cl_object o = ecl_va_arg(args);
      _hash_equal(&state, o, 3);
    }
    h = hash_digest(&state);
  }
  /* We truncate the hash to fit in a fixnum. */
  @(return hash_to_fixnum(h));
@)

@(defun si::hash-equalp (&rest args)
@
  cl_hashkey h;
  if (narg == 1) {
    h = _ecl_hash_equalp(ecl_va_arg(args));
  } else {
    cl_hashstate state;
    hash_reset(&state);
    for (; narg; narg--) {
      cl_object o = ecl_va_arg(args);
      _hash_equalp(&state, o, 3);
    }
    h = hash_digest(&state);
  }
  /* We truncate the hash to fit in a fixnum. */
  @(return hash_to_fixnum(h));
@)

cl_object
cl_sxhash(cl_object key)
{
  /* SXHASH is specified to respect object similarity. For symbols similarity is
     defined as (equal (symbol-name a) (symbol-name b). That requirement is not
     binding for hash tables and EQUAL for symbols calls EQ. -- jd 2026-11-02 */
  if (ECL_SYMBOLP(key))
    key = SYMBOL_NAME(key);
  /* We truncate the hash to fit in a fixnum. */
  @(return hash_to_fixnum(_ecl_hash_equal(key)));
}


/* New swiss-like hash table (prototype) */

/* Weak hash tables (enum ecl_htweak) description:

   - Entries may disappear without explicit REMHASH
   - Control bytes should be updated by GETHASH etc
   - Updates should reclaim tombstones similar to how REMHASH does
   - Queries for keys and values must be normalized

   Note that new implementation behaves differently regarding how entries are
   reclaimed after garbage collection. */
#ifdef ECL_WEAK_HASH
static inline cl_object
_ecl_hentry_store_key (cl_htable ht, cl_object key) {
  switch (ht->weak) {
  case ecl_htt_weak_key:
  case ecl_htt_weak_key_and_value:
  case ecl_htt_weak_key_or_value:
    return si_make_weak_pointer(key);
  default:
    return key;
  }
}


static inline cl_object
_ecl_hentry_store_val (cl_htable ht, cl_object val) {
  switch (ht->weak) {
  case ecl_htt_weak_value:
  case ecl_htt_weak_key_and_value:
  case ecl_htt_weak_key_or_value:
    return si_make_weak_pointer(val);
  default:
    return val;
  }
}

static void *
normalize_weak_key(cl_hentry e) {
  return (void*)(e->key = e->key->weak.value);
}

static void *
normalize_weak_val(cl_hentry e) {
  return (void*)(e->value = e->value->weak.value);
}

static void *
normalize_weak_kav(cl_hentry e) {
  if ((e->key = e->key->weak.value) && (e->value = e->value->weak.value))
    return (void*)e;
  else
    return 0;
}

static void *
normalize_weak_kov(cl_hentry e) {
  e->key = e->key->weak.value;
  e->value = e->value->weak.value;
  if (e->key || e->value)
    return (void*)e;
  else
    return 0;
}

/* This function normalizes entries. That means that si_weak_pointer_value
   shouldn't be called on resulting entry key and value. -- jd 2019-05-28 */
static inline cl_hentry
_ecl_normalize_hentry(cl_htable ht, cl_index index, cl_hentry output)
{
  cl_hentry entry = &ht->data[index];
  *output = *entry;
  switch (ht->weak) {
  case ecl_htt_weak_key:
    if (GC_call_with_alloc_lock((GC_fn_type)normalize_weak_key, output))
      return output;
    break;
  case ecl_htt_weak_value:
    if (GC_call_with_alloc_lock((GC_fn_type)normalize_weak_val, output))
      return output;
    break;
  case ecl_htt_weak_key_and_value:
    if (GC_call_with_alloc_lock((GC_fn_type)normalize_weak_kav, output))
      return output;
    break;
  case ecl_htt_weak_key_or_value:
    if (GC_call_with_alloc_lock((GC_fn_type)normalize_weak_kov, output))
      return output;
    break;
  case ecl_htt_not_weak:
  default:
    return output;
  }
  /* The entry was garbage collected. */
  entry->key = OBJNULL;
  entry->value = OBJNULL;
  ht->ctrl[index] = ECL_TOMBSTONE;
  ht->fill--;
  return NULL;
}
#else
# define _ecl_hentry_store_key(ht,key) (key)
# define _ecl_hentry_store_val(ht,val) (val)
# define _ecl_normalize_hentry(ht,index,output) (&ht->data[index])
#endif

/* Package Hashtables (ecl_htt_pack) description:

   - Keys supplied to _ecl_*_pack are strings
   - Keys stored in the package are shortened hashes (fixnums)
   - Values stored in the package are symbols
   - Probe test is (and (= shortened-key hkey)
                        (string= key (symbol-name hval)))

   Shortened hashes are used as prefilters and they are fixnums to ensure that
   there is no additional pointer dereference. I hate clever. -- jd 2026-09-29
 */

static inline cl_object
_ecl_hentry_key(cl_htable ht, cl_hentry record)
{
  return (ht->test == ecl_htt_pack)
    ? SYMBOL_NAME(record->value)
    : record->key;
}

static inline void
_ecl_insert_hentry(cl_htable ht, cl_index index, uint64_t hash,
                   cl_object key, cl_object val)
{
  uint8_t fp = ecl_hash_fingerprint(hash);
  if (ht->test == ecl_htt_pack) {
    key = hash_to_fixnum(hash);
  }
  ht->data[index].key   = _ecl_hentry_store_key(ht, key);
  ht->data[index].value = _ecl_hentry_store_val(ht, val);
  ht->ctrl[index] = fp;
}

static inline cl_hashkey
_ecl_hash_key(cl_htable ht, cl_object key) {
  switch (ht->test) {
  case ecl_htt_eq:      return _ecl_hash_eq(key);
  case ecl_htt_eql:     return _ecl_hash_eql(key);
  case ecl_htt_equal:   return _ecl_hash_equal(key);
  case ecl_htt_equalp:  return _ecl_hash_equalp(key);
  case ecl_htt_pack:    return _ecl_hash_equal(key);
  case ecl_htt_generic: {
    cl_object fun = ecl_htable_generic_hash(ht);
    cl_object hash = _ecl_funcall2(fun, key);
    if (!ECL_FIXNUMP(hash)) {
      FEwrong_type_argument(@'fixnum', hash);
    }
    /* Don't trust the user hashes to be properly mixed. -- jd 2026-09-28 */
    return hash_once(hash);
  }
  default:
    ecl_internal_error("hash-table: unknown hash function.");
  }
}

static inline bool
_ecl_hash_test(cl_htable ht, cl_hashkey hash, cl_object key, cl_hentry record)
{
  cl_object hkey = record->key;
  cl_object hval = record->value;
  switch (ht->test) {
  case ecl_htt_eq:     return (key == hkey);
  case ecl_htt_eql:    return ecl_eql(key, hkey);
  case ecl_htt_equal:  return ecl_equal(key, hkey);
  case ecl_htt_equalp: return ecl_equalp(key, hkey);
  case ecl_htt_pack: {
    cl_object sh = hash_to_fixnum(hash);
    return (sh == hkey && ecl_string_eq(key, SYMBOL_NAME(hval)));
  }
  case ecl_htt_generic: {
    cl_object fun = ecl_htable_generic_test(ht);
    return _ecl_funcall3(fun, key, hkey) != ECL_NIL;
  }
  default:
    ecl_internal_error("hash-table: unknown test function.");
  }
}

static inline cl_index
_ecl_htable_limit(cl_htable ht)
{
  uint8_t sizepw = ht->sizepw;
  uint8_t threshold = (ht->rehash>>2)+1;
  /* examples:
     size=2^5= 32, threshold=48, limit=48>>1 = 24
     size=2^6= 64, threshold=48, limit=48<<0 = 48
     size=2^7=128, threshold=48, limit=48<<1 = 96 */
  cl_index limit = (sizepw < 6)
    ? ((cl_index)threshold >> (6-sizepw))
    : ((cl_index)threshold << (sizepw-6));
  return limit;
}

static inline void
_ecl_clear_htable(cl_htable ht)
{
  cl_index size = ecl_htable_size(ht);
  cl_index left = _ecl_htable_limit(ht);
  ht->fill = 0;
  ht->left = left;
  memset(ht->ctrl, ECL_EMPTYSLOT, size);
  memset(ht->data, 0, size*sizeof(ecl_hentry));
}

static cl_htable
_ecl_make_htable(uint8_t sizepw, uint8_t rehash)
{
  /* INV hash tables always have size>0 (unlike gf-cache). */
  cl_htable ht = ecl_alloc(sizeof(ecl_htable));
  cl_index size;
  uint8_t threshold = (rehash>>2)+1;
  if(sizepw<4) sizepw=4;
  else if(sizepw>ECL_HTABLE_MAX_SIZEPW)
    FEerror("make-hash-table: size 2^~a is too big.",
            1, ecl_make_fixnum(sizepw));
  /* Ensure that 2 <= limit <= size. That also implies invariant
     2*size*threshold/64 > size*threshold/64 holds (limit grows). */
  if (threshold<8)      threshold = 8;
  else if(threshold>60) threshold = 60;
  rehash = (uint8_t)((rehash & 3) | ((threshold - 1) << 2));
  ht->test = ecl_htt_eql;
  ht->weak = ecl_htt_not_weak; /* stronk! */
  ht->sizepw = sizepw;
  ht->rehash = rehash;
  size = ecl_htable_size(ht);
  ht->ctrl = ecl_alloc_atomic(size);
  ht->data = ecl_alloc(size*sizeof(ecl_hentry));
  _ecl_clear_htable(ht);
  return ht;
}

static inline cl_object
_ecl_search_htable(cl_htable ht, cl_object key)
{
  cl_index size = ecl_htable_size(ht);
  uint64_t hash = _ecl_hash_key(ht, key);
  uint8_t fp = ecl_hash_fingerprint(hash);
  cl_index index = ecl_hash_group_index(hash, size);
  cl_index idx;
  uint8_t *head = ht->ctrl;
  ecl_hentry record_aux, *record;
  bool emptyp = false;
  /* ignorable */
  (void)record_aux;
  while(1) {
    /* This is the SIMD-optimizable hot path. */
    for(idx=0; idx<16; idx++) {
      uint8_t ctrl = head[index+idx];
      if (ctrl == ECL_EMPTYSLOT) {
        emptyp = true;
        continue;
      }
      if (ctrl & ECL_CONTROLBIT) continue;       /* special */
      if (ctrl != fp) continue;                  /* mismatch */
      record = _ecl_normalize_hentry(ht, index+idx, &record_aux);
      if (record && ecl_likely(_ecl_hash_test(ht, hash, key, record))) {
        return record->value;
      }
    }
    if (ecl_likely(emptyp))
      return OBJNULL;
    index = (index+16)&(size-1);
  }
  /* INV impossible (table is full). */
  ecl_internal_error("hash-table: the hash table is full.");
}

static inline cl_object
_ecl_upsert_htable(cl_htable ht, cl_object key, cl_object val)
{
  cl_index size = ecl_htable_size(ht);
  uint64_t hash = _ecl_hash_key(ht, key);
  uint8_t fp = ecl_hash_fingerprint(hash);
  cl_index index = ecl_hash_group_index(hash, size);
  cl_index idx;
  uint8_t *head = ht->ctrl;
  ecl_hentry record_aux, *record;
  bool freedp = false;
  bool emptyp = false;
  cl_index keep_freed = 0;
  cl_index keep_empty = 0;
  /* ignorable */
  (void)record_aux;
  while(1) {
    /* This is the SIMD-optimizable hot path. */
    for(idx=0; idx<16; idx++) {
      uint8_t ctrl = head[index+idx];
      if (!emptyp && ctrl == ECL_EMPTYSLOT) {
        emptyp = true;
        keep_empty = index+idx;
        continue;
      }
      if (!freedp && ctrl == ECL_TOMBSTONE) {
        freedp = true;
        keep_freed = index+idx;
        continue;
      }
      if (ctrl & ECL_CONTROLBIT) continue;       /* special */
      if (ctrl != fp) continue;                  /* mismatch */
      record = _ecl_normalize_hentry(ht, index+idx, &record_aux);
      if (ecl_likely(record && _ecl_hash_test(ht, hash, key, record))) {
        _ecl_insert_hentry(ht, index+idx, hash, key, val);
        return ECL_T;
      }
    }
    if (ecl_likely(emptyp)) {
      cl_index index2 = (freedp ? keep_freed : keep_empty);
      _ecl_insert_hentry(ht, index2, hash, key, val);
      ht->fill++;
      if (!freedp) {
        /* INV the caller ensures that we have capacity. */
        ht->left--;
      }
      return ECL_NIL;
    }
    index = (index+16)&(size-1);
  }
  /* INV impossible (table is full). */
  ecl_internal_error("hash-table: the hash table is full.");
}

static inline cl_object
_ecl_delete_htable(cl_htable ht, cl_object key)
{
  cl_index size = ecl_htable_size(ht);
  uint64_t hash = _ecl_hash_key(ht, key);
  uint8_t fp = ecl_hash_fingerprint(hash);
  cl_index index = ecl_hash_group_index(hash, size);
  cl_index idx;
  uint8_t *head = ht->ctrl;
  ecl_hentry *tail = ht->data, record_aux, *record;
  bool emptyp = false;
  bool foundp = false;
  cl_index keep_found = 0;
  /* ignorable */
  (void)record_aux;
  while(1) {
    /* This is the SIMD-optimizable hot path. */
    for(idx=0; idx<16; idx++) {
      uint8_t ctrl = head[index+idx];
      if (ctrl == ECL_EMPTYSLOT) {
        emptyp = true;
        continue;
      }
      if (ctrl & ECL_CONTROLBIT) continue;       /* special */
      if (ctrl != fp) continue;                  /* mismatch */
      if (!foundp) {
        record = _ecl_normalize_hentry(ht, index+idx, &record_aux);
        if (ecl_likely(record && _ecl_hash_test(ht, hash, key, record))) {
          foundp = true;
          keep_found = index+idx;
        }
      }
    }
    if (ecl_likely(foundp)) {
      if (ecl_likely(emptyp)) {
        head[keep_found] = ECL_EMPTYSLOT;
        ht->fill--;
        ht->left++;
      } else {
        head[keep_found] = ECL_TOMBSTONE;
        ht->fill--;
      }
      record = &tail[keep_found];
      record->key = OBJNULL;
      record->value = OBJNULL;
      return ECL_T;
    }
    if (ecl_likely(emptyp))
      return ECL_NIL;
    index = (index+16)&(size-1);
  }
  return ECL_NIL;
}

static void
_ecl_insert_htable(cl_htable ht, cl_object key, cl_object val)
{
  cl_index size = ecl_htable_size(ht);
  uint64_t hash = _ecl_hash_key(ht, key);
  cl_index index = ecl_hash_group_index(hash, size);
  cl_index idx;
  uint8_t *head = ht->ctrl;
  while(1) {
    /* This is the SIMD-optimizable hot path. */
    for(idx=0; idx<16; idx++) {
      uint8_t ctrl = head[index+idx];
      /* We test only for the emptyslot, because _ecl_insert_htable is called
         only from the rehash function, so there are no tombstones yet. */
      if (ctrl == ECL_EMPTYSLOT) {
        _ecl_insert_hentry(ht, index+idx, hash, key, val);
        ht->fill++;
        ht->left--;
        return;
      }
    }
    index = (index+16)&(size-1);
  }
  /* INV impossible (table is full). */
  ecl_internal_error("hash-table: the hash table is full.");
}

static cl_htable
_ecl_rehash_htable(cl_htable ht)
{
  cl_index size = ecl_htable_size(ht);
  cl_index limit = _ecl_htable_limit(ht);
  uint8_t sizepw = ht->sizepw;
  uint8_t rehash_pw = ((ht->rehash)&3) + 1;
  /* We grow the table when >=75% is filled (converserly <=25% of space may be
     reclaimed from tombtstones). Otherwise we allocate the table of the same
     size and fill it only with live entries. */
  uint8_t new_sizepw = (ht->fill >= limit - (limit>>2))
    ? (sizepw + rehash_pw)
    : (sizepw);
  ecl_hentry record_aux, *record;
  cl_htable new_table;
  cl_index index = 0;
  cl_index idx, probe;
  uint8_t *head = ht->ctrl;
  (void)record_aux;
  new_table = _ecl_make_htable(new_sizepw, ht->rehash);
  new_table->test = ht->test;
  new_table->weak = ht->weak;
  new_table->slot[0] = ht->slot[0];
  new_table->slot[1] = ht->slot[1];
  new_table->slot[2] = ht->slot[2];
  /* Insert live elements. */
  for(probe=0; probe<size; probe+=16) {
    /* This is the SIMD-optimizable hot path. */
    for(idx=0; idx<16; idx++) {
      uint8_t ctrl = head[index+idx];
      if (ctrl & ECL_CONTROLBIT) continue; /* special */
      record = _ecl_normalize_hentry(ht, index+idx, &record_aux);
      if (record!=NULL) {
        cl_object key = _ecl_hentry_key(ht, record);
        _ecl_insert_htable(new_table, key, record->value);
      }
    }
    index = (index+16)&(size-1);
  }
  return new_table;
}

static void
_ecl_mapc_htable(cl_htable ht, cl_object fun)
{
  cl_index size = ecl_htable_size(ht);
  ecl_hentry record_aux, *record;
  cl_index index = 0;
  cl_index idx, probe;
  uint8_t *head = ht->ctrl;
  (void)record_aux;
  if (!ht->fill) return;
  for(probe=0; probe<size; probe+=16) {
    /* This is the SIMD-optimizable hot path. */
    for(idx=0; idx<16; idx++) {
      uint8_t ctrl = head[index+idx];
      if (ctrl & ECL_CONTROLBIT) continue; /* special */
      record = _ecl_normalize_hentry(ht, index+idx, &record_aux);
      if (record!=NULL) {
        cl_object key = _ecl_hentry_key(ht, record);
        cl_funcall(3, fun, key, record->value);
      }
    }
    index = (index+16)&(size-1);
  }
}

/* Closure iterator over the hash table. */
static cl_object
_ecl_iter_htable(cl_narg narg, ...)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object env = the_env->function->cclosure.env;
  cl_object idx  = ECL_CONS_CAR(env);
  ecl_htable *ht;
  ecl_hentry *record, record_aux;
  cl_index index, size;
  uint8_t *head;
  (void)record_aux;
  if (idx == ECL_NIL) {
    ecl_return1(the_env, ECL_NIL);
  }
  ht = ECL_CONS_CDR(env)->hash.table;
  head = ht->ctrl;
  index = ecl_fixnum(idx);
  size = ecl_htable_size(ht);
  /* We can speed up by searching in 16 braches with SIMD. */
  for (record=NULL; index<size && !record; index++) {
    uint8_t ctrl = head[index];
    if (ctrl & ECL_CONTROLBIT) continue;       /* special */
    record = _ecl_normalize_hentry(ht, index, &record_aux);
  }
  if (record!=NULL) {
    idx = ecl_make_fixnum(index);
    ECL_RPLACA(env, idx);
    ecl_return3(the_env, idx, record->key, record->value);
  }
  ECL_RPLACA(env, ECL_NIL);
  ECL_RPLACD(env, ECL_NIL);
  ecl_return1(the_env, ECL_NIL);
}

static cl_object
_ecl_cons_htable(cl_htable ht)
{
  cl_object output = ECL_NIL;
  cl_index size = ecl_htable_size(ht);
  ecl_hentry record_aux, *record;
  cl_index index;
  uint8_t *head = ht->ctrl;
  /* ignorable */
  (void)record_aux;
  if (!ht->fill) return ECL_NIL;
  /* We can speed up by searching in 16 braches with SIMD. */
  for(index=0; index<size; index++) {
    uint8_t ctrl = head[index];
    if (ctrl & ECL_CONTROLBIT) continue; /* special */
    record = _ecl_normalize_hentry(ht, index, &record_aux);
    if (record!=NULL) {
      cl_object key=record->key;
      cl_object val=record->value;
      output = ecl_cons(ecl_cons(key, val), output);
    }
  }
  return output;
}


/* Hash tables */

static void ECL_INLINE
assert_type_hash_table(cl_object function, cl_narg narg, cl_object p)
{
  unlikely_if (!ECL_HASH_TABLE_P(p))
    FEwrong_type_nth_arg(function, narg, p, @[hash-table]);
}

static inline enum ecl_httest
_ecl_normalize_test(cl_object test)
{
  if (test == @'eq'     || test == ECL_SYM_FUN(@'eq'))     return ecl_htt_eq;
  if (test == @'eql'    || test == ECL_SYM_FUN(@'eql'))    return ecl_htt_eql;
  if (test == @'equal'  || test == ECL_SYM_FUN(@'equal'))  return ecl_htt_equal;
  if (test == @'equalp' || test == ECL_SYM_FUN(@'equalp')) return ecl_htt_equalp;
  /* Extensions and internal interfaces: */
  if (test == @'package')
    return ecl_htt_pack;
  return ecl_htt_generic;
}

static inline enum ecl_htweak
_ecl_normalize_weak(cl_object weak)
{
  if (weak == ECL_NIL)           return ecl_htt_not_weak;
#ifdef ECL_WEAK_HASH
  if (weak == @':key')           return ecl_htt_weak_key;
  if (weak == @':value')         return ecl_htt_weak_value;
  if (weak == @':key-and-value') return ecl_htt_weak_key_and_value;
  if (weak == @':key-or-value')  return ecl_htt_weak_key_or_value;
#endif
  FEwrong_type_key_arg(@[make-hash-table],
                       @[:weakness],
                       cl_list(5, @'member',
                               ECL_NIL, @':key', @':value',
                               @':key-and-value', @':key-or-value'),
                       weak);
}

#ifdef ECL_THREADS
# define _ecl_normalize_sync(sync) (Null(sync) ? ECL_NIL : ecl_make_rwlock(ECL_NIL))
#else
# define _ecl_normalize_sync(sync) (Null(sync) ? ECL_NIL : ECL_T)
#endif


/*
 * HIGHER LEVEL INTERFACE
 */

cl_object
ecl_gethash(cl_object key, cl_object hashtable)
{
  assert_type_hash_table(@[gethash], 2, hashtable);
  if (hashtable->hash.new_htable_p) {
    cl_htable ht = hashtable->hash.table;
    cl_object output;
#ifdef ECL_THREADS
    if (hashtable->hash.sync_lock != ECL_NIL) {
      cl_object sync_lock = hashtable->hash.sync_lock;
      mp_get_rwlock_read_wait(sync_lock);
      ECL_UNWIND_PROTECT_BEGIN(ecl_process_env()) {
        output = _ecl_search_htable(ht, key);
      } ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT {
        mp_giveup_rwlock_read(sync_lock);
      } ECL_UNWIND_PROTECT_THREAD_SAFE_END;
    } else
#endif
      output = _ecl_search_htable(ht, key);
    return output;
  }
  ecl_internal_error("hash-table [get]: unexpected path.");
}

cl_object
ecl_gethash_safe(cl_object key, cl_object hashtable, cl_object def)
{
  cl_object result = ecl_gethash(key, hashtable);;
  return (result==OBJNULL) ? def : result;
}

static inline cl_object
_ecl_puthash(cl_object hashtable, cl_object key, cl_object value) {
  cl_htable ht = hashtable->hash.table;
  if (!ht->left) {
    ht = _ecl_rehash_htable(ht);
    hashtable->hash.table = ht;
  }
  _ecl_upsert_htable(ht, key, value);
  return hashtable;
}

cl_object
_ecl_sethash(cl_object key, cl_object hashtable, cl_object value)
{
  if (hashtable->hash.new_htable_p) {
    cl_object output;
#ifdef ECL_THREADS
    if (hashtable->hash.sync_lock != ECL_NIL) {
      cl_object sync_lock = hashtable->hash.sync_lock;
      mp_get_rwlock_write_wait(sync_lock);
      ECL_UNWIND_PROTECT_BEGIN(ecl_process_env()) {
        output = _ecl_puthash(hashtable, key, value);
      } ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT {
        mp_giveup_rwlock_write(sync_lock);
      } ECL_UNWIND_PROTECT_THREAD_SAFE_END;
    } else
#endif
      output = _ecl_puthash(hashtable, key, value);
    return output;
  }
  ecl_internal_error("hash-table [set]: unexpected path.");
}

cl_object
ecl_sethash(cl_object key, cl_object hashtable, cl_object value)
{
  assert_type_hash_table(@[si::hash-set], 2, hashtable);
  return _ecl_sethash(key, hashtable, value);
}

cl_object
ecl_remhash(cl_object key, cl_object hashtable)
{
  assert_type_hash_table(@[remhash], 2, hashtable);
  if (hashtable->hash.new_htable_p) {
    cl_htable ht = hashtable->hash.table;
    cl_object output;
#ifdef ECL_THREADS
    if (hashtable->hash.sync_lock != ECL_NIL) {
      cl_object sync_lock = hashtable->hash.sync_lock;
      mp_get_rwlock_write_wait(sync_lock);
      ECL_UNWIND_PROTECT_BEGIN(ecl_process_env()) {
        output = _ecl_delete_htable(ht, key);
      } ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT {
        mp_giveup_rwlock_write(sync_lock);
      } ECL_UNWIND_PROTECT_THREAD_SAFE_END;
    } else
#endif
      output = _ecl_delete_htable(ht, key);
    return output;
  }
  ecl_internal_error("hash-table [rem]: unexpected path.");
}

static inline cl_htable
_ecl_inner_ht(cl_object size, cl_object rehash_size, cl_object rehash_threshold,
              enum ecl_httest httest, enum ecl_htweak htweak)
{
  ecl_htable *table;
  int sizepw=4, rehash, rehash_pw=0, threshold;
  float threshold_float = ecl_to_float(rehash_threshold);
  while ((sizepw<ECL_HTABLE_MAX_SIZEPW) &&
         (((cl_index)1<<sizepw) < ecl_fixnum(size))) {
    sizepw++;
  }
  /* When rehash_size is a fixnum, we fix it to x2. */
  if (floatp(rehash_size)) {
    float rsize = ecl_to_float(rehash_size);
    while ((rehash_pw<3) && (2<<rehash_pw < rsize)) {
      rehash_pw++;
    }
  }
  if (threshold_float<0.0) threshold_float=0.0;
  else if (threshold_float>1.0) threshold_float=1.0;
  threshold = (uint8_t)(threshold_float*64.0);
  rehash = rehash_pw | ((threshold?threshold-1:0)<<2);
  table = _ecl_make_htable(sizepw, rehash);
  table->test = httest;
  table->weak = htweak;
  return table;
}

static inline void
_ecl_assert_ht_size(cl_object size) {
  if (ecl_likely(ECL_FIXNUMP(size)
                 && ecl_fixnum(size) >= 0
                 && ecl_fixnum(size) <= ECL_ARRAY_TOTAL_LIMIT))
    return;
  FEwrong_type_key_arg(@[make-hash-table], @[:size], size,
                       ecl_make_integer_type(ecl_make_fixnum(0),
                                             ecl_make_fixnum(ECL_ARRAY_TOTAL_LIMIT)));
}

static inline void
_ecl_assert_ht_rehash_size(cl_object rehash_size) {
  if (ecl_likely((ecl_plusp(rehash_size)
                  && (ECL_FIXNUMP(rehash_size)
                      || (floatp(rehash_size)
                          && ecl_number_compare(rehash_size, ecl_make_fixnum(1)) > 0)))))
    return;
  FEwrong_type_key_arg(@[make-hash-table], @[:rehash-size], rehash_size,
                       ecl_read_from_cstring("(OR (INTEGER 1 *) (FLOAT (1.0) *))"));
}

static inline void
_ecl_assert_ht_rehash_threshold(cl_object rehash_threshold) {
  if (ecl_likely((ecl_number_compare(rehash_threshold, ecl_make_fixnum(0)) >= 0)
                 && (ecl_number_compare(rehash_threshold, ecl_make_fixnum(1)) <= 0)))
      return;
  FEwrong_type_key_arg(@[make-hash-table], @[:rehash-threshold], rehash_threshold,
                       ecl_read_from_cstring("(REAL 0 1)"));
}

cl_object
ecl_make_hash_table(cl_object test, cl_object hash, cl_object size,
                    cl_object rehash_size, cl_object rehash_threshold,
                    cl_object weakness, cl_object synchronized)
{
  enum ecl_httest httest = _ecl_normalize_test(test);
  enum ecl_htweak htweak = _ecl_normalize_weak(weakness);
  cl_object htsync = _ecl_normalize_sync(synchronized), hashtable;
  cl_object generic_testfn=ECL_NIL, generic_hashfn=ECL_NIL;
  cl_htable ht;
  /* Validate size and rehash parameters. */
  _ecl_assert_ht_size(size);
  _ecl_assert_ht_rehash_size(rehash_size);
  _ecl_assert_ht_rehash_threshold(rehash_threshold);
  if (httest==ecl_htt_generic) {
    /* INV ecl_fdefinition will error on undefined functions. */
    generic_testfn = si_coerce_to_function(test);
    generic_hashfn = si_coerce_to_function(hash);
  }
  /* Build the inner table */
  ht = _ecl_inner_ht(size, rehash_size, rehash_threshold, httest, htweak);
  ecl_htable_generic_test(ht) = generic_testfn;
  ecl_htable_generic_hash(ht) = generic_hashfn;
  /* Read the effective values. */
  size = ecl_make_integer(ecl_htable_size(ht));
  rehash_size = ecl_make_single_float(ecl_htable_rehash_size(ht));
  rehash_threshold = ecl_make_single_float(ecl_htable_rehash_threshold(ht));
  /* Build the hash table */
  hashtable = ecl_alloc_object(t_hashtable);
  hashtable->hash.test = httest;
  hashtable->hash.weak = htweak;
  hashtable->hash.generic_test = generic_testfn;
  hashtable->hash.generic_hash = generic_hashfn;
  hashtable->hash.sync_lock = htsync;
  hashtable->hash.size = ecl_htable_size(ht);
  hashtable->hash.rehash_size = rehash_size;
  hashtable->hash.threshold = rehash_threshold;
  hashtable->hash.factor = ecl_htable_rehash_threshold(ht);
  hashtable->hash.limit = ecl_htable_size(ht)-1;
  hashtable->hash.entries = 0;
  /* Connect tables */
  hashtable->hash.table = ht;
  hashtable->hash.new_htable_p = true;
  /* Let there be light. */
  return hashtable;
}

@(defun make_hash_table (&key (test @'eql')
                         (hash_function ECL_NIL)
                         (weakness ECL_NIL)
                         (synchronized ECL_NIL)
                         (size ecl_make_fixnum(1024))
                         (rehash_size ecl_ct_default_rehash_size)
                         (rehash_threshold ecl_ct_default_rehash_threshold))
@ {
    const cl_env_ptr the_env = ecl_process_env();
    ecl_return1(the_env, ecl_make_hash_table(test, hash_function, size,
                                             rehash_size, rehash_threshold,
                                             weakness, synchronized));
} @)

cl_object
cl__make_hash_table(cl_object test, cl_object size,
                    cl_object rehash_size, cl_object rehash_threshold)
{
  return ecl_make_hash_table(test, ECL_NIL, size,
                             rehash_size, rehash_threshold,
                             ECL_NIL, ECL_NIL);
}

#ifdef ECL_EXTERNALIZABLE
void
ecl_reconstruct_serialized_hashtable(cl_object h) {
}
#endif

cl_object
cl_hash_table_p(cl_object ht)
{
  @(return (ECL_HASH_TABLE_P(ht) ? ECL_T : ECL_NIL));
}

cl_object
si_hash_table_weakness(cl_object ht)
{
  cl_object output = ECL_NIL;
#ifdef ECL_WEAK_HASH
  switch (ht->hash.weak) {
  case ecl_htt_weak_key: output = @':key'; break;
  case ecl_htt_weak_value: output = @':value'; break;
  case ecl_htt_weak_key_and_value: output = @':key-and-value'; break;
  case ecl_htt_weak_key_or_value: output = @':key-or-value'; break;
  case ecl_htt_not_weak: default: output = ECL_NIL; break;
  }
#endif
  @(return output);
}

cl_object
si_hash_table_synchronized_p(cl_object ht)
{

  if (!Null(ht->hash.sync_lock)) {
    return ECL_T;
  }
  return ECL_NIL;
}

@(defun gethash (key ht &optional (no_value ECL_NIL))
  @
  {
    const cl_env_ptr the_env = ecl_process_env();
    cl_object result = ecl_gethash(key, ht);
    if (result == OBJNULL)
      ecl_return2(the_env, no_value, ECL_NIL);
    else
      ecl_return2(the_env, result, ECL_T);
  }
  @)

cl_object
si_hash_set(cl_object key, cl_object ht, cl_object val)
{
  /* INV: ecl_sethash() checks the type of hashtable */
  const cl_env_ptr the_env = ecl_process_env();
  ecl_sethash(key, ht, val);
  ecl_return1(the_env, val);
}


cl_object
cl_remhash(cl_object key, cl_object ht)
{
  /* INV: ecl_remhash() checks the type of hashtable */
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_remhash(key, ht));
}

cl_object
cl_clrhash(cl_object hashtable)
{
  const cl_env_ptr the_env = ecl_process_env();
  assert_type_hash_table(@[clrhash], 1, hashtable);
  if (hashtable->hash.new_htable_p) {
    cl_htable ht = hashtable->hash.table;
    _ecl_clear_htable(ht);
    ecl_return1(the_env, hashtable);
  }
  ecl_internal_error("hash-table [zero]: unexpected path.");
}

cl_object
cl_hash_table_test(cl_object hashtable)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object output;
  assert_type_hash_table(@[hash-table-test], 1, hashtable);
  if (hashtable->hash.new_htable_p) {
    cl_htable ht = hashtable->hash.table;
    switch(ht->test) {
    case ecl_htt_eq:      output = @'eq';     break;
    case ecl_htt_eql:     output = @'eql';    break;
    case ecl_htt_equal:   output = @'equal';  break;
    case ecl_htt_equalp:  output = @'equalp'; break;
    case ecl_htt_pack:    output = @'equal';  break;
    case ecl_htt_generic: output = ecl_htable_generic_test(ht); break;
    default: FEerror("hash-table-test: unknown test.", 0);
    }
  } else {
    ecl_internal_error("hash-table [test]: unexpected path.");
  }
  ecl_return1(the_env, output);
}

cl_object
cl_hash_table_size(cl_object hashtable)
{
  const cl_env_ptr the_env = ecl_process_env();
  assert_type_hash_table(@[hash-table-size], 1, hashtable);
  if (hashtable->hash.new_htable_p) {
    cl_htable ht = hashtable->hash.table;
    cl_index size = ecl_htable_size(ht);
    ecl_return1(the_env, ecl_make_integer(size));
  }
  ecl_internal_error("hash-table [size]: unexpected path.");
}

cl_index
ecl_hash_table_count(cl_object hashtable)
{
  if (hashtable->hash.new_htable_p) {
    /* INV implies ecl_htt_not_weak */
    cl_htable ht = hashtable->hash.table;
    return ht->fill;
  }
  ecl_internal_error("hash-table [fill]: unexpected path.");
}

cl_object
cl_hash_table_count(cl_object hashtable)
{
  assert_type_hash_table(@[hash-table-count], 1, hashtable);
  @(return (ecl_make_integer(ecl_hash_table_count(hashtable))));
}

cl_object
si_hash_table_iterator(cl_object hashtable)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object closure, closure_vars;
  assert_type_hash_table(@[si::hash-table-iterator], 1, hashtable);
  if (hashtable->hash.new_htable_p) {
    closure_vars = ecl_cons(ecl_make_fixnum(0), hashtable);
    closure = ecl_make_cclosure_va(_ecl_iter_htable, closure_vars,
                                   @'si::hash-table-iterator', 0);
    ecl_return1(the_env, closure);
  }
  ecl_internal_error("hash-table [iter]: unexpected path.");
}

cl_object
cl_hash_table_rehash_size(cl_object hashtable)
{
  const cl_env_ptr the_env = ecl_process_env();
  assert_type_hash_table(@[hash-table-rehash-size], 1, hashtable);
  if (hashtable->hash.new_htable_p) {
    cl_htable ht = hashtable->hash.table;
    int rehash_size = ecl_htable_rehash_size(ht);
    ecl_return1(the_env, ecl_make_single_float(rehash_size));
  }
  ecl_return1(the_env, hashtable->hash.rehash_size);
}

cl_object
cl_hash_table_rehash_threshold(cl_object hashtable)
{
  const cl_env_ptr the_env = ecl_process_env();
  assert_type_hash_table(@[hash-table-rehash-threshold], 1, hashtable);
  if (hashtable->hash.new_htable_p) {
    cl_htable ht = hashtable->hash.table;
    float threshold = ecl_htable_rehash_threshold(ht);
    ecl_return1(the_env, ecl_make_single_float(threshold));
  }
  ecl_return1(the_env, hashtable->hash.threshold);
}

cl_object
cl_maphash(cl_object fun, cl_object hashtable)
{
  const cl_env_ptr the_env = ecl_process_env();
  assert_type_hash_table(@[maphash], 2, hashtable);
  if (hashtable->hash.new_htable_p) {
    cl_htable ht = hashtable->hash.table;
    _ecl_mapc_htable(ht, fun);
    ecl_return1(the_env, ECL_NIL);
  }
  ecl_internal_error("hash-table [mapc]: unexpected path.");
}

cl_object
si_hash_table_content(cl_object hashtable)
{
  const cl_env_ptr the_env = ecl_process_env();
  assert_type_hash_table(@[ext::hash-table-content], 1, hashtable);
  if (hashtable->hash.new_htable_p) {
    ecl_htable *ht = hashtable->hash.table;
    ecl_return1(the_env, _ecl_cons_htable(ht));
  }
  ecl_internal_error("hash-table [cons]: unexpected path.");
}

cl_object
si_hash_table_fill(cl_object hashtable, cl_object values)
{
  const cl_env_ptr the_env = ecl_process_env();
  assert_type_hash_table(@[ext::hash-table-fill], 1, hashtable);
  while (!Null(values)) {
    cl_object pair = ecl_car(values);
    cl_object key = ecl_car(pair);
    cl_object value = ECL_CONS_CDR(pair);
    values = ECL_CONS_CDR(values);
    _ecl_sethash(key, hashtable, value);
  }
  ecl_return1(the_env, hashtable);
}

cl_object
si_copy_hash_table(cl_object hashtable)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object test = cl_hash_table_test(hashtable);
  cl_object size = cl_hash_table_size(hashtable);
  cl_object rehash_size = cl_hash_table_rehash_size(hashtable);
  cl_object rehash_threshold = cl_hash_table_rehash_threshold(hashtable);
  cl_object hash = cl__make_hash_table(test, size, rehash_size, rehash_threshold);
  if (hashtable->hash.new_htable_p) {
    ecl_htable *old_table = hashtable->hash.table;
    ecl_htable *new_table = _ecl_make_htable(old_table->sizepw, old_table->rehash);
    cl_index size2 = ecl_htable_size(new_table);
    new_table->test = old_table->test;
    new_table->weak = old_table->weak;
    new_table->fill = old_table->fill;
    new_table->left = old_table->left;
    memcpy(new_table->slot, old_table->slot, 3*sizeof(cl_object));
    memcpy(new_table->ctrl, old_table->ctrl, size2);
    memcpy(new_table->data, old_table->data, size2*sizeof(ecl_hentry));
    /* Insert the inner table */
    hash->hash.table = new_table;
    hash->hash.new_htable_p = true;
    ecl_return1(the_env, hash);
  }
  ecl_internal_error("hash-table [copy]: unexpected path.");
}
