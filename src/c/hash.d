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
#include <ecl/xxhash.h>
#include <ecl/bithacks.h>

typedef uint64_t cl_hashkey;
typedef ecl_xxh3_state cl_hashstate;

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
      _hash_equal(h, ECL_NIL_SYMBOL->symbol.name, depth);
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
  case t_symbol:
    x = x->symbol.name;
    (ecl_t_of(x) == t_base_string)
      ? hash_base_string(h, x)
      : hash_full_string(h, x);
    return;
    /* Bytes may differ strings may be still equal. Hash each character. */
  case t_base_string:
    hash_base_string(h, x);
    return;
  case t_string:
    hash_full_string(h, x);
    return;
#else
  case t_symbol:
    x = x->symbol.name;
    /* fall-through */
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

/* To ensure consistent hashing of numbers under the predicate EQUALP:

   1. Floats that are integers are cast and hashed as integer
   2. Floats that are not integers are cast and hashed as double-float
   3. Ratios that are dyadic are cast and hashed as double-float

   For long floats we must test for integer before casting to double, because
   otherwise bignums that do not fit in a double would have a different hash.

   To ensure that equal ratios and floats have the same hash, we must cast all
   floats to the same representation as is targeted by ratios.

   This approach is a clear improvement. Still it has a few issues:
   - conses bignums for hashing large integer floats
   - dyadic ratios are cast to double even even when they exceed float precision

   -- jd 2026-10-07 */

/* Hashes integer unconditionally. */
static void
hash_integer(cl_hashstate *h, cl_object x)
{
  if (ECL_FIXNUMP(x)) {
    hash_word(h, ecl_fixnum(x));
  } else {
    hash_word(h, ecl_bigsgn(x));
    hash_data(h,
              ECL_BIGNUM_LIMBS(x),
              ECL_BIGNUM_USIZE(x) * sizeof(ecl_limb_t));
  }
}

static inline void
hash_fixnum(cl_hashstate *h, cl_object x) {
  hash_word(h, ecl_fixnum(x));
}

static inline void
hash_bignum(cl_hashstate *h, cl_object x) {
  hash_word(h, ecl_bigsgn(x));
  hash_data(h,
            ECL_BIGNUM_LIMBS(x),
            ECL_BIGNUM_USIZE(x) * sizeof(ecl_limb_t));
}

static inline void
hash_double_float(cl_hashstate *h, double value) {
  if (isfinite(value) && (value==trunc(value))) {
    cl_object x = _ecl_double_to_integer(value);
    hash_integer(h, x);
  } else {
    hash_data(h, &value, sizeof(value));
  }
}

static inline void
hash_long_float(cl_hashstate *h, long double value) {
  if (isfinite(value) && (value==truncl(value))) {
    cl_object x = _ecl_long_double_to_integer(value);
    hash_integer(h, x);
  } else {
    /* A cast value may be an integer(!). */
    hash_double_float(h, (double)value);
  }
}

static inline void
hash_single_float(cl_hashstate *h, float value) {
  hash_double_float(h, (double)value);
}

static inline void
hash_ratio(cl_hashstate *h, cl_object x) {
  /* When the ratio is dyadic, then we always cast it to double to ensure the
     same hash value. Otherwise we hash separately each part. */
  cl_object num = ecl_ratio_num(x);
  cl_object den = ecl_ratio_den(x);
  cl_object test = ecl_boole(ECL_BOOLAND, den, ecl_one_minus(den));
  if (ecl_zerop(test)) {
    double value = ecl_to_double(x);
    hash_double_float(h, value);
  } else {
    hash_word(h, t_ratio);
    hash_integer(h, num);
    hash_integer(h, den);
  }
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
      _hash_equalp(h, ECL_NIL_SYMBOL->symbol.name, depth);
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
    hash_fixnum(h, x);
    return;
  case t_bignum:
    hash_bignum(h, x);
    return;
  case t_ratio: {
    hash_ratio(h, x);
    return;
  }
  case t_singlefloat:
    hash_single_float(h, ecl_single_float(x));
    return;
  case t_doublefloat:
    hash_double_float(h, ecl_double_float(x));
    return;
  case t_longfloat:
    hash_long_float(h, ecl_long_float(x));
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
      hash_single_float(h, real);
    } else {
      hash_word(h, t_complex);
      hash_single_float(h, real);
      hash_single_float(h, imag);
    }
    return;
  }
  case t_cdfloat: {
    _Complex double v = ecl_cdfloat(x);
    double real = creal(v);
    double imag = cimag(v);
    if (imag == 0) {
      hash_double_float(h, real);
    } else {
      hash_word(h, t_complex);
      hash_double_float(h, real);
      hash_double_float(h, imag);
    }
    return;
  }
  case t_clfloat: {
    _Complex long double v = ecl_clfloat(x);
    long double real = creall(v);
    long double imag = cimagl(v);
    if (imag == 0) {
      hash_long_float(h, real);
    } else {
      hash_word(h, t_complex);
      hash_long_float(h, real);
      hash_long_float(h, imag);
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

static cl_hashkey
_hash_generic(cl_object ht, cl_object key) {
  cl_object hash_fun = ht->hash.generic_hash;
  cl_object h_object = _ecl_funcall2(hash_fun, key);
  if (!ECL_FIXNUMP(h_object) || ecl_fixnum_minusp(h_object)) {
    FEwrong_type_argument(@'fixnum', h_object);
  }
  return ecl_fixnum(h_object);
}

/* Internal top-level interface to hashing functions. */

static inline uint64_t
_ecl_hash_eq(cl_object key) {
  return hash_once(key);
}

static inline uint64_t
_ecl_hash_eql(cl_object key) {
  /* Some types that may be hashed in one-shot are optimized here. */
  switch(ecl_t_of(key)) {
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

static inline uint64_t
_ecl_hash_equal(cl_object key) {
  /* Some types that may be hashed in one-shot are optimized here. */
 AGAIN:
  switch(ecl_t_of(key)) {
  case t_symbol:
    key = key->symbol.name;
    goto AGAIN;
#ifdef ECL_UNICODE
  case t_string:
    return hash_only(key->string.self,
                     key->string.fillp * sizeof(ecl_character));
#else
  case t_base_string:
    return hash_only(key->base_string.self,
                     key->base_string.fillp * sizeof(ecl_base_char));
#endif
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

static inline uint64_t
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
  uint64_t h;
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
  uint64_t h;
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
  uint64_t h;
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
  cl_index output = _ecl_hash_equal(key);
  /* We truncate the hash to fit in a fixnum. */
  @(return hash_to_fixnum(output));
}


/* Hash tables */

#define SYMBOL_NAME(x) (Null(x)? ECL_NIL_SYMBOL->symbol.name : (x)->symbol.name)

static void ECL_INLINE
assert_type_hash_table(cl_object function, cl_narg narg, cl_object p)
{
  unlikely_if (!ECL_HASH_TABLE_P(p))
    FEwrong_type_nth_arg(function, narg, p, @[hash-table]);
}

#define HASH_TABLE_LOOP(hkey,hval,h,HASH_TABLE_LOOP_TEST) {             \
    cl_index i, hsize = hashtable->hash.size;                           \
    /* INV there is at least one empty bucket (loop terminates). */     \
    for (i = h % hsize; ; i = (i + 1) % hsize) {                        \
      struct ecl_hashtable_entry *e = hashtable->hash.data + i;         \
      cl_object hkey = e->key, hval = e->value;                         \
      (void)hval; /* silence unused-variable compiler warning. */       \
      if (hkey == OBJNULL || (HASH_TABLE_LOOP_TEST)) return e;          \
    }                                                                   \
  }

#define HASH_TABLE_SET(h,loop,compute_key,store_key,store_val) {        \
    cl_hashkey h = compute_key;                                         \
    struct ecl_hashtable_entry *e;                                      \
  AGAIN:                                                                \
    e = loop(h, key, hashtable);                                        \
    if (e->key == OBJNULL) {                                            \
      cl_index i = hashtable->hash.entries + 1;                         \
      if (i >= hashtable->hash.limit) {                                 \
        hashtable = ecl_extend_hashtable(hashtable);                    \
        goto AGAIN;                                                     \
      }                                                                 \
      hashtable->hash.entries = i;                                      \
      e->key = store_key;                                               \
    }                                                                   \
    e->value = store_val;                                               \
    return hashtable;                                                   \
  }

/* HASH_TABLE_REMOVE tries to fills up holes generated by deleting
 * entries from a hashtable as follows. Iterate through all entries f
 * to the right of the deleted entry e (the hole). If the distance
 * between f's current and its optimal location is greater than the
 * distance between e and f, then we can put f into the hole. Repeat
 * with the new hole at the location of f until the holes are all
 * filled. */

#define HASH_TABLE_REMOVE(hkey,hval,h,HASH_TABLE_LOOP_TEST,compute_key) { \
    cl_index i, hsize = hashtable->hash.size;                           \
    /* INV there is at least one empty bucket (loop terminates). */     \
    for (i = h % hsize; ; i = (i + 1) % hsize) {                        \
      struct ecl_hashtable_entry *e = hashtable->hash.data + i;         \
      cl_object hkey = e->key, hval = e->value;                         \
      (void)hval; /* silence unused-variable compiler warning */        \
      if (hkey == OBJNULL) return 0;                                    \
      if (HASH_TABLE_LOOP_TEST) {                                       \
        cl_index j = (i+1) % hsize, k;                                  \
        for (k = 1; k <= hsize; j = (j+1) % hsize, k++) {               \
          struct ecl_hashtable_entry *f = hashtable->hash.data + j;     \
          hkey = f->key;                                                \
          hval = f->value;                                              \
          if (hkey == OBJNULL) {                                        \
            e->key = OBJNULL;                                           \
            e->value = OBJNULL;                                         \
            break;                                                      \
          }                                                             \
          cl_hashkey hf = compute_key;                                  \
          cl_index m = hf % hsize;                                      \
          /* d: distance of f from the optimal position */              \
          cl_index d = (j >= m) ? (j - m) : (j + hsize - m);            \
          if (k <= d) {                                                 \
            e->key = hkey;                                              \
            e->value = hval;                                            \
            e = f;                                                      \
            i = j;                                                      \
            k = 0;                                                      \
          }                                                             \
        }                                                               \
        hashtable->hash.entries--;                                      \
        return 1;                                                       \
      }                                                                 \
    }                                                                   \
  }

/*
 * EQ HASHTABLES
 */

#define _hash_eq(k) ((cl_hashkey)(k) >> 2)

static struct ecl_hashtable_entry *
_ecl_hash_loop_eq(cl_hashkey h, cl_object key, cl_object hashtable)
{
  HASH_TABLE_LOOP(hkey, hval, h, key == hkey);
}

static cl_object
_ecl_gethash_eq(cl_object key, cl_object hashtable, cl_object def)
{
  cl_hashkey h = _hash_eq(key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_eq(h, key, hashtable);
  return (e->key == OBJNULL)? def : e->value;
}

static cl_object
_ecl_sethash_eq(cl_object key, cl_object hashtable, cl_object value)
{
  HASH_TABLE_SET(h, _ecl_hash_loop_eq, _hash_eq(key), key, value);
}

static bool
_ecl_remhash_eq(cl_object key, cl_object hashtable)
{
  HASH_TABLE_REMOVE(hkey, hval, _hash_eq(key), key == hkey, _hash_eq(hkey));
}

/*
 * EQL HASHTABLES
 */

static struct ecl_hashtable_entry *
_ecl_hash_loop_eql(cl_hashkey h, cl_object key, cl_object hashtable)
{
  HASH_TABLE_LOOP(hkey, hval, h, ecl_eql(key, hkey));
}

static cl_object
_ecl_gethash_eql(cl_object key, cl_object hashtable, cl_object def)
{
  uint64_t hash = _ecl_hash_eql(key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_eql(hash, key, hashtable);
  return (e->key == OBJNULL)? def : e->value;
}

static cl_object
_ecl_sethash_eql(cl_object key, cl_object hashtable, cl_object value)
{
  HASH_TABLE_SET(h, _ecl_hash_loop_eql, _ecl_hash_eql(key), key, value);
}

static bool
_ecl_remhash_eql(cl_object key, cl_object hashtable)
{
  HASH_TABLE_REMOVE(hkey, hval, _ecl_hash_eql(key), ecl_eql(key, hkey),
                    _ecl_hash_eql(hkey));
}

/*
 * EQUAL HASHTABLES
 */

static struct ecl_hashtable_entry *
_ecl_hash_loop_equal(cl_hashkey h, cl_object key, cl_object hashtable)
{
  HASH_TABLE_LOOP(hkey, hval, h, ecl_equal(key, hkey));
}

static cl_object
_ecl_gethash_equal(cl_object key, cl_object hashtable, cl_object def)
{
  cl_hashkey h = _ecl_hash_equal(key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_equal(h, key, hashtable);
  return (e->key == OBJNULL)? def : e->value;
}

static cl_object
_ecl_sethash_equal(cl_object key, cl_object hashtable, cl_object value)
{
  HASH_TABLE_SET(h, _ecl_hash_loop_equal, _ecl_hash_equal(key), key, value);
}

static bool
_ecl_remhash_equal(cl_object key, cl_object hashtable)
{
  HASH_TABLE_REMOVE(hkey, hval, _ecl_hash_equal(key),
                    ecl_equal(key, hkey), _ecl_hash_equal(hkey));
}

/*
 * EQUALP HASHTABLES
 */

static struct ecl_hashtable_entry *
_ecl_hash_loop_equalp(cl_hashkey h, cl_object key, cl_object hashtable)
{
  HASH_TABLE_LOOP(hkey, hval, h, ecl_equalp(key, hkey));
}

static cl_object
_ecl_gethash_equalp(cl_object key, cl_object hashtable, cl_object def)
{
  cl_hashkey h = _ecl_hash_equalp(key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_equalp(h, key, hashtable);
  return (e->key == OBJNULL)? def : e->value;
}

static cl_object
_ecl_sethash_equalp(cl_object key, cl_object hashtable, cl_object value)
{
  HASH_TABLE_SET(h, _ecl_hash_loop_equalp, _ecl_hash_equalp(key), key, value);
}

static bool
_ecl_remhash_equalp(cl_object key, cl_object hashtable)
{
  HASH_TABLE_REMOVE(hkey, hval, _ecl_hash_equalp(key),
                    ecl_equalp(key, hkey), _ecl_hash_equalp(hkey));
}

/*
 * PACKAGE HASHTABLES
 */

static struct ecl_hashtable_entry *
_ecl_hash_loop_pack(cl_hashkey h, cl_object key, cl_object hashtable)
{
  cl_object ho = ecl_make_fixnum(h & 0xFFFFFFF);
  HASH_TABLE_LOOP(hkey, hval, h,
                  (ho==hkey) && ecl_string_eq(key,SYMBOL_NAME(hval)));
}

static cl_object
_ecl_gethash_pack(cl_object key, cl_object hashtable, cl_object def)
{
  cl_hashkey h = _ecl_hash_equal(key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_pack(h, key, hashtable);
  return (e->key == OBJNULL)? def : e->value;
}

static cl_object
_ecl_sethash_pack(cl_object key, cl_object hashtable, cl_object value)
{
  HASH_TABLE_SET(h, _ecl_hash_loop_pack, _ecl_hash_equal(key),
                 ecl_make_fixnum(h & 0xFFFFFFF), value);
}

static bool
_ecl_remhash_pack(cl_object key, cl_object hashtable)
{
  cl_hashkey h = _ecl_hash_equal(key);
  cl_object ho = ecl_make_fixnum(h & 0xFFFFFFF);
  HASH_TABLE_REMOVE(hkey, hval, h,
                    (ho==hkey) && ecl_string_eq(key,SYMBOL_NAME(hval)),
                    _ecl_hash_equal(SYMBOL_NAME(hval)));
}

/*
 * Generic HASHTABLES
 */

static bool
_ecl_generic_hash_test(cl_object fun, cl_object key, cl_object hkey) {
  return (_ecl_funcall3(fun, key, hkey) != ECL_NIL);
}

static struct ecl_hashtable_entry *
_ecl_hash_loop_generic(cl_hashkey h, cl_object key, cl_object hashtable)
{
  cl_object test_fun = hashtable->hash.generic_test;
  HASH_TABLE_LOOP(hkey, hval, h, _ecl_generic_hash_test(test_fun, key, hkey));
}

static cl_object
_ecl_gethash_generic(cl_object key, cl_object hashtable, cl_object def)
{
  cl_hashkey h = _hash_generic(hashtable, key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_generic(h, key, hashtable);
  return (e->key == OBJNULL)? def : e->value;
}

static cl_object
_ecl_sethash_generic(cl_object key, cl_object hashtable, cl_object value)
{
  HASH_TABLE_SET(h, _ecl_hash_loop_generic, _hash_generic(hashtable, key),
                 key, value);
}

static bool
_ecl_remhash_generic(cl_object key, cl_object hashtable)
{
  cl_object test_fun = hashtable->hash.generic_test;
  HASH_TABLE_REMOVE(hkey, hval, _hash_generic(hashtable, key),
                    _ecl_generic_hash_test(test_fun, key, hkey),
                    _hash_generic(hashtable, hkey));
}

/*
 * WEAK HASH TABLES
 *
 * Entries in a weak hash table may disappear without explicit REMHASH. Our
 * implementation handles collisions with open addressing, that is we put the
 * colliding element at the first free spot starting from its hash.
 *
 * Until recently we've implemented REMHASH by inserting "tombstones", that is
 * by simply invalidating the entry. Currently after removing the hash we fill
 * the hole by shifting elements after it to left and that yields some benefits
 * for scenarios where we frequently add and remove elements.
 *
 * Since weak entries may disappear at any time, we need to either fill holes in
 * GETHASH/SETHASH too, or we need to revert back to inserting "tombstones".
 * Notice that some functions are common to hash tables with and without weak
 * entries - for example MAPHASH. These functions assume that entry indexes do
 * not change while iterating, so we can't shift values in copy_entry unless we
 * customize these functions too.
 *
 * For reasons above weak entries are not normalized to OBJNULL but rather we
 * leave the weak entry in the same place as a tombstone. SETHASH reuses these
 * entries while REMHASH behaves the same for all hash tables.
 *
 * [key=OBJNULL, value=OBJNULL] - free bucket
 * [key=ECL_NIL, value=OBJNULL] - tombstone
 * [key=OBJNULL, value=ECL_NIL] - tombstone copy
 *
 */

#ifndef ECL_WEAK_HASH
#define copy_entry(e,h) *(e)
#endif

#ifdef ECL_WEAK_HASH
static cl_hashkey
_ecl_hash_key(cl_object h, cl_object o) {
  switch (h->hash.test) {
  case ecl_htt_eq:      return _ecl_hash_eq(o);
  case ecl_htt_eql:     return _ecl_hash_eql(o);
  case ecl_htt_equal:   return _ecl_hash_equal(o);
  case ecl_htt_equalp:  return _ecl_hash_equalp(o);
  case ecl_htt_pack:    return _ecl_hash_equal(o);
  case ecl_htt_generic: return _hash_generic(h, o);
  default:
    ecl_internal_error("Unknown hash test.");
  }
}

static bool
_ecl_hash_test(cl_object hashtable, cl_object key, cl_object hkey) {
  switch (hashtable->hash.test) {
  case ecl_htt_eq:     return (key == hkey);
  case ecl_htt_eql:    return ecl_eql(key, hkey);
  case ecl_htt_equal:  return ecl_equal(key, hkey);
  case ecl_htt_equalp: return ecl_equalp(key, hkey);
  case ecl_htt_generic:
    return _ecl_generic_hash_test(hashtable->hash.generic_test, key, hkey);
  default:
    ecl_internal_error("Unknown hash test.");
  }
}

static void *
normalize_weak_key_entry(struct ecl_hashtable_entry *e) {
  return (void*)(e->key = e->key->weak.value);
}

static void *
normalize_weak_value_entry(struct ecl_hashtable_entry *e) {
  return (void*)(e->value = e->value->weak.value);
}

static void *
normalize_weak_key_and_value_entry(struct ecl_hashtable_entry *e) {
  if ((e->key = e->key->weak.value) && (e->value = e->value->weak.value))
    return (void*)e;
  else
    return 0;
}

static void *
normalize_weak_key_or_value_entry(struct ecl_hashtable_entry *e) {
  e->key = e->key->weak.value;
  e->value = e->value->weak.value;
  if (e->key || e->value)
    return (void*)e;
  else
    return 0;
}

static cl_object
_ecl_store_key (cl_object hashtable, cl_object key) {
  switch (hashtable->hash.weak) {
  case ecl_htt_weak_key:
  case ecl_htt_weak_key_and_value:
  case ecl_htt_weak_key_or_value:
    return si_make_weak_pointer(key);
  default:
    return key;
  }
}


static cl_object
_ecl_store_val (cl_object hashtable, cl_object val) {
  switch (hashtable->hash.weak) {
  case ecl_htt_weak_value:
  case ecl_htt_weak_key_and_value:
  case ecl_htt_weak_key_or_value:
    return si_make_weak_pointer(val);
  default:
    return val;
  }
}

/* This function normalizes entries. That means that si_weak_pointer_value
   shouldn't be called on resulting entry key and value. -- jd 2019-05-28 */
static struct ecl_hashtable_entry
copy_entry(struct ecl_hashtable_entry *e, cl_object h)
{
  if (e->key == OBJNULL) {
    return *e;
  } else if (e->value == OBJNULL) {
    struct ecl_hashtable_entry output = *e;
    output.key = OBJNULL;
    output.value = ECL_NIL;
    return output;
  } else {
    struct ecl_hashtable_entry output = *e;
    switch (h->hash.weak) {
    case ecl_htt_weak_key:
      if (GC_call_with_alloc_lock
          ((GC_fn_type)normalize_weak_key_entry, &output)) {
        return output;
      }
      break;
    case ecl_htt_weak_value:
      if (GC_call_with_alloc_lock
          ((GC_fn_type)normalize_weak_value_entry, &output)) {
        return output;
      }
      break;
    case ecl_htt_weak_key_and_value:
      if (GC_call_with_alloc_lock
          ((GC_fn_type)normalize_weak_key_and_value_entry, &output)) {
        return output;
      }
      break;
    case ecl_htt_weak_key_or_value:
      if (GC_call_with_alloc_lock
          ((GC_fn_type)normalize_weak_key_or_value_entry, &output)) {
        return output;
      }
      break;
    case ecl_htt_not_weak:
    default:
      return output;
    }
    output.key = OBJNULL;
    output.value = ECL_NIL;
    e->key = ECL_NIL;
    e->value = OBJNULL;
    return output;
  }
}

static cl_object
_ecl_gethash_weak(cl_object key, cl_object hashtable, cl_object def)
{
  cl_index i, hsize = hashtable->hash.size;
  cl_hashkey h = _ecl_hash_key(hashtable, key);
  struct ecl_hashtable_entry *p, e;
  for (i = h % hsize; ;  i = (i + 1) % hsize) {
    p = hashtable->hash.data + i;
    e = copy_entry(p, hashtable);
    if (p->key   == OBJNULL) return def;
    if (p->value == OBJNULL) continue; /* skip the tombstone */
    if (_ecl_hash_test(hashtable, key, e.key)) return e.value;
  }
}

static cl_object
_ecl_sethash_weak(cl_object key, cl_object hashtable, cl_object value)
{
  cl_index i, hsize = hashtable->hash.size;
  cl_hashkey h = _ecl_hash_key(hashtable, key);
  struct ecl_hashtable_entry e, *p, *f = NULL;
 AGAIN:
  for (i = h % hsize; ;  i = (i + 1) % hsize) {
    p = hashtable->hash.data + i;
    e = copy_entry(p, hashtable);
    if (p->key   == OBJNULL) { break; }
    if (p->value == OBJNULL) { f = p; continue; }
    if (_ecl_hash_test(hashtable, key, e.key)) {
      f = p;
      break;
    }
  }
  if (p->key == OBJNULL && f == NULL) {
    cl_index i = hashtable->hash.entries + 1;
    if (i >= hashtable->hash.limit) {
      hashtable = ecl_extend_hashtable(hashtable);
      goto AGAIN;
    }
    hashtable->hash.entries = i;
    f = p;
  }
  f->key = _ecl_store_key(hashtable, key);
  f->value = _ecl_store_val(hashtable, value);
  return hashtable;
}

static bool
_ecl_remhash_weak(cl_object key, cl_object hashtable)
{
  cl_index i, hsize = hashtable->hash.size;
  cl_hashkey h = _ecl_hash_key(hashtable, key);
  struct ecl_hashtable_entry *p, e;
  for (i = h % hsize; ;  i = (i + 1) % hsize) {
    p = hashtable->hash.data + i;
    e = copy_entry(p, hashtable);
    if (p->key == OBJNULL) return 0;
    /* We could try to shift consecutive tombstones here(!) */
    if (e.key == OBJNULL) continue;
    if (_ecl_hash_test(hashtable, key, e.key)) {
      cl_index j = (i+1) % hsize, k;
      struct ecl_hashtable_entry *q, f;
      for (k = 1; k <= hsize; j = (j+1) % hsize, k++) {
        q = hashtable->hash.data + j;
        f = copy_entry(q, hashtable);
        if (f.key == OBJNULL) {
          p->key = OBJNULL;
          p->value = OBJNULL;
          break;
        }
        cl_hashkey hf = _ecl_hash_key(hashtable, f.key);
        cl_index m = hf % hsize;
        cl_index d = (j >= m) ? (j - m) : (j + hsize - m);
        if (k <= d) {
          p->key = _ecl_store_key(hashtable, f.key);
          p->value = _ecl_store_val(hashtable, f.value);
          p = q;
          i = j;
          k = 0;
        }
      }
      hashtable->hash.entries--;
      return 1;
    }
  }
}
#endif

/* SYNCHRONIZED HASH TABLES */
#ifdef ECL_THREADS
static cl_object
_ecl_sethash_sync(cl_object key, cl_object hashtable, cl_object value)
{
  cl_object output = ECL_NIL;
  cl_object sync_lock = hashtable->hash.sync_lock;
  mp_get_rwlock_write_wait(sync_lock);
  ECL_UNWIND_PROTECT_BEGIN(ecl_process_env()) {
    output = hashtable->hash.set_unsafe(key, hashtable, value);
  } ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT {
    mp_giveup_rwlock_write(sync_lock);
  } ECL_UNWIND_PROTECT_THREAD_SAFE_END;
  return output;
}

static cl_object
_ecl_gethash_sync(cl_object key, cl_object hashtable, cl_object def)
{
  cl_object output = ECL_NIL;
  cl_object sync_lock = hashtable->hash.sync_lock;
  mp_get_rwlock_read_wait(sync_lock);
  ECL_UNWIND_PROTECT_BEGIN(ecl_process_env()) {
    output = hashtable->hash.get_unsafe(key, hashtable, def);
  } ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT {
    mp_giveup_rwlock_read(sync_lock);
  } ECL_UNWIND_PROTECT_THREAD_SAFE_END;
  return output;
}

static bool
_ecl_remhash_sync(cl_object key, cl_object hashtable)
{
  bool output = 0;
  cl_object sync_lock = hashtable->hash.sync_lock;
  mp_get_rwlock_write_wait(sync_lock);
  ECL_UNWIND_PROTECT_BEGIN(ecl_process_env()) {
    output = hashtable->hash.rem_unsafe(key, hashtable);
  } ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT {
    mp_giveup_rwlock_write(sync_lock);
  } ECL_UNWIND_PROTECT_THREAD_SAFE_END;
  return output;
}
#endif

/*
 * HIGHER LEVEL INTERFACE
 */

cl_object
ecl_gethash(cl_object key, cl_object hashtable)
{
  assert_type_hash_table(@[gethash], 2, hashtable);
  return hashtable->hash.get(key, hashtable, OBJNULL);
}

cl_object
ecl_gethash_safe(cl_object key, cl_object hashtable, cl_object def)
{
  assert_type_hash_table(@[gethash], 2, hashtable);
  return hashtable->hash.get(key, hashtable, def);
}

cl_object
_ecl_sethash(cl_object key, cl_object hashtable, cl_object value)
{
  return hashtable->hash.set(key, hashtable, value);
}

cl_object
ecl_sethash(cl_object key, cl_object hashtable, cl_object value)
{
  assert_type_hash_table(@[si::hash-set], 2, hashtable);
  hashtable = hashtable->hash.set(key, hashtable, value);
  return hashtable;
}

cl_object
ecl_extend_hashtable(cl_object hashtable)
{
  cl_object old, new;
  cl_index old_size, new_size, i;
  cl_object new_size_obj;

  assert_type_hash_table(@[si::hash-set], 2, hashtable);
  old_size = hashtable->hash.size;
  /* We do the computation with lisp datatypes, just in case the sizes contain
   * weird numbers */
  if (ECL_FIXNUMP(hashtable->hash.rehash_size)) {
    new_size_obj = ecl_plus(hashtable->hash.rehash_size,
                            ecl_make_fixnum(old_size));
  } else {
    new_size_obj = ecl_times(hashtable->hash.rehash_size,
                             ecl_make_fixnum(old_size));
    new_size_obj = ecl_ceiling1(new_size_obj);
  }
  if (!ECL_FIXNUMP(new_size_obj)) {
    /* New size is too large */
    new_size = old_size * 2;
  } else {
    new_size = ecl_fixnum(new_size_obj);
  }
  if (hashtable->hash.test == ecl_htt_pack) {
    new = ecl_alloc_object(t_hashtable);
    new->hash = hashtable->hash;
    old = hashtable;
  } else {
    old = ecl_alloc_object(t_hashtable);
    old->hash = hashtable->hash;
    new = hashtable;
  }
  new->hash.data = NULL; /* for GC sake */
  new->hash.entries = 0;
  new->hash.size = new_size;
  new->hash.limit = new->hash.size * new->hash.factor;
  if (new->hash.limit >= new_size) {
    new->hash.limit = new_size - 1;
  }
  new->hash.data = (struct ecl_hashtable_entry *)
    ecl_alloc(new_size * sizeof(struct ecl_hashtable_entry));
  for (i = 0;  i < new_size;  i++) {
    new->hash.data[i].key = OBJNULL;
    new->hash.data[i].value = OBJNULL;
  }
  for (i = 0;  i < old_size;  i++) {
    struct ecl_hashtable_entry e = copy_entry(old->hash.data + i, old);
    if (e.key != OBJNULL) {
      new = (new->hash.test == ecl_htt_pack)
        ? new->hash.set_unsafe(SYMBOL_NAME(e.value), new, e.value)
        : new->hash.set_unsafe(e.key, new, e.value);
    }
  }
  return new;
}

@(defun make_hash_table (&key (test @'eql')
                         (hash_function ECL_NIL)
                         (weakness ECL_NIL)
                         (synchronized ECL_NIL)
                         (size ecl_make_fixnum(1024))
                         (rehash_size ecl_ct_default_rehash_size)
                         (rehash_threshold ecl_ct_default_rehash_threshold))
@ {
    cl_object hash = cl__make_hash_table(test, size, rehash_size, rehash_threshold);
    if (hash->hash.test == ecl_htt_generic) {
      /* Normally we would make hash_function an argument to cl__make_hash_table
         and put this test in there and void unnecessary object allocation, but
         we do not want to compromise the API. -- jd 2019-05-23 */
      if (hash_function == ECL_NIL) {
        FEerror("~S is an illegal hash-table test function.", 1, test);
      }
      hash_function = si_coerce_to_function(hash_function);
      hash->hash.generic_hash = hash_function;
    }
    /* FIXME we should have separate getters and setters for each predicate. */
#ifdef ECL_WEAK_HASH
    if (!Null(weakness)) {
      if (weakness == @':key') {
        hash->hash.weak = ecl_htt_weak_key;
      } else if (weakness == @':value') {
        hash->hash.weak = ecl_htt_weak_value;
      } else if (weakness == @':key-and-value') {
        hash->hash.weak = ecl_htt_weak_key_and_value;
      } else if (weakness == @':key-or-value') {
        hash->hash.weak = ecl_htt_weak_key_or_value;
      } else {
        FEwrong_type_key_arg(@[make-hash-table],
                             @[:weakness],
                             cl_list(5, @'member',
                                     ECL_NIL, @':key', @':value',
                                     @':key-and-value', @':key-or-value'),
                             weakness);
      }
      hash->hash.get = _ecl_gethash_weak;
      hash->hash.set = _ecl_sethash_weak;
      hash->hash.rem = _ecl_remhash_weak;
    }
#endif
    /* Always bind unsafe variants. */
    hash->hash.get_unsafe = hash->hash.get;
    hash->hash.set_unsafe = hash->hash.set;
    hash->hash.rem_unsafe = hash->hash.rem;
    if (!Null(synchronized)) {
#ifdef ECL_THREADS
      hash->hash.sync_lock = ecl_make_rwlock(ECL_NIL);
      hash->hash.get = _ecl_gethash_sync;
      hash->hash.set = _ecl_sethash_sync;
      hash->hash.rem = _ecl_remhash_sync;
#else
      /* for hash-table-synchronized-p predicate */
      hash->hash.sync_lock = ECL_T;
#endif
    }

    @(return hash);
} @)

static void
do_clrhash(cl_object ht)
{
  /*
   * Fill a hash with null pointers and ensure it does not have
   * any entry. We separate this routine because it is needed
   * both by clrhash and hash table initialization.
   */
  cl_index i;
  ht->hash.entries = 0;
  for(i = 0; i < ht->hash.size; i++) {
    ht->hash.data[i].key = OBJNULL;
    ht->hash.data[i].value = OBJNULL;
  }
}

ecl_def_ct_single_float(min_threshold, 0.1, static, const);

cl_object
cl__make_hash_table(cl_object test, cl_object size, cl_object rehash_size,
                    cl_object rehash_threshold)
{
  int htt;
  cl_index hsize;
  cl_object h;
  cl_object hash_test = ECL_NIL, hash_func = ECL_NIL;
  cl_object (*get)(cl_object, cl_object, cl_object);
  cl_object (*set)(cl_object, cl_object, cl_object);
  bool (*rem)(cl_object, cl_object);
  /*
   * Argument checking
   */
  if (test == @'eq' || test == ECL_SYM_FUN(@'eq')) {
    htt = ecl_htt_eq;
    get = _ecl_gethash_eq;
    set = _ecl_sethash_eq;
    rem = _ecl_remhash_eq;
  } else if (test == @'eql' || test == ECL_SYM_FUN(@'eql')) {
    htt = ecl_htt_eql;
    get = _ecl_gethash_eql;
    set = _ecl_sethash_eql;
    rem = _ecl_remhash_eql;
  } else if (test == @'equal' || test == ECL_SYM_FUN(@'equal')) {
    htt = ecl_htt_equal;
    get = _ecl_gethash_equal;
    set = _ecl_sethash_equal;
    rem = _ecl_remhash_equal;
  } else if (test == @'equalp' || test == ECL_SYM_FUN(@'equalp')) {
    htt = ecl_htt_equalp;
    get = _ecl_gethash_equalp;
    set = _ecl_sethash_equalp;
    rem = _ecl_remhash_equalp;
  } else if (test == @'package') {
    htt = ecl_htt_pack;
    get = _ecl_gethash_pack;
    set = _ecl_sethash_pack;
    rem = _ecl_remhash_pack;
  } else {
    htt = ecl_htt_generic;
    get = _ecl_gethash_generic;
    set = _ecl_sethash_generic;
    rem = _ecl_remhash_generic;
    hash_test = si_coerce_to_function(test);
  }
  if (ecl_unlikely(!ECL_FIXNUMP(size) ||
                   ecl_fixnum_minusp(size) ||
                   ecl_fixnum_geq(size,ecl_make_fixnum(ECL_ARRAY_TOTAL_LIMIT)))) {
    FEwrong_type_key_arg(@[make-hash-table], @[:size], size,
                         ecl_make_integer_type(ecl_make_fixnum(0),
                                               ecl_make_fixnum(ECL_ARRAY_TOTAL_LIMIT)));
  }
  hsize = ecl_fixnum(size);
  if (hsize < 16) {
    hsize = 16;
  }
 AGAIN:
  if (ecl_minusp(rehash_size)) {
  ERROR1:
    rehash_size =
      ecl_type_error(@'make-hash-table',"rehash-size",
                     rehash_size,
                     ecl_read_from_cstring("(OR (INTEGER 1 *) (FLOAT 0 (1)))"));
    goto AGAIN;
  }
  if (floatp(rehash_size)) {
    if (ecl_number_compare(rehash_size, ecl_make_fixnum(1)) < 0 ||
        ecl_minusp(rehash_size)) {
      goto ERROR1;
    }
    rehash_size = ecl_make_double_float(ecl_to_double(rehash_size));
  } else if (!ECL_FIXNUMP(rehash_size)) {
    goto ERROR1;
  }
  while (!ecl_numberp(rehash_threshold) ||
         ecl_minusp(rehash_threshold) ||
         ecl_number_compare(rehash_threshold, ecl_make_fixnum(1)) > 0)
    {
      rehash_threshold =
        ecl_type_error(@'make-hash-table',"rehash-threshold",
                       rehash_threshold,
                       ecl_read_from_cstring("(REAL 0 1)"));
    }
  /*
   * Build actual hash.
   */
  h = ecl_alloc_object(t_hashtable);
  h->hash.test = htt;
  h->hash.weak = ecl_htt_not_weak;
  h->hash.generic_test = hash_test;
  h->hash.generic_hash = hash_func;
  h->hash.get = h->hash.get_unsafe = get;
  h->hash.set = h->hash.set_unsafe = set;
  h->hash.rem = h->hash.rem_unsafe = rem;
  h->hash.size = hsize;
  h->hash.entries = 0;
  h->hash.rehash_size = rehash_size;
  h->hash.threshold = rehash_threshold;
  rehash_threshold = cl_max(2, min_threshold, rehash_threshold);
  h->hash.factor = ecl_to_double(rehash_threshold);
  h->hash.limit = h->hash.size * h->hash.factor;
  if (h->hash.limit >= hsize) {
    h->hash.limit = hsize - 1;
  }
  h->hash.data = NULL;    /* for GC sake */
  h->hash.data = (struct ecl_hashtable_entry *)
    ecl_alloc(hsize * sizeof(struct ecl_hashtable_entry));
  do_clrhash(h);
  return h;
}

#ifdef ECL_EXTERNALIZABLE
void
ecl_reconstruct_serialized_hashtable(cl_object h) {
  switch (h->hash.test) {
  case ecl_htt_eq:
    h->hash.get = _ecl_gethash_eq;
    h->hash.set = _ecl_sethash_eq;
    h->hash.rem = _ecl_remhash_eq;
    break;
  case ecl_htt_eql:
    h->hash.get = _ecl_gethash_eql;
    h->hash.set = _ecl_sethash_eql;
    h->hash.rem = _ecl_remhash_eql;
    break;
  case ecl_htt_equal:
    h->hash.get = _ecl_gethash_equal;
    h->hash.set = _ecl_sethash_equal;
    h->hash.rem = _ecl_remhash_equal;
    break;
  case ecl_htt_equalp:
    h->hash.get = _ecl_gethash_equalp;
    h->hash.set = _ecl_sethash_equalp;
    h->hash.rem = _ecl_remhash_equalp;
    break;
  case ecl_htt_pack:
    h->hash.get = _ecl_gethash_pack;
    h->hash.set = _ecl_sethash_pack;
    h->hash.rem = _ecl_remhash_pack;
    break;
  case ecl_htt_generic:
    h->hash.get = _ecl_gethash_generic;
    h->hash.set = _ecl_sethash_generic;
    h->hash.rem = _ecl_remhash_generic;
    break;
  }
  /* FIXME we should have separate getters and setters for each predicate. */
  if (h->hash.weak != ecl_htt_not_weak) {
    h->hash.get = _ecl_gethash_weak;
    h->hash.set = _ecl_sethash_weak;
    h->hash.rem = _ecl_remhash_weak;
  }
  /* Always bind unsafe variants. */
  h->hash.get_unsafe = h->hash.get;
  h->hash.set_unsafe = h->hash.set;
  h->hash.rem_unsafe = h->hash.rem;
  if (h->hash.sync_lock != OBJNULL
      && (ecl_t_of(h->hash.sync_lock) == t_lock
          || ecl_t_of(h->hash.sync_lock) == t_rwlock)) {
    h->hash.get = _ecl_gethash_sync;
    h->hash.set = _ecl_sethash_sync;
    h->hash.rem = _ecl_remhash_sync;
  }
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
    assert_type_hash_table(@[gethash], 2, ht);
    {
      cl_object v = ht->hash.get(key, ht, OBJNULL);
      if (v != OBJNULL) {
        @(return v ECL_T);
      } else {
        @(return no_value ECL_NIL);
      }
    }
  }
  @)

cl_object
si_hash_set(cl_object key, cl_object ht, cl_object val)
{
  /* INV: ecl_sethash() checks the type of hashtable */
  ecl_sethash(key, ht, val);
  @(return val);
}

bool
ecl_remhash(cl_object key, cl_object hashtable)
{
  assert_type_hash_table(@[remhash], 2, hashtable);
  return hashtable->hash.rem(key, hashtable);
}

cl_object
cl_remhash(cl_object key, cl_object ht)
{
  /* INV: _ecl_remhash() checks the type of hashtable */
  @(return (ecl_remhash(key, ht)? ECL_T : ECL_NIL));
}

cl_object
cl_clrhash(cl_object ht)
{
  assert_type_hash_table(@[clrhash], 1, ht);
  if (ht->hash.entries) {
    do_clrhash(ht);
  }
  @(return ht);
}

cl_object
cl_hash_table_test(cl_object ht)
{
  cl_object output;
  assert_type_hash_table(@[hash-table-test], 1, ht);
  switch(ht->hash.test) {
  case ecl_htt_eq:      output = @'eq';     break;
  case ecl_htt_eql:     output = @'eql';    break;
  case ecl_htt_equal:   output = @'equal';  break;
  case ecl_htt_equalp:  output = @'equalp'; break;
  case ecl_htt_pack:    output = @'equal';  break;
  case ecl_htt_generic: output = ht->hash.generic_test; break;
  default: FEerror("hash-table-test: unknown test.", 0);
  }
  @(return output);
}

cl_object
cl_hash_table_size(cl_object ht)
{
  assert_type_hash_table(@[hash-table-size], 1, ht);
  @(return ecl_make_fixnum(ht->hash.size));
}

cl_index
ecl_hash_table_count(cl_object ht)
{
  if (ht->hash.weak == ecl_htt_not_weak) {
    return ht->hash.entries;
  } else if (ht->hash.size) {
    cl_index i, j;
    for (i = j = 0; i < ht->hash.size; i++) {
      struct ecl_hashtable_entry output = copy_entry(ht->hash.data + i, ht);
      if (output.key != OBJNULL) {
        if (++j == ht->hash.size)
          break;
      }
    }
    return ht->hash.entries = j;
  } else {
    return 0;
  }
}


cl_object
cl_hash_table_count(cl_object ht)
{
  assert_type_hash_table(@[hash-table-count], 1, ht);
  @(return (ecl_make_fixnum(ecl_hash_table_count(ht))));
}


/* HASH TABLE ITERATION
 *
 * We iterate from right to left across each group of consecutive
 * non-empty buckets. This allows removing the current iteration
 * element without iterating over elements twice or missing elements
 * because we only change elements to the right of the current
 * element when removing an element. For example, a hashtable of
 * size 10 with 5 filled buckets is iterated over as follows:
 *
 *  a..bc...de
 *      ^
 *  a..bc...de
 *     ^
 *  a..bc...de
 *  ^
 *  a..bc...de
 *           ^
 *  a..bc...de
 *          ^
 *
 * If for example the element `e` is removed and the element `a` moves
 * up at the place that `e` previously occupied, we don't iterate
 * twice over `a`.
 */

static cl_object
si_hash_table_iterate(cl_narg narg, ...)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object env = the_env->function->cclosure.env;
  cl_object index = CAR(env);
  cl_object endpoint = CADR(env);
  cl_object ht = CADDR(env);
  cl_fixnum i, j;
  if (!Null(index)) {
    i = ecl_fixnum(endpoint);
    j = ecl_fixnum(index);
    do {
      j = (j == 0) ? ht->hash.size-1 : j - 1;
      struct ecl_hashtable_entry e = copy_entry(ht->hash.data + j, ht);
      if (e.key != OBJNULL) {
        cl_object ndx = ecl_make_fixnum(j);
        ECL_RPLACA(env, ndx);
        @(return ndx e.key e.value);
      }
    } while (j != i);
    ECL_RPLACA(env, ECL_NIL);
  }
  @(return ECL_NIL);
}

cl_object
si_hash_table_iterator(cl_object ht)
{
  cl_fixnum i;
  assert_type_hash_table(@[si::hash-table-iterator], 1, ht);
  /* Make sure we don't start in the middle of a group of consecutive
   * filled buckets. */
  for (i = ht->hash.size-1; ht->hash.data[i].key != OBJNULL; i--);
  @(return ecl_make_cclosure_va(si_hash_table_iterate,
                                cl_list(3, ecl_make_fixnum(i),
                                        ecl_make_fixnum(i),
                                        ht),
                                @'si::hash-table-iterator',
                                0));
}

cl_object
cl_hash_table_rehash_size(cl_object ht)
{
  assert_type_hash_table(@[hash-table-rehash-size], 1, ht);
  @(return ht->hash.rehash_size);
}

cl_object
cl_hash_table_rehash_threshold(cl_object ht)
{
  assert_type_hash_table(@[hash-table-rehash-threshold], 1, ht);
  @(return ht->hash.threshold);
}

cl_object
cl_maphash(cl_object fun, cl_object ht)
{
  cl_index i, j, hsize;

  assert_type_hash_table(@[maphash], 2, ht);
  if (ht->hash.entries == 0) {
    @(return ECL_NIL);
  }
  hsize = ht->hash.size;
  /* Make sure we don't start in the middle of a group of consecutive
   * filled buckets. */
  for (i = hsize-1; ht->hash.data[i].key != OBJNULL; i--);
  j = i;
  do {
    j = (j == 0) ? hsize-1 : j - 1;
    struct ecl_hashtable_entry e = copy_entry(ht->hash.data + j, ht);
    if (e.key != OBJNULL) {
      cl_object key = e.key;
      cl_object val = e.value;
      funcall(3, fun, key, val);
    }
  } while (j != i);
  @(return ECL_NIL);
}

cl_object
si_hash_table_content(cl_object ht)
{
  cl_index i;
  cl_object output = ECL_NIL;
  assert_type_hash_table(@[ext::hash-table-content], 2, ht);
  for (i = 0;  i < ht->hash.size;  i++) {
    struct ecl_hashtable_entry e = copy_entry(ht->hash.data + i, ht);
    if (e.key != OBJNULL)
      output = ecl_cons(ecl_cons(e.key, e.value), output);
  }
  @(return output);
}

cl_object
si_hash_table_fill(cl_object ht, cl_object values)
{
  assert_type_hash_table(@[ext::hash-table-fill], 2, ht);
  while (!Null(values)) {
    cl_object pair = ecl_car(values);
    cl_object key = ecl_car(pair);
    cl_object value = ECL_CONS_CDR(pair);
    values = ECL_CONS_CDR(values);
    ecl_sethash(key, ht, value);
  }
  @(return ht);
}

cl_object
si_copy_hash_table(cl_object orig)
{
  cl_object hash;
  hash = cl__make_hash_table(cl_hash_table_test(orig),
                             cl_hash_table_size(orig),
                             cl_hash_table_rehash_size(orig),
                             cl_hash_table_rehash_threshold(orig));
  hash->hash.generic_hash = orig->hash.generic_hash,
  memcpy(hash->hash.data, orig->hash.data,
         orig->hash.size * sizeof(*orig->hash.data));
  hash->hash.entries = orig->hash.entries;
  @(return hash);
}
