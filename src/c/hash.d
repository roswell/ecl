/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * hash.d - hash tables
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 * Copyright (c) 2017 Daniel Kochmanski
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
#include "newhash.h"

#define SYMBOL_NAME(x) (Null(x)? ECL_NIL_SYMBOL->symbol.name : (x)->symbol.name)

static void ECL_INLINE
assert_type_hash_table(cl_object function, cl_narg narg, cl_object p)
{
  unlikely_if (!ECL_HASH_TABLE_P(p))
    FEwrong_type_nth_arg(function, narg, p, @[hash-table]);
}

static cl_hashkey
_hash_eql(cl_hashkey h, cl_object x)
{
  switch (ecl_t_of(x)) {
  case t_bignum:
    return hash_string(h, (unsigned char*)ECL_BIGNUM_LIMBS(x),
                       labs(ECL_BIGNUM_SIZE(x)) *
                       sizeof(mp_limb_t));
  case t_ratio:
    h = _hash_eql(h, x->ratio.num);
    return _hash_eql(h, x->ratio.den);
  case t_singlefloat:
    return hash_string(h, (unsigned char*)&ecl_single_float(x), sizeof(ecl_single_float(x)));
  case t_doublefloat:
    return hash_string(h, (unsigned char*)&ecl_double_float(x), sizeof(ecl_double_float(x)));
  case t_longfloat: {
    /* We coerce to double because long double has extra bits that
     * give rise to different hash key and are not meaningful. */
    struct { double mantissa; int exponent; int sign; } aux;
    aux.mantissa = frexpl(ecl_long_float(x), &aux.exponent);
    aux.sign = (ecl_long_float(x) < 0)? -1: 1;
    return hash_string(h, (unsigned char*)&aux, sizeof(aux));
  }
  case t_complex:
    h = _hash_eql(h, x->gencomplex.real);
    return _hash_eql(h, x->gencomplex.imag);
#ifdef ECL_COMPLEX_FLOAT
  case t_csfloat: return hash_string(h, (unsigned char*)&ecl_csfloat(x), sizeof(ecl_csfloat(x)));
  case t_cdfloat: return hash_string(h, (unsigned char*)&ecl_cdfloat(x), sizeof(ecl_cdfloat(x)));
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
    return hash_string(h, (unsigned char*)&aux, sizeof(aux));
  }
#endif
  case t_character:
    return hash_word(h, ECL_CHAR_CODE(x));
#ifdef ECL_SSE2
  case t_sse_pack:
    return hash_string(h, x->sse.data.b8, 16);
#endif
  default:
    return hash_word(h, ((cl_hashkey)x >> 2));
  }
}

static cl_hashkey
_hash_equal(int depth, cl_hashkey h, cl_object x)
{
  switch (ecl_t_of(x)) {
  case t_list:
    if (Null(x)) {
      return _hash_equal(depth, h, ECL_NIL_SYMBOL->symbol.name);
    }
    if (--depth == 0) {
      return h;
    } else {
      h = _hash_equal(depth, h, ECL_CONS_CAR(x));
      return _hash_equal(depth, h, ECL_CONS_CDR(x));
    }
  case t_symbol:
    x = x->symbol.name;
#ifdef ECL_UNICODE
  case t_base_string:
    return hash_base_string((ecl_base_char *)x->base_string.self,
                            x->base_string.fillp, h);
  case t_string:
    return hash_full_string(x->string.self, x->string.fillp, h);
#else
  case t_base_string:
    return hash_string(h, (ecl_base_char *)x->base_string.self,
                       x->base_string.fillp);
#endif
  case t_pathname:
    h = _hash_equal(0, h, x->pathname.directory);
    h = _hash_equal(0, h, x->pathname.name);
    h = _hash_equal(0, h, x->pathname.type);
    h = _hash_equal(0, h, x->pathname.host);
    h = _hash_equal(0, h, x->pathname.device);
    return _hash_equal(0, h, x->pathname.version);
  case t_bitvector:
    /* Notice that we may round out some bits. We must do this
     * because the fill pointer may be set in the middle of a byte.
     * If so, the extra bits _must_ _not_ take part in the hash,
     * because otherwise two bit arrays which are EQUAL might
     * have different hash keys. */
    return hash_string(h, x->vector.self.bc, x->vector.fillp / 8);
  case t_random: {
    cl_object array = x->random.value;
    return hash_string
      (h, (unsigned char*)array->vector.self.b8, 4*624);
  }
#ifdef ECL_SIGNED_ZERO
    /* According to 3.2.4.2.2 Definition of Similarity two numbers are
       "similar" if they are of the same type and represent the same
       mathematical value. -- jd 2019-05-06*/
  case t_singlefloat: {
    float f = ecl_single_float(x);
    if (f == 0.0) f = 0.0;
    return hash_string(h, (unsigned char*)&f, sizeof(f));
  }
  case t_doublefloat: {
    double f = ecl_double_float(x);
    if (f == 0.0) f = 0.0;
    return hash_string(h, (unsigned char*)&f, sizeof(f));
  }
  case t_longfloat: {
    /* We coerce to double because long double has extra bits
     * that give rise to different hash key and are not
     * meaningful */
    struct { double mantissa; int exponent; int sign; } aux;
    aux.mantissa = frexpl(ecl_long_float(x), &aux.exponent);
    aux.sign = (ecl_long_float(x) < 0)? -1: 1;
    if (aux.mantissa == 0.0) aux.mantissa = 0.0;
    return hash_string(h, (unsigned char*)&aux, sizeof(aux));
  }
  case t_complex: {
    h = _hash_equal(depth, h, x->gencomplex.real);
    return _hash_equal(depth, h, x->gencomplex.imag);
  }
# ifdef ECL_COMPLEX_FLOAT
  case t_csfloat: {
    _Complex float f = ecl_csfloat(x);
    if (crealf(f) == 0.0) f = 0.0 + I * cimagf(f);
    if (cimagf(f) == 0.0) f = crealf(f) + I * 0.0;
    return hash_string(h, (unsigned char*)&(f), sizeof(f));
  }
  case t_cdfloat: {
    _Complex double f = ecl_cdfloat(x);
    if (creal(f) == 0.0) f = 0.0 + I * cimag(f);
    if (cimag(f) == 0.0) f = creal(f) + I * 0.0;
    return hash_string(h, (unsigned char*)&(f), sizeof(f));
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
    return hash_string(h, (unsigned char*)&aux, sizeof(aux));
  }
# endif
#endif
  default:
    return _hash_eql(h, x);
  }
}

static cl_hashkey
_hash_equalp(int depth, cl_hashkey h, cl_object x)
{
  cl_index i, len;
  switch (ecl_t_of(x)) {
  case t_character:
    return hash_word(h, ecl_char_upcase(ECL_CHAR_CODE(x)));
  case t_list:
    if (Null(x)) {
      return _hash_equalp(depth, h, ECL_NIL_SYMBOL->symbol.name);
    }
    if (--depth == 0) {
      return h;
    } else {
      h = _hash_equalp(depth, h, ECL_CONS_CAR(x));
      return _hash_equalp(depth, h, ECL_CONS_CDR(x));
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
  SCAN:   if (--depth) {
      for (i = 0; i < len; i++) {
        h = _hash_equalp(depth, h, ecl_aref_unsafe(x, i));
      }
    }
    return h;
  case t_fixnum:
    return hash_word(h, ecl_fixnum(x));
  case t_singlefloat:
    /* FIXME! We should be more precise here! */
    return hash_word(h, (cl_index)ecl_single_float(x));
  case t_doublefloat:
    /* FIXME! We should be more precise here! */
    return hash_word(h, (cl_index)ecl_double_float(x));
  case t_bignum:
    /* FIXME! We should be more precise here! */
    return hash_string(h, (unsigned char*)x->big.big_num->_mp_d,
                       abs(x->big.big_num->_mp_size) *
                       sizeof(mp_limb_t));
  case t_ratio:
    h = _hash_equalp(0, h, x->ratio.num);
    return _hash_equalp(0, h, x->ratio.den);
  case t_complex:
    h = _hash_equalp(0, h, x->gencomplex.real);
    return _hash_equalp(0, h, x->gencomplex.imag);
#ifdef ECL_COMPLEX_FLOAT
    /* FIXME! We should be more precise here! */
  case t_csfloat: return hash_word(h, (cl_index)ecl_csfloat(x));
  case t_cdfloat: return hash_word(h, (cl_index)ecl_cdfloat(x));
  case t_clfloat: return hash_word(h, (cl_index)ecl_clfloat(x));
#endif
  case t_instance:
  case t_hashtable:
    /* FIXME! We should be more precise here! */
    return hash_word(h, 42);
  default:
    return _hash_equal(depth, h, x);
  }
}

static cl_hashkey _hash_generic(cl_object ht, cl_object key) {
  cl_object hash_fun = ht->hash.generic_hash;
  cl_object h_object = _ecl_funcall2(hash_fun, key);
  if (!ECL_FIXNUMP(h_object) || ecl_fixnum_minusp(h_object)) {
    FEwrong_type_argument(@'fixnum', h_object);
  }
  return ecl_fixnum(h_object);
}

#define HASH_TABLE_LOOP(hkey,hvalue,h,HASH_TABLE_LOOP_TEST) {           \
    cl_index hsize = hashtable->hash.size;                              \
    cl_index i = h % hsize, j = hsize, k;                               \
    for (k = 0; k < hsize;  i = (i + 1) % hsize, k++) {                 \
      struct ecl_hashtable_entry *e = hashtable->hash.data + i;         \
      cl_object hkey = e->key, hvalue = e->value;                       \
      if (hkey == OBJNULL) {                                            \
        if (hvalue == OBJNULL) {                                        \
          if (j == hsize)                                               \
            return e;                                                   \
          else                                                          \
            return hashtable->hash.data + j;                            \
        } else {                                                        \
          if (j == hsize)                                               \
            j = i;                                                      \
          else if (j == i)                                              \
            return e;                                                   \
        }                                                               \
        continue;                                                       \
      }                                                                 \
      if (HASH_TABLE_LOOP_TEST) return hashtable->hash.data + i;        \
    }                                                                   \
    return hashtable->hash.data + j;                                    \
  }

#define HASH_TABLE_SET(h,loop,compute_key,store_key) {                  \
    cl_hashkey h = compute_key;                                         \
    struct ecl_hashtable_entry *e;                                      \
AGAIN:                                                                  \
 e = loop(h, key, hashtable);                                           \
 if (e->key == OBJNULL) {                                               \
                         cl_index i = hashtable->hash.entries + 1;      \
                         if (i >= hashtable->hash.limit) {              \
                                                          hashtable = ecl_extend_hashtable(hashtable); \
                                                          goto AGAIN;   \
                                                          }             \
                         hashtable->hash.entries = i;                   \
                         e->key = store_key;                            \
                         }                                              \
 e->value = value;                                                      \
 return hashtable;                                                      \
 }

/*
 * EQ HASHTABLES
 */

#define _hash_eq(k) ((cl_hashkey)(k) >> 2)

static struct ecl_hashtable_entry *
_ecl_hash_loop_eq(cl_hashkey h, cl_object key, cl_object hashtable)
{
  HASH_TABLE_LOOP(hkey, hvalue, h, key == hkey);
}

static cl_object
_ecl_gethash_eq(cl_object key, cl_object hashtable, cl_object def)
{
  cl_hashkey h = _hash_eq(key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_eq(h, key, hashtable);
  return (e->key == OBJNULL)? def : e->value;
}

static bool
_ecl_remhash_eq(cl_object key, cl_object hashtable)
{
  cl_hashkey h = _hash_eq(key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_eq(h, key, hashtable);
  if (e->key == OBJNULL) {
    return 0;
  } else {
    e->key = OBJNULL;
    e->value = ECL_NIL;
    hashtable->hash.entries--;
    return 1;
  }
}

static cl_object
_ecl_sethash_eq(cl_object key, cl_object hashtable, cl_object value)
{
  HASH_TABLE_SET(h, _ecl_hash_loop_eq, _hash_eq(key), key);
}

/*
 * EQL HASHTABLES
 */

static struct ecl_hashtable_entry *
_ecl_hash_loop_eql(cl_hashkey h, cl_object key, cl_object hashtable)
{
  HASH_TABLE_LOOP(hkey, hvalue, h, ecl_eql(key, hkey));
}

static cl_object
_ecl_gethash_eql(cl_object key, cl_object hashtable, cl_object def)
{
  cl_hashkey h = _hash_eql(0, key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_eql(h, key, hashtable);
  return (e->key == OBJNULL)? def : e->value;
}

static cl_object
_ecl_sethash_eql(cl_object key, cl_object hashtable, cl_object value)
{
  HASH_TABLE_SET(h, _ecl_hash_loop_eql, _hash_eql(0, key), key);
}

static bool
_ecl_remhash_eql(cl_object key, cl_object hashtable)
{
  cl_hashkey h = _hash_eql(0, key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_eql(h, key, hashtable);
  if (e->key == OBJNULL) {
    return 0;
  } else {
    e->key = OBJNULL;
    e->value = ECL_NIL;
    hashtable->hash.entries--;
    return 1;
  }
}

/*
 * EQUAL HASHTABLES
 */

static struct ecl_hashtable_entry *
_ecl_hash_loop_equal(cl_hashkey h, cl_object key, cl_object hashtable)
{
  HASH_TABLE_LOOP(hkey, hvalue, h, ecl_equal(key, hkey));
}

static cl_object
_ecl_gethash_equal(cl_object key, cl_object hashtable, cl_object def)
{
  cl_hashkey h = _hash_equal(3, 0, key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_equal(h, key, hashtable);
  return (e->key == OBJNULL)? def : e->value;
}

static cl_object
_ecl_sethash_equal(cl_object key, cl_object hashtable, cl_object value)
{
  HASH_TABLE_SET(h, _ecl_hash_loop_equal, _hash_equal(3, 0, key), key);
}

static bool
_ecl_remhash_equal(cl_object key, cl_object hashtable)
{
  cl_hashkey h = _hash_equal(3, 0, key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_equal(h, key, hashtable);
  if (e->key == OBJNULL) {
    return 0;
  } else {
    e->key = OBJNULL;
    e->value = ECL_NIL;
    hashtable->hash.entries--;
    return 1;
  }
}

/*
 * EQUALP HASHTABLES
 */

static struct ecl_hashtable_entry *
_ecl_hash_loop_equalp(cl_hashkey h, cl_object key, cl_object hashtable)
{
  HASH_TABLE_LOOP(hkey, hvalue, h, ecl_equalp(key, hkey));
}

static cl_object
_ecl_gethash_equalp(cl_object key, cl_object hashtable, cl_object def)
{
  cl_hashkey h = _hash_equalp(3, 0, key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_equalp(h, key, hashtable);
  return (e->key == OBJNULL)? def : e->value;
}

static cl_object
_ecl_sethash_equalp(cl_object key, cl_object hashtable, cl_object value)
{
  HASH_TABLE_SET(h, _ecl_hash_loop_equalp, _hash_equalp(3, 0, key), key);
}

static bool
_ecl_remhash_equalp(cl_object key, cl_object hashtable)
{
  cl_hashkey h = _hash_equalp(3, 0, key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_equalp(h, key, hashtable);
  if (e->key == OBJNULL) {
    return 0;
  } else {
    e->key = OBJNULL;
    e->value = ECL_NIL;
    hashtable->hash.entries--;
    return 1;
  }
}

/*
 * PACKAGE HASHTABLES
 */

static struct ecl_hashtable_entry *
_ecl_hash_loop_pack(cl_hashkey h, cl_object key, cl_object hashtable)
{
  cl_object ho = ecl_make_fixnum(h & 0xFFFFFFF);
  HASH_TABLE_LOOP(hkey, hvalue, h, (ho==hkey) && ecl_string_eq(key,SYMBOL_NAME(hvalue)));
}

static cl_object
_ecl_gethash_pack(cl_object key, cl_object hashtable, cl_object def)
{
  cl_hashkey h = _hash_equal(3, 0, key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_pack(h, key, hashtable);
  return (e->key == OBJNULL)? def : e->value;
}

static cl_object
_ecl_sethash_pack(cl_object key, cl_object hashtable, cl_object value)
{
  HASH_TABLE_SET(h, _ecl_hash_loop_pack, _hash_equal(3, 0, key), ecl_make_fixnum(h & 0xFFFFFFF));
}

static bool
_ecl_remhash_pack(cl_object key, cl_object hashtable)
{
  cl_hashkey h = _hash_equal(3, 0, key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_pack(h, key, hashtable);
  if (e->key == OBJNULL) {
    return 0;
  } else {
    e->key = OBJNULL;
    e->value = ECL_NIL;
    hashtable->hash.entries--;
    return 1;
  }
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
  HASH_TABLE_LOOP(hkey, hvalue, h, _ecl_generic_hash_test(test_fun, key, hkey));
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
  HASH_TABLE_SET(h, _ecl_hash_loop_generic, _hash_generic(hashtable, key), key);
}

static bool
_ecl_remhash_generic(cl_object key, cl_object hashtable)
{
  cl_hashkey h = _hash_generic(hashtable, key);
  struct ecl_hashtable_entry *e = _ecl_hash_loop_generic(h, key, hashtable);
  if (e->key == OBJNULL) {
    return 0;
  } else {
    e->key = OBJNULL;
    e->value = ECL_NIL;
    hashtable->hash.entries--;
    return 1;
  }
}

/*
 * WEAK HASH TABLES
 */
#ifndef ECL_WEAK_HASH
#define copy_entry(e,h) *(e)
#endif

#ifdef ECL_WEAK_HASH
static cl_hashkey
_ecl_hash_key(cl_object h, cl_object o) {
  switch (h->hash.test) {
  case ecl_htt_eq:      return _hash_eq(o);
  case ecl_htt_eql:     return _hash_eql(0, o);
  case ecl_htt_equal:   return _hash_equal(3, 0, o);
  case ecl_htt_equalp:  return _hash_equalp(3, 0, o);
  case ecl_htt_pack:    return _hash_equal(3, 0, o);
  case ecl_htt_generic: return _hash_generic(h, o);
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

static struct ecl_hashtable_entry
copy_entry(struct ecl_hashtable_entry *e, cl_object h)
{
  if (e->key == OBJNULL) {
    return *e;
  } else {
    struct ecl_hashtable_entry output = *e;
    switch (h->hash.weak) {
    case ecl_htt_weak_key:
      if (GC_call_with_alloc_lock((GC_fn_type)normalize_weak_key_entry,
                                  &output)) {
        return output;
      }
      break;
    case ecl_htt_weak_value:
      if (GC_call_with_alloc_lock((GC_fn_type)normalize_weak_value_entry,
                                  &output)) {
        return output;
      }
      break;
    case ecl_htt_weak_key_and_value:
      if (GC_call_with_alloc_lock((GC_fn_type)normalize_weak_key_and_value_entry,
                                  &output)) {
        return output;
      }
      break;
    case ecl_htt_weak_key_or_value:
      if (GC_call_with_alloc_lock((GC_fn_type)normalize_weak_key_or_value_entry,
                                  &output)) {
        return output;
      }
      break;
    case ecl_htt_not_weak:
    default:
      return output;
    }
    h->hash.entries--;
    output.key = OBJNULL;
    output.value = ECL_NIL;
    return *e = output;
  }
}

static struct ecl_hashtable_entry *
_ecl_weak_hash_loop(cl_hashkey h, cl_object key, cl_object hashtable,
                    struct ecl_hashtable_entry *aux)
{
  cl_index hsize = hashtable->hash.size;
  cl_index i = h % hsize, j = hsize, k;
  for (k = 0; k < hsize;  i = (i + 1) % hsize, k++) {
    struct ecl_hashtable_entry *p = hashtable->hash.data + i;
    struct ecl_hashtable_entry e = *aux = copy_entry(p, hashtable);
    if (e.key == OBJNULL) {
      if (e.value == OBJNULL) {
        if (j == hsize) {
          return p;
        } else {
          return hashtable->hash.data + j;
        }
      } else {
        if (j == hsize) {
          j = i;
        } else if (j == i) {
          return p;
        }
      }
      continue;
    }
    switch (hashtable->hash.test) {
    case ecl_htt_eq:
      if (e.key == key)
        return p;
      break;
    case ecl_htt_eql:
      if (ecl_eql(e.key, key))
        return p;
      break;
    case ecl_htt_equal:
      if (ecl_equal(e.key, key))
        return p;
      break;
    case ecl_htt_equalp:
      if (ecl_equalp(e.key, key))
        return p;
      break;
    case ecl_htt_generic:
      if (_ecl_generic_hash_test(hashtable->hash.generic_test, e.key, key))
        return p;
      break;
    default:
        ecl_internal_error("Unknown hash test.");
    }
  }
  return hashtable->hash.data + j;
}

static cl_object
_ecl_gethash_weak(cl_object key, cl_object hashtable, cl_object def)
{
  cl_hashkey h = _ecl_hash_key(hashtable, key);
  struct ecl_hashtable_entry aux[1];
  _ecl_weak_hash_loop(h, key, hashtable, aux);
  if (aux->key == OBJNULL) {
    return def;
  }
  /* _ecl_weak_hash_loop "normalizes" entries. That means that
     si_weak_pointer_value shouldn't be called because value is
     already "unwrapped". -- jd 2019-05-28 */
  return aux->value;
}

static cl_object
_ecl_sethash_weak(cl_object key, cl_object hashtable, cl_object value)
{
  cl_hashkey h = _ecl_hash_key(hashtable, key);
  struct ecl_hashtable_entry aux[1];
  struct ecl_hashtable_entry *e;
 AGAIN:
  e = _ecl_weak_hash_loop(h, key, hashtable, aux);
  if (aux->key == OBJNULL) {
    cl_index i = hashtable->hash.entries + 1;
    if (i >= hashtable->hash.limit) {
      hashtable = ecl_extend_hashtable(hashtable);
      goto AGAIN;
    }
    hashtable->hash.entries = i;
    switch (hashtable->hash.weak) {
    case ecl_htt_weak_key:
    case ecl_htt_weak_key_and_value:
    case ecl_htt_weak_key_or_value:
      key = si_make_weak_pointer(key);
    }
    e->key = key;
  }
  switch (hashtable->hash.weak) {
  case ecl_htt_weak_value:
  case ecl_htt_weak_key_and_value:
  case ecl_htt_weak_key_or_value:
    value = si_make_weak_pointer(value);
  }
  e->value = value;
  return hashtable;
}


static bool
_ecl_remhash_weak(cl_object key, cl_object hashtable)
{
  cl_hashkey h = _ecl_hash_key(hashtable, key);
  struct ecl_hashtable_entry aux[1];
  struct ecl_hashtable_entry *e =
    _ecl_weak_hash_loop(h, key, hashtable, aux);
  if (aux->key != OBJNULL) {
    hashtable->hash.entries--;
    e->key = OBJNULL;
    e->value = ECL_NIL;
    return 1;
  } else {
    return 0;
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
  new->hash.data = (struct ecl_hashtable_entry *)
    ecl_alloc(new_size * sizeof(struct ecl_hashtable_entry));
  for (i = 0;  i < new_size;  i++) {
    new->hash.data[i].key = OBJNULL;
    new->hash.data[i].value = OBJNULL;
  }
  for (i = 0;  i < old_size;  i++) {
    struct ecl_hashtable_entry e =
      copy_entry(old->hash.data + i, old);
    if (e.key != OBJNULL) {
      new = new->hash.set(new->hash.test == ecl_htt_pack?
                          SYMBOL_NAME(e.value) : e.key,
                          new, e.value);
    }
  }
  return new;
}

@(defun make_hash_table (&key (test @'eql')
                         (hash_function ECL_NIL)
                         (weakness ECL_NIL)
                         (synchronized ECL_NIL)
                         (size ecl_make_fixnum(1024))
                         (rehash_size cl_core.rehash_size)
                         (rehash_threshold cl_core.rehash_threshold))
@ {
    cl_object hash = cl__make_hash_table(test, size, rehash_size, rehash_threshold);
    if (hash->hash.test == ecl_htt_generic) {
      /* Normally we would make hash_function an argument to
         cl__make_hash_table and put this test in there and void
         unnecessary object allocation, but we do not want to
         compromise the API. -- jd 2019-05-23 */
      if (hash_function == ECL_NIL) {
        FEerror("~S is an illegal hash-table test function.", 1, test);
      }
      hash_function = si_coerce_to_function(hash_function);
      hash->hash.generic_hash = hash_function;
    }
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

    if (!Null(synchronized)) {
#ifdef ECL_THREADS
      hash->hash.sync_lock = ecl_make_rwlock(ECL_NIL);
      hash->hash.get_unsafe = hash->hash.get;
      hash->hash.set_unsafe = hash->hash.set;
      hash->hash.rem_unsafe = hash->hash.rem;
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
  h->hash.get = get;
  h->hash.set = set;
  h->hash.rem = rem;
  h->hash.size = hsize;
  h->hash.entries = 0;
  h->hash.rehash_size = rehash_size;
  h->hash.threshold = rehash_threshold;
  rehash_threshold = cl_max(2, min_threshold, rehash_threshold);
  h->hash.factor = ecl_to_double(rehash_threshold);
  h->hash.limit = h->hash.size * h->hash.factor;
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
  if (h->hash.weak != ecl_htt_not_weak) {
    h->hash.get = _ecl_gethash_weak;
    h->hash.set = _ecl_sethash_weak;
    h->hash.rem = _ecl_remhash_weak;
  }
  if (h->hash.sync_lock != OBJNULL
      && (ecl_t_of(h->hash.sync_lock) == t_lock
          || ecl_t_of(h->hash.sync_lock) == t_rwlock)) {
    h->hash.get_unsafe = h->hash.get;
    h->hash.set_unsafe = h->hash.set;
    h->hash.rem_unsafe = h->hash.rem;
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
  case ecl_htt_generic: output = ht->hash.generic_test;
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
      struct ecl_hashtable_entry output =
        copy_entry(ht->hash.data + i, ht);
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

static cl_object
si_hash_table_iterate(cl_narg narg, ...)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object env = the_env->function->cclosure.env;
  cl_object index = CAR(env);
  cl_object ht = CADR(env);
  cl_fixnum i;
  if (!Null(index)) {
    i = ecl_fixnum(index);
    if (i < 0)
      i = -1;
    for (; ++i < ht->hash.size; ) {
      struct ecl_hashtable_entry e =
        copy_entry(ht->hash.data + i, ht);
      if (e.key != OBJNULL) {
        cl_object ndx = ecl_make_fixnum(i);
        ECL_RPLACA(env, ndx);
        @(return ndx e.key e.value);
      }
    }
    ECL_RPLACA(env, ECL_NIL);
  }
  @(return ECL_NIL);
}

cl_object
si_hash_table_iterator(cl_object ht)
{
  assert_type_hash_table(@[si::hash-table-iterator], 1, ht);
  @(return ecl_make_cclosure_va(si_hash_table_iterate,
                                cl_list(2, ecl_make_fixnum(-1), ht),
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
cl_sxhash(cl_object key)
{
  cl_index output = _hash_equal(3, 0, key);
  const cl_index mask = ((cl_index)1 << (ECL_FIXNUM_BITS - 3)) - 1;
  @(return ecl_make_fixnum(output & mask));
}

@(defun si::hash-eql (&rest args)
  cl_index h;
@
  for (h = 0; narg; narg--) {
    cl_object o = ecl_va_arg(args);
    h = _hash_eql(h, o);
  }
  @(return ecl_make_fixnum(h));
@)

@(defun si::hash-equal (&rest args)
  cl_index h;
@
  for (h = 0; narg; narg--) {
    cl_object o = ecl_va_arg(args);
    h = _hash_equal(3, h, o);
  }
  @(return ecl_make_fixnum(h));
@)

@(defun si::hash-equalp (&rest args)
  cl_index h;
@
  for (h = 0; narg; narg--) {
    cl_object o = ecl_va_arg(args);
    h = _hash_equalp(3, h, o);
  }
  @(return ecl_make_fixnum(h));
@)

cl_object
cl_maphash(cl_object fun, cl_object ht)
{
  cl_index i;

  assert_type_hash_table(@[maphash], 2, ht);
  for (i = 0;  i < ht->hash.size;  i++) {
    struct ecl_hashtable_entry e = ht->hash.data[i];
    if(e.key != OBJNULL) {
      cl_object key = e.key;
      cl_object val = e.value;
      switch (ht->hash.weak) {
      case ecl_htt_weak_key:
        key = si_weak_pointer_value(key);
        break;
      case ecl_htt_weak_value:
        val = si_weak_pointer_value(val);
        break;
      case ecl_htt_weak_key_and_value:
      case ecl_htt_weak_key_or_value:
        key = si_weak_pointer_value(key);
        val = si_weak_pointer_value(val);
      }
      funcall(3, fun, key, val);
    }
  }
  @(return ECL_NIL);
}

cl_object
si_hash_table_content(cl_object ht)
{
  cl_index i;
  cl_object output = ECL_NIL;
  assert_type_hash_table(@[ext::hash-table-content], 2, ht);
  for (i = 0;  i < ht->hash.size;  i++) {
    struct ecl_hashtable_entry e = ht->hash.data[i];
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
