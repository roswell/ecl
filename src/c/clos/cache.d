/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * cache.d - thread-local cache for generic function dispatch
 *
 * Copyright (c) 2011 Juan Jose Garcia Ripoll
 * Copyright (c) 2026 Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/cache.h>
#include <ecl/internal.h>
#include "newhash.h"

#define RECORD_KEY(e) ((e)[0])
#define RECORD_VALUE(e) ((e)[1])
#define RECORD_GEN(e) ecl_fixnum((e+2)[0])
#define RECORD_GEN_SET(e,v) ((e+2)[0]=ecl_make_fixnum(v))

static void
empty_cache(ecl_cache_ptr cache)
{
  cl_object table = cache->table;
  cl_index i, total_size = table->vector.dim;
  cache->generation = 0;
  for (i = 0; i < total_size; i+=3) {
    table->vector.self.t[i] = OBJNULL;
    table->vector.self.t[i+1] = OBJNULL;
    table->vector.self.fix[i+2] = 0;
  }
#ifdef ECL_THREADS
  cache->clear_list = ECL_NIL;
#endif
}

#ifndef ECL_THREADS
static void
clear_one_from_cache(ecl_cache_ptr cache, cl_object target)
{
  cl_object table = cache->table;
  cl_index i, total_size = table->vector.dim;
  for (i = 0; i < total_size; i+=3) {
    cl_object key = table->vector.self.t[i];
    if (key != OBJNULL) {
      if (target == key->vector.self.t[0]) {
        table->vector.self.t[i] = OBJNULL;
        table->vector.self.fix[i+2] = 0;
      }
    }
  }
}
#else
static void
clear_list_from_cache(ecl_cache_ptr cache)
{
  cl_object list = ecl_atomic_get(&cache->clear_list);
  cl_object table = cache->table;
  cl_index i, total_size = table->vector.dim;
  for (i = 0; i < total_size; i+=3) {
    cl_object key = table->vector.self.t[i];
    if (key != OBJNULL) {
      if (ecl_member_eq(key->vector.self.t[0], list)) {
        table->vector.self.t[i] = OBJNULL;
        table->vector.self.fix[i+2] = 0;
      }
    }
  }
}
#endif

ecl_cache_ptr
ecl_make_cache(cl_index key_size, cl_index cache_size)
{
  ecl_cache_ptr cache = ecl_alloc(sizeof(struct ecl_cache));
  cache->table =
    si_make_vector(ECL_T, /* element type */
                   ecl_make_fixnum(3*cache_size), /* Maximum size */
                   ECL_NIL, /* adjustable */
                   ECL_NIL, /* fill pointer */
                   ECL_NIL, /* displaced */
                   ECL_NIL);
  empty_cache(cache);
  return cache;
}

cl_object ecl_cache_make_key(ecl_cache_ptr cache, cl_index len, cl_object *keys)
{
  cl_index idx;
  cl_object key = si_make_vector(ECL_T,
                                 ecl_make_integer(len),
                                 ECL_NIL, /* adjustable */
                                 ECL_NIL, /* fill pointer */
                                 ECL_NIL, /* displaced */
                                 ECL_NIL);
  for(idx=0; idx<len; idx++) {
    key->vector.self.t[idx] = keys[idx];
  }
  return key;
}


void
ecl_cache_remove_one(ecl_cache_ptr cache, cl_object first_key)
{
#ifdef ECL_THREADS
  ecl_atomic_push(&cache->clear_list, first_key);
#else
  clear_one_from_cache(cache, first_key);
#endif
}

static cl_index
vector_hash_key(cl_index n, cl_object *keys)
{
  cl_index c = n, a = GOLDEN_RATIO, b = GOLDEN_RATIO;
  for (; n >= 3; ) {
    c += (cl_index)keys[--n];
    b += (cl_index)keys[--n];
    a += (cl_index)keys[--n];
    mix(a, b, c);
  }
  switch (n) {
  case 2: b += (cl_index)keys[--n];
  case 1: a += (cl_index)keys[--n];
    mix(a,b,c);
  }
  return c;
}


/*
 * variation of ecl_gethash from hash.d, which takes an array of objects as key
 * It also assumes that entries are never removed except by clrhash.  This
 * method must be called with interrupts disabled!
 */

ecl_cache_record_ptr
ecl_search_cache(ecl_cache_ptr cache, cl_index argno, cl_object *keys)
{
#ifdef ECL_THREADS
  if (!Null(cache->clear_list)) {
    clear_list_from_cache(cache);
  }
#endif
  {
    cl_object table = cache->table;
    cl_index idx = vector_hash_key(argno, keys);
    cl_index total_size = table->vector.dim;
    cl_fixnum min_gen, gen;
    cl_object *min_e;
    int k;
    idx = idx % total_size;
    idx = idx - (idx % 3);
    min_gen = cache->generation;
    min_e = 0;
    for (k = 20; k--; ) {
      cl_object *e = table->vector.self.t + idx;
      cl_object hkey = RECORD_KEY(e);
      if (hkey == OBJNULL) {
        min_gen = -1;
        min_e = e;
        if (RECORD_VALUE(e) == OBJNULL) {
          /* This record is not only deleted but empty
           * Hence we cannot find our method ahead */
          break;
        }
        /* Else we only know that the record has been
         * delete, but we might find our data ahead. */
      } else if (argno == hkey->vector.fillp) {
        cl_index n;
        for (n = 0; n < argno; n++) {
          if (keys[n] != hkey->vector.self.t[n])
            goto NO_MATCH;
        }
        min_e = e;
        goto FOUND;
      } else if (min_gen >= 0) {
      NO_MATCH:
        /* Unless we have found a deleted record, keep
         * looking for the oldest record that we can
         * overwrite with the new data. */
        gen = RECORD_GEN(e);
        if (gen < min_gen) {
          min_gen = gen;
          min_e = e;
        }
      }
      idx += 3;
      if (idx >= total_size) idx = 0;
    }
    if (min_e == 0) {
      ecl_internal_error("search_method_hash");
    }
    RECORD_KEY(min_e) = OBJNULL;
    cache->generation++;
  FOUND:
    /*
     * Once we have reached here, we set the new generation of
     * this record and perform a global shift so that the total
     * generation number does not become too large and we can
     * expire some elements.
     */
    gen = cache->generation;
    RECORD_GEN_SET(min_e, gen);
    if (gen >= total_size/2) {
      cl_object *e = table->vector.self.t;
      gen = 0.5*gen;
      cache->generation -= gen;
      for (idx = table->vector.dim; idx; idx-= 3, e += 3) {
        cl_fixnum g = RECORD_GEN(e) - gen;
        if (g <= 0) {
          RECORD_KEY(e) = OBJNULL;
          RECORD_VALUE(e) = ECL_NIL;
          g = 0;
        }
        RECORD_GEN_SET(e, g);
      }
    }
    return (ecl_cache_record_ptr)min_e;
  }
}

void
ecl_update_cache(ecl_cache_ptr cache, cl_index argno, cl_object *keys, cl_object value)
{
  cl_object table = cache->table;
  cl_index idx = vector_hash_key(argno, keys);
  cl_index total_size = table->vector.dim;
  cl_fixnum min_gen, gen;
  cl_object *min_e;
  int k;
  idx = idx % total_size;
  idx = idx - (idx % 3);
  min_gen = cache->generation;
  min_e = NULL;
  /* We look for the fist empty or deleted entry and fill it. */
  for (k = 20; k--; ) {
    cl_object *e = table->vector.self.t + idx;
    cl_object hkey = RECORD_KEY(e);
    if (hkey == OBJNULL) {
      RECORD_KEY(e) = ecl_cache_make_key(cache, argno, keys);
      RECORD_VALUE(e) = value;
      return;
    }
    gen = RECORD_GEN(e);
    if (gen < min_gen) {
      min_gen = gen;
      min_e = e;
    }
    idx += 3;
    if (idx >= total_size) idx = 0;
  }

  RECORD_KEY(min_e) = ecl_cache_make_key(cache, argno, keys);
  RECORD_VALUE(min_e) = value;
  cache->generation++;
  gen = cache->generation;
  RECORD_GEN_SET(min_e, gen);
  /* Once we have reached here perform a global shift so that the total
   * generation number does not become too large and we can expire some
   * elements. */
  if (gen >= total_size/2) {
    cl_object *e = table->vector.self.t;
    gen = 0.5*gen;
    cache->generation -= gen;
    for (idx = table->vector.dim; idx; idx-= 3, e += 3) {
      cl_fixnum g = RECORD_GEN(e) - gen;
      if (g <= 0) {
        RECORD_KEY(e) = OBJNULL;
        RECORD_VALUE(e) = ECL_NIL;
        g = 0;
      }
      RECORD_GEN_SET(e, g);
    }
  }
}

