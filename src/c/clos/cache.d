/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * cache.d - thread-local cache for a variety of operations
 *
 * Copyright (c) 2011 Juan Jose Garcia Ripoll
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

ecl_cache_ptr
ecl_make_cache(cl_index key_size, cl_index cache_size)
{
  ecl_cache_ptr cache = ecl_alloc(sizeof(struct ecl_cache));
  cl_index i, total_size = 3*cache_size;
  cl_object table = si_make_vector(ECL_T, /* element type */
                                   ecl_make_fixnum(total_size), /* Maximum size */
                                   ECL_NIL, /* adjustable */
                                   ECL_NIL, /* fill pointer */
                                   ECL_NIL, /* displaced */
                                   ECL_NIL);
  cache->key_length = key_size;
  cache->table = table;
  cache->generation = 0;
  for (i = 0; i < total_size; i+=3) {
    table->vector.self.t[i] = OBJNULL;
    table->vector.self.t[i+1] = OBJNULL;
    table->vector.self.fix[i+2] = 0;
  }
  return cache;
}

void
ecl_cache_update_record(ecl_cache_ptr cache, ecl_cache_record_ptr record,
                        cl_object *keys, cl_object value)
{
  cl_index idx, key_length = cache->key_length;
  cl_object key = si_make_vector(ECL_T,
                                 ecl_make_integer(key_length),
                                 ECL_NIL, /* adjustable */
                                 ECL_NIL, /* fill pointer */
                                 ECL_NIL, /* displaced */
                                 ECL_NIL);
  for(idx=0; idx<key_length; idx++) {
    key->vector.self.t[idx] = keys[idx];
  }
  record->key = key;
  record->value = value;
}

void
ecl_update_cache(ecl_cache_ptr cache, cl_index hash,
                 cl_object *keys, cl_index argno, cl_object value)
{
  
}

/*
 * variation of ecl_gethash from hash.d, which takes an array of objects as key
 * It also assumes that entries are never removed except by clrhash.
 * This method must be called with interrupts disabled!
 */

ecl_cache_record_ptr
ecl_search_cache(ecl_cache_ptr cache, cl_index hash, cl_object *keys, cl_index argno)
{
  cl_object table = cache->table;
  cl_index idx = hash;
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
    } else {
      cl_index n;
      for (n = 0; n < argno; n++) {
        if (keys[n] != hkey->vector.self.t[n]) {
          goto NO_MATCH;
        }
      }
      min_e = e;
      goto FOUND;
    }
    NO_MATCH:
      /* Unless we have found a deleted record, keep
       * looking for the oldest record that we can
       * overwrite with the new data. */
      gen = RECORD_GEN(e);
      if (gen < min_gen) {
        min_gen = gen;
        min_e = e;
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
    for (idx = table->vector.dim; idx; idx -= 3, e += 3) {
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

