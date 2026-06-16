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

#define RECORD_SIZE 3
#define RECORD_KEY(e) ((e)[0])
#define RECORD_VAL(e) ((e)[1])
#define RECORD_GEN(e) ecl_fixnum((e+2)[0])
#define RECORD_GEN_SET(e,v) ((e+2)[0]=ecl_make_fixnum(v))

static void
empty_cache(ecl_cache_ptr cache)
{
  cl_object table = cache->table;
  cl_index i, total_size = table->vector.dim;
  for (i = 0; i < total_size; i+=RECORD_SIZE) {
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
  for (i = 0; i < total_size; i+=RECORD_SIZE) {
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
  ecl_disable_interrupts();
  cl_object list = ecl_atomic_get(&cache->clear_list);
  cl_object table = cache->table;
  cl_index i, total_size = table->vector.dim;
  for (i = 0; i < total_size; i+=RECORD_SIZE) {
    cl_object key = table->vector.self.t[i];
    if (key != OBJNULL) {
      if (ecl_member_eq(key->vector.self.t[0], list)) {
        table->vector.self.t[i] = OBJNULL;
        table->vector.self.fix[i+2] = 0;
      }
    }
  }
  ecl_enable_interrupts();
}
#endif

ecl_cache_ptr
ecl_make_cache(cl_index key_size, cl_index cache_size)
{
  ecl_cache_ptr cache = ecl_alloc(sizeof(struct ecl_cache));
  cache->table =
    si_make_vector(ECL_T, /* element type */
                   ecl_make_fixnum(RECORD_SIZE*cache_size), /* Maximum size */
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


/* Variation of ecl_gethash from hash.d, which takes an array of objects as key
 * It also assumes that entries are never removed except by clrhash. */

cl_object
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
    int k;
    idx = idx % total_size;
    idx = idx - (idx % RECORD_SIZE);
    for (k = 16; k--; ) {
      cl_object *e = table->vector.self.t + idx;
      cl_object hkey = RECORD_KEY(e);
      if (hkey == OBJNULL) {
        if (RECORD_VAL(e) == OBJNULL) {
          /* This record is not only deleted but empty
           * Hence we cannot find our method ahead */
          return ECL_NIL;
        }
        /* Else we only know that the record has been
         * delete, but we might find our data ahead. */
      } else if (argno == hkey->vector.fillp) {
        cl_index n;
        for (n = 0; n < argno; n++) {
          if (keys[n] != hkey->vector.self.t[n])
            goto NO_MATCH;
        }
        RECORD_GEN_SET(e, 1);   /* used */
        return RECORD_VAL(e);
      }
    NO_MATCH:
      idx += RECORD_SIZE;
      if (idx >= total_size) idx = 0;
    }
    return ECL_NIL;
  }
}


/* This function is responsible for resizing the cache when we have too many
   entries. When the cache is resized to its maximal size, we evict unused
   entries. Currently LRU, later clocked pseudo-lru. */
#define MAX_CACHE_SIZE 256*4096*RECORD_SIZE
static void
_ecl_resize_cache(ecl_cache_ptr cache)
{
  cl_object table = cache->table;
  cl_index total_size = table->vector.dim;
  cl_index new_size = 1.5*total_size;
  cl_object new_table, key;
  cl_index idx, hash, new_idx, chain_counter;
  cl_object *e1, *e2;
  new_size = new_size - (new_size % RECORD_SIZE);
  if (new_size > MAX_CACHE_SIZE) {
    /* Prune cache by removing turning unused entries into tombtones. */
    for (idx=0; idx<total_size; idx+=RECORD_SIZE) {
      e1 = table->vector.self.t+idx;
      if(!RECORD_GEN(e1)) {
        RECORD_KEY(e1) = OBJNULL;
        RECORD_VAL(e1) = ECL_NIL; /* tombstone */
      } else {
        RECORD_GEN_SET(e1, 0);   /* reset clock */
      }
    }
    return;
  }
  new_table = si_make_vector(ecl_elttype_to_symbol(ecl_array_elttype(table)),
                             ecl_make_integer(new_size),
                             ECL_NIL, ECL_NIL, ECL_NIL, ECL_NIL);
  for (idx = 0; idx < new_size; idx+=RECORD_SIZE) {
    new_table->vector.self.t[idx] = OBJNULL;
    new_table->vector.self.t[idx+1] = OBJNULL;
    new_table->vector.self.fix[idx+2] = 0;
  }
  for (idx = 0; idx<total_size; idx+=3) {
    e1 = table->vector.self.t + idx;
    key = RECORD_KEY(e1);
    if (key == OBJNULL) continue;
    hash = vector_hash_key(key->vector.dim, key->vector.self.t);
    new_idx = hash % new_size;
    new_idx = new_idx - (new_idx % RECORD_SIZE);
    chain_counter=20;
    while (chain_counter--) {
      e2 = new_table->vector.self.t + new_idx;
      if (e2[0] != OBJNULL) {
        new_idx += RECORD_SIZE;
        continue;
      }
      e2[0] = key;
      e2[1] = e1[1];
      e2[2] = e1[2];
      break;
    }
  }
  cache->table = new_table;
}

void
ecl_update_cache(ecl_cache_ptr cache, cl_index argno, cl_object *keys, cl_object value)
{
  ecl_disable_interrupts();
 AGAIN:
  cl_object table = cache->table;
  cl_index idx = vector_hash_key(argno, keys);
  cl_index total_size = table->vector.dim;
  int k;
  cl_object *e, *min_e = NULL;
  idx = idx % total_size;
  idx = idx - (idx % RECORD_SIZE);
  /* We look for the fist empty or deleted entry and fill it. */
  for (k = 16; k--; ) {
    e = table->vector.self.t + idx;
    cl_object hkey = RECORD_KEY(e);
    if (hkey == OBJNULL) {
      RECORD_KEY(e) = ecl_cache_make_key(cache, argno, keys);
      RECORD_VAL(e) = value;
      RECORD_GEN_SET(e, 1);     /* used! */
      goto FINISH;
    } else if (!min_e && !RECORD_GEN(e)) {
      /* Opportunistically we fallback to evicting unused entry when there are
         no empty ones. */
      min_e = e;
    }
    idx += RECORD_SIZE;
    if (idx >= total_size) idx = 0;
  }
  if (!min_e) {
    /* Once we have reached here we know that the chain is either too long and
       we need to resize the table. We pick up max chain length=8. */
    _ecl_resize_cache(cache);
    goto AGAIN;
  }
  RECORD_KEY(min_e) = ecl_cache_make_key(cache, argno, keys);
  RECORD_VAL(min_e) = value;
  RECORD_GEN_SET(min_e, 1);     /* used! */
 FINISH:
  ecl_enable_interrupts();
}

