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

#define RECORD_SIZE 4

#define RECORD_ARG(e) ((e)[0])
#define RECORD_KEY(e) ((e)[1])
#define RECORD_VAL(e) ((e)[2])
#define RECORD_CLK(e) ((e)[3])

#define RECORD_GEN(e) ecl_fixnum((e+3)[0])
#define RECORD_GEN_SET(e,v) ((e+3)[0]=ecl_make_fixnum(v))

static void
empty_cache(ecl_cache_ptr cache)
{
  cl_object table = cache->table;
  cl_index i, total_size = table->vector.dim;
  for (i = 0; i < total_size; i+=RECORD_SIZE) {
    table->vector.self.t[i+0] = OBJNULL;
    table->vector.self.t[i+1] = OBJNULL;
    table->vector.self.t[i+2] = OBJNULL;
    table->vector.self.fix[i+3] = 0;
  }
}

ecl_cache_ptr
ecl_make_cache(cl_index cache_size)
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
ecl_cache_remove_one(ecl_cache_ptr cache, cl_object any_key)
{
  ecl_internal_error("not implemented");
}

void
ecl_cache_invalidate(ecl_cache_ptr cache)
{
  empty_cache(cache);
}

/* Variation of ecl_gethash from hash.d, which takes an array of objects as key
 * It also assumes that entries are never removed except by clrhash. */

cl_object
ecl_search_cache(ecl_cache_ptr cache, cl_index hash, cl_index argno, cl_object *keys)
{
  cl_object table = cache->table;
  cl_index total_size = table->vector.dim;
  cl_index idx = hash % total_size;
  cl_index k;
  idx = idx - (idx % RECORD_SIZE);
  for (k = 16; k--; ) {
    cl_object *e = table->vector.self.t + idx;
    cl_object harg = RECORD_ARG(e);
    cl_object hkey = RECORD_KEY(e);
    if (hkey == OBJNULL) {
      if (RECORD_VAL(e) == OBJNULL) {
        /* This record is not only deleted but empty
         * Hence we cannot find our method ahead */
        return ECL_NIL;
      }
      /* Else we only know that the record has been
       * delete, but we might find our data ahead. */
    } else if (argno == 0) {
      /* This case should be handled direclty by the discriminator. */
      RECORD_GEN_SET(e, 1);
      return RECORD_VAL(e);
    } else if (keys[0] == harg) {
      cl_index n;
      cl_object *vkey = hkey->vector.self.t;
      for (n = 1; n < argno; n++) {
        if (keys[n] != vkey[n])
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
  cl_object new_table, key, arg;
  cl_index idx, hash, new_idx, chain_counter;
  cl_object *e1, *e2;
  new_size = new_size - (new_size % RECORD_SIZE);
  if (new_size > MAX_CACHE_SIZE) {
    /* Prune cache by removing turning unused entries into tombtones. */
    for (idx=0; idx<total_size; idx+=RECORD_SIZE) {
      e1 = table->vector.self.t+idx;
      if(!RECORD_GEN(e1)) {
        RECORD_ARG(e1) = OBJNULL;
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
    e2 = new_table->vector.self.t+idx;
    RECORD_ARG(e2) = OBJNULL;
    RECORD_KEY(e2) = OBJNULL;
    RECORD_VAL(e2) = OBJNULL; /* empty */
    RECORD_GEN_SET(e2, 0);    /* reset clock */
  }
  for (idx = 0; idx<total_size; idx+=3) {
    e1 = table->vector.self.t + idx;
    arg = RECORD_ARG(e1);
    key = RECORD_KEY(e1);
    if (key == OBJNULL) continue;
    hash = vector_hash_keys(key->vector.dim, key->vector.self.t);
    new_idx = hash % new_size;
    new_idx = new_idx - (new_idx % RECORD_SIZE);
    chain_counter=20;
    while (chain_counter--) {
      e2 = new_table->vector.self.t + new_idx;
      if (RECORD_KEY(e2) != OBJNULL) {
        new_idx += RECORD_SIZE;
        continue;
      }
      RECORD_ARG(e2) = arg;
      RECORD_KEY(e2) = key;
      RECORD_VAL(e2) = RECORD_VAL(e1);
      RECORD_CLK(e2) = RECORD_CLK(e1);
      break;
    }
  }
  cache->table = new_table;
}

void
ecl_update_cache(ecl_cache_ptr cache, cl_index hash, cl_index argno, cl_object *keys, cl_object value)
{
  ecl_disable_interrupts();
 AGAIN:
  cl_object table = cache->table;
  cl_index total_size = table->vector.dim;
  cl_object *e, *min_e = NULL;
  cl_index idx = hash % total_size;
  cl_index k;
  idx = idx - (idx % RECORD_SIZE);
  /* We look for the fist empty or deleted entry and fill it. */
  for (k = 16; k--; ) {
    e = table->vector.self.t + idx;
    cl_object harg = RECORD_ARG(e);
    cl_object hkey = RECORD_KEY(e);
    if (hkey == OBJNULL) {
      RECORD_ARG(e) = argno ? keys[0] : OBJNULL;
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
  RECORD_ARG(min_e) = argno ? keys[0] : OBJNULL;
  RECORD_KEY(min_e) = ecl_cache_make_key(cache, argno, keys);
  RECORD_VAL(min_e) = value;
  RECORD_GEN_SET(min_e, 1);     /* used! */
 FINISH:
  ecl_enable_interrupts();
}
