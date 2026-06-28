/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * Copyright (c) 2011, Juan Jose Garcia Ripoll
 * Copyright (c) 2026, Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/* cache.h -- thread-local cache for generic function dispatch. This cache is
   optimized for 64B cache line ailgnment and is inspired by Swiss Tables. */

#ifndef ECL_CACHE_H
#define ECL_CACHE_H

#include <ecl/internal.h>
#include "xxhash.h"

#ifdef __cplusplus
extern "C" {
#endif

static cl_index
vector_hash_keys(cl_index n, cl_object *keys)
{
  return xxh64((uint64_t *)keys, n, 0);
}

static cl_index
vector_hash_key1(cl_object *keys)
{
  return xxh64_small((uint64_t *)keys, 1, 0);
}

/*   */

#define RESIZE_LIMIT (1<<12)

#define CONTROLBIT 0x80         /* denotes special meaning */
#define FINGERMASK 0x7F         /* a mask for the fingerprint  */

#define EMPTYSLOT 0x80          /* empty entry */
#define TOMBSTONE 0xfe          /* freed entry */
#define RESERVED  0xfd          /* header meta  */
#define SENTINEL  0xff          /* not used yet */

/* [head] stores hash fingerprints of elements. Each fingerprint is 8 bits,
   where 1b is reserved to signify a special state.

   ------------------------------------------
   #b0xxx xxxx :: occupied (7bit fingerprint)
   #b1xxx xxxx :: special states        [vvv]
   ------------------------------------------
   #b1000 0000 :: empty
   #b1111 1110 :: tombstone
   #b1111 1101 :: reserved
   #b1111 1111 :: sentinel
   ------------------------------------------

   When computing the fingerprint, we want to take highest bits to avoid
   collisions with indexes.
*/
static inline uint8_t
hash_fingerprint(uint64_t hash)
{
  return (hash >> 57) & FINGERMASK;
}

typedef struct {
  cl_object key;
  cl_object val;
} ecl_cache_record;

/*
  The cache bucket is an organizational unit of the dispatch table. The first 16
  bytes represent metadata about the bucket and elements followed by 15 records.

  [head][elem][elem][elem]
  [elem][elem][elem][elem]
  [elem][elem][elem][elem]
  [elem][elem][elem][elem]

  head[0] is currently not used and we can use it in the future for something.
  Like we can store here a probe max distance, or store a lock for the bucket.
*/
typedef struct {
  uint8_t head[16];
  ecl_cache_record tail[15];
} ecl_cache_bucket;


/*
  The cache header occuplies exactly 64B. [meta] contains size and fill counters
  and remaining slots are at a disposal of the client who uses the table. They
  may be used for example to implement PIC cache.

  [meta][slot][slot][slot]

  This is what we initially allocate instaed of a dispatch table. Functions that
  are never called won't extend beyond the header. It is a responsibility of
  ecl_update_cache to resize the table to fit the element.

   size == 0 -> header
   size >= 1 -> header + buckets
*/
typedef struct {
  uint64_t size;                /* 1 : number of buckets */
  uint64_t fill;                /* 1 : number of records */
  cl_object slot[6];            /* 6 : general purpose slots */
} ecl_cache_header, *ecl_cache_ptr;


/* General purpose cache. */

static inline void
clear_cache(ecl_cache_ptr cache)
{
  uint64_t cache_size = cache->size;
  ecl_cache_bucket *table=(ecl_cache_bucket *)(cache+1);
  /* Initialize header. */
  cache->size = cache_size;
  cache->fill = 0;
  for(int j=0; j<6; j++)
    cache->slot[j]=OBJNULL;
  /* Initialize buckets. */
  for(int i=0; i<cache_size; i++)
    for(int j=0; j<16; j++)
      table[i].head[j]=EMPTYSLOT;
}

static inline ecl_cache_ptr
ecl_make_cache(uint64_t cache_size)
{
  /* Our indexing is per bucket. To avoid division on each probe, sizes are
     powers of two. Size=0 means that we have allocated only the feeler. */
  ecl_cache_ptr cache = ecl_alloc(sizeof(ecl_cache_header) +
                                  sizeof(ecl_cache_bucket) * cache_size);
  cache->size = cache_size;
  clear_cache(cache);
  return cache;
}

static inline bool
keys_match(int argno, cl_object *keys, cl_object *record_keys)
{
  switch(argno) {
  case 0: return true;
  case 1: return keys[0] == record_keys[0];
  case 2: return (keys[0] == record_keys[0] &&
                  keys[1] == record_keys[1] );
  case 3: return (keys[0] == record_keys[0] &&
                  keys[1] == record_keys[1] &&
                  keys[2] == record_keys[2] );
  case 4: return (keys[0] == record_keys[0] &&
                  keys[1] == record_keys[1] &&
                  keys[2] == record_keys[2] &&
                  keys[3] == record_keys[3] );
  default:
    for (int i=0; i<argno; i++) {
      if (keys[i] != record_keys[i])
        return false;
    }
    return true;
  }
}

static inline cl_object
ecl_search_cache(ecl_cache_ptr cache, uint64_t hash, cl_index argno, cl_object *keys)
{
  ecl_cache_record *record;
  cl_object *record_key;
  uint64_t size = cache->size;
  if (size==0) return OBJNULL;
  uint8_t fp = hash_fingerprint(hash);
  ecl_cache_bucket *table=(ecl_cache_bucket *)(cache+1), *bucket;
  uint64_t index = hash & (size-1);
  uint64_t probe_count = size;
  while(probe_count--) {
    bucket = table+index;
    for (int idx=1; idx<16; idx++) {
      uint8_t ctrl = bucket->head[idx];
      if (ctrl == EMPTYSLOT) return OBJNULL; /* empty */
      if (ctrl & CONTROLBIT) continue;       /* special */
      if (ctrl != fp)   continue;       /* mismatch */
      record = &bucket->tail[idx-1];
      record_key = record->key->vector.self.t;
      likely_if (keys_match(argno, keys, record_key)) {
        return record->val;
      }
    }
    index = (index+1) & (size-1);
  }
  return OBJNULL;
}

static inline cl_object
make_key(cl_index len, cl_object *keys)
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

static inline void
insert_cache(ecl_cache_ptr cache, uint64_t hash,
             cl_index argno, cl_object *keys, cl_object val)
{
  uint64_t size = cache->size;
  uint8_t fp = hash_fingerprint(hash);
  ecl_cache_bucket *table=(ecl_cache_bucket *)(cache+1), *bucket;
  uint64_t index = hash & (size-1);
  uint64_t probe_count = size;
  while (probe_count--) {
    bucket = table+index;
    for (int idx=1; idx<16; idx++) {
      uint8_t ctrl = bucket->head[idx];
      /* This simple test relies on not upserting, that is we update the cache
         only on cache miss (or when we rehash the whole hash table). Otherwise
         we would need be see if they key already exists until EMPTYSLOT. */
      if (ctrl == EMPTYSLOT || ctrl == TOMBSTONE) {
        bucket->head[idx] = fp;
        bucket->tail[idx-1].key = make_key(argno, keys);
        bucket->tail[idx-1].val = val;
        cache->fill++;
        return;
      }
    }
    index = (index+1) & (size-1);
  }
  /* never reached */
}

static inline void
upsert_cache(ecl_cache_ptr cache, uint64_t hash,
             cl_index argno, cl_object *keys, cl_object val)
{
  ecl_internal_error("upsert: not implemented yet.");
}

static inline void
delete_cache(ecl_cache_ptr cache, uint64_t hash,
             cl_index argno, cl_object *keys, cl_object val)
{
  ecl_internal_error("delete: not implemented yet.");
}

/* A few notes about cache overflow, resizing the table and eviction. The caller
   of update_cache is responsible for checking fill against the size and decide
   whether we should make more room.

   The table size is always a power of two to avoid division when we compute the
   next index. The size limit is a parameter (i.e 2^12), and the size is a
   number of buckets (including header). The fill is the number of records.

   Until we reach the limit, we resize when fill reaches 7/8. After we reach the
   limit, we only rehash and skip every fourth live record when inserting. This
   reduces the fill count, shortens chains and removes tombstones. */
static inline ecl_cache_ptr
rehash_cache(ecl_cache_ptr cache)
{
  uint64_t size = cache->size;
  uint64_t fill = cache->fill;
  uint64_t capacity = 15*size;
  uint64_t limit = (7*capacity)/8;

  if (size == 0) {              /* only feeler */
    ecl_cache_ptr new_cache = ecl_make_cache(1);
    /* Copy slots and fix the header. */
    *new_cache = *cache;
    new_cache->size = 1;
    new_cache->fill = 0;
    return new_cache;
  }

  if (fill < limit) {           /* we have space */
    return cache;
  }

  ecl_cache_ptr new_cache = NULL;
  ecl_cache_bucket *old_table = (ecl_cache_bucket *)(cache+1);
  /* Eviction always goes through the same indexes, but this is fine, because we
     have a strong randomized hash. Even assuming that we don't resize, evicting
     items moves the next item in the bucket one position up, so that item will
     be evicted next time.  */
  uint64_t new_size, evict_clock;
  bool evictp;
  if (size < RESIZE_LIMIT) {
    evict_clock = 0;
    evictp = 0;
    new_size = size<<1;
  } else {
    evict_clock = 0;
    evictp = 1;
    new_size = size;
  }

  new_cache = ecl_make_cache(new_size);
  /* Copy slots and fix the header. */
  *new_cache = *cache;
  new_cache->size = new_size;
  new_cache->fill = 0;
  /* Insert live bucket elements. */
  for(int i = 0; i<size; i++) {
    for(int idx = 1; idx<16; idx++) {
      uint8_t ctrl = old_table[i].head[idx];
      if (ctrl == EMPTYSLOT) break;    /* nothing after this point */
      if (ctrl & CONTROLBIT) continue; /* special marker (i.e tomb) */
      if (evictp && !(evict_clock++ & 3)) continue; /* skip  */
      ecl_cache_record *record = &old_table[i].tail[idx-1];
      cl_object key = record->key;
      cl_object val = record->val;
      uint64_t key_length = key->vector.dim;
      cl_object *key_vector = key->vector.self.t;
      uint64_t hash = vector_hash_keys(key_length, key_vector);
      insert_cache(new_cache, hash, key_length, key_vector, val);
    }
  }
  return new_cache;
}



#ifdef ECL_THREADS
static inline cl_object
ecl_bds_get_value(const cl_env_ptr env, cl_object gf)
{
  cl_object cache;
  cl_index index = gf->instance.binding;
  unlikely_if (index >= env->bds_stack.tl_bindings_size) {
    index = ecl_atomic_index_incf(&ecl_core.last_var_index);
    gf->instance.binding = index;
    if(index >= env->bds_stack.tl_bindings_size) {
      cl_index osize = env->bds_stack.tl_bindings_size;
      cl_index nsize = ecl_core.last_var_index * 1.25;
      cl_object *old_vector = env->bds_stack.tl_bindings;
      cl_object *new_vector = ecl_realloc(old_vector,
                                          osize*sizeof(cl_object*),
                                          nsize*sizeof(cl_object*));
      while(osize < nsize) {
        new_vector[osize++] = ECL_NO_TL_BINDING;
      }
      env->bds_stack.tl_bindings = new_vector;
      env->bds_stack.tl_bindings_size = nsize;
    }
  }
  cache = env->bds_stack.tl_bindings[index];
  unlikely_if (cache == ECL_NO_TL_BINDING) {
    cache = (cl_object)ecl_make_cache(0);
    env->bds_stack.tl_bindings[index] = cache;
  }
  return cache;
}

static inline void              /* INV already bound */
ecl_bds_set_value(const cl_env_ptr env, cl_object gf, cl_object cache)
{
  cl_index index = gf->instance.binding;
  env->bds_stack.tl_bindings[index] = cache;
}
# define ECL_GET_GFUN_CACHE(env,g)   (ecl_cache_ptr)(ecl_bds_get_value((env),(g)))
# define ECL_SET_GFUN_CACHE(env,g,v) (ecl_bds_set_value((env),(g),(cl_object)(v)))
#else
# define ECL_GET_GFUN_CACHE(env,g)   ((g)->instance.method_cache)
# define ECL_SET_GFUN_CACHE(env,g,v) ((g)->instance.method_cache=(v))
#endif

#ifdef __cplusplus
}
#endif



static inline void
ecl_update_cache(const cl_env_ptr env, cl_object gf,
                 ecl_cache_ptr cache, cl_index hash, cl_index argno,
                 cl_object *keys, cl_object value)
{
  ecl_cache_ptr recache;
  ecl_disable_interrupts();
  recache = rehash_cache(cache);  /* checks fill */
  if (recache != cache) {
    cache = recache;
    ECL_SET_GFUN_CACHE(env, gf, cache);
  }
  insert_cache(cache, hash, argno, keys, value);
  ecl_enable_interrupts();
}

static inline void
ecl_cache_invalidate(const cl_env_ptr env, cl_object gf)
{
  /* We first call ECL_GET_GFUN_CACHE to ensure, that the cache
     exists. ECL_SET_GFUN_CACHE quietly assumes that this is true.*/
  ECL_GET_GFUN_CACHE(env, gf);
  ECL_SET_GFUN_CACHE(env, gf, ecl_make_cache(1));
}


#endif /* !ECL_CACHE_H */
