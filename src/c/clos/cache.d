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

ecl_cache_ptr
ecl_make_cache(cl_index cache_size)
{
  return create_cache(cache_size);
}

cl_object ecl_cache_make_key(cl_index len, cl_object *keys)
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
ecl_cache_invalidate(const cl_env_ptr env, cl_object gf)
{
  /* We first call ECL_GET_GFUN_CACHE to ensure, that the cache
     exists. ECL_SET_GFUN_CACHE quietly assumes that this is true.*/
  ECL_GET_GFUN_CACHE(env, gf);
  ECL_SET_GFUN_CACHE(env, gf, create_cache(1));
}

/* ecl_search_cache is defined directly in the header. */

void
ecl_update_cache(const cl_env_ptr env, cl_object gf,
                 ecl_cache_ptr cache, cl_index hash, cl_index argno,
                 cl_object *keys, cl_object value)
{
  cl_object result;
  ecl_cache_ptr recache;
  ecl_disable_interrupts();
  recache = rehash_cache(cache);  /* checks fill */
  if (recache != cache) {
    cache = recache;
    ECL_SET_GFUN_CACHE(env, gf, cache);
  }
  update_cache(cache, hash, argno, keys, value);
  ecl_enable_interrupts();
}
