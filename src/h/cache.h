/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
 * Copyright (c) 2011, Juan Jose Garcia Ripoll.
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/* cache.h -- thread-local cache for a variety of operations */

#ifndef ECL_CACHE_H
#define ECL_CACHE_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct ecl_cache {
        cl_object table;
#ifdef ECL_THREADS
        cl_object clear_list;
#endif
} *ecl_cache_ptr;

typedef struct ecl_cache_record {
        cl_object key; /* vector[ndx] */
        cl_object value; /* vector[ndx+1] */
        cl_object gen; /* vector[ndx+2] */
} *ecl_cache_record_ptr;

extern ecl_cache_ptr ecl_make_cache(cl_index cache_size);
extern cl_object ecl_cache_make_key(ecl_cache_ptr cache, cl_index len, cl_object *keys);

extern cl_object ecl_search_cache(ecl_cache_ptr cache, cl_index argno, cl_object *keys);
extern void ecl_update_cache(ecl_cache_ptr cache, cl_index argno, cl_object *keys, cl_object value);
extern void ecl_cache_remove_one(ecl_cache_ptr cache, cl_object first_key);


#ifdef ECL_THREADS
static inline cl_object
ecl_bds_get_value(const cl_env_ptr env, cl_object gf)
{
        cl_object cache;
        cl_index index = gf->instance.binding;
        if (index >= env->bds_stack.tl_bindings_size) {
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
                cache = (cl_object)ecl_make_cache(128);
                env->bds_stack.tl_bindings[index] = cache;
                return cache;

        }
        cache = env->bds_stack.tl_bindings[index];
        if (cache == ECL_NO_TL_BINDING) {
                cache = (cl_object)ecl_make_cache(128);
                env->bds_stack.tl_bindings[index] = cache;
        }
        return cache;
}

# define ECL_GFUN_CACHE(env,g) (ecl_cache_ptr)(ecl_bds_get_value((env),(g)))
#else
# define ECL_GFUN_CACHE(env,g) ((g)->instance.method_cache)
#endif

#ifdef __cplusplus
}
#endif


#endif /* !ECL_CACHE_H */
