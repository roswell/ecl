/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

#ifndef ECL_NUCLEUS_H
#define ECL_NUCLEUS_H

#include "external.h"

struct ecl_core_struct {
        cl_env_ptr first_env;
#ifdef ECL_THREADS
        cl_object processes;
        ecl_mutex_t processes_lock;
        ecl_mutex_t global_lock;
        ecl_mutex_t error_lock;
        ecl_rwlock_t global_env_lock;
        cl_index last_var_index;
        cl_object reused_indices;
#endif
        size_t max_heap_size;
        cl_object bytes_consed;
        cl_object gc_counter;
        bool gc_stats;
        char *safety_region;

        void *default_sigmask;
        cl_index default_sigmask_bytes;
        cl_object known_signals;

        int path_max;
        cl_object pathname_translations;

        cl_object libraries;
        cl_object library_pathname;
};

extern ECL_API void             /* try all handlers and abort */
ecl_signal(cl_object condition, cl_object continuation, cl_object thread);

#endif  /* ECL_NUCLEUS_H */
