/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

#ifndef ECL_NUCLEUS_H
#define ECL_NUCLEUS_H

#include <unistd.h>
#include "external.h"

struct ecl_core_struct {
  cl_env_ptr first_env;
#ifdef ECL_THREADS
  cl_env_ptr *threads;
  cl_index nthreads;
  cl_index sthreads;
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

  cl_index default_sigmask_bytes;
  cl_object known_signals;

  int path_max;
  cl_object pathname_translations;

  cl_object modules;
  cl_object libraries;
  cl_object library_pathname;
};

/* process.c */
cl_env_ptr ecl_adopt_cpu();
cl_env_ptr ecl_spawn_cpu();
void ecl_disown_cpu();

/* control.c */
cl_object ecl_escape(cl_object continuation) ecl_attr_noreturn;
cl_object ecl_signal(cl_object condition, cl_object returns, cl_object thread);
cl_object ecl_call_with_handler(cl_object handler, cl_object continuation);

#define ECL_WITH_HANDLER_BEGIN(the_env, handler) do {                   \
  cl_object __ecl_hnd = ecl_cast_ptr(cl_object,&the_env->hnd_stack);    \
  cl_object __ecl_sym = ECL_SIGNAL_HANDLERS;                            \
  cl_object __ecl_hnd = ECL_SYM_VAL(the_env, symbol);                   \
  struct ecl_cons __ecl_hnds = { .t = t_list,                           \
                                 .car = __ecl_hnd,                      \
                                 .cdr = __ecl_sym };                    \
  ecl_bds_bind(__the_env, __ecl_sym, ecl_cast_ptr(cl_object,__ecl_hnds);

#define ECL_WITH_HANDLER_END ecl_bds_unwind1(the_env); } while(0)

cl_object ecl_raise(ecl_ex_type t, bool ret, cl_object a1, cl_object a2);

cl_object ecl_ferror(ecl_ex_type extype, cl_object type, cl_object args);
cl_object ecl_cerror(ecl_ex_type extype, cl_object type, cl_object args);

#endif  /* ECL_NUCLEUS_H */
