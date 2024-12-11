/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

#ifndef ECL_NUCLEUS_H
#define ECL_NUCLEUS_H

#include "external.h"

struct ecl_core_struct {
  cl_env_ptr first_env;
#ifdef ECL_THREADS
  cl_object threads;
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
void ecl_escape(cl_object continuation) ecl_attr_noreturn;
cl_object ecl_signal(cl_object condition, cl_object returns, cl_object thread);
cl_object ecl_call_with_handler(cl_object handler, cl_object continuation);

/* Binding a handler conses a new list, but at this stage we don't assume the
   the garbage collector to work! Luckily the extent of the binding is dynamic
   and we can allocate cons on the stack. */
#define ECL_WITH_HANDLER_BEGIN(the_env, handler) do {                   \
  const cl_env_ptr __the_env = the_env;                                 \
  cl_object __ecl_sym = ECL_SIGNAL_HANDLERS;                            \
  cl_object __ecl_hnd = ECL_SYM_VAL(__the_env, __ecl_sym);              \
  cl_object __ecl_hnds = ecl_cons_stack(handler, __ecl_hnd);            \
  ecl_bds_bind(__the_env, __ecl_sym, __ecl_hnds);

#define ECL_WITH_HANDLER_END ecl_bds_unwind1(__the_env); } while(0)

cl_object ecl_raise(ecl_ex_type t, bool ret,
                    cl_object a1, cl_object a2, cl_object a3, void *a4);

cl_object ecl_ferror(ecl_ex_type extype, cl_object type, cl_object args);
cl_object ecl_cerror(ecl_ex_type extype, cl_object type, cl_object args);

#endif  /* ECL_NUCLEUS_H */
