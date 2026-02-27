/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/* mem_nogc.d - this is a reference of what GC should minimally implement */

/* -- imports ---------------------------------------------------------------- */

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/external.h>

/* -- implementation --------------------------------------------------------- */

void _ecl_set_max_heap_size(size_t new_size) {}
void ecl_register_root(cl_object *p) {}
void ecl_set_finalizer_unprotected(cl_object o, cl_object f) {}

cl_object
si_gc(cl_narg narg, ...)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return0(the_env);
}

cl_object
si_get_finalizer(cl_object o)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return0(the_env);
}

cl_object
si_set_finalizer(cl_object o, cl_object f)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return0(the_env);
}

cl_object
si_make_weak_pointer(cl_object o)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ECL_NIL);
}

cl_object
si_weak_pointer_value(cl_object o)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return2(the_env, ECL_NIL, ECL_NIL);
}

ecl_def_ct_base_string(str_gc, "NO-GC", 5, static, const);

static struct ecl_module module_gc = {
  .t = t_module,
  .name = str_gc,
  .create = ecl_module_no_op,
  .enable = ecl_module_no_op,
  .init_env = ecl_module_no_op_env,
  .init_cpu = ecl_module_no_op_cpu,
  .free_cpu = ecl_module_no_op_cpu,
  .free_env = ecl_module_no_op_env,
  .disable = ecl_module_no_op,
  .destroy = ecl_module_no_op
};

cl_object ecl_module_gc = (cl_object)&module_gc;
