/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/* module.c - managing runtime modules */

/* -- imports ---------------------------------------------------------------- */
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/external.h>

/* -- test module ------------------------------------------------------------ */

static cl_object create() {
  printf("DUMMY: Creating the module!\n");
  return ECL_NIL;
}

static cl_object enable() {
  printf("DUMMY: Enabling the module!\n");
  return ECL_NIL;
}

static cl_object init_env(cl_env_ptr the_env) {
#ifdef ECL_THREADS
  ecl_thread_t thread_id = the_env->thread;
  printf("DUMMY: init_env [cpu %p env %p]\n", &thread_id, the_env);
#else
  printf("DUMMY: init_env [env %p]\n", the_env);
#endif
  return ECL_NIL;
}

static cl_object init_cpu(cl_env_ptr the_env) {
#ifdef ECL_THREADS
  ecl_thread_t thread_id = the_env->thread;
  printf("DUMMY: init_cpu [cpu %p env %p]\n", &thread_id, the_env);
#else
  printf("DUMMY: init_cpu [env %p]\n", the_env);
#endif
  return ECL_NIL;
}

static cl_object free_cpu(cl_env_ptr the_env) {
#ifdef ECL_THREADS
  ecl_thread_t thread_id = the_env->thread;
  printf("DUMMY: free_cpu [cpu %p env %p]\n", &thread_id, the_env);
#else
  printf("DUMMY: free_cpu [env %p]\n", the_env);
#endif
  return ECL_NIL;
}

static cl_object free_env(cl_env_ptr the_env) {
#ifdef ECL_THREADS
  ecl_thread_t thread_id = the_env->thread;
  printf("DUMMY: free_env [cpu %p env %p]\n", &thread_id, the_env);
#else
  printf("DUMMY: free_env [env %p]\n", the_env);
#endif
  return ECL_NIL;
}

static cl_object disable() {
  printf("DUMMY: Disabling the module!\n");
  return ECL_NIL;
}

static cl_object destroy() {
  printf("DUMMY: Destroying the module!\n");
  return ECL_NIL;
}

ecl_def_ct_base_string(str_dummy, "DUMMY", 5, static, const);

static struct ecl_module module_dummy = {
  .name = str_dummy,
  .create = create,
  .enable = enable,
  .init_env = init_env,
  .init_cpu = init_cpu,
  .free_cpu = free_cpu,
  .free_env = free_env,
  .disable = disable,
  .destroy = destroy
};

cl_object ecl_module_dummy = (cl_object)&module_dummy;

/* -- implementation --------------------------------------------------------- */

cl_object
ecl_module_no_op()
{
  return ECL_NIL;
}

cl_object
ecl_module_no_op_env(cl_env_ptr the_env)
{
  return ECL_NIL;
}

cl_object
ecl_module_no_op_cpu(cl_env_ptr the_env)
{
  return ECL_NIL;
}

cl_object
ecl_add_module(cl_object self)
{
  self->module.create();
  self->module.init_cpu(ecl_core.first_env);
  self->module.init_env(ecl_core.first_env);
  ecl_stack_push(ecl_core.modules, self);
  return ECL_NIL;
}

cl_object
ecl_del_module(cl_object self)
{
  ecl_stack_del(ecl_core.modules, self);
  self->module.disable();
  self->module.free_env(ecl_core.first_env);
  self->module.free_cpu(ecl_core.first_env);
  self->module.destroy();
  return ECL_NIL;
}

cl_object
ecl_modules_init_env(cl_env_ptr the_env) {
  loop_across_stack_fifo(var, ecl_core.modules) {
    /* printf("> init_env: %s\n", (var->module.name)->base_string.self); */
    var->module.init_env(the_env);
    /* printf("< init_env: %s\n", (var->module.name)->base_string.self); */
  } end_loop_across_stack();
  return ECL_NIL;
}

cl_object
ecl_modules_init_cpu(cl_env_ptr the_env) {
  loop_across_stack_fifo(var, ecl_core.modules) {
    /* printf("> init_cpu: %s\n", (var->module.name)->base_string.self); */
    var->module.init_cpu(the_env);
    /* printf("< init_cpu: %s\n", (var->module.name)->base_string.self); */
  } end_loop_across_stack();
  return ECL_NIL;
}

cl_object
ecl_modules_free_cpu(cl_env_ptr the_env) {
  loop_across_stack_filo(var, ecl_core.modules) {
    /* printf("> free_cpu: %s\n", (var->module.name)->base_string.self); */
    var->module.free_cpu(the_env);
    /* printf("< free_cpu: %s\n", (var->module.name)->base_string.self); */
  } end_loop_across_stack();
  return ECL_NIL;
}

cl_object
ecl_modules_free_env(cl_env_ptr the_env) {
  loop_across_stack_filo(var, ecl_core.modules) {
    /* printf("> free_env: %s\n", (var->module.name)->base_string.self); */
    var->module.free_env(the_env);
    /* printf("< free_env: %s\n", (var->module.name)->base_string.self); */
  } end_loop_across_stack();
  return ECL_NIL;
}

/* INV all modules must be loaded before we make new threads.  */
/* FIXME enforce this invariant. */
void
init_modules()
{
  cl_object self = ecl_make_stack(16);
  ecl_core.modules = self;
}
