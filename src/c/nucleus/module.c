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
  printf("Creating the module!\n");
  return ECL_NIL;
}

static cl_object delete() {
  printf("Deleting the module!\n");
  return ECL_NIL;
}

static cl_object enter_thread() {
  printf("Leaving the thread!\n");
  return ECL_NIL;
}

static cl_object leave_thread() {
  printf("Entering the thread!\n");
  return ECL_NIL;
}

static cl_object start_module() {
  printf("Brrrt\n");
  return ECL_NIL;
}

static cl_object close_module() {
  printf("Pffft\n");
  return ECL_NIL;
}

static struct ecl_module dummy_module = {
  .create = create,
  .delete = delete,
  .enter_thread = enter_thread,
  .leave_thread = leave_thread,
  .start_module = start_module,
  .close_module = close_module
};

/* -- implementation --------------------------------------------------------- */

void
ecl_add_module(cl_object self)
{
  /* First pass of the module initialization */
  self->module.create();
  /* For each thread (except main? add a flag!) */
  self->module.enter_thread();
  /* Final pass of the module initialization */
  self->module.start_module();

  ecl_stack_push(ecl_core.modules, self);
}

void
ecl_del_module(cl_object self)
{
  ecl_stack_del(ecl_core.modules, self);

  /* First pass of the module deallocation */
  self->module.close_module();
  /* For each thread (except main? add a flag!) */
  self->module.leave_thread();
  /* Final pass of the module deallocation */
  self->module.delete();
}

void
init_modules()
{
  cl_object self = ecl_malloc(sizeof(struct ecl_stack));
  ecl_stack_init(self, 16, 0);
  ecl_core.modules = self;

  ecl_add_module(&dummy_module);
}
