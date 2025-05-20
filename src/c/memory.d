/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * memory.c - memory managament
 *
 * Copyright (c) 2024 Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/* -- imports --------------------------------------------------------------- */

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/external.h>

#include <stdlib.h>
#include <string.h>

/* -- implementation -------------------------------------------------------- */

static void
out_of_memory()
{
  /* FIXME we should signal the STORAGE-EXHAUSTED and allow a potential handler
     an attempt to release some memory:

       ecl_cerror(ecl_ct_oom_tag);
       goto AGAIN;

     For now let's crash with an appropriate error. */
  ecl_internal_error("*** memory allocator: out of memory\n");
}

void *
ecl_malloc(cl_index n)
{
  const cl_env_ptr the_env = ecl_process_env_unsafe();
  void *ptr;
  if (!the_env) {
    ptr = malloc(n);
  } else {
    ecl_disable_interrupts_env(the_env);
    ptr = malloc(n);
    ecl_enable_interrupts_env(the_env);
  }
  if (ptr == NULL) out_of_memory();
  return ptr;
}

void
ecl_free(void *ptr)
{
  const cl_env_ptr the_env = ecl_process_env_unsafe();
  if (!the_env) {
    free(ptr);
  } else {
    ecl_disable_interrupts_env(the_env);
    free(ptr);
    ecl_enable_interrupts_env(the_env);
  }
}

void *
ecl_realloc(void *ptr, cl_index osize, cl_index nsize)
{
  const cl_env_ptr the_env = ecl_process_env_unsafe();
  if (!the_env) {
    ptr = realloc(ptr, nsize);
  } else {
    ecl_disable_interrupts_env(the_env);
    ptr = realloc(ptr, nsize);
    ecl_enable_interrupts_env(the_env);
  }
  if (ptr == NULL) out_of_memory();
  return ptr;
}

void
ecl_copy(void *dst, void *src, cl_index ndx)
{
  memcpy(dst, src, ndx);
}

void
ecl_mset(void *ptr, byte c, cl_index n)
{
  memset(ptr, c, n);
}

/* -- Constructors ---------------------------------------------------------- */

cl_object
ecl_alloc_object(cl_type t)
{
  return ecl_core.allocator->allocate_object(t);
}

void *
ecl_alloc_memory(cl_index n)
{
  return ecl_core.allocator->allocate_memory(n);
}

void
ecl_free_object(cl_object ptr)
{
  return ecl_core.allocator->free_object(ptr);
}

void
ecl_free_memory(void *ptr)
{
  return ecl_core.allocator->free_memory(ptr);
}

/* -- Helpers --------------------------------------------------------------- */

cl_object
ecl_cons(cl_object a, cl_object d)
{
  struct ecl_cons *obj = ecl_alloc_memory(sizeof(struct ecl_cons));
#ifdef ECL_SMALL_CONS
  obj->car = a;
  obj->cdr = d;
  return ECL_PTR_CONS(obj);
#else
  obj->t = t_list;
  obj->car = a;
  obj->cdr = d;
  return (cl_object)obj;
#endif
}

cl_object
ecl_append_unsafe(cl_object x, cl_object y)
{
  cl_object head = ECL_NIL, cons;
  cl_object *tail = &head;
  loop_for_on_unsafe(x) {
    cons = ecl_list1(ECL_CONS_CAR(x));
    *tail = cons;
    tail = &ECL_CONS_CDR(cons);
  } end_loop_for_on_unsafe(x);
  *tail = y;
  return head;
}

/* -- Rudimentary manual memory allocator ----------------------------------- */

static cl_object
alloc_object(cl_type t)
{
  ecl_internal_error("*** memory: alloc_object not implemented.\n");
}

static void
free_object(cl_object self)
{
  ecl_internal_error("*** memory: free_object not implemented.\n");
}

struct ecl_allocator_ops manual_allocator = {
  .allocate_memory = ecl_malloc,
  .allocate_object = alloc_object,
  .free_memory = ecl_free,
  .free_object = free_object
};

void
init_memory ()
{
  ecl_core.allocator = &manual_allocator;
}
