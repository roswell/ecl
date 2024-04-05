/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * memory.c - manual memory managament
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
  ecl_internal_error("*** manual memory allocator: out of memory\n");
}

void *
ecl_malloc(cl_index n)
{
  /* GC-free equivalent of ecl_alloc_atomic. */
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

void *
ecl_realloc(void *ptr, cl_index n)
{
  const cl_env_ptr the_env = ecl_process_env_unsafe();
  if (!the_env) {
    ptr = realloc(ptr, n);
  } else {
    ecl_disable_interrupts_env(the_env);
    ptr = realloc(ptr, n);
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

void
ecl_copy(void *dst, void *src, cl_index ndx)
{
  memcpy(dst, src, ndx);
}
