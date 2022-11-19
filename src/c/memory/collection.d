/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * collection.c - garbage collector configuration
 *
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 * Copyright (c) 2022 Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/threads.h>
#include <ecl/internal.h>
#include <gc/gc_mark.h>

static void (*GC_old_start_callback)(void) = NULL;
static void gather_statistics(void);
static void update_bytes_consed(void);
static void ecl_mark_env(struct cl_env_struct *env);

extern void (*GC_push_other_roots)();
static void (*old_GC_push_other_roots)();
static void stacks_scanner();

static void no_warnings(char *msg, GC_word arg);
static void *out_of_memory(size_t requested_bytes);
 
#ifdef GBC_BOEHM_PRECISE
# if GBC_BOEHM
#  undef GBC_BOEHM_PRECISE
# else
#  include <gc/gc_typed.h>
#  define GBC_BOEHM_OWN_MARKER
static int cl_object_kind, cl_object_mark_proc_index;
static void **cl_object_free_list;
extern void GC_init_explicit_typing(void);
# endif
#endif

void
_ecl_set_max_heap_size(size_t new_size)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_disable_interrupts_env(the_env);
  GC_set_max_heap_size(cl_core.max_heap_size = new_size);
  if (new_size == 0) {
    cl_index size = ecl_option_values[ECL_OPT_HEAP_SAFETY_AREA];
    cl_core.safety_region = ecl_alloc_atomic_unprotected(size);
  } else if (cl_core.safety_region) {
    GC_FREE(cl_core.safety_region);
    cl_core.safety_region = 0;
  }
  ecl_enable_interrupts_env(the_env);
}

void
init_GC(void)
{
  /*
   * Garbage collector restrictions: we set up the garbage collector
   * library to work as follows
   *
   * 1) The garbage collector shall not scan shared libraries
   *    explicitely.
   * 2) We only detect objects that are referenced by a pointer to
   *    the begining or to the first byte.
   * 3) Out of the incremental garbage collector, we only use the
   *    generational component.
   */
  GC_set_no_dls(1);
  GC_set_all_interior_pointers(0);
  GC_set_time_limit(GC_TIME_UNLIMITED);
  GC_init();
#ifdef ECL_THREADS
# if GC_VERSION_MAJOR > 7 || GC_VERSION_MINOR > 1
  GC_allow_register_threads();
# endif
#endif
  if (ecl_option_values[ECL_OPT_INCREMENTAL_GC]) {
    GC_enable_incremental();
  }
  GC_register_displacement(1);
#ifdef GBC_BOEHM_PRECISE
  GC_init_explicit_typing();
#endif
  GC_clear_roots();
  GC_disable();

#ifdef GBC_BOEHM_PRECISE
# ifdef GBC_BOEHM_OWN_MARKER
  cl_object_free_list = (void **)GC_new_free_list_inner();
  cl_object_mark_proc_index = GC_new_proc((GC_mark_proc)cl_object_mark_proc);
  cl_object_kind = GC_new_kind_inner(cl_object_free_list,
                                     GC_MAKE_PROC(cl_object_mark_proc_index, 0),
                                     FALSE, TRUE);
# endif
#endif /* !GBC_BOEHM_PRECISE */

  GC_set_max_heap_size(cl_core.max_heap_size = ecl_option_values[ECL_OPT_HEAP_SIZE]);
  /* Save some memory for the case we get tight. */
  if (cl_core.max_heap_size == 0) {
    cl_index size = ecl_option_values[ECL_OPT_HEAP_SAFETY_AREA];
    cl_core.safety_region = ecl_alloc_atomic_unprotected(size);
  } else if (cl_core.safety_region) {
    cl_core.safety_region = 0;
  }

  old_GC_push_other_roots = GC_push_other_roots;
  GC_push_other_roots = stacks_scanner;
  GC_old_start_callback = GC_get_start_callback();
  GC_set_start_callback(gather_statistics);
  GC_set_java_finalization(1);
  GC_set_oom_fn(out_of_memory);
  GC_set_warn_proc(no_warnings);
  GC_enable();
}

/* If we do not build our own version of the library, we do not have
 * control over the existence of this variable. */
#if GBC_BOEHM == 0
extern int GC_print_stats;
#else
static int GC_print_stats;
#endif

cl_object
si_gc_stats(cl_object enable)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object old_status;
  cl_object size1;
  cl_object size2;
  if (cl_core.gc_stats == 0) {
    old_status = ECL_NIL;
  } else if (GC_print_stats) {
    old_status = @':full';
  } else {
    old_status = ECL_T;
  }
  if (cl_core.bytes_consed == ECL_NIL) {
    cl_core.bytes_consed = ecl_alloc_object(t_bignum);
    mpz_init2(ecl_bignum(cl_core.bytes_consed), 128);
    cl_core.gc_counter = ecl_alloc_object(t_bignum);
    mpz_init2(ecl_bignum(cl_core.gc_counter), 128);
  }

  update_bytes_consed();
  /* We need fresh copies of the bignums */
  size1 = _ecl_big_register_copy(cl_core.bytes_consed);
  size2 = _ecl_big_register_copy(cl_core.gc_counter);

  if (enable == ECL_NIL) {
    GC_print_stats = 0;
    cl_core.gc_stats = 0;
  } else if (enable == ecl_make_fixnum(0)) {
    mpz_set_ui(ecl_bignum(cl_core.bytes_consed), 0);
    mpz_set_ui(ecl_bignum(cl_core.gc_counter), 0);
  } else {
    cl_core.gc_stats = 1;
    GC_print_stats = (enable == @':full');
  }
  ecl_return3(the_env, size1, size2, old_status);
}

/* This procedure is invoked after garbage collection. Note that we
 * cannot cons because this procedure is invoked with the garbage
 * collection lock on. */
static void
gather_statistics()
{
  /* GC stats rely on bignums */
  if (cl_core.gc_stats) {
    update_bytes_consed();
    mpz_add_ui(ecl_bignum(cl_core.gc_counter),
               ecl_bignum(cl_core.gc_counter),
               1);
  }
  if (GC_old_start_callback)
    GC_old_start_callback();
}

static void
update_bytes_consed () {
#if GBC_BOEHM == 0
  mpz_add_ui(ecl_bignum(cl_core.bytes_consed),
             ecl_bignum(cl_core.bytes_consed),
             GC_get_bytes_since_gc());
#else
  /* This is not accurate and may wrap around. We try to detect this
     assuming that an overflow in an unsigned integer will produce
     a smaller integer.*/
  static cl_index bytes = 0;
  cl_index new_bytes = GC_get_total_bytes();
  if (bytes > new_bytes) {
    cl_index wrapped;
    wrapped = ~((cl_index)0) - bytes;
    mpz_add_ui(ecl_bignum(cl_core.bytes_consed),
               ecl_bignum(cl_core.bytes_consed),
               wrapped);
    mpz_add_ui(ecl_bignum(cl_core.bytes_consed),
               ecl_bignum(cl_core.bytes_consed),
               new_bytes);
  } else {
    mpz_add_ui(ecl_bignum(cl_core.bytes_consed),
               ecl_bignum(cl_core.bytes_consed),
               new_bytes - bytes);
  }
  bytes = new_bytes;
#endif
}

static void
ecl_mark_env(struct cl_env_struct *env)
{
#if 1
  if (env->stack) {
    GC_push_conditional((void *)env->stack, (void *)env->stack_top, 1);
    GC_set_mark_bit((void *)env->stack);
  }
  if (env->frs_top) {
    GC_push_conditional((void *)env->frs_org, (void *)(env->frs_top+1), 1);
    GC_set_mark_bit((void *)env->frs_org);
  }
  if (env->bds_top) {
    GC_push_conditional((void *)env->bds_org, (void *)(env->bds_top+1), 1);
    GC_set_mark_bit((void *)env->bds_org);
  }
#endif
  /*memset(env->values[env->nvalues], 0, (64-env->nvalues)*sizeof(cl_object));*/
#if defined(ECL_THREADS) && !defined(ECL_USE_MPROTECT) && !defined(ECL_USE_GUARD_PAGE)
  /* When using threads, "env" is a pointer to memory allocated by ECL. */
  GC_push_conditional((void *)env, (void *)(env + 1), 1);
  GC_set_mark_bit((void *)env);
#else
  /* When not using threads, "env" is mmaped or statically allocated. */
  GC_push_all((void *)env, (void *)(env + 1));
#endif
}

static void
stacks_scanner()
{
  cl_env_ptr the_env = ecl_process_env_unsafe();
  cl_object l;
  l = cl_core.libraries;
  if (l) {
    for (; l != ECL_NIL; l = ECL_CONS_CDR(l)) {
      cl_object dll = ECL_CONS_CAR(l);
      if (dll->cblock.locked) {
        GC_push_conditional((void *)dll, (void *)(&dll->cblock + 1), 1);
        GC_set_mark_bit((void *)dll);
      }
    }
  }
  GC_push_all((void *)(&cl_core), (void *)(&cl_core + 1));
  GC_push_all((void *)cl_symbols, (void *)(cl_symbols + cl_num_symbols_in_core));
  if (the_env != NULL)
    ecl_mark_env(the_env);
#ifdef ECL_THREADS
  l = cl_core.processes;
  if (l != OBJNULL) {
    cl_index i, size;
    for (i = 0, size = l->vector.dim; i < size; i++) {
      cl_object process = l->vector.self.t[i];
      if (!Null(process)) {
        cl_env_ptr env = process->process.env;
        if (env && (env != the_env)) ecl_mark_env(env);
      }
    }
  }
#endif
  if (old_GC_push_other_roots)
    (*old_GC_push_other_roots)();
}

void
ecl_register_root(cl_object *p)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_disable_interrupts_env(the_env);
  GC_add_roots((char*)p, (char*)(p+1));
  ecl_enable_interrupts_env(the_env);
}

cl_object
si_gc(cl_narg narg, ...)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_disable_interrupts_env(the_env);
  GC_gcollect();
  ecl_enable_interrupts_env(the_env);
  ecl_return0(the_env);
}

cl_object
si_gc_dump()
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_disable_interrupts_env(the_env);
  GC_dump();
  ecl_enable_interrupts_env(the_env);
  ecl_return0(the_env);
}

static int failure;
static void *
out_of_memory_check(size_t requested_bytes)
{
  failure = 1;
  return 0;
}

static void
no_warnings(char *msg, GC_word arg)
{
}

static void *
out_of_memory(size_t requested_bytes)
{
  const cl_env_ptr the_env = ecl_process_env();
  int interrupts = the_env->disable_interrupts;
  int method = 0;
  void *output;
  /* Disable interrupts only with the ext::*interrupts-enabled*
   * mechanism to allow for writes in the thread local environment */
  if (interrupts)
    ecl_enable_interrupts_env(the_env);
  ecl_bds_bind(the_env, @'ext::*interrupts-enabled*', ECL_NIL);
  /* Free the input / output buffers */
  the_env->string_pool = ECL_NIL;

  /* The out of memory condition may happen in more than one thread */
  /* But then we have to ensure the error has not been solved */
#ifdef ECL_THREADS
  ecl_mutex_lock(&cl_core.error_lock);
  ECL_UNWIND_PROTECT_BEGIN(the_env)
#endif
  {
    failure = 0;
    GC_gcollect();
    GC_set_oom_fn(out_of_memory_check);
    {
      output = GC_MALLOC(requested_bytes);
      GC_set_oom_fn(out_of_memory);
      if (output != 0 && failure == 0) {
        method = 2;
        goto OUTPUT;
      }
    }
    if (cl_core.max_heap_size == 0) {
      /* We did not set any limit in the amount of memory,
       * yet we failed, or we had some limits but we have
       * not reached them. */
      if (cl_core.safety_region) {
        /* We can free some memory and try handling the error */
        GC_FREE(cl_core.safety_region);
        the_env->string_pool = ECL_NIL;
        cl_core.safety_region = 0;
        method = 0;
      } else {
        /* No possibility of continuing */
        method = 2;
      }
    } else {
      cl_core.max_heap_size += ecl_option_values[ECL_OPT_HEAP_SAFETY_AREA];
      GC_set_max_heap_size(cl_core.max_heap_size);
      method = 1;
    }
  OUTPUT:
    (void)0;
  }
#ifdef ECL_THREADS
  ECL_UNWIND_PROTECT_EXIT {
    ecl_mutex_unlock(&cl_core.error_lock);
  } ECL_UNWIND_PROTECT_END;
#endif
  ecl_bds_unwind1(the_env);
  ecl_check_pending_interrupts(the_env);
  switch (method) {
  case 0: cl_error(1, @'ext::storage-exhausted');
    break;
  case 1: cl_cerror(2, @"Extend heap size",
                    @'ext::storage-exhausted');
    break;
  case 2:
    return output;
  default:
    ecl_internal_error("Memory exhausted, quitting program.");
    break;
  }
  if (!interrupts)
    ecl_disable_interrupts_env(the_env);
  GC_set_max_heap_size(cl_core.max_heap_size +=
                       cl_core.max_heap_size / 2);
  /* Default allocation. Note that we do not allocate atomic. */
  return GC_MALLOC(requested_bytes);
}
