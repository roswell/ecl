/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/* mem_bdwgc.d - memory allocator and garbage collector based on bdwgc  */

/* -- imports ---------------------------------------------------------------- */
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/external.h>

#include <stdio.h>

#ifdef ECL_THREADS
# ifdef ECL_WINDOWS_THREADS
#  include <windows.h>
# else
#  include <pthread.h>
# endif
#endif

#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/page.h>

#ifdef ECL_WSOCK
# include <winsock.h>
#endif

#ifdef GBC_BOEHM
# include <gc/gc_mark.h>
#endif

static void (*GC_old_start_callback)(void) = NULL;
static void gather_statistics(void);
static void update_bytes_consed(void);
static void ecl_mark_env(struct cl_env_struct *env);

#ifdef GBC_BOEHM_PRECISE
# if GBC_BOEHM
#  undef GBC_BOEHM_PRECISE
# else
#  include <gc/gc_typed.h>
#  define GBC_BOEHM_OWN_MARKER
static int cl_object_kind, cl_object_mark_proc_index;
static void **cl_object_free_list;
# endif
#endif

/* -- object allocation ------------------------------------------------------ */

void
_ecl_set_max_heap_size(size_t new_size)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_disable_interrupts_env(the_env);
  GC_set_max_heap_size(ecl_core.max_heap_size = new_size);
  if (new_size == 0) {
    cl_index size = ecl_option_values[ECL_OPT_HEAP_SAFETY_AREA];
    ecl_core.safety_region = ecl_alloc_atomic_unprotected(size);
  } else if (ecl_core.safety_region) {
    GC_FREE(ecl_core.safety_region);
    ecl_core.safety_region = 0;
  }
  ecl_enable_interrupts_env(the_env);
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
  the_env->token_pool = ECL_NIL;

  /* The out of memory condition may happen in more than one thread */
  /* But then we have to ensure the error has not been solved */
#ifdef ECL_THREADS
  ecl_mutex_lock(&ecl_core.error_lock);
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
    if (ecl_core.max_heap_size == 0) {
      /* We did not set any limit in the amount of memory,
       * yet we failed, or we had some limits but we have
       * not reached them. */
      if (ecl_core.safety_region) {
        /* We can free some memory and try handling the error */
        GC_FREE(ecl_core.safety_region);
        the_env->string_pool = ECL_NIL;
        the_env->token_pool = ECL_NIL;
        ecl_core.safety_region = 0;
        method = 0;
      } else {
        /* No possibility of continuing */
        method = 2;
      }
    } else {
      ecl_core.max_heap_size += ecl_option_values[ECL_OPT_HEAP_SAFETY_AREA];
      GC_set_max_heap_size(ecl_core.max_heap_size);
      method = 1;
    }
  OUTPUT:
    (void)0;
  }
#ifdef ECL_THREADS
  ECL_UNWIND_PROTECT_EXIT {
    ecl_mutex_unlock(&ecl_core.error_lock);
  } ECL_UNWIND_PROTECT_END;
#endif
  ecl_bds_unwind1(the_env);
  ecl_check_pending_interrupts(the_env);
  switch (method) {
  case 0: cl_error(1, @'ext::storage-exhausted');
    break;
  case 1: cl_cerror(2, @"Extend heap size", @'ext::storage-exhausted');
    break;
  case 2:
    return output;
  default:
    ecl_internal_error("Memory exhausted, quitting program.");
    break;
  }
  if (!interrupts)
    ecl_disable_interrupts_env(the_env);
  ecl_core.max_heap_size += (ecl_core.max_heap_size / 2);
  GC_set_max_heap_size(ecl_core.max_heap_size);
  /* Default allocation. Note that we do not allocate atomic. */
  return GC_MALLOC(requested_bytes);
}

static struct bdw_type_information {
  size_t size;
#ifdef GBC_BOEHM_PRECISE
  GC_word descriptor;
#endif
  cl_object (*allocator)(struct bdw_type_information *);
  size_t t;
} bdw_type_info[t_end];

static cl_object
allocate_object_error(struct bdw_type_information *bdw_type_info)
{
  printf("\ttype = %zx\n", bdw_type_info->t);
  ecl_internal_error("allocate_object_error: alloc botch.");
}

static cl_object
allocate_object_atomic(struct bdw_type_information *bdw_type_info)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object op;
  ecl_disable_interrupts_env(the_env);
  op = GC_MALLOC_ATOMIC(bdw_type_info->size);
  op->d.t = bdw_type_info->t;
  ecl_enable_interrupts_env(the_env);
  return op;
}

static cl_object
allocate_object_full(struct bdw_type_information *bdw_type_info)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object op;
  ecl_disable_interrupts_env(the_env);
  op = GC_MALLOC(bdw_type_info->size);
  op->d.t = bdw_type_info->t;
  ecl_enable_interrupts_env(the_env);
  return op;
}

#ifdef GBC_BOEHM_PRECISE
static cl_object
allocate_object_typed(struct bdw_type_information *bdw_type_info)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object op;
  ecl_disable_interrupts_env(the_env);
  op = GC_malloc_explicitly_typed(bdw_type_info->size, bdw_type_info->descriptor);
  op->d.t = bdw_type_info->t;
  ecl_enable_interrupts_env(the_env);
  return op;
}
#endif

#ifdef GBC_BOEHM_OWN_MARKER

static struct GC_ms_entry *
cl_object_mark_proc(void *addr, struct GC_ms_entry *msp, struct GC_ms_entry *msl,
                    GC_word env)
{
  cl_type t = ((cl_object)addr)->d.t;
  if (ecl_likely(t > t_start && t < t_end)) {
    struct bdw_type_information *info = bdw_type_info + t;
    GC_word d = info->descriptor;
    GC_word *p;
    for (p = addr; d; p++, d<<=1) {
      if ((GC_signed_word)d < 0) {
        GC_word aux = *p;
        if ((aux & 2) ||
            aux <= (GC_word)GC_least_plausible_heap_addr ||
            aux >= (GC_word)GC_greatest_plausible_heap_addr)
          continue;
        msp = GC_mark_and_push((void*)aux, (void*)msp,
                               (void*)msl, (void*)p);
      }
    }
  }
  return msp;
}

static cl_object
allocate_object_marked(struct bdw_type_information *bdw_type_info)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object op;
  ecl_disable_interrupts_env(the_env);
  op = GC_generic_malloc(bdw_type_info->size, cl_object_kind);
  op->d.t = bdw_type_info->t;
  ecl_enable_interrupts_env(the_env);
  return op;
}
#endif

static cl_object
alloc_object(cl_type t)
{
  struct bdw_type_information *ti = bdw_type_info + t;
  return ti->allocator(ti);
}

cl_object
ecl_alloc_compact_object(cl_type t, cl_index extra_space)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_index size = bdw_type_info[t].size;
  cl_object x;
  ecl_disable_interrupts_env(the_env);
  x = (cl_object)GC_MALLOC_ATOMIC(size + extra_space);
  ecl_enable_interrupts_env(the_env);
  x->array.t = t;
  x->array.displaced = (void*)(((char*)x) + size);
  return x;
}

cl_object
ecl_alloc_instance(cl_index slots)
{
  cl_object i;
  i = ecl_alloc_object(t_instance);
  i->instance.slots = (cl_object *)ecl_alloc(sizeof(cl_object) * slots);
  i->instance.length = slots;
  i->instance.isgf = ECL_NOT_FUNCALLABLE;
  i->instance.entry = FEnot_funcallable_vararg;
  i->instance.slotds = ECL_UNBOUND;
  return i;
}

static cl_index stamp = 0;
cl_index ecl_next_stamp() {
#if ECL_THREADS
  return AO_fetch_and_add((AO_t*)&stamp, 1) + 1;
#else
  return ++stamp;
#endif
}

void *
ecl_alloc_uncollectable(size_t size)
{
  const cl_env_ptr the_env = ecl_process_env();
  void *output;
  ecl_disable_interrupts_env(the_env);
  output = GC_MALLOC_UNCOLLECTABLE(size);
  ecl_enable_interrupts_env(the_env);
  return output;
}

void
ecl_free_uncollectable(void *pointer)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_disable_interrupts_env(the_env);
  GC_FREE(pointer);
  ecl_enable_interrupts_env(the_env);
}

void *
ecl_alloc_unprotected(cl_index n)
{
  return GC_MALLOC_IGNORE_OFF_PAGE(n);
}

void *
ecl_alloc_atomic_unprotected(cl_index n)
{
  return GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE(n);
}

void *
ecl_alloc(cl_index n)
{
  const cl_env_ptr the_env = ecl_process_env();
  void *output;
  ecl_disable_interrupts_env(the_env);
  output = ecl_alloc_unprotected(n);
  ecl_enable_interrupts_env(the_env);
  return output;
}

void *
ecl_alloc_atomic(cl_index n)
{
  const cl_env_ptr the_env = ecl_process_env();
  void *output;
  ecl_disable_interrupts_env(the_env);
  output = ecl_alloc_atomic_unprotected(n);
  ecl_enable_interrupts_env(the_env);
  return output;
}

void
ecl_dealloc(void *ptr)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_disable_interrupts_env(the_env);
  GC_FREE(ptr);
  ecl_enable_interrupts_env(the_env);
}

/* -- weak pointers ---------------------------------------------------------- */

cl_object
ecl_alloc_weak_pointer(cl_object o)
{
  const cl_env_ptr the_env = ecl_process_env();
  struct ecl_weak_pointer *obj;
  ecl_disable_interrupts_env(the_env);
  obj = GC_MALLOC_ATOMIC(sizeof(struct ecl_weak_pointer));
  ecl_enable_interrupts_env(the_env);
  obj->t = t_weak_pointer;
  obj->value = o;
  if (!ECL_IMMEDIATE(o)) {
    GC_GENERAL_REGISTER_DISAPPEARING_LINK((void**)&(obj->value), (void*)o);
    si_set_finalizer((cl_object)obj, ECL_T);
  }
  return (cl_object)obj;
}

static cl_object
ecl_weak_pointer_value(cl_object o)
{
  return ecl_weak_pointer(o);
}

cl_object
si_make_weak_pointer(cl_object o)
{
  cl_object pointer = ecl_alloc_weak_pointer(o);
  @(return pointer);
}

cl_object
si_weak_pointer_value(cl_object o)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object value;
  if (ecl_unlikely(ecl_t_of(o) != t_weak_pointer))
    FEwrong_type_only_arg(@[ext::weak-pointer-value], o,
                          @[ext::weak-pointer]);
  value = (cl_object)GC_call_with_alloc_lock((GC_fn_type)ecl_weak_pointer_value, o);
  if (value) {
    ecl_return2(the_env, value, ECL_T);
  } else {
    ecl_return2(the_env, ECL_NIL, ECL_NIL);
  }
}

/* -- graph traversal -------------------------------------------------------- */

#ifdef GBC_BOEHM_PRECISE
static cl_index
to_bitmap(void *x, void *y)
{
  cl_index n = (char*)y - (char*)x;
  if (n % sizeof(void*))
    ecl_internal_error("Misaligned pointer in ECL structure.");
  n /= sizeof(void*);
  return 1 << n;
}
#endif

void init_bdw_type_info (void)
{
  int i;
  for (i = 0; i < t_end; i++) {
    uintmax_t desc = ecl_type_info[i].descriptor;
    bdw_type_info[i].t = i;
    bdw_type_info[i].size = ecl_type_info[i].size;
    bdw_type_info[i].allocator =
      (desc==0) ? allocate_object_atomic : allocate_object_full;
#ifdef GC_BOEHM_PRECISE
    bdw_type_info[t_list].descriptor = desc;
#endif
  }
#ifdef GC_BOEHM_PRECISE
  for (i = 0; i < t_end; i++) {
    GC_word descriptor = bdw_type_info[i].descriptor;
    int bits = bdw_type_info[i].size / sizeof(GC_word);
    if (descriptor) {
#ifdef GBC_BOEHM_OWN_MARKER
      bdw_type_info[i].allocator = allocate_object_marked;
      descriptor = GC_make_descriptor(&descriptor, bits);
      descriptor &= ~GC_DS_TAGS;
#else
      GC_word mask = (1 << (bits-1)) - 1;
      mask ^= (descriptor >> 1);
      bdw_type_info[i].allocator =
        (mask == 0) ? allocate_object_full : allocate_object_typed;
      descriptor = GC_make_descriptor(&descriptor, bits);
#endif
      bdw_type_info[i].descriptor = descriptor;
    }
  }
#endif /* GBC_BOEHM_PRECISE */
  /* INV these cases are handled inline in ecl_alloc_object. */
  bdw_type_info[t_list].allocator = allocate_object_error;
  bdw_type_info[t_character].allocator = allocate_object_error;
  bdw_type_info[t_fixnum].allocator = allocate_object_error;
}

extern void (*GC_push_other_roots)();
static void (*old_GC_push_other_roots)();
static void stacks_scanner();

/* -- finalization ----------------------------------------------------------- */

static void
standard_finalizer(cl_object o)
{
  switch (o->d.t) {
#ifdef ENABLE_DLOPEN
  case t_codeblock:
    ecl_library_close(o);
    break;
#endif
  case t_stream:
    cl_close(1, o);
    break;
  case t_weak_pointer:
    GC_unregister_disappearing_link((void**)&(o->weak.value));
    break;
#ifdef ECL_THREADS
  case t_lock: {
    const cl_env_ptr the_env = ecl_process_env();
    ecl_disable_interrupts_env(the_env);
    ecl_mutex_destroy(&o->lock.mutex);
    ecl_enable_interrupts_env(the_env);
    break;
  }
  case t_condition_variable: {
    const cl_env_ptr the_env = ecl_process_env();
    ecl_disable_interrupts_env(the_env);
    ecl_cond_var_destroy(&o->condition_variable.cv);
    ecl_enable_interrupts_env(the_env);
    break;
  }
  case t_barrier: {
    const cl_env_ptr the_env = ecl_process_env();
    ecl_disable_interrupts_env(the_env);
    ecl_mutex_destroy(&o->barrier.mutex);
    ecl_cond_var_destroy(&o->barrier.cv);
    ecl_enable_interrupts_env(the_env);
    break;
  }
  case t_semaphore: {
    const cl_env_ptr the_env = ecl_process_env();
    ecl_disable_interrupts_env(the_env);
    ecl_mutex_destroy(&o->semaphore.mutex);
    ecl_cond_var_destroy(&o->semaphore.cv);
    ecl_enable_interrupts_env(the_env);
    break;
  }
  case t_mailbox: {
    const cl_env_ptr the_env = ecl_process_env();
    ecl_disable_interrupts_env(the_env);
    ecl_mutex_destroy(&o->mailbox.mutex);
    ecl_cond_var_destroy(&o->mailbox.reader_cv);
    ecl_cond_var_destroy(&o->mailbox.writer_cv);
    ecl_enable_interrupts_env(the_env);
    break;
  }
  case t_rwlock: {
    const cl_env_ptr the_env = ecl_process_env();
    ecl_disable_interrupts_env(the_env);
    ecl_rwlock_destroy(&o->rwlock.mutex);
    ecl_enable_interrupts_env(the_env);
    break;
  }
  case t_process: {
    const cl_env_ptr the_env = ecl_process_env();
    ecl_disable_interrupts_env(the_env);
    ecl_mutex_destroy(&o->process.start_stop_lock);
    ecl_cond_var_destroy(&o->process.exit_barrier);
    ecl_enable_interrupts_env(the_env);
    break;
  }
  case t_symbol: {
    if (o->symbol.binding != ECL_MISSING_SPECIAL_BINDING) {
      ecl_atomic_push(&ecl_core.reused_indices, ecl_make_fixnum(o->symbol.binding));
      o->symbol.binding = ECL_MISSING_SPECIAL_BINDING;
    }
  }
#endif /* ECL_THREADS */
  default:;
  }
}

static void
wrapped_finalizer(cl_object o, cl_object finalizer);

static void
register_finalizer(cl_object o, void *finalized_object,
                   GC_finalization_proc fn, void *cd,
                   GC_finalization_proc *ofn, void **ocd)
{
  /* Finalizers for some builtin objects are only run when the object is not
   * reachable by any means, including through other finalizers which might
   * make the object reachable again. The objects must not contain any cyclic
   * references for which finalizers are registered.
   *
   * We don't use this type of finalizer for user-defined finalizers, because
   * those might contain cyclic references which would prevent the objects
   * from being garbage collected. It is instead the duty of the user to write
   * the finalizers in a consistent way.
   *
   * case t_symbol: is not finalized with the "unreachable" finalizer because
   * it might contain cyclic references; Also running the finalizer too early
   * doesn't lead to any problems, we will simply choose a new binding index
   * the next time a binding is established. */
  switch (o->d.t) {
#ifdef ENABLE_DLOPEN
  case t_codeblock:
#endif
  case t_stream:
#if defined(ECL_THREADS)
  case t_lock:
  case t_condition_variable:
  case t_barrier:
  case t_semaphore:
  case t_mailbox:
  case t_rwlock:
  case t_process:
#endif  /* ECL_THREADS */
    /* Don't delete the standard finalizer. */
    if (fn == NULL) {
      fn = (GC_finalization_proc)wrapped_finalizer;
      cd = ECL_T;
    }
    GC_REGISTER_FINALIZER_UNREACHABLE(finalized_object, fn, cd, ofn, ocd);
    break;
  case t_weak_pointer:
#if defined(ECL_THREADS)
  case t_symbol:
#endif
    /* Don't delete the standard finalizer. */
    if (fn == NULL) {
      fn = (GC_finalization_proc)wrapped_finalizer;
      cd = ECL_T;
    }
    /* fallthrough */
  default:
    GC_REGISTER_FINALIZER_NO_ORDER(finalized_object, fn, cd, ofn, ocd);
    break;
  }
}

static void
deferred_finalizer(cl_object* x)
{
  wrapped_finalizer(x[0], x[1]);
}

static void
wrapped_finalizer(cl_object o, cl_object finalizer)
{
  if (finalizer != ECL_NIL && finalizer != NULL) {
#ifdef ECL_THREADS
    const cl_env_ptr the_env = ecl_process_env_unsafe();
    if (!the_env
        || !the_env->own_process
        || the_env->own_process->process.phase < ECL_PROCESS_ACTIVE)
    {
       /*
        * The finalizer is invoked while we are registering or setup a
        * new lisp process.  As example that may happen when we are
        * doing ecl_import_current_thread.  That mean the finalizer
        * can not be executed right now, so in some way we need to
        * queue the finalization.  When we return from this function
        * the original finalizer is no more registered to o, and if o
        * is not anymore reachable it will be colleted.  To prevent
        * this we need to make this object reachable again after that
        * roundtrip and postpone the finalization to the next garbage
        * collection.  Given that this is a rare condition one way to
        * do that is:
        */
       GC_finalization_proc ofn;
       void *odata;
       cl_object* wrapper = GC_MALLOC(2*sizeof(cl_object));
       wrapper[0] = o;
       wrapper[1] = finalizer;

       register_finalizer(o, wrapper,
                          (GC_finalization_proc)deferred_finalizer, 0,
                          &ofn, &odata);
       return;
    }
#endif /* ECL_THREADS */
    CL_NEWENV_BEGIN {
      if (finalizer != ECL_T) {
        funcall(2, finalizer, o);
      }
      standard_finalizer(o);
    } CL_NEWENV_END;
  }
}

cl_object
si_get_finalizer(cl_object o)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object output;
  GC_finalization_proc ofn;
  void *odata;
  ecl_disable_interrupts_env(the_env);
  register_finalizer(o, o, (GC_finalization_proc)0, 0, &ofn, &odata);
  if (ofn == 0) {
    output = ECL_NIL;
  } else if (ofn == (GC_finalization_proc)wrapped_finalizer) {
    output = (cl_object)odata;
  } else {
    output = ECL_NIL;
  }
  register_finalizer(o, o, ofn, odata, &ofn, &odata);
  ecl_enable_interrupts_env(the_env);
  @(return output);
}

void
ecl_set_finalizer_unprotected(cl_object o, cl_object finalizer)
{
  GC_finalization_proc ofn;
  void *odata;
  if (finalizer == ECL_NIL) {
    register_finalizer(o, o, (GC_finalization_proc)0, 0, &ofn, &odata);
  } else {
    GC_finalization_proc newfn;
    newfn = (GC_finalization_proc)wrapped_finalizer;
    register_finalizer(o, o, newfn, finalizer, &ofn, &odata);
  }
}

cl_object
si_set_finalizer(cl_object o, cl_object finalizer)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_disable_interrupts_env(the_env);
  ecl_set_finalizer_unprotected(o, finalizer);
  ecl_enable_interrupts_env(the_env);
  @(return);
}

/* -- GC stats --------------------------------------------------------------- */

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
  cl_object old_status;
  cl_object size1;
  cl_object size2;
  if (ecl_core.gc_stats == 0) {
    old_status = ECL_NIL;
  } else if (GC_print_stats) {
    old_status = @':full';
  } else {
    old_status = ECL_T;
  }
  if (ecl_core.bytes_consed == ECL_NIL) {
    ecl_core.bytes_consed = ecl_alloc_object(t_bignum);
    mpz_init2(ecl_bignum(ecl_core.bytes_consed), 128);
    ecl_core.gc_counter = ecl_alloc_object(t_bignum);
    mpz_init2(ecl_bignum(ecl_core.gc_counter), 128);
  }

  update_bytes_consed();
  /* We need fresh copies of the bignums */
  size1 = _ecl_big_register_copy(ecl_core.bytes_consed);
  size2 = _ecl_big_register_copy(ecl_core.gc_counter);

  if (enable == ECL_NIL) {
    GC_print_stats = 0;
    ecl_core.gc_stats = 0;
  } else if (enable == ecl_make_fixnum(0)) {
    mpz_set_ui(ecl_bignum(ecl_core.bytes_consed), 0);
    mpz_set_ui(ecl_bignum(ecl_core.gc_counter), 0);
  } else {
    ecl_core.gc_stats = 1;
    GC_print_stats = (enable == @':full');
  }
  @(return size1 size2 old_status);
}

/* This procedure is invoked after garbage collection. Note that we
 * cannot cons because this procedure is invoked with the garbage
 * collection lock on. */
static void
gather_statistics()
{
  /* GC stats rely on bignums */
  if (ecl_core.gc_stats) {
    update_bytes_consed();
    mpz_add_ui(ecl_bignum(ecl_core.gc_counter),
               ecl_bignum(ecl_core.gc_counter),
               1);
  }
  if (GC_old_start_callback)
    GC_old_start_callback();
}

static void
update_bytes_consed () {
#if GBC_BOEHM == 0
  mpz_add_ui(ecl_bignum(ecl_core.bytes_consed),
             ecl_bignum(ecl_core.bytes_consed),
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
    mpz_add_ui(ecl_bignum(ecl_core.bytes_consed),
               ecl_bignum(ecl_core.bytes_consed),
               wrapped);
    mpz_add_ui(ecl_bignum(ecl_core.bytes_consed),
               ecl_bignum(ecl_core.bytes_consed),
               new_bytes);
  } else {
    mpz_add_ui(ecl_bignum(ecl_core.bytes_consed),
               ecl_bignum(ecl_core.bytes_consed),
               new_bytes - bytes);
  }
  bytes = new_bytes;
#endif
}

/* -- garbage collection ----------------------------------------------------- */

static void
ecl_mark_env(struct cl_env_struct *env)
{
  /* Environments and stacks are allocated without GC */
  if (env->run_stack.org)
    GC_push_all((void *)env->run_stack.org, (void *)env->run_stack.top);
  if (env->frs_stack.org)
    GC_push_all((void *)env->frs_stack.org, (void *)(env->frs_stack.top+1));
  if (env->bds_stack.org)
    GC_push_all((void *)env->bds_stack.org, (void *)(env->bds_stack.top+1));
#ifdef ECL_THREADS
  if (env->bds_stack.tl_bindings)
    GC_push_all((void *)env->bds_stack.tl_bindings,
                (void *)(env->bds_stack.tl_bindings
                         + env->bds_stack.tl_bindings_size));
#endif
  GC_push_all((void *)env, (void *)(env + 1));
}

static void
stacks_scanner()
{
  cl_object l = ecl_core.libraries;
  loop_for_on_unsafe(l) {
    cl_object dll = ECL_CONS_CAR(l);
    if (dll->cblock.locked) {
      GC_push_conditional((void *)dll, (void *)(&dll->cblock + 1), 1);
      GC_set_mark_bit((void *)dll);
    }
  } end_loop_for_on_unsafe(l);
  /* ECL runtime */
  GC_push_all((void *)(&ecl_core), (void *)(&ecl_core + 1));
  GC_push_all((void *)ecl_vr_shandlers, (void *)(ecl_vr_shandlers + 1));
  /* Common Lisp */
  GC_push_all((void *)(&cl_core), (void *)(&cl_core + 1));
  GC_push_all((void *)cl_symbols, (void *)(cl_symbols + cl_num_symbols_in_core));
  ecl_mark_env(ecl_core.first_env);
#ifdef ECL_THREADS
  loop_across_stack_fifo(_env, ecl_core.threads) {
    cl_env_ptr env = ecl_cast_ptr(cl_env_ptr, _env);
    if(env != ecl_core.first_env)
      ecl_mark_env(env);
  } end_loop_across_stack();
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
  @(return);
}

cl_object
si_gc_dump()
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_disable_interrupts_env(the_env);
  GC_dump();
  ecl_enable_interrupts_env(the_env);
  @(return);
}

/* -- module definition ------------------------------------------------------ */

static void *
alloc_memory(cl_index size)
{
  return GC_MALLOC(size);
}

static void
free_object(cl_object o)
{
  standard_finalizer(o);
  ecl_dealloc(o);
}

struct ecl_allocator_ops gc_ops = {
  .allocate_memory = alloc_memory,
  .allocate_object = alloc_object,
  .free_memory = ecl_dealloc,
  .free_object = free_object
};

static cl_object
create_gc()
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
   * 4) GC should handle fork() which is used to run subprocess on
   *    some platforms.
   */
  GC_set_no_dls(1);
  GC_set_all_interior_pointers(0);
  GC_set_time_limit(GC_TIME_UNLIMITED);
#ifndef ECL_MS_WINDOWS_HOST
  GC_set_handle_fork(1);
#endif
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
  ecl_core.max_heap_size = ecl_option_values[ECL_OPT_HEAP_SIZE];
  GC_set_max_heap_size(ecl_core.max_heap_size);
  /* Save some memory for the case we get tight. */
  if (ecl_core.max_heap_size == 0) {
    cl_index size = ecl_option_values[ECL_OPT_HEAP_SAFETY_AREA];
    ecl_core.safety_region = ecl_alloc_atomic_unprotected(size);
  } else if (ecl_core.safety_region) {
    ecl_core.safety_region = 0;
  }

  init_bdw_type_info();

  old_GC_push_other_roots = GC_push_other_roots;
  GC_push_other_roots = stacks_scanner;
  GC_old_start_callback = GC_get_start_callback();
  GC_set_start_callback(gather_statistics);
  GC_set_java_finalization(1);
  GC_set_oom_fn(out_of_memory);
  GC_set_warn_proc(no_warnings);

  ecl_core.allocator = &gc_ops;

  return ECL_NIL;
}

static cl_object
enable_gc ()
{
  GC_enable();
  return ECL_NIL;
}

static cl_object
disable_gc ()
{
  GC_disable();
  return ECL_NIL;
}

static cl_object
init_cpu(cl_env_ptr the_env)
{
#ifdef GBC_BOEHM
  struct GC_stack_base stack;
  GC_get_stack_base(&stack);
  the_env->c_stack.org = (char*)stack.mem_base;
# ifdef ECL_THREADS
  if (GC_thread_is_registered() == 0) {
    GC_register_my_thread(&stack);
  }
# endif
#endif
  return ECL_NIL;
}

static cl_object
free_cpu(cl_env_ptr the_env)
{
#ifdef GBC_BOEHM
# ifdef ECL_THREADS
  if (GC_thread_is_registered() == 1) {
    GC_unregister_my_thread();
  }
# endif
#endif
  return ECL_NIL;
}

ecl_def_ct_base_string(str_gc, "BDW-GC", 6, static, const);

static struct ecl_module module_gc = {
  .t = t_module,
  .name = str_gc,
  .create = create_gc,
  .enable = enable_gc,
  .init_env = ecl_module_no_op_env,
  .init_cpu = init_cpu,
  .free_cpu = free_cpu,
  .free_env = ecl_module_no_op_env,
  .disable = disable_gc,
  .destroy = ecl_module_no_op
};

cl_object ecl_module_gc = (cl_object)&module_gc;
