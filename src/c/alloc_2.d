/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * alloc_2.c - memory allocation based on the Boehm GC
 *
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <stdio.h>
#include <ecl/ecl.h>
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
#include <winsock.h>
#endif

#ifdef GBC_BOEHM
#include <gc/gc_mark.h>

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

/**********************************************************
 *              OBJECT ALLOCATION                         *
 **********************************************************/

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

static struct ecl_type_information {
  size_t size;
#ifdef GBC_BOEHM_PRECISE
  GC_word descriptor;
#endif
  cl_object (*allocator)(struct ecl_type_information *);
  size_t t;
} type_info[t_end];

#ifdef GBC_BOEHM_PRECISE
static void
error_wrong_tag(cl_type t)
{
  ecl_internal_error("Collector called with invalid tag number.");
}
#endif

cl_index
ecl_object_byte_size(cl_type t)
{
  if (t == t_fixnum || t == t_character)
    FEerror("ecl_object_byte_size invoked with an immediate type ~D",
            1, ecl_make_fixnum(1));
  if (t >= t_end)
    FEerror("ecl_object_byte_size invoked with an unknown type ~D",
            1, ecl_make_fixnum(1));
  return type_info[t].size;
}

static cl_object
allocate_object_atomic(struct ecl_type_information *type_info)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object op;
  ecl_disable_interrupts_env(the_env);
  op = GC_MALLOC_ATOMIC(type_info->size);
  op->d.t = type_info->t;
  ecl_enable_interrupts_env(the_env);
  return op;
}

static cl_object
allocate_object_full(struct ecl_type_information *type_info)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object op;
  ecl_disable_interrupts_env(the_env);
  op = GC_MALLOC(type_info->size);
  op->d.t = type_info->t;
  ecl_enable_interrupts_env(the_env);
  return op;
}

#ifdef GBC_BOEHM_PRECISE
static cl_object
allocate_object_typed(struct ecl_type_information *type_info)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object op;
  ecl_disable_interrupts_env(the_env);
  op = GC_malloc_explicitly_typed(type_info->size, type_info->descriptor);
  op->d.t = type_info->t;
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
    struct ecl_type_information *info = type_info + t;
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
allocate_object_marked(struct ecl_type_information *type_info)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object op;
  ecl_disable_interrupts_env(the_env);
  op = GC_generic_malloc(type_info->size, cl_object_kind);
  op->d.t = type_info->t;
  ecl_enable_interrupts_env(the_env);
  return op;
}
#endif

cl_object
ecl_alloc_object(cl_type t)
{
#ifdef GBC_BOEHM_PRECISE
  struct ecl_type_information *ti;
  if (ecl_likely(t > t_start && t < t_end)) {
    ti = type_info + t;
    return ti->allocator(ti);
  }
  error_wrong_tag(t);
  return OBJNULL;
#else
  const cl_env_ptr the_env = ecl_process_env();

  /* GC_MALLOC already resets objects */
  switch (t) {
  case t_fixnum:
    return ecl_make_fixnum(0); /* Immediate fixnum */
  case t_character:
    return ECL_CODE_CHAR(' '); /* Immediate character */
#ifdef ECL_SSE2
  case t_sse_pack:
#endif
  case t_longfloat:
#ifdef ECL_COMPLEX_FLOAT
  case t_csfloat:
  case t_cdfloat:
  case t_clfloat:
#endif
  case t_singlefloat:
  case t_doublefloat: {
    cl_object obj;
    ecl_disable_interrupts_env(the_env);
    obj = (cl_object)GC_MALLOC_ATOMIC(type_info[t].size);
    ecl_enable_interrupts_env(the_env);
    obj->d.t = t;
    return obj;
  }
  case t_bignum:
  case t_ratio:
  case t_complex:
  case t_symbol:
  case t_package:
  case t_hashtable:
  case t_array:
  case t_vector:
  case t_base_string:
#ifdef ECL_UNICODE
  case t_string:
#endif
  case t_bitvector:
  case t_stream:
  case t_random:
  case t_readtable:
  case t_pathname:
  case t_bytecodes:
  case t_bclosure:
  case t_cfun:
  case t_cfunfixed:
  case t_cclosure:
  case t_instance:
#ifdef ECL_THREADS
  case t_process:
  case t_lock:
  case t_rwlock:
  case t_condition_variable:
  case t_semaphore:
  case t_barrier:
  case t_mailbox:
#endif
  case t_foreign:
  case t_codeblock: {
    cl_object obj;
    ecl_disable_interrupts_env(the_env);
    obj = (cl_object)GC_MALLOC(type_info[t].size);
    ecl_enable_interrupts_env(the_env);
    obj->d.t = t;
    return obj;
  }
  default:
    printf("\ttype = %d\n", t);
    ecl_internal_error("alloc botch.");
  }
#endif
}

cl_object
ecl_alloc_compact_object(cl_type t, cl_index extra_space)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_index size = type_info[t].size;
  cl_object x;
  ecl_disable_interrupts_env(the_env);
  x = (cl_object)GC_MALLOC_ATOMIC(size + extra_space);
  ecl_enable_interrupts_env(the_env);
  x->array.t = t;
  x->array.displaced = (void*)(((char*)x) + size);
  return x;
}

cl_object
ecl_cons(cl_object a, cl_object d)
{
  const cl_env_ptr the_env = ecl_process_env();
  struct ecl_cons *obj;
  ecl_disable_interrupts_env(the_env);
  obj = GC_MALLOC(sizeof(struct ecl_cons));
  ecl_enable_interrupts_env(the_env);
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
ecl_alloc_instance(cl_index slots)
{
  cl_object i;
  i = ecl_alloc_object(t_instance);
  i->instance.slots = (cl_object *)ecl_alloc(sizeof(cl_object) * slots);
  i->instance.length = slots;
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

void init_type_info (void)
{
#ifdef GBC_BOEHM_PRECISE
  union cl_lispunion o;
  struct ecl_cons c;
#endif
  int i;
#define init_tm(/* cl_type  */ type,                            \
                /* char*    */ name,                            \
                /* cl_index */ object_size,                     \
                /* cl_index */ maxpage) {                       \
    type_info[type].size = (object_size);                       \
    if ((maxpage) == 0) {                                       \
      type_info[type].allocator = allocate_object_atomic;       \
    }                                                           \
  }
  for (i = 0; i < t_end; i++) {
    type_info[i].t = i;
    type_info[i].size = 0;
    type_info[i].allocator = allocate_object_full;
  }
  init_tm(t_list, "CONS", sizeof(struct ecl_cons), 2);
  init_tm(t_bignum, "BIGNUM", sizeof(struct ecl_bignum), 2);
  init_tm(t_ratio, "RATIO", sizeof(struct ecl_ratio), 2);
  init_tm(t_singlefloat, "SINGLE-FLOAT", sizeof(struct ecl_singlefloat), 0);
  init_tm(t_doublefloat, "DOUBLE-FLOAT", sizeof(struct ecl_doublefloat), 0);
  init_tm(t_longfloat, "LONG-FLOAT", sizeof(struct ecl_long_float), 0);
  init_tm(t_complex, "COMPLEX", sizeof(struct ecl_complex), 2);
#ifdef ECL_COMPLEX_FLOAT
  init_tm(t_csfloat, "COMPLEX-SINGLE-FLOAT", sizeof(struct ecl_csfloat), 0);
  init_tm(t_cdfloat, "COMPLEX-DOUBLE-FLOAT", sizeof(struct ecl_cdfloat), 0);
  init_tm(t_clfloat, "COMPLEX-LONG-FLOAT", sizeof(struct ecl_clfloat), 0);
#endif
  init_tm(t_symbol, "SYMBOL", sizeof(struct ecl_symbol), 5);
  init_tm(t_package, "PACKAGE", sizeof(struct ecl_package), -1); /* 36 */
#ifdef ECL_THREADS
  init_tm(t_hashtable, "HASH-TABLE", sizeof(struct ecl_hashtable), 3);
#else
  init_tm(t_hashtable, "HASH-TABLE", sizeof(struct ecl_hashtable), 4);
#endif
  init_tm(t_array, "ARRAY", sizeof(struct ecl_array), 3);
  init_tm(t_vector, "VECTOR", sizeof(struct ecl_vector), 2);
#ifdef ECL_UNICODE
  init_tm(t_string, "STRING", sizeof(struct ecl_string), 2);
#endif
  init_tm(t_base_string, "BASE-STRING", sizeof(struct ecl_base_string), 2);
  init_tm(t_bitvector, "BIT-VECTOR", sizeof(struct ecl_vector), 2);
  init_tm(t_stream, "STREAM", sizeof(struct ecl_stream), 6);
  init_tm(t_random, "RANDOM-STATE", sizeof(struct ecl_random), -1);
  init_tm(t_readtable, "READTABLE", sizeof(struct ecl_readtable), 2);
  init_tm(t_pathname, "PATHNAME", sizeof(struct ecl_pathname), -1);
  init_tm(t_bytecodes, "BYTECODES", sizeof(struct ecl_bytecodes), -1);
  init_tm(t_bclosure, "BCLOSURE", sizeof(struct ecl_bclosure), 3);
  init_tm(t_cfun, "CFUN", sizeof(struct ecl_cfun), -1);
  init_tm(t_cfunfixed, "CFUNFIXED", sizeof(struct ecl_cfunfixed), -1);
  init_tm(t_cclosure, "CCLOSURE", sizeof(struct ecl_cclosure), -1);
  init_tm(t_instance, "INSTANCE", sizeof(struct ecl_instance), 4);
#ifdef ECL_THREADS
  init_tm(t_process, "PROCESS", sizeof(struct ecl_process), 8);
  init_tm(t_lock, "LOCK", sizeof(struct ecl_lock), 2);
  init_tm(t_rwlock, "RWLOCK", sizeof(struct ecl_rwlock), 0);
  init_tm(t_condition_variable, "CONDITION-VARIABLE",
          sizeof(struct ecl_condition_variable), 0);
  init_tm(t_semaphore, "SEMAPHORES", sizeof(struct ecl_semaphore), 0);
  init_tm(t_barrier, "BARRIER", sizeof(struct ecl_barrier), 0);
  init_tm(t_mailbox, "MAILBOX", sizeof(struct ecl_mailbox), 0);
#endif
  init_tm(t_codeblock, "CODEBLOCK", sizeof(struct ecl_codeblock), -1);
  init_tm(t_foreign, "FOREIGN", sizeof(struct ecl_foreign), 2);
  init_tm(t_frame, "STACK-FRAME", sizeof(struct ecl_stack_frame), 2);
  init_tm(t_weak_pointer, "WEAK-POINTER", sizeof(struct ecl_weak_pointer), 0);
#ifdef ECL_SSE2
  init_tm(t_sse_pack, "SSE-PACK", sizeof(struct ecl_sse_pack), 0);
#endif
#ifdef GBC_BOEHM_PRECISE
  type_info[t_list].descriptor =
    to_bitmap(&c, &(c.car)) |
    to_bitmap(&c, &(c.cdr));
  type_info[t_bignum].descriptor =
    to_bitmap(&o, &(ECL_BIGNUM_LIMBS(&o)));
  type_info[t_ratio].descriptor =
    to_bitmap(&o, &(o.ratio.num)) |
    to_bitmap(&o, &(o.ratio.den));
  type_info[t_singlefloat].descriptor = 0;
  type_info[t_doublefloat].descriptor = 0;
  type_info[t_longfloat].descriptor = 0;
  type_info[t_complex].descriptor =
    to_bitmap(&o, &(o.gencomplex.real)) |
    to_bitmap(&o, &(o.gencomplex.imag));
#ifdef ECL_COMPLEX_FLOAT
  type_info[t_csfloat].descriptor = 0;
  type_info[t_cdfloat].descriptor = 0;
  type_info[t_clfloat].descriptor = 0;
#endif
  type_info[t_symbol].descriptor =
    to_bitmap(&o, &(o.symbol.value)) |
    to_bitmap(&o, &(o.symbol.gfdef)) |
    to_bitmap(&o, &(o.symbol.macfun)) |
    to_bitmap(&o, &(o.symbol.plist)) |
    to_bitmap(&o, &(o.symbol.name)) |
    to_bitmap(&o, &(o.symbol.hpack));
  type_info[t_package].descriptor =
    to_bitmap(&o, &(o.pack.name)) |
    to_bitmap(&o, &(o.pack.nicknames)) |
    to_bitmap(&o, &(o.pack.local_nicknames)) |
    to_bitmap(&o, &(o.pack.nicknamedby)) |
    to_bitmap(&o, &(o.pack.shadowings)) |
    to_bitmap(&o, &(o.pack.uses)) |
    to_bitmap(&o, &(o.pack.usedby)) |
    to_bitmap(&o, &(o.pack.internal)) |
    to_bitmap(&o, &(o.pack.external));
  type_info[t_hashtable].descriptor =
    to_bitmap(&o, &(o.hash.data)) |
    to_bitmap(&o, &(o.hash.sync_lock)) |
    to_bitmap(&o, &(o.hash.generic_test)) |
    to_bitmap(&o, &(o.hash.generic_hash)) |
    to_bitmap(&o, &(o.hash.rehash_size)) |
    to_bitmap(&o, &(o.hash.threshold));
  type_info[t_array].descriptor =
    to_bitmap(&o, &(o.array.dims)) |
    to_bitmap(&o, &(o.array.self.t)) |
    to_bitmap(&o, &(o.array.displaced));
  type_info[t_vector].descriptor =
    to_bitmap(&o, &(o.vector.self.t)) |
    to_bitmap(&o, &(o.vector.displaced));
# ifdef ECL_UNICODE
  type_info[t_string].descriptor =
    to_bitmap(&o, &(o.string.self)) |
    to_bitmap(&o, &(o.string.displaced));
# endif
  type_info[t_base_string].descriptor =
    to_bitmap(&o, &(o.base_string.self)) |
    to_bitmap(&o, &(o.base_string.displaced));
  type_info[t_bitvector].descriptor =
    to_bitmap(&o, &(o.vector.self.t)) |
    to_bitmap(&o, &(o.vector.displaced));
  type_info[t_stream].descriptor =
    to_bitmap(&o, &(o.stream.ops)) |
    to_bitmap(&o, &(o.stream.object0)) |
    to_bitmap(&o, &(o.stream.object1)) |
    to_bitmap(&o, &(o.stream.byte_stack)) |
    to_bitmap(&o, &(o.stream.buffer)) |
    to_bitmap(&o, &(o.stream.format)) |
    to_bitmap(&o, &(o.stream.format_table));
  type_info[t_random].descriptor =
    to_bitmap(&o, &(o.random.value));
  type_info[t_readtable].descriptor =
# ifdef ECL_UNICODE
    to_bitmap(&o, &(o.readtable.hash)) |
# endif
    to_bitmap(&o, &(o.readtable.table));
  type_info[t_pathname].descriptor =
    to_bitmap(&o, &(o.pathname.version)) |
    to_bitmap(&o, &(o.pathname.type)) |
    to_bitmap(&o, &(o.pathname.name)) |
    to_bitmap(&o, &(o.pathname.directory)) |
    to_bitmap(&o, &(o.pathname.device)) |
    to_bitmap(&o, &(o.pathname.host));
  type_info[t_bytecodes].descriptor =
    to_bitmap(&o, &(o.bytecodes.name)) |
    to_bitmap(&o, &(o.bytecodes.definition)) |
    to_bitmap(&o, &(o.bytecodes.code)) |
    to_bitmap(&o, &(o.bytecodes.data)) |
    to_bitmap(&o, &(o.bytecodes.file)) |
    to_bitmap(&o, &(o.bytecodes.file_position));
  type_info[t_bclosure].descriptor =
    to_bitmap(&o, &(o.bclosure.code)) |
    to_bitmap(&o, &(o.bclosure.lex));
  type_info[t_cfun].descriptor =
    to_bitmap(&o, &(o.cfun.name)) |
    to_bitmap(&o, &(o.cfun.block)) |
    to_bitmap(&o, &(o.cfun.file)) |
    to_bitmap(&o, &(o.cfun.file_position));
  type_info[t_cfunfixed].descriptor =
    to_bitmap(&o, &(o.cfunfixed.name)) |
    to_bitmap(&o, &(o.cfunfixed.block)) |
    to_bitmap(&o, &(o.cfunfixed.file)) |
    to_bitmap(&o, &(o.cfunfixed.file_position));
  type_info[t_cclosure].descriptor =
    to_bitmap(&o, &(o.cclosure.env)) |
    to_bitmap(&o, &(o.cclosure.block)) |
    to_bitmap(&o, &(o.cclosure.file)) |
    to_bitmap(&o, &(o.cclosure.file_position));
  type_info[t_instance].descriptor =
    to_bitmap(&o, &(o.instance.clas)) |
    to_bitmap(&o, &(o.instance.slotds)) |
    to_bitmap(&o, &(o.instance.slots));
# ifdef ECL_THREADS
  type_info[t_process].descriptor =
    to_bitmap(&o, &(o.process.name)) |
    to_bitmap(&o, &(o.process.function)) |
    to_bitmap(&o, &(o.process.args)) |
    to_bitmap(&o, &(o.process.env)) |
    to_bitmap(&o, &(o.process.interrupt)) |
    to_bitmap(&o, &(o.process.initial_bindings)) |
    to_bitmap(&o, &(o.process.parent)) |
    to_bitmap(&o, &(o.process.exit_values)) |
    to_bitmap(&o, &(o.process.woken_up));
  type_info[t_lock].descriptor =
    to_bitmap(&o, &(o.lock.name)) |
    to_bitmap(&o, &(o.lock.owner));
  type_info[t_rwlock].descriptor =
    to_bitmap(&o, &(o.rwlock.name));
  type_info[t_condition_variable].descriptor = 0;
  type_info[t_semaphore].descriptor = 
    to_bitmap(&o, &(o.semaphore.name));
  type_info[t_barrier].descriptor = 
    to_bitmap(&o, &(o.barrier.name));
  type_info[t_mailbox].descriptor = 
    to_bitmap(&o, &(o.mailbox.name)) |
    to_bitmap(&o, &(o.mailbox.data));
# endif
  type_info[t_codeblock].descriptor =
    to_bitmap(&o, &(o.cblock.data)) |
    to_bitmap(&o, &(o.cblock.temp_data)) |
    to_bitmap(&o, &(o.cblock.next)) |
    to_bitmap(&o, &(o.cblock.name)) |
    to_bitmap(&o, &(o.cblock.links)) |
    to_bitmap(&o, &(o.cblock.source)) |
    to_bitmap(&o, &(o.cblock.refs)) |
    to_bitmap(&o, &(o.cblock.error));
  type_info[t_foreign].descriptor =
    to_bitmap(&o, &(o.foreign.data)) |
    to_bitmap(&o, &(o.foreign.tag));
  type_info[t_frame].descriptor =
    to_bitmap(&o, &(o.frame.stack)) |
    to_bitmap(&o, &(o.frame.base)) |
    to_bitmap(&o, &(o.frame.env));
  type_info[t_weak_pointer].descriptor = 0;
#ifdef ECL_SSE2
  type_info[t_sse_pack].descriptor = 0;
#endif
  for (i = 0; i < t_end; i++) {
    GC_word descriptor = type_info[i].descriptor;
    int bits = type_info[i].size / sizeof(GC_word);
    if (descriptor) {
#ifdef GBC_BOEHM_OWN_MARKER
      type_info[i].allocator = allocate_object_marked;
      descriptor = GC_make_descriptor(&descriptor, bits);
      descriptor &= ~GC_DS_TAGS;
#else
      GC_word mask = (1 << (bits-1)) - 1;
      mask ^= (descriptor >> 1);
      if (mask == 0)
        type_info[i].allocator = allocate_object_full;
      else
        type_info[i].allocator = allocate_object_typed;
      descriptor = GC_make_descriptor(&descriptor, bits);
#endif
    } else {
      type_info[i].allocator = allocate_object_atomic;
      descriptor = 0;
    }
    type_info[i].descriptor = descriptor;
  }
#endif /* GBC_BOEHM_PRECISE */
}

extern void (*GC_push_other_roots)();
static void (*old_GC_push_other_roots)();
static void stacks_scanner();

static int alloc_initialized = FALSE;

void
init_alloc(void)
{
  if (alloc_initialized) return;
  alloc_initialized = TRUE;
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

  GC_set_max_heap_size(cl_core.max_heap_size = ecl_option_values[ECL_OPT_HEAP_SIZE]);
  /* Save some memory for the case we get tight. */
  if (cl_core.max_heap_size == 0) {
    cl_index size = ecl_option_values[ECL_OPT_HEAP_SAFETY_AREA];
    cl_core.safety_region = ecl_alloc_atomic_unprotected(size);
  } else if (cl_core.safety_region) {
    cl_core.safety_region = 0;
  }

  init_type_info();

  old_GC_push_other_roots = GC_push_other_roots;
  GC_push_other_roots = stacks_scanner;
  GC_old_start_callback = GC_get_start_callback();
  GC_set_start_callback(gather_statistics);
  GC_set_java_finalization(1);
  GC_set_oom_fn(out_of_memory);
  GC_set_warn_proc(no_warnings);
  GC_enable();
}

/**********************************************************
 *              FINALIZATION                              *
 **********************************************************/

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
    ecl_atomic_push(&cl_core.reused_indices,
                    ecl_make_fixnum(o->symbol.binding));
    o->symbol.binding = ECL_MISSING_SPECIAL_BINDING;
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
  @(return size1 size2 old_status);
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

/**********************************************************
 *              GARBAGE COLLECTOR                         *
 **********************************************************/

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

/**********************************************************
 *              GARBAGE COLLECTION                        *
 **********************************************************/

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

/**********************************************************************
 * WEAK POINTERS
 */

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

#endif /* GBC_BOEHM */
