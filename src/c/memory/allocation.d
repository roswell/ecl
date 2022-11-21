/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * allocation.c - memory allocation based on the Boehm GC
 *
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 * Copyright (c) 2022 Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <stdio.h>
#include <ecl/ecl.h>
#include <ecl/threads.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/page.h>
#ifdef ECL_WSOCK
#include <winsock.h>
#endif

#ifdef GBC_BOEHM
#include <gc/gc_mark.h>

static struct ecl_type_information {
  size_t size;
#ifdef GBC_BOEHM_PRECISE
  GC_word descriptor;
#endif
  cl_object (*allocator)(struct ecl_type_information *);
  size_t t;
} type_info[t_end];

cl_index
ecl_object_byte_size(cl_type t)
{
  /* Immediate type. */
  if (t == t_fixnum || t == t_character)
    return 0;
  if (t >= t_end)
    ecl_internal_error("ecl_object_byte_size invoked with invalid tag number.");
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
  ecl_internal_error("Collector called with invalid tag number.");
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
    to_bitmap(&o, &(o.process.woken_up)) |
    to_bitmap(&o, &(o.process.queue_record));
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

void
init_alloc(void)
{
  static int alloc_initialized = FALSE;
  if (alloc_initialized) return;
  alloc_initialized = TRUE;
  init_type_info();
  init_GC();
}

#endif /* GBC_BOEHM */
