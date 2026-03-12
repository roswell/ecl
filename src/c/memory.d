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
  void *ptr = malloc(n);
  if (ptr == NULL) out_of_memory();
  return ptr;
}

void
ecl_free(void *ptr)
{
  free(ptr);
}

void *
ecl_resize(void *ptr, cl_index osize, cl_index nsize)
{
  ptr = realloc(ptr, nsize);
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

/* -- Object database ------------------------------------------------------- */
struct ecl_type_information ecl_type_info[t_end];

static void
assert_type_tag(cl_type t)
{
  if (ecl_unlikely(t <= t_start || t >= t_end)) {
    printf("\ttype = %d\n", t);
    ecl_internal_error("Collector called with invalid tag number.");
  }
}

cl_index
ecl_object_byte_size(cl_type t)
{
  assert_type_tag(t);
  return ecl_type_info[t].size;
}

static void
init_type_info(cl_type type, const char *name, cl_index size, uintmax_t desc)
{
  ecl_type_info[type].name = name;
  ecl_type_info[type].size = size;
  ecl_type_info[type].descriptor = desc;
}

/* Note that a bitmap in some cases describe pointers that are not ~cl_object~,
   like ~vector.self.t~ and ~readtable.table~. */
static cl_index
to_bitmap(void *x, void *y)
{
  cl_index n = (char*)y - (char*)x;
  if (n % sizeof(void*))
    ecl_internal_error("Misaligned pointer in ECL structure.");
  n /= sizeof(void*);
  return 1 << n;
}

#define init_tm(type, name, struct_name, descriptor)            \
  init_type_info(type, name, sizeof(struct struct_name), descriptor)

static void
init_type_info_database(void)
{
  union cl_lispunion o;
  struct ecl_cons c;
  int i;
  for (i = 0; i < t_end; i++) {
    ecl_type_info[i].t = i;
    ecl_type_info[i].size = 0;
    ecl_type_info[i].descriptor = 0;
  }
  ecl_type_info[t_character].name = "CHARACTER";
  ecl_type_info[t_fixnum].name = "FIXNUM";
  init_tm(t_list, "CONS", ecl_cons,
          to_bitmap(&c, &(c.car)) |
          to_bitmap(&c, &(c.cdr)));
  init_tm(t_bignum, "BIGNUM", ecl_bignum,
          to_bitmap(&o, &(ECL_BIGNUM_LIMBS(&o))));
  init_tm(t_ratio, "RATIO", ecl_ratio,
          to_bitmap(&o, &(o.ratio.num)) |
          to_bitmap(&o, &(o.ratio.den)));
  init_tm(t_singlefloat, "SINGLE-FLOAT", ecl_singlefloat, 0);
  init_tm(t_doublefloat, "DOUBLE-FLOAT", ecl_doublefloat, 0);
  init_tm(t_longfloat, "LONG-FLOAT", ecl_long_float, 0);
  init_tm(t_complex, "COMPLEX", ecl_complex,
          to_bitmap(&o, &(o.gencomplex.real)) |
          to_bitmap(&o, &(o.gencomplex.imag)));
#ifdef ECL_COMPLEX_FLOAT
  init_tm(t_csfloat, "COMPLEX-SINGLE-FLOAT", ecl_csfloat, 0);
  init_tm(t_cdfloat, "COMPLEX-DOUBLE-FLOAT", ecl_cdfloat, 0);
  init_tm(t_clfloat, "COMPLEX-LONG-FLOAT", ecl_clfloat, 0);
#endif
  init_tm(t_symbol, "SYMBOL", ecl_symbol,
          to_bitmap(&o, &(o.symbol.value)) |
          to_bitmap(&o, &(o.symbol.gfdef)) |
          to_bitmap(&o, &(o.symbol.macfun)) |
          to_bitmap(&o, &(o.symbol.sfdef)) |
          to_bitmap(&o, &(o.symbol.plist)) |
          to_bitmap(&o, &(o.symbol.name)) |
          to_bitmap(&o, &(o.symbol.hpack)));
  init_tm(t_package, "PACKAGE", ecl_package,
          to_bitmap(&o, &(o.pack.name)) |
          to_bitmap(&o, &(o.pack.nicknames)) |
          to_bitmap(&o, &(o.pack.local_nicknames)) |
          to_bitmap(&o, &(o.pack.nicknamedby)) |
          to_bitmap(&o, &(o.pack.shadowings)) |
          to_bitmap(&o, &(o.pack.uses)) |
          to_bitmap(&o, &(o.pack.usedby)) |
          to_bitmap(&o, &(o.pack.internal)) |
          to_bitmap(&o, &(o.pack.external)));
  init_tm(t_hashtable, "HASH-TABLE", ecl_hashtable,
          to_bitmap(&o, &(o.hash.data)) |
          to_bitmap(&o, &(o.hash.sync_lock)) |
          to_bitmap(&o, &(o.hash.generic_test)) |
          to_bitmap(&o, &(o.hash.generic_hash)) |
          to_bitmap(&o, &(o.hash.rehash_size)) |
          to_bitmap(&o, &(o.hash.threshold)));
  init_tm(t_array, "ARRAY", ecl_array,
          to_bitmap(&o, &(o.array.dims)) |
          to_bitmap(&o, &(o.array.self.t)) |
          to_bitmap(&o, &(o.array.displaced)));
  init_tm(t_vector, "VECTOR", ecl_vector,
          to_bitmap(&o, &(o.vector.self.t)) |
          to_bitmap(&o, &(o.vector.displaced)));
#ifdef ECL_UNICODE
  init_tm(t_string, "STRING", ecl_string,
          to_bitmap(&o, &(o.string.self)) |
          to_bitmap(&o, &(o.string.displaced)));
#endif
  init_tm(t_base_string, "BASE-STRING", ecl_base_string,
          to_bitmap(&o, &(o.base_string.self)) |
          to_bitmap(&o, &(o.base_string.displaced)));
  init_tm(t_bitvector, "BIT-VECTOR", ecl_vector,
          to_bitmap(&o, &(o.vector.self.t)) |
          to_bitmap(&o, &(o.vector.displaced)));
  init_tm(t_stream, "STREAM", ecl_stream,
          to_bitmap(&o, &(o.stream.ops)) |
          to_bitmap(&o, &(o.stream.object0)) |
          to_bitmap(&o, &(o.stream.object1)) |
          to_bitmap(&o, &(o.stream.last_byte)) |
          to_bitmap(&o, &(o.stream.byte_stack)) |
          to_bitmap(&o, &(o.stream.buffer)) |
          to_bitmap(&o, &(o.stream.format)) |
          to_bitmap(&o, &(o.stream.format_table)));
  init_tm(t_random, "RANDOM-STATE", ecl_random,
          to_bitmap(&o, &(o.random.value)));
  init_tm(t_readtable, "READTABLE", ecl_readtable,
# ifdef ECL_UNICODE
          to_bitmap(&o, &(o.readtable.hash)) |
# endif
          to_bitmap(&o, &(o.readtable.table)));
  init_tm(t_pathname, "PATHNAME", ecl_pathname,
          to_bitmap(&o, &(o.pathname.version)) |
          to_bitmap(&o, &(o.pathname.type)) |
          to_bitmap(&o, &(o.pathname.name)) |
          to_bitmap(&o, &(o.pathname.directory)) |
          to_bitmap(&o, &(o.pathname.device)) |
          to_bitmap(&o, &(o.pathname.host)));
  init_tm(t_bytecodes, "BYTECODES", ecl_bytecodes,
          to_bitmap(&o, &(o.bytecodes.name)) |
          to_bitmap(&o, &(o.bytecodes.definition)) |
          to_bitmap(&o, &(o.bytecodes.code)) |
          to_bitmap(&o, &(o.bytecodes.data)) |
          to_bitmap(&o, &(o.bytecodes.flex)) |
          to_bitmap(&o, &(o.bytecodes.file)) |
          to_bitmap(&o, &(o.bytecodes.file_position)));
  init_tm(t_bclosure, "BCLOSURE", ecl_bclosure,
          to_bitmap(&o, &(o.bclosure.code)) |
          to_bitmap(&o, &(o.bclosure.lex)));
  init_tm(t_cfun, "CFUN", ecl_cfun,
          to_bitmap(&o, &(o.cfun.name)) |
          to_bitmap(&o, &(o.cfun.block)) |
          to_bitmap(&o, &(o.cfun.file)) |
          to_bitmap(&o, &(o.cfun.file_position)));
  init_tm(t_cfunfixed, "CFUNFIXED", ecl_cfunfixed,
          to_bitmap(&o, &(o.cfunfixed.name)) |
          to_bitmap(&o, &(o.cfunfixed.block)) |
          to_bitmap(&o, &(o.cfunfixed.file)) |
          to_bitmap(&o, &(o.cfunfixed.file_position)));
  init_tm(t_cclosure, "CCLOSURE", ecl_cclosure,
          to_bitmap(&o, &(o.cclosure.env)) |
          to_bitmap(&o, &(o.cclosure.block)) |
          to_bitmap(&o, &(o.cclosure.file)) |
          to_bitmap(&o, &(o.cclosure.file_position)));
  init_tm(t_instance, "INSTANCE", ecl_instance,
          to_bitmap(&o, &(o.instance.clas)) |
          to_bitmap(&o, &(o.instance.slotds)) |
          to_bitmap(&o, &(o.instance.slots)));
#ifdef ECL_THREADS
  init_tm(t_process, "PROCESS", ecl_process,
          to_bitmap(&o, &(o.process.name)) |
          to_bitmap(&o, &(o.process.function)) |
          to_bitmap(&o, &(o.process.args)) |
          to_bitmap(&o, &(o.process.inherit_bindings_p)) |
          to_bitmap(&o, &(o.process.exit_values)) |
          to_bitmap(&o, &(o.process.woken_up)) |
          to_bitmap(&o, &(o.process.env)));
  init_tm(t_lock, "LOCK", ecl_lock,
          to_bitmap(&o, &(o.lock.name)) |
          to_bitmap(&o, &(o.lock.owner)));
  init_tm(t_rwlock, "RWLOCK", ecl_rwlock,
          to_bitmap(&o, &(o.rwlock.name)));
  init_tm(t_condition_variable, "CONDITION-VARIABLE", ecl_condition_variable, 0);
  init_tm(t_semaphore, "SEMAPHORE", ecl_semaphore,
          to_bitmap(&o, &(o.semaphore.name)));
  init_tm(t_barrier, "BARRIER", ecl_barrier,
          to_bitmap(&o, &(o.barrier.name)));
  init_tm(t_mailbox, "MAILBOX", ecl_mailbox,
          to_bitmap(&o, &(o.mailbox.name)) |
          to_bitmap(&o, &(o.mailbox.data)));
#endif
  init_tm(t_cont, "CONTINUATION", ecl_cont,
          to_bitmap(&o, &(o.cont.thread)) |
          to_bitmap(&o, &(o.cont.stack)) |
          to_bitmap(&o, &(o.cont.env)));
  init_tm(t_thread, "THREAD", ecl_thread,
          to_bitmap(&o, &(o.thread.fun)) |
          to_bitmap(&o, &(o.thread.cont)));
  init_tm(t_codeblock, "CODEBLOCK", ecl_codeblock,
          to_bitmap(&o, &(o.cblock.data)) |
          to_bitmap(&o, &(o.cblock.temp_data)) |
          to_bitmap(&o, &(o.cblock.next)) |
          to_bitmap(&o, &(o.cblock.name)) |
          to_bitmap(&o, &(o.cblock.links)) |
          to_bitmap(&o, &(o.cblock.source)) |
          to_bitmap(&o, &(o.cblock.refs)) |
          to_bitmap(&o, &(o.cblock.error)));
  init_tm(t_foreign, "FOREIGN", ecl_foreign,
          to_bitmap(&o, &(o.foreign.data)) |
          to_bitmap(&o, &(o.foreign.tag)));
  init_tm(t_frame, "STACK-FRAME", ecl_stack_frame,
          to_bitmap(&o, &(o.frame.env)));
  init_tm(t_token, "TOKEN", ecl_token,
          to_bitmap(&o, &(o.token.string)) |
          to_bitmap(&o, &(o.token.escape)));
  init_tm(t_module, "MODULE", ecl_module, 0);
  init_tm(t_exception, "EXCEPTION", ecl_exception,
          to_bitmap(&o, &(o.exception.arg1)) |
          to_bitmap(&o, &(o.exception.arg2)) |
          to_bitmap(&o, &(o.exception.arg3)));
  init_tm(t_weak_pointer, "WEAK-POINTER", ecl_weak_pointer, 0);
#ifdef ECL_SSE2
  init_tm(t_sse_pack, "SSE-PACK", ecl_sse_pack, 0);
#endif
}

/* -- Constructors ---------------------------------------------------------- */

void *
ecl_alloc(cl_index n)
{
  const cl_env_ptr the_env = ecl_process_env_unsafe();
  void *ptr = NULL;
  if(!the_env) {
    return ecl_core.allocator->allocate_memory(n);
  } else {
    ecl_disable_interrupts_env(the_env);
    ptr = ecl_core.allocator->allocate_memory(n);
    ecl_enable_interrupts_env(the_env);
  }
  return ptr;
}

void *
ecl_alloc_atomic(cl_index n)
{
  const cl_env_ptr the_env = ecl_process_env_unsafe();
  void *ptr = NULL;
  if(!the_env) {
    return ecl_core.allocator->allocate_atomic(n);
  } else {
    ecl_disable_interrupts_env(the_env);
    ptr = ecl_core.allocator->allocate_atomic(n);
    ecl_enable_interrupts_env(the_env);
  }
  return ptr;
}

void *
ecl_alloc_manual(cl_index n)
{
  const cl_env_ptr the_env = ecl_process_env_unsafe();
  void *ptr = NULL;
  if(!the_env) {
    return ecl_core.allocator->allocate_manual(n);
  } else {
    ecl_disable_interrupts_env(the_env);
    ptr = ecl_core.allocator->allocate_manual(n);
    ecl_enable_interrupts_env(the_env);
  }
  return ptr;
}

void *
ecl_realloc(void *ptr, cl_index o, cl_index n)
{
  const cl_env_ptr the_env = ecl_process_env_unsafe();
  if(!the_env) {
    ecl_core.allocator->realloc_memory(ptr, o, n);
  } else {
    ecl_disable_interrupts_env(the_env);
    ecl_core.allocator->realloc_memory(ptr, o, n);
    ecl_enable_interrupts_env(the_env);
  }
}

void
ecl_dealloc(void *ptr)
{
  const cl_env_ptr the_env = ecl_process_env_unsafe();
  if(!the_env) {
    ecl_core.allocator->dealloc_memory(ptr);
  } else {
    ecl_disable_interrupts_env(the_env);
    ecl_core.allocator->dealloc_memory(ptr);
    ecl_enable_interrupts_env(the_env);
  }
}

cl_object
ecl_alloc_object(cl_type t)
{
  assert_type_tag(t);
  switch(t) {
  case t_list:                  /* Small cons (no d.t) */
    return ecl_cons(ECL_NIL, ECL_NIL);
  case t_character:
    return ECL_CODE_CHAR(' ');  /* Immediate character */
  case t_fixnum:
    return ecl_make_fixnum(0);  /* Immediate fixnum */
  default:
    {
      const cl_env_ptr the_env = ecl_process_env_unsafe();
      cl_object o;
      if(the_env) ecl_disable_interrupts_env(the_env);
      o = ecl_core.allocator->make_object(t);
      o->d.t = t;
      if(the_env) ecl_enable_interrupts_env(the_env);
      return o;
    }
  }
}

void
ecl_free_object(cl_object ptr)
{
  const cl_env_ptr the_env = ecl_process_env_unsafe();
  if(!the_env) {
    ecl_core.allocator->free_object(ptr);
  } else {
    ecl_disable_interrupts_env(the_env);
    ecl_core.allocator->free_object(ptr);
    ecl_enable_interrupts_env(the_env);
  }
}

/* -- Helpers --------------------------------------------------------------- */

cl_object                       /* used by bignum.d */
ecl_alloc_compact_object(cl_type t, cl_index extra_space)
{
  cl_index size = ecl_type_info[t].size;
  cl_object x = ecl_alloc_atomic(size + extra_space);
  x->array.t = t;
  x->array.displaced = (void*)(((char*)x) + size);
  return x;
}

cl_object
ecl_cons(cl_object a, cl_object d)
{
  struct ecl_cons *obj = ecl_alloc(sizeof(struct ecl_cons));
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

/*
  Make a string of a certain size, with some leading zeros to keep C happy. The
  string must be adjustable, to allow further growth. (See unixfsys.c for its
  use).
*/
cl_object
ecl_alloc_adjustable_base_string(cl_index l)
{
  cl_object output = ecl_alloc_object(t_base_string);
  output->base_string.self       = (ecl_base_char *)ecl_alloc_atomic(l+1);
  output->base_string.self[l]    = 0;
  output->base_string.flags      = ECL_FLAG_HAS_FILL_POINTER | ECL_FLAG_ADJUSTABLE;
  output->base_string.elttype    = ecl_aet_bc;
  output->base_string.displaced  = ECL_NIL;
  output->base_string.dim        = l;
  output->base_string.fillp      = 0;
  return output;
}

#ifdef ECL_UNICODE
cl_object
ecl_alloc_adjustable_extended_string(cl_index l)
{
  cl_index bytes = sizeof(ecl_character) * l;
  cl_object output = ecl_alloc_object(t_string);
  output->string.self       = (ecl_character *)ecl_alloc_atomic(bytes);
  output->string.flags      = ECL_FLAG_HAS_FILL_POINTER | ECL_FLAG_ADJUSTABLE;
  output->string.elttype    = ecl_aet_ch;
  output->string.displaced  = ECL_NIL;
  output->string.dim        = l;
  output->string.fillp      = 0;
  return output;
}
#endif

static cl_object
ecl_alloc_token()
{
  cl_object o = ecl_alloc_object(t_token);
  o->token.escaped = 0;
  o->token.string = si_get_buffer_string();
  o->token.escape = ecl_make_stack(0);
  return o;
}

/* -- Resource manager ------------------------------------------------------ */

cl_object
si_get_buffer_string()
{
  const cl_env_ptr env = ecl_process_env();
  cl_object pool = env->string_pool;
  cl_object output;
  if (pool == ECL_NIL) {
#ifdef ECL_UNICODE
    output = ecl_alloc_adjustable_extended_string(ECL_BUFFER_STRING_SIZE);
#else
    output = ecl_alloc_adjustable_base_string(ECL_BUFFER_STRING_SIZE);
#endif
  } else {
    output = CAR(pool);
    env->string_pool = CDR(pool);
  }
  TOKEN_STRING_FILLP(output) = 0;
  @(return output);
}

/* FIXME pools should be resizeable stacks. */
cl_object
si_put_buffer_string(cl_object string)
{
  if (string != ECL_NIL) {
    const cl_env_ptr env = ecl_process_env();
    cl_object pool = env->string_pool;
    cl_index l = 0;
    if (pool != ECL_NIL) {
      /* We store the size of the pool in the string index */
      l = TOKEN_STRING_FILLP(ECL_CONS_CAR(pool));
    }
    if (l < ECL_MAX_STRING_POOL_SIZE) {
      TOKEN_STRING_FILLP(string) = l+1;
      env->string_pool = CONS(string, pool);
    }
  }
  @(return);
}

/* FIXME pools should be resizeable stacks. */
cl_object
ecl_get_reader_token(void)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object pool = the_env->token_pool;
  cl_object aux;
  if (pool != ECL_NIL) {
    aux = CAR(pool);
    the_env->token_pool = CDR(pool);
    return aux;
  }
  return ecl_alloc_token();
}

void
ecl_put_reader_token(cl_object token)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object pool = the_env->token_pool;
  TOKEN_STRING_FILLP(token->token.string) = 0;
  TOKEN_ESCAPE_FILLP(token->token.escape) = 0;
  token->token.escaped = 0;
  the_env->token_pool = CONS(token, pool);
}

/* -- Rudimentary manual memory allocator ----------------------------------- */

static cl_object
make_object(cl_type t)
{
  struct ecl_type_information *ti = ecl_type_info + t;
  return ecl_malloc(ti->size);
}

static void
free_object(cl_object o)
{
  /* FIXME this should invoke the finalizer! That is - reify finalizers here. */
  ecl_free(o);
}

struct ecl_allocator_ops manual_allocator = {
  .allocate_memory = ecl_malloc,
  .allocate_atomic = ecl_malloc,
  .allocate_manual = ecl_malloc,
  .realloc_memory = ecl_resize,
  .dealloc_memory = ecl_free,
  .make_object = make_object,
  .free_object = free_object
};

void
init_memory ()
{
  init_type_info_database();
  ecl_core.allocator = &manual_allocator;
}
