/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * serialize.d - serialize a bunch of lisp data
 *
 * Copyright (c) 2010 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <string.h>
#define ECL_DEFINE_AET_SIZE
#include <ecl/internal.h>

#ifdef ECL_EXTERNALIZABLE

struct fake_package {
  _ECL_HDR;
  cl_object name;
};

struct fake_symbol {
  _ECL_HDR;
  cl_object name;
  cl_object pack;
};

#define ROUND_TO_WORD(int)                                      \
  ((int + sizeof(cl_fixnum) - 1) & ~(sizeof(cl_fixnum) - 1))
#define ROUNDED_SIZE(name)                      \
  ROUND_TO_WORD(sizeof(struct name))

static cl_index object_size[] = {
  0, /* t_start */
  ROUNDED_SIZE(ecl_cons), /* t_list */
  0, /* t_character = 2 */
  0, /* t_fixnum = 3 */
  ROUNDED_SIZE(ecl_bignum), /* t_bignum = 4 */
  ROUNDED_SIZE(ecl_ratio), /* t_ratio */
  ROUNDED_SIZE(ecl_singlefloat), /* t_singlefloat */
  ROUNDED_SIZE(ecl_doublefloat), /* t_doublefloat */
  ROUNDED_SIZE(ecl_long_float), /* t_longfloat */
  ROUNDED_SIZE(ecl_complex), /* t_complex */
  ROUNDED_SIZE(fake_symbol), /* t_symbol */
  ROUNDED_SIZE(fake_package), /* t_package */
  ROUNDED_SIZE(ecl_hashtable), /* t_hashtable */
  ROUNDED_SIZE(ecl_array), /* t_array */
  ROUNDED_SIZE(ecl_vector), /* t_vector */
#ifdef ECL_UNICODE
  ROUNDED_SIZE(ecl_string), /* t_string */
#endif
  ROUNDED_SIZE(ecl_base_string), /* t_base_string */
  ROUNDED_SIZE(ecl_vector), /* t_bitvector */
  ROUNDED_SIZE(ecl_stream), /* t_stream */
  ROUNDED_SIZE(ecl_random), /* t_random */
  ROUNDED_SIZE(ecl_readtable), /* t_readtable */
  ROUNDED_SIZE(ecl_pathname), /* t_pathname */
  ROUNDED_SIZE(ecl_bytecodes), /* t_bytecodes */
  ROUNDED_SIZE(ecl_bclosure), /* t_bclosure */
  ROUNDED_SIZE(ecl_cfun), /* t_cfun */
  ROUNDED_SIZE(ecl_cfunfixed), /* t_cfunfixed */
  ROUNDED_SIZE(ecl_cclosure), /* t_cclosure */
  ROUNDED_SIZE(ecl_instance), /* t_instance */
#ifdef ECL_THREADS
  ROUNDED_SIZE(ecl_process), /* t_process */
  ROUNDED_SIZE(ecl_lock), /* t_lock */
  ROUNDED_SIZE(ecl_rwlock), /* t_rwlock */
  ROUNDED_SIZE(ecl_condition_variable), /* t_condition_variable */
  ROUNDED_SIZE(ecl_semaphore), /* t_semaphore */
  ROUNDED_SIZE(ecl_barrier), /* t_barrier */
  ROUNDED_SIZE(ecl_mailbox), /* t_mailbox */
#endif
  ROUNDED_SIZE(ecl_codeblock), /* t_codeblock */
  ROUNDED_SIZE(ecl_foreign), /* t_foreign */
  ROUNDED_SIZE(ecl_frame), /* t_frame */
  ROUNDED_SIZE(ecl_weak_pointer) /* t_weak_pointer */
#ifdef ECL_SSE2
  , ROUNDED_SIZE(ecl_sse_pack) /* t_sse_pack */
#endif
};

typedef struct pool {
  cl_object data; /* vector of bytes containing the serialized objects */
  cl_object hash; /* hashtable mapping already serialized objects to indices in data */
  cl_object queue; /* queue of objects to be serialized */
  cl_object last; /* last cons cell of queue */
} *pool_t;

static cl_index
alloc(pool_t pool, cl_index size)
{
  cl_index bytes = ROUND_TO_WORD(size);
  cl_index fillp = pool->data->vector.fillp;
  cl_index next_fillp = fillp + bytes;
  if (next_fillp >= pool->data->vector.dim) {
    cl_index new_dim = next_fillp + next_fillp / 2;
    pool->data = si_adjust_vector(pool->data, ecl_make_fixnum(new_dim));
  }
  pool->data->vector.fillp = next_fillp;
  return fillp;
}

/* Set the tag bits of an index into the array of serialized objects
   to zero to make it distinguishable from an ordinary fixnum */
static cl_object
fix_to_ptr(cl_object ptr)
{
  cl_fixnum i = (cl_fixnum)ptr;
  return (cl_object)(i & ~ECL_IMMEDIATE_TAG);
}

static cl_object
enqueue(pool_t pool, cl_object what)
{
  cl_object index;
  if (ECL_FIXNUMP(what) || ECL_CHARACTERP(what) || what == OBJNULL) {
    return what;
  }
  if (Null(what))
    return what;
  index = ecl_gethash_safe(what, pool->hash, OBJNULL);
  if (index == OBJNULL) {
    cl_object cons;
    index = ecl_make_fixnum(pool->hash->hash.entries);
    ecl_sethash(what, pool->hash, index);
    cons = ecl_cons(what, ECL_NIL);
    ECL_RPLACD(pool->last, cons);
    pool->last = cons;
  }
  return fix_to_ptr(index);
}

typedef struct {
  _ECL_HDR;
  cl_object car, cdr;
} large_cons;
typedef large_cons *large_cons_ptr;

static cl_index
serialize_bits(pool_t pool, void *data, cl_index size)
{
  cl_index index = alloc(pool, size);
  memcpy(pool->data->vector.self.b8 + index, data, size);
  return index;
}

static void
serialize_object_ptr(pool_t pool, cl_object *ptr, cl_index dim)
{
  cl_index index = serialize_bits(pool, ptr, dim*sizeof(cl_object));
  for (; dim; dim--, index += sizeof(cl_object)) {
    cl_object *p = (cl_object *)(pool->data->vector.self.b8 + index);
    *p = enqueue(pool, *p);
  }
}

static void serialize_vector(pool_t pool, cl_object v);

static void
serialize_displaced_vector(pool_t pool, cl_object v)
{
  cl_object disp = v->vector.displaced;
  cl_object to = ECL_CONS_CAR(disp);
  if (Null(to)) {
    v->vector.displaced = ECL_NIL;
    serialize_vector(pool, v);
  } else {
    /* TODO: Implement serialization of displaced bit vectors */
    if (v->vector.elttype == ecl_aet_bit) {
      FEerror("ECL can not yet serialize displaced bitvectors", 0);
    }
    cl_index index = (v->vector.self.b8 - to->vector.self.b8) / ecl_aet_size[v->vector.elttype];
    v->vector.displaced = enqueue(pool, to);
    v->vector.self.b8 = (uint8_t*)index;
  }
}

static size_t
vector_self_size(cl_object v) {
  if (v->vector.elttype == ecl_aet_bit) {
    return ROUND_TO_WORD((v->vector.dim + (CHAR_BIT-1))/CHAR_BIT);
  } else {
    return ROUND_TO_WORD(v->vector.dim * ecl_aet_size[v->vector.elttype]);
  }
}

static void
serialize_vector(pool_t pool, cl_object v)
{
  if (!Null(v->vector.displaced)) {
    serialize_displaced_vector(pool, v);
  } else if (v->vector.elttype == ecl_aet_object) {
    serialize_object_ptr(pool, v->vector.self.t, v->vector.dim);
  } else {
    serialize_bits(pool, v->vector.self.b8, vector_self_size(v));
  }
}

static void
serialize_hashtable(pool_t pool, cl_object h)
{
  /* FIXME: Serializing all of h->hash.data is a big waste if the
     hashtable has only a small number of entries. */
  cl_index size = h->hash.size;
  cl_index index = serialize_bits(pool, h->hash.data, size*sizeof(struct ecl_hashtable_entry));
  for (; size; size--, index += sizeof(struct ecl_hashtable_entry)) {
    struct ecl_hashtable_entry *p = (struct ecl_hashtable_entry *)(pool->data->vector.self.b8 + index);
    p->key = enqueue(pool, p->key);
    p->value = enqueue(pool, p->value);
  }
}

static void
serialize_one(pool_t pool, cl_object what)
{
  cl_index bytes, index;
  cl_object buffer;
  if (ECL_LISTP(what)) {
    cl_index bytes = ROUND_TO_WORD(sizeof(large_cons));
    cl_index index = alloc(pool, bytes);
    large_cons_ptr cons =
      (large_cons_ptr)(pool->data->vector.self.b8 + index);
    memset(cons, 0, bytes);
    cons->t = t_list;
    if (!Null(what)) {
      cons->car = enqueue(pool, ECL_CONS_CAR(what));
      cons->cdr = enqueue(pool, ECL_CONS_CDR(what));
    }
    return;
  }
  bytes = object_size[ecl_t_of(what)];
  index = alloc(pool, bytes);
  buffer = (cl_object)(pool->data->vector.self.b8 + index);
  memcpy(buffer, what, bytes);
  switch (ecl_t_of(what)) {
  case t_singlefloat:
  case t_doublefloat:
  case t_longfloat:
    break;
  case t_bignum: {
    int8_t sign = mpz_sgn(buffer->big.big_num);
    serialize_bits(pool, &sign, 1);
    cl_index bytes = (mpz_sizeinbase(buffer->big.big_num, 2) + 7) / 8;
    serialize_bits(pool, &bytes, sizeof(cl_index));
    cl_index index = alloc(pool, bytes);
    cl_index bytes_written;
    mpz_export(pool->data->vector.self.b8 + index, &bytes_written, 1, 1, 1, 0, buffer->big.big_num);
    break;
  }
  case t_ratio: {
    buffer->ratio.den = enqueue(pool, buffer->ratio.den);
    buffer->ratio.num = enqueue(pool, buffer->ratio.num);
    break;
  }
  case t_complex: {
    buffer->gencomplex.real = enqueue(pool, buffer->gencomplex.real);
    buffer->gencomplex.imag = enqueue(pool, buffer->gencomplex.imag);
    break;
  }
  case t_hashtable:
    buffer->hash.sync_lock = enqueue(pool, buffer->hash.sync_lock);
    buffer->hash.rehash_size = enqueue(pool, buffer->hash.rehash_size);
    buffer->hash.threshold = enqueue(pool, buffer->hash.threshold);
    serialize_hashtable(pool, buffer);
    break;
#ifdef ECL_UNICODE
  case t_string:
#endif
  case t_vector:
  case t_bitvector:
  case t_base_string: {
    serialize_vector(pool, buffer);
    break;
  }
  case t_array: {
    serialize_bits(pool, buffer->array.dims, sizeof(cl_index) * buffer->array.rank);
    /* We might have allocated memory in pool->data and thus the
       adress of pool->data->vector.self might have changed, hence we
       have to reload buffer. */
    buffer = (cl_object)(pool->data->vector.self.b8 + index);
    serialize_vector(pool, buffer);
    break;
  }
  case t_package: {
    struct fake_package *p = (struct fake_package *)buffer;
    p->name = enqueue(pool, what->pack.name);
    break;
  }
  case t_symbol: {
    struct fake_symbol *p = (struct fake_symbol *)buffer;
    p->name = enqueue(pool, what->symbol.name);
    p->pack = enqueue(pool, what->symbol.hpack);
    break;
  }
  case t_pathname:
    buffer->pathname.host =
      enqueue(pool, buffer->pathname.host);
    buffer->pathname.device =
      enqueue(pool, buffer->pathname.device);
    buffer->pathname.directory =
      enqueue(pool, buffer->pathname.directory);
    buffer->pathname.name = enqueue(pool, buffer->pathname.name);
    buffer->pathname.type = enqueue(pool, buffer->pathname.type);
    buffer->pathname.version =
      enqueue(pool, buffer->pathname.version);
    break;
  case t_random: {
    buffer->random.value = enqueue(pool, buffer->random.value);
    break;
  }
  case t_bclosure: {
    buffer->bclosure.code = enqueue(pool, buffer->bclosure.code);
    buffer->bclosure.lex = enqueue(pool, buffer->bclosure.lex);
    break;
  }
  case t_bytecodes: {
    buffer->bytecodes.name = enqueue(pool, buffer->bytecodes.name);
    buffer->bytecodes.definition = enqueue(pool, buffer->bytecodes.definition);
    buffer->bytecodes.data = enqueue(pool, buffer->bytecodes.data);
    buffer->bytecodes.file = enqueue(pool, buffer->bytecodes.file);
    buffer->bytecodes.file_position = enqueue(pool, buffer->bytecodes.file_position);
    buffer->bytecodes.code_size = serialize_bits(pool, buffer->bytecodes.code,
                                                 buffer->bytecodes.code_size);
    break;
  }
  default:
    FEerror("Unable to serialize object ~A", 1, what);
  }
}

static void
init_pool(pool_t pool, cl_object root)
{
  pool->data = si_make_vector(@'ext::byte8',
                              ecl_make_fixnum(1024),
                              ECL_T,
                              ecl_make_fixnum(2 * sizeof(cl_index)),
                              ECL_NIL,
                              ecl_make_fixnum(0));
  pool->hash = cl__make_hash_table(@'eql', ecl_make_fixnum(256),
                                   cl_core.rehash_size,
                                   cl_core.rehash_threshold);
  ecl_sethash(root, pool->hash, ecl_make_fixnum(0));
  pool->queue = ecl_list1(root);
  pool->last = pool->queue;
}

static cl_object
close_pool(pool_t pool)
{
  pool->data->vector.self.index[0] = pool->data->vector.fillp;
  pool->data->vector.self.index[1] = pool->hash->hash.entries;
  return pool->data;
}

cl_object
si_serialize(cl_object root)
{
  struct pool pool[1];
  init_pool(pool, root);
  while (!Null(pool->queue)) {
    cl_object what = ECL_CONS_CAR(pool->queue);
    serialize_one(pool, what);
    pool->queue = ECL_CONS_CDR(pool->queue);
  }
  @(return close_pool(pool));
}

static void *
reconstruct_bits(uint8_t *data, cl_index bytes)
{
  void *output = ecl_alloc_atomic(ROUND_TO_WORD(bytes));
  memcpy(output, data, bytes);
  return output;
}

static void *
reconstruct_object_ptr(uint8_t *data, cl_index bytes)
{
  void *output = ecl_alloc(ROUND_TO_WORD(bytes));
  memcpy(output, data, bytes);
  return output;
}

static uint8_t *
reconstruct_bytecodes(cl_object o, uint8_t *data)
{
  o->bytecodes.code = reconstruct_bits(data, o->bytecodes.code_size);
  data += o->bytecodes.code_size;
  return data;
}

static uint8_t *
reconstruct_vector(cl_object v, uint8_t *data)
{
  if (v->vector.displaced == ECL_NIL) {
    cl_index bytes = vector_self_size(v);
    if (v->vector.elttype == ecl_aet_object) {
      v->vector.self.t = reconstruct_object_ptr(data, bytes);
    } else {
      v->vector.self.t = reconstruct_bits(data, bytes);
    }
    data += bytes;
  }
  return data;
}

static uint8_t *
reconstruct_array(cl_object a, uint8_t *data)
{
  cl_index bytes = ROUND_TO_WORD(a->array.rank * sizeof(cl_index));
  a->array.dims = reconstruct_bits(data, bytes);
  return reconstruct_vector(a, data + bytes);
}

static uint8_t *
reconstruct_hashtable(cl_object h, uint8_t *data)
{
  cl_index bytes = ROUND_TO_WORD(h->hash.size * sizeof(struct ecl_hashtable_entry));
  h->hash.data = ecl_alloc(bytes);
  memcpy(h->hash.data, data, bytes);
  return data + bytes;
}

static uint8_t *
duplicate_object(uint8_t *data, cl_object *output)
{
  cl_type t = ecl_t_of((cl_object)data);
  cl_object o = ecl_alloc_object(t);
  cl_index bytes = object_size[t];
  memcpy(o, data, bytes);
  *output = o;
  return data + bytes;
}

static uint8_t *
reconstruct_one(uint8_t *data, cl_object *output)
{
  cl_object o = (cl_object)data;
  switch (ecl_t_of(o)) {
  case t_list: {
    large_cons_ptr c = (large_cons_ptr)data;
    cl_object car = c->car;
    cl_object cdr = c->cdr;
    if (car == (cl_object) 0 && cdr == (cl_object) 0) {
      *output = ECL_NIL;
    } else {
      *output = ecl_cons(car, cdr);
    }
    data += ROUND_TO_WORD(sizeof(large_cons));
    break;
  }
  case t_bignum: {
    data = duplicate_object(data, output);
    int8_t sign = (int8_t) *data;
    data += ROUND_TO_WORD(1);
    cl_index bytes = (cl_index) *data;
    data += ROUND_TO_WORD(sizeof(cl_index));
    mpz_init((*output)->big.big_num);
    mpz_import((*output)->big.big_num, bytes, 1, 1, 1, 0, data);
    if (sign == -1) {
      mpz_neg((*output)->big.big_num, (*output)->big.big_num);
    }
    data += ROUND_TO_WORD(bytes);
    break;
  }
  case t_hashtable:
    data = duplicate_object(data, output);
    data = reconstruct_hashtable(*output, data);
    break;
#ifdef ECL_UNICODE
  case t_string:
#endif
  case t_base_string:
  case t_vector:
  case t_bitvector:
    data = duplicate_object(data, output);
    data = reconstruct_vector(*output, data);
    break;
  case t_array:
    data = duplicate_object(data, output);
    data = reconstruct_array(*output, data);
    break;
  case t_package:
    *output = (cl_object)data;
    data += ROUND_TO_WORD(sizeof(struct fake_package));
    break;
  case t_symbol:
    *output = (cl_object)data;
    data += ROUND_TO_WORD(sizeof(struct fake_symbol));
    break;
  case t_bytecodes:
    data = duplicate_object(data, output);
    data = reconstruct_bytecodes(*output, data);
    break;
  default:
    data = duplicate_object(data, output);
  }
  return data;
}

static cl_object
get_object(cl_object o_or_index, cl_object *o_list)
{
  if (ECL_IMMEDIATE(o_or_index) || o_or_index == OBJNULL) {
    return o_or_index;
  } else {
    cl_index i = (cl_index)o_or_index >> 2;
    return o_list[i];
  }
}

static void
fixup_vector(cl_object v, cl_object *o_list)
{
  if (!ECL_IMMEDIATE(v->vector.displaced)) {
    cl_object to = get_object(v->vector.displaced, o_list);
    if (to != ECL_NIL) {
      cl_index offset = (cl_index)v->vector.self.b8;
      v->vector.displaced = ECL_NIL;
      ecl_displace(v, to, ecl_make_fixnum(offset));
      return;
    }
  }
  if (v->vector.elttype == ecl_aet_object) {
    cl_index i;
    cl_object *p = v->vector.self.t;
    for (i = v->vector.dim; i; i--, p++) {
      *p = get_object(*p, o_list);
    }
  }
}

static void
fixup_hashtable(cl_object h, cl_object *o_list)
{
  cl_index i;
  for (i = 0; i < h->hash.size; i++) {
    h->hash.data[i].key = get_object(h->hash.data[i].key, o_list);
    h->hash.data[i].value = get_object(h->hash.data[i].value, o_list);
  }
  h->hash.rehash_size = get_object(h->hash.rehash_size, o_list);
  h->hash.threshold = get_object(h->hash.threshold, o_list);
  ecl_reconstruct_serialized_hashtable(h);
}

static void
fixup(cl_object o, cl_object *o_list)
{
  if (ECL_LISTP(o)) {
    if (!Null(o)) {
      ECL_RPLACA(o, get_object(ECL_CONS_CAR(o), o_list));
      ECL_RPLACD(o, get_object(ECL_CONS_CDR(o), o_list));
    }
    return;
  }
  switch (ecl_t_of(o)) {
  case t_ratio:
    o->ratio.den = get_object(o->ratio.den, o_list);
    o->ratio.num = get_object(o->ratio.num, o_list);
    break;
  case t_complex:
    o->gencomplex.real = get_object(o->gencomplex.real, o_list);
    o->gencomplex.imag = get_object(o->gencomplex.imag, o_list);
    break;
  case t_hashtable:
    fixup_hashtable(o, o_list);
    break;
#ifdef ECL_UNICODE
  case t_string:
#endif
  case t_base_string:
  case t_vector:
  case t_bitvector:
  case t_array:
    fixup_vector(o, o_list);
    break;
  case t_pathname:
    o->pathname.host = get_object(o->pathname.host, o_list);
    o->pathname.device =
      get_object(o->pathname.device, o_list);
    o->pathname.directory =
      get_object(o->pathname.directory, o_list);
    o->pathname.name = get_object(o->pathname.name, o_list);
    o->pathname.type = get_object(o->pathname.type, o_list);
    o->pathname.version =
      get_object(o->pathname.version, o_list);
    break;
  case t_random:
    o->random.value = get_object(o->random.value, o_list);
    break;
  case t_bclosure:
    o->bclosure.code = get_object(o->bclosure.code, o_list);
    o->bclosure.lex = get_object(o->bclosure.lex, o_list);
    o->bclosure.entry = _ecl_bclosure_dispatch_vararg;
    break;
  case t_bytecodes:
    o->bytecodes.name = get_object(o->bytecodes.name, o_list);
    o->bytecodes.definition = get_object(o->bytecodes.definition, o_list);
    o->bytecodes.data = get_object(o->bytecodes.data, o_list);
    o->bytecodes.file = get_object(o->bytecodes.file, o_list);
    o->bytecodes.file_position = get_object(o->bytecodes.file_position, o_list);
    o->bytecodes.entry = _ecl_bytecodes_dispatch_vararg;
    break;
  default:
    break;
  }
}

cl_object
ecl_deserialize(uint8_t *raw)
{
  cl_env_ptr the_env = ecl_process_env();
  cl_index *data = (cl_index*)raw;
  cl_index i, num_el = data[1];
  cl_object *output = ecl_alloc(sizeof(cl_object) * num_el);
  raw += 2*sizeof(cl_index);
  for (i = 0; i < num_el; i++) {
    raw = reconstruct_one(raw, output+i);
  }
  for (i = 0; i < num_el; i++) {
    cl_object package = output[i];
    if (ecl_t_of(package) == t_package) {
      cl_object name = get_object(package->pack.name,
                                  output);
      output[i] = ecl_find_package_nolock(name);
      if (Null(output[i])) {
        unlikely_if (Null(the_env->packages_to_be_created_p)) {
          FEerror("There is no package with the name ~A.",
                  1, name);
        }
        output[i] = _ecl_package_to_be_created(the_env, name);
      }
    }
  }
  for (i = 0; i < num_el; i++) {
    cl_object symbol = output[i];
    if (ecl_t_of(symbol) == t_symbol) {
      struct fake_symbol *s = (struct fake_symbol *)symbol;
      cl_object name = get_object(s->name, output);
      cl_object pack = get_object(s->pack, output);
      if (Null(pack)) {
        output[i] = cl_make_symbol(name);
      } else {
        int flag;
        output[i] = ecl_intern(name, pack, &flag);
      }
    }
  }
  for (i = 0; i < num_el; i++) {
    fixup(output[i], output);
  }
  return output[0];
}


cl_object
si_deserialize(cl_object data)
{
  @(return ecl_deserialize(data->vector.self.b8));
}

#endif  /* ECL_EXTERNALIZABLE */
