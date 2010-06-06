/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    serialize.d -- Serialize a bunch of lisp data.
*/
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <strings.h>
#define ECL_DEFINE_AET_SIZE
#include <ecl/internal.h>

typedef struct pool {
        cl_object data;
        cl_object hash;
        cl_object queue;
} *pool_t;

static cl_index
round_to_word(cl_index i)
{
        i = (i + sizeof(cl_index) - 1) / sizeof(cl_index);
        return i * sizeof(cl_index);
}

static cl_index
object_byte_size(cl_object o)
{
#ifdef ECL_SMALL_CONS
        if (ECL_LISTP(o)) {
                return round_to_word(sizeof(struct ecl_cons));
        }
#endif
        switch (type_of(o)) {
        case t_bignum:
                return round_to_word(sizeof(struct ecl_bignum));
        case t_ratio:
                return round_to_word(sizeof(struct ecl_ratio));
        case t_singlefloat:
                return round_to_word(sizeof(struct ecl_singlefloat));
        case t_doublefloat:
                return round_to_word(sizeof(struct ecl_doublefloat));
        case t_base_string:
                return round_to_word(sizeof(struct ecl_base_string));
#ifdef ECL_UNICODE
        case t_string:
                return round_to_word(sizeof(struct ecl_string));
#endif
        case t_vector:
                return round_to_word(sizeof(struct ecl_vector));
        case t_array:
                return round_to_word(sizeof(struct ecl_array));
        default:
                FEerror("Cannot serialize object ~A", 1, o);
        }
}


static cl_index
alloc(pool_t pool, cl_index size)
{
        cl_index bytes = round_to_word(size);
        cl_index fillp = pool->data->vector.fillp;
        cl_index next_fillp = fillp + bytes;
        if (next_fillp >= pool->data->vector.dim) {
                cl_index new_dim = next_fillp + next_fillp / 2;
                pool->data = cl_funcall(3, @'adjust-array', pool->data,
                                        MAKE_FIXNUM(new_dim));
        }
        pool->data->vector.fillp = next_fillp;
        return fillp;
}

static cl_object
fix_to_ptr(cl_object ptr)
{
        cl_fixnum i = (cl_fixnum)ptr;
        return (cl_object)(i & ~IMMEDIATE_TAG);
}

static cl_object
enqueue(pool_t pool, cl_object what)
{
        cl_object record, index;
        if (FIXNUMP(what) || CHARACTERP(what)) {
                return what;
        }
        index = ecl_gethash_safe(what, pool->hash, OBJNULL);
        if (index == OBJNULL) {
                index = MAKE_FIXNUM(pool->hash->hash.entries);
                ecl_sethash(what, pool->hash, index);
                pool->queue = ecl_cons(what, pool->queue);
        }
        return fix_to_ptr(index);
}

#ifdef ECL_SMALL_CONS
typedef struct {
        HEADER;
        cl_object car, cdr;
} large_cons;
typedef large_cons *large_cons_ptr;
#endif

static cl_index
serialize_bits(pool_t pool, void *data, cl_index size)
{
        cl_index index = alloc(pool, size);
        memcpy(pool->data->vector.self.b8 + index, data, size);
        return index;
}

static void
serialize_object_ptr(pool_t pool, cl_object *ptr, cl_index size)
{
        cl_index index = serialize_bits(pool, ptr, size*sizeof(cl_object));
        cl_object *p;
        for (p = pool->data->vector.self.t + index; index; p++, index--) {
                *p = enqueue(pool, *p);
                p++;
        }
}

static void serialize_vector(pool_t pool, cl_object v);

static void
serialize_displaced_vector(pool_t pool, cl_object v)
{
        cl_object disp = v->vector.displaced;
        cl_object to = ECL_CONS_CAR(disp);
        if (Null(to)) {
                v->vector.displaced = Cnil;
                serialize_vector(pool, v);
        } else {
                cl_index index = v->vector.self.b8 - to->vector.self.b8;
                v->vector.displaced = enqueue(pool, to);
                v->vector.self.b8 = (uint8_t*)index;
        }
}

static void
serialize_vector(pool_t pool, cl_object v)
{
        if (!Null(v->vector.displaced)) {
                serialize_displaced_vector(pool, v);
        } else if (v->vector.elttype == aet_object) {
                serialize_object_ptr(pool, v->vector.self.t, v->vector.dim);
        } else {
                serialize_bits(pool, v->vector.self.b8,
                               v->vector.dim * ecl_aet_size[v->vector.elttype]);
        }
}

static void
serialize_array(pool_t pool, cl_object a)
{
        serialize_bits(pool, a->array.dims, sizeof(cl_index) * a->array.rank);
        serialize_vector(pool, a);
}

static void
serialize_one(pool_t pool, cl_object what)
{
        cl_index bytes, index;
        cl_object buffer;
#ifdef ECL_SMALL_CONS
        if (ECL_LISTP(what)) {
                cl_index bytes = round_to_word(sizeof(large_cons));
                cl_index index = alloc(pool, bytes);
                large_cons_ptr cons =
                        (large_cons_ptr)(pool->data->vector.self.b8 + index);
                memset(cons, 0, bytes);
                cons->t = t_list;
                cons->car = enqueue(pool, ECL_CONS_CAR(what));
                cons->cdr = enqueue(pool, ECL_CONS_CDR(what));
                return;
        }
#endif
        bytes = object_byte_size(what);
        index = alloc(pool, bytes);
        buffer = (cl_object)(pool->data->vector.self.b8 + index);
        memcpy(buffer, what, object_byte_size(what));
        switch (buffer->d.t) {
#ifndef ECL_SMALL_CONS
        case t_cons:
                buffer->cons.car = enqueue(pool, buffer->cons.car);
                buffer->cons.cdr = enqueue(pool, buffer->cons.car);
                break;
#endif
        case t_bignum: {
                cl_fixnum size = buffer->big.big_size;
                cl_index dim = ((size < 0) ? (-size) : size);
                cl_index bytes = dim * sizeof(mp_limb_t);
                serialize_bits(pool, buffer->big.big_limbs, bytes);
                break;
        }
        case t_ratio: {
                buffer->ratio.den = enqueue(pool, buffer->ratio.den);
                buffer->ratio.num = enqueue(pool, buffer->ratio.num);
                break;
        }
        case t_complex: {
                buffer->complex.real = enqueue(pool, buffer->complex.real);
                buffer->complex.imag = enqueue(pool, buffer->complex.imag);
                break;
        }
        case t_base_string: {
                serialize_vector(pool, buffer);
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
                                    MAKE_FIXNUM(1024),
                                    Ct,
                                    MAKE_FIXNUM(2 * sizeof(cl_index)),
                                    Cnil,
                                    MAKE_FIXNUM(0));
        pool->hash = cl__make_hash_table(@'eql', MAKE_FIXNUM(256),
                                         ecl_make_singlefloat(1.5f),
                                         ecl_make_singlefloat(0.5f),
                                         Cnil);
        ecl_sethash(root, pool->hash, MAKE_FIXNUM(0));
        pool->queue = ecl_list1(root);
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
                pool->queue = ECL_CONS_CDR(pool->queue);
                serialize_one(pool, what);
        }
        @(return close_pool(pool));
}

static void *
reconstruct_bits(uint8_t **data, cl_index size, int atomic)
{
        void *output = atomic?
                ecl_alloc_atomic(size) :
                ecl_alloc(size);
        memcpy(output, *data, size);
        *data += size;
        return output;
}

static uint8_t *
reconstruct_vector(cl_object v, uint8_t *data)
{
        if (v->vector.displaced == Cnil) {
                cl_type t = v->vector.elttype;
                cl_index size = round_to_word(v->vector.dim * ecl_aet_size[t]);
                v->vector.self.t = reconstruct_bits(&data, size, t != aet_object);
        }
        return data;
}

static uint8_t *
reconstruct_array(cl_object a, uint8_t *data)
{
        a->array.dims = reconstruct_bits(&data,
                                         a->array.rank * sizeof(cl_index),
                                         1);
        return reconstruct_vector(a, data);
}

static uint8_t *
reconstruct_one(uint8_t *data, cl_object *output)
{
        cl_object p = (cl_object)data;
#ifdef ECL_SMALL_CONS
        if (p->d.t == t_list) {
                large_cons_ptr c = (large_cons_ptr)p;
                *output = ecl_cons(c->car, c->cdr);
                return data + round_to_word(sizeof(large_cons));
        }
#endif
        {
                cl_object o = ecl_alloc_object(p->d.t);
                cl_index bytes = object_byte_size(o);
                memcpy(o, data, bytes);
                data += bytes;
                *output = o;
                switch (o->d.t) {
                case t_base_string:
                        data = reconstruct_vector(o, data);
                        break;
                case t_vector:
                        data = reconstruct_vector(o, data);
                        break;
                case t_array:
                        data = reconstruct_array(o, data);
                        break;
                }
                return data;
        }
}

static cl_object
get_object(cl_object o_or_index, cl_object *o_list)
{
        if (IMMEDIATE(o_or_index)) {
                return o_or_index;
        } else {
                cl_index i = (cl_index)o_or_index >> 2;
                return o_list[i];
        }
}

static void
fixup_vector(cl_object v, cl_object *o_list)
{
        if (!IMMEDIATE(v->vector.displaced)) {
                cl_object disp = get_object(v->vector.displaced, o_list);
                cl_object to = ECL_CONS_CAR(disp);
                if (to != Cnil) {
                        cl_index offset = (cl_index)v->vector.self.b8;
                        v->vector.displaced = Cnil;
                        ecl_displace(v, to, MAKE_FIXNUM(offset));
                        return;
                }
        }
        if (v->vector.elttype == aet_object) {
                cl_index i;
                cl_object *p = v->vector.self.t;
                for (i = v->vector.dim; i; i--, p++) {
                        *p = get_object(*p, o_list);
                }
        }
}

static void
fixup(cl_object o, cl_object *o_list)
{
#ifdef ECL_SMALL_CONS
        if (ECL_LISTP(o)) {
                ECL_RPLACA(o, get_object(ECL_CONS_CAR(o), o_list));
                ECL_RPLACD(o, get_object(ECL_CONS_CDR(o), o_list));
                return;
        }
#endif
        switch (o->d.t) {
        case t_ratio:
                o->ratio.den = get_object(o->ratio.den, o_list);
                o->ratio.num = get_object(o->ratio.num, o_list);
                break;
        case t_complex:
                o->complex.real = get_object(o->complex.real, o_list);
                o->complex.imag = get_object(o->complex.imag, o_list);
                break;
        case t_base_string:
        case t_vector:
        case t_array:
                fixup_vector(o, o_list);
                break;
        default:
                break;
        }
}

cl_object
si_deserialize(cl_object data)
{
        cl_index i, num_el = data->vector.self.index[1];
        cl_object *output = ecl_alloc(sizeof(cl_object) * num_el);
        uint8_t *raw = (uint8_t*)(data->vector.self.index + 2);
        for (i = 0; i < num_el; i++) {
                raw = reconstruct_one(raw, output+i);
        }
        for (i = 0; i < num_el; i++) {
                fixup(output[i], output);
        }
        @(return output[0])
}
