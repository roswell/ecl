/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * sse2.d -  SSE2 vector type support
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <limits.h>
#include <string.h>
#include <ecl/ecl.h>
#define ECL_DEFINE_AET_SIZE
#include <ecl/internal.h>

#ifdef ECL_SSE2

/* Predicates */

cl_object
si_sse_pack_p(cl_object x)
{
  @(return (ECL_SSE_PACK_P(x) ? ECL_T : ECL_NIL));
}

/* Element type substitution */

static void verify_sse_elttype(cl_elttype eltt) {
  switch (eltt) {
  case ecl_aet_sf:
  case ecl_aet_df:
  case ecl_aet_b8:
  case ecl_aet_i8:
#ifdef ecl_uint16_t
  case ecl_aet_b16:
  case ecl_aet_i16:
#endif
#ifdef ecl_uint32_t
  case ecl_aet_b32:
  case ecl_aet_i32:
#endif
#ifdef ecl_uint64_t
  case ecl_aet_b64:
  case ecl_aet_i64:
#endif
    break; /* OK */
  default:
    FEerror("Invalid element type for an SSE pack: ~S", 1, ecl_elttype_to_symbol(eltt));
  }
}

static
cl_elttype symbol_to_sse_elttype(cl_object type) {
  cl_elttype eltt = ecl_symbol_to_elttype(type);
  verify_sse_elttype(eltt);
  return eltt;
}

cl_object
si_sse_pack_as_elt_type(cl_object x, cl_object type)
{
  cl_elttype rtype;

  if (ecl_unlikely(!ECL_SSE_PACK_P(x))) {
    FEwrong_type_nth_arg(@[ext::sse-pack-as-elt-type], 1, x, @[ext::sse-pack]);
  }

  rtype = symbol_to_sse_elttype(type);

  if (ECL_SSE_PACK_ELTTYPE(x) != rtype) {
    cl_object new = ecl_alloc_object(t_sse_pack);
    ECL_SSE_PACK_ELTTYPE(new) = rtype;
    ECL_SSE_PACK_DATA(new).vi = ECL_SSE_PACK_DATA(x).vi;
    x = new;
  }

  @(return x);
}

cl_object
si_sse_pack_element_type(cl_object x)
{
  if (ecl_unlikely(!ECL_SSE_PACK_P(x))) {
    FEwrong_type_nth_arg(@[ext::sse-pack-element-type], 1, x, @[ext::sse-pack]);
  }

  cl_elttype etype = ECL_SSE_PACK_ELTTYPE(x);
  @(return ecl_elttype_to_symbol(etype) ecl_make_fixnum(etype));
}

/* Conversion to and from specialized vectors */

cl_object
si_sse_pack_to_vector(cl_object x, cl_object elt_type)
{
  cl_elttype etype;
  cl_object vec;

  if (ecl_unlikely(!ECL_SSE_PACK_P(x))) {
    FEwrong_type_nth_arg(@[ext::sse-pack-to-vector], 1, x, @[ext::sse-pack]);
  }

  etype = ECL_SSE_PACK_ELTTYPE(x)
  if (elt_type != ECL_NIL)
    etype = symbol_to_sse_elttype(elt_type);

  vec = ecl_alloc_simple_vector(16/ecl_aet_size[etype], etype);
  memcpy(vec->vector.self.b8, ECL_SSE_PACK_DATA(x).b8, 16);

  @(return vec);
}

cl_object
si_vector_to_sse_pack(cl_object x)
{
  cl_object ssev;

  if (ecl_unlikely(!ECL_ARRAYP(x))) {
    FEwrong_type_nth_arg(@[ext::vector-to-sse-pack], 1, x, @[array]);
  }

  verify_sse_elttype(x->vector.elttype);

  if (ecl_unlikely(x->vector.dim * ecl_aet_size[x->vector.elttype] != 16))
    FEerror("Wrong vector size in VECTOR-TO-SSE-PACK: ~S",1,ecl_make_fixnum(x->vector.dim));

  ssev = ecl_alloc_object(t_sse_pack);
  ECL_SSE_PACK_ELTTYPE(ssev) = x->vector.elttype;
  memcpy(ECL_SSE_PACK_DATA(ssev).b8, x->vector.self.b8, 16);

  @(return ssev);
}

/* Boxing and unboxing.

   The unboxing primitives accept any kind of sse-pack on purpose. */

cl_object
ecl_make_int_sse_pack(__m128i value)
{
  cl_object obj = ecl_alloc_object(t_sse_pack);
  ECL_SSE_PACK_ELTTYPE(obj) = ecl_aet_b8;
  ECL_SSE_PACK_DATA(obj).vi = value;
  @(return obj);
}

__m128i
ecl_unbox_int_sse_pack(cl_object x)
{
  do {
    if (ECL_SSE_PACK_P(x))
      return ECL_SSE_PACK_DATA(x).vi;
    x = ecl_type_error(@'coerce', "variable", x, @'ext::sse-pack');
  } while(1);
}

cl_object
ecl_make_float_sse_pack(__m128 value)
{
  cl_object obj = ecl_alloc_object(t_sse_pack);
  ECL_SSE_PACK_ELTTYPE(obj) = ecl_aet_sf;
  ECL_SSE_PACK_DATA(obj).vf = value;
  @(return obj);
}

__m128
ecl_unbox_float_sse_pack(cl_object x)
{
  do {
    if (ECL_SSE_PACK_P(x))
      return ECL_SSE_PACK_DATA(x).vf;
    x = ecl_type_error(@'coerce', "variable", x, @'ext::sse-pack');
  } while(1);
}

cl_object
ecl_make_double_sse_pack(__m128d value)
{
  cl_object obj = ecl_alloc_object(t_sse_pack);
  ECL_SSE_PACK_ELTTYPE(obj) = ecl_aet_df;
  ECL_SSE_PACK_DATA(obj).vd = value;
  @(return obj);
}

__m128d
ecl_unbox_double_sse_pack(cl_object x)
{
  do {
    if (ECL_SSE_PACK_P(x))
      return ECL_SSE_PACK_DATA(x).vd;
    x = ecl_type_error(@'coerce', "variable", x, @'ext::sse-pack');
  } while(1);
}

#endif /* ECL_SSE2 */
