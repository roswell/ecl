/* -*- Mode: C; c-basic-offset: 4; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=4 shiftwidth=4 expandtab: */

/*
 * Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
 * Copyright (c) 1990, Giuseppe Attardi.
 * Copyright (c) 2001, Juan Jose Garcia Ripoll.
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/* object.h  -- Data structure definitions. */
#include <float.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
        Integer and boolean types (see config.h)
*/

#define TRUE            1       /*  boolean true value  */
#define FALSE           0       /*  boolean false value  */

typedef unsigned char byte;

/*
        Implementation types.
*/
typedef enum {
        t_start = 0,
        t_list = 1,
        t_extend = 2,           /* extended immediates */
        t_constant = 2,          /* constant values */
        /* numbers */
        t_fixnum = 3,           /* immediate fixnum */
        t_bignum = 4,
        t_ratio,
        /* t_shortfloat, */
        t_singlefloat,
        t_doublefloat,
        t_longfloat,
        t_last_real = t_longfloat,
        t_complex,
#ifdef ECL_COMPLEX_FLOAT
        /* t_cmfloat, */
        t_csfloat,
        t_cdfloat,
        t_clfloat,
        t_last_number = t_clfloat,
#else
        t_last_number = t_complex,
#endif
        t_character = 18,       /* immediate character */
        t_symbol,
        t_package,
        t_hashtable,
        t_array,
        t_vector,
#ifdef ECL_UNICODE
        t_string,
#endif
        t_base_string,
        t_bitvector,
        t_stream,
        t_random,
        t_readtable,
        t_pathname,
        t_bytecodes,
        t_bclosure,
        t_cfun,
        t_cfunfixed,
        t_cclosure,
        t_instance,
        t_structure = t_instance,
#ifdef ECL_THREADS
        t_process,
        t_lock,
        t_rwlock,
        t_condition_variable,
        t_semaphore,
        t_barrier,
        t_mailbox,
#endif
        t_codeblock,
        t_foreign,
        t_frame,
        t_token,
        t_weak_pointer,
#ifdef ECL_SSE2
        t_sse_pack,
#endif
        t_end,
        t_other,
        t_contiguous,           /*  contiguous block  */
        FREE = 127              /*  free object  */
} cl_type;

/* The most specific numeric types have lower rank. Used by some routines,
   notably by cl_expt, ecl_atan2 and ecl_make_complex. See ANSI 12.1.1.2. */

static inline int
ecl_number_rank(cl_type tx)
{
  switch(tx) {
  case t_fixnum: return 1;
  case t_bignum: return 2;
  case t_ratio:  return 3;
  /* case t_shortfloat:  return 4; */
  case t_singlefloat: return 5;
  case t_doublefloat: return 6;
  case t_longfloat:   return 7;
  case t_complex:     return 8;
#ifdef ECL_COMPLEX_FLOAT
  /* case t_cmfloat:     return 9; */
  case t_csfloat:     return 10;
  case t_cdfloat:     return 11;
  case t_clfloat:     return 12;
#endif
  default: return -1;
  }
}

/*
        Definition of the type of LISP objects.
*/
typedef union cl_lispunion *cl_object;
typedef union cl_lispunion ecl_object;
typedef cl_object cl_return;
typedef cl_fixnum cl_narg;
typedef cl_object (*cl_objectfn)(cl_narg narg, ...);
typedef cl_object (*cl_objectfn_fixed)();
typedef cl_object (*cl_objectfn_parse)(cl_object,cl_object,int);

/*
        OBJect NULL value.
        It should not coincide with any legal object value.
*/
#define OBJNULL         ((cl_object)NULL)

#define ecl_to_bool(x) ((x)!=ECL_NIL)
#define ecl_make_bool(x) ((x)? ECL_T : ECL_NIL)

/*
        Definition of each implementation type.
*/

#if ECL_FIXNUM_BITS == 32
# define ECL_TAG_BITS 2
# define ECL_IMMEDIATE_TAG 0b11

# define ECL_FTAG_BITS 2
# define ECL_FMMEDIATE_TAG 0b11

# define ECL_XTAG_BITS 4
# define ECL_XMMEDIATE_TAG 0b1111

# define ECL_STAG_BITS 6
# define ECL_SMMEDIATE_TAG 0b111111
#elif ECL_FIXNUM_BITS == 64
# define ECL_TAG_BITS 3
# define ECL_IMMEDIATE_TAG 0b111

# define ECL_FTAG_BITS 2
# define ECL_FMMEDIATE_TAG 0b11

# define ECL_XTAG_BITS 5
# define ECL_XMMEDIATE_TAG 0b11111

# define ECL_STAG_BITS 7
# define ECL_SMMEDIATE_TAG 0b1111111
#else
# error "Unspecialized ECL tagging scheme."
#endif

#define ECL_IMMEDIATE(o) ((cl_fixnum)(o) & ECL_IMMEDIATE_TAG) /* ==0 for boxed */
#define ECL_FMMEDIATE(o) ((cl_fixnum)(o) & ECL_FMMEDIATE_TAG) /* !=0 for 2bit tag */
#define ECL_XMMEDIATE(o) ((cl_fixnum)(o) & ECL_XMMEDIATE_TAG) /* !=0 for extended */
#define ECL_SMMEDIATE(o) ((cl_fixnum)(o) & ECL_SMMEDIATE_TAG) /* !=0 for sub-tags */

/* Tagged pointers may be either values or pointers to boxed objects. In the
   latter case GC needs to scan them. Currently it is only t_list, but this set
   may be extended in the future. Then this predicate needs to be updated. */
#define ECL_IMMEDIATE_VALUE_P(o) ((o)&2)

#if ECL_FIXNUM_BITS == 64
# define ECL_IMMEDIATE_SFLOAT
# define ECL_IMMEDIATE_DFLOAT
#endif

/* -- Immediate fixnums ----------------------------------------------------- */
#define ECL_FIXNUM_TAG          t_fixnum
#define ECL_FIXNUMP(o)          (ECL_FMMEDIATE(o) == t_fixnum)
#define ecl_make_fixnum(n)      ((cl_object)(((cl_fixnum)(n) << ECL_FTAG_BITS) \
                                             | t_fixnum))
#define ecl_fixnum_lower(a,b)   ((cl_fixnum)(a) < (cl_fixnum)(b))
#define ecl_fixnum_greater(a,b) ((cl_fixnum)(a) > (cl_fixnum)(b))
#define ecl_fixnum_leq(a,b)     ((cl_fixnum)(a) <= (cl_fixnum)(b))
#define ecl_fixnum_geq(a,b)     ((cl_fixnum)(a) >= (cl_fixnum)(b))
#define ecl_fixnum_plusp(a)     ((cl_fixnum)(a) > (cl_fixnum)ecl_make_fixnum(0))
#define ecl_fixnum_minusp(a)    ((cl_fixnum)(a) < (cl_fixnum)(0))
#define ecl_fixnum(a)           (((cl_fixnum)(a)) >> ECL_FTAG_BITS)

/* -- Immediate extends ----------------------------------------------------- */
#define ECL_EXTEND_TAG          t_extend
#define ECL_EXTENDP(o)          (ECL_IMMEDIATE(o) == t_extend)
#define ECL_EXTENDED_TAG(x)     (((x)<<ECL_TAG_BITS) | ECL_EXTEND_TAG)

/* -- Immediate constant values --------------------------------------------- */

/* Constant values are represented within the extended tag space. We reserve 8b
   for the immediate tag, 8b to encoded the value type and the rest for its id.
   For example on 32bit entries could look like this:

   [  enumerated id] [t_type] [   tag] ; [ val] [    cl_type] [always b10]
   00000000 00000000 00111000 00000010 ; #<hk!> t_hk          constant-tag
   00000000 00000001 00001000 00000010 ; -0.0d0 t_doublefloat constant-tag
   00000000 00000001 00001001 00000010 ; -0.0l0 t_longfloat   constant-tag

   etc. This allows us to decode the type without derefencing the object. Each
   downstream type is responsible for accounting for defined constant values.
   ecl_t_of will return the type tag. Currently cl_type uses 7b, but we leave
   spare space for future compatibility (16b is enough to store all sentinels).

   -- jd 2026-11-12 */
#define ECL_CONSTANT_TAG   ECL_EXTENDED_TAG(0b0000)
#define ECL_CONSTANTP(o)   (ECL_SMMEDIATE(o) == ECL_CONSTANT_TAG)
#define ECL_CONSTANT_TYPE(o) ((((cl_index)(o))>>8) & 0xff)
#define ECL_CONST_TYPEP(o,t) (ECL_CONSTANTP(o) && (ECL_CONSTANT_TYPE(o) == (t)))
#define ECL_ENUMERATE(v,t)   ((cl_object)((v<<16) | (t<<8) | ECL_CONSTANT_TAG))

/* -- Immediate characters -------------------------------------------------- */
#define ECL_CHARACTER_TAG   ECL_EXTENDED_TAG(0b0100)
#define ECL_CHARACTERP(o)   (ECL_SMMEDIATE(o) == ECL_CHARACTER_TAG)
#define ECL_TAG_CHAR(x)     ((cl_object)((cl_index)((x)<<ECL_STAG_BITS) | ECL_CHARACTER_TAG))

#ifdef ECL_UNICODE
#define ECL_BASE_CHAR_MASK  (0xFFFFFFFF & ~(0xFF << ECL_STAG_BITS))
#define ECL_BASE_CHAR_P(o)  ((((cl_fixnum)(o)) & ECL_BASE_CHAR_MASK)   \
                             == ECL_CHARACTER_TAG)
#define ECL_CODE_CHAR(c)    ECL_TAG_CHAR(c)
#define ECL_CHAR_CODE(o)    (((cl_fixnum)(o)) >> ECL_STAG_BITS)
#else
#define ECL_BASE_CHAR_P(o)  ECL_CHARACTERP(o)
#define ECL_CODE_CHAR(c)    ECL_TAG_CHAR((c)&0xff)
#define ECL_CHAR_CODE(o)    ((((cl_fixnum)(o)) >> ECL_STAG_BITS) & 0xff)
#endif
#define ECL_BASE_CHAR_CODE_P(x) (((x) & ~((cl_fixnum)0xFF)) == 0)
#define ECL_CHAR_CODE_RETURN    13
#define ECL_CHAR_CODE_NEWLINE   10
#define ECL_CHAR_CODE_LINEFEED  10

/* -- Type predicates ------------------------------------------------------- */
#define ECL_NUMBER_TYPE_P(t)    (t >= t_fixnum && t <= t_last_number)
#define ECL_REAL_TYPE_P(t)      (t >= t_fixnum && t <= t_last_real)

/* -- Object type predicates ------------------------------------------------ */
#define ECL_BOXED_TYPEP(x,t0) ((ECL_IMMEDIATE(x)==0) && ((x)->d.t==(t0)))
#define ECL_BOXED_TYPEP_CONST(x,t0) (ECL_IMMEDIATE(x)           \
                                     ? ECL_CONST_TYPEP(x,t0)    \
                                     : ((x)->d.t==(t0)))
#define ECL_BOXED_TYPEP_RANGE(x,t1,t2) \
        ((ECL_IMMEDIATE(x)==0) && ((x)->d.t>=(t1)) && ((x)->d.t<=(t2)))

#define ECL_ARRAYP(x)  ECL_BOXED_TYPEP_RANGE(x, t_array,  t_bitvector)
#define ECL_VECTORP(x) ECL_BOXED_TYPEP_RANGE(x, t_vector, t_bitvector)
#define ECL_BIT_VECTOR_P(x) ECL_BOXED_TYPEP(x, t_bitvector)
#ifdef ECL_UNICODE
#define ECL_STRINGP(x) ECL_BOXED_TYPEP_RANGE(x, t_string, t_base_string)
#define ECL_EXTENDED_STRING_P(x) ECL_BOXED_TYPEP(x,t_string)
#else
#define ECL_STRINGP(x) ECL_BOXED_TYPEP(x,t_base_string)
#define ECL_EXTENDED_STRING_P(x) 0
#endif
#define ECL_BASE_STRING_P(x) ECL_BOXED_TYPEP(x,t_base_string)
#define ECL_HASH_TABLE_P(x)  ECL_BOXED_TYPEP(x,t_hashtable)
#define ECL_BIGNUMP(x)       ECL_BOXED_TYPEP(x,t_bignum)
#ifdef ECL_COMPLEX_FLOAT
#define ECL_COMPLEXP(x) ECL_BOXED_TYPEP_RANGE(x, t_complex, t_clfloat)
#define ECL_COMPLEX_SINGLE_FLOAT_P(x) ECL_BOXED_TYPEP(x,t_csfloat)
#define ECL_COMPLEX_DOUBLE_FLOAT_P(x) ECL_BOXED_TYPEP(x,t_cdfloat)
#define ECL_COMPLEX_LONG_FLOAT_P(x)   ECL_BOXED_TYPEP(x,t_clfloat)
#else
#define ECL_COMPLEXP(x)         ECL_BOXED_TYPEP(x,t_complex)
#endif
#define ECL_RANDOM_STATE_P(x)   ECL_BOXED_TYPEP(x,t_random)
#define ECL_PACKAGEP(x)         ECL_BOXED_TYPEP(x,t_package)
#define ECL_PATHNAMEP(x)        ECL_BOXED_TYPEP(x,t_pathname)
#define ECL_READTABLEP(x)       ECL_BOXED_TYPEP(x,t_readtable)
#define ECL_FOREIGN_DATA_P(x)   ECL_BOXED_TYPEP(x,t_foreign)
#ifdef ECL_SSE2
#define ECL_SSE_PACK_P(x)       ECL_BOXED_TYPEP(x,t_sse_pack)
#endif

#define _ECL_HDR                        int8_t t, m, padding1, padding2
#define _ECL_HDR1(field)                int8_t t, m, field, padding
#define _ECL_HDR2(field1,field2)        int8_t t, m, field1, field2

/* -- float common ---------------------------------------------------------- */

#define ECL_PI_D 3.14159265358979323846264338327950288
#define ECL_PI_L 3.14159265358979323846264338327950288l
#define ECL_PI2_D 1.57079632679489661923132169163975144
#define ECL_PI2_L 1.57079632679489661923132169163975144l

/* -- single float ---------------------------------------------------------- */

/* FIXME dpp should preprocess these to skip constant and use directly the
   immediate single float instead of hopping through constants dispatch.

   Also define enum instead of going with raw hex numbers. */

#define ECL_LEAST_POSITIVE_SINGLE_FLOAT ECL_ENUMERATE(0x11, t_singlefloat)
#define ECL_LEAST_NEGATIVE_SINGLE_FLOAT ECL_ENUMERATE(0x12, t_singlefloat)
#define ECL_LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT ECL_ENUMERATE(0x13, t_singlefloat)
#define ECL_LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT ECL_ENUMERATE(0x14, t_singlefloat)
#define ECL_MOST_POSITIVE_SINGLE_FLOAT ECL_ENUMERATE(0x15, t_singlefloat)
#define ECL_MOST_NEGATIVE_SINGLE_FLOAT ECL_ENUMERATE(0x16, t_singlefloat)

#define ECL_SFLOAT_NEGATIVE_ZERO ECL_ENUMERATE(0x17, t_singlefloat)
#define ECL_SFLOAT_POSITIVE_ZERO ECL_ENUMERATE(0x18, t_singlefloat)
#define ECL_SFLOAT_ONE ECL_ENUMERATE(0x19, t_singlefloat)
#define ECL_SFLOAT_TWO ECL_ENUMERATE(0x1a, t_singlefloat)

#define ECL_DEFAULT_REHASH_SIZE ECL_ENUMERATE(0x20, t_singlefloat)
#define ECL_DEFAULT_REHASH_THRESHOLD ECL_ENUMERATE(0x21, t_singlefloat)
#define ECL_MINIMUM_REHASH_THRESHOLD ECL_ENUMERATE(0x22, t_singlefloat)

static inline float
decode_const_single_float(cl_object o) {
    switch((cl_index)o) {
    case (cl_index)ECL_LEAST_NEGATIVE_SINGLE_FLOAT: return -FLT_TRUE_MIN;
    case (cl_index)ECL_LEAST_POSITIVE_SINGLE_FLOAT: return +FLT_TRUE_MIN;
    case (cl_index)ECL_LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT: return -FLT_MIN;
    case (cl_index)ECL_LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT: return +FLT_MIN;
    case (cl_index)ECL_MOST_NEGATIVE_SINGLE_FLOAT: return -FLT_MAX;
    case (cl_index)ECL_MOST_POSITIVE_SINGLE_FLOAT: return +FLT_MAX;
    case (cl_index)ECL_SFLOAT_POSITIVE_ZERO: return +0.0f;
    case (cl_index)ECL_SFLOAT_NEGATIVE_ZERO: return -0.0f;
    case (cl_index)ECL_SFLOAT_TWO: return 2.0f;
    case (cl_index)ECL_SFLOAT_ONE: return 1.0f;
    /*  parameters  */
    case (cl_index)ECL_DEFAULT_REHASH_SIZE:      return 1.5f;
    case (cl_index)ECL_DEFAULT_REHASH_THRESHOLD: return 0.75f;
    case (cl_index)ECL_MINIMUM_REHASH_THRESHOLD: return 0.1f;
    default: return NAN;
    }
}

#ifdef ECL_IMMEDIATE_SFLOAT
#endif

struct ecl_singlefloat {
        _ECL_HDR;
        float SFVAL;    /*  singlefloat value  */
};

#ifdef ECL_IMMEDIATE_SFLOAT
# define ECL_SINGLE_FLOAT_TAG ECL_EXTENDED_TAG(0b01)
# define ECL_SINGLE_FLOAT_P(o) ((ECL_XMMEDIATE(o) == ECL_SINGLE_FLOAT_TAG) \
                                || ECL_BOXED_TYPEP_CONST((o),t_singlefloat))
# define ecl_single_float(o) ((ECL_XMMEDIATE(o) == ECL_SINGLE_FLOAT_TAG) \
                              ? ecl_decode_sfloat(o)                    \
                              : (ECL_IMMEDIATE(o)                       \
                                 ? decode_const_single_float(o)         \
                                 : (o)->SF.SFVAL))

static inline cl_object
ecl_encode_sfloat(float f)
{
    union {float f; uint32_t i;} u;
    u.f = f;
    return (cl_object)((((cl_index)(u.i)) << ECL_XTAG_BITS) | ECL_SINGLE_FLOAT_TAG);
}

static inline float
ecl_decode_sfloat(cl_object o)
{
    union {float f; uint32_t i;} u;
    u.i = ((cl_index)(o)) >> ECL_XTAG_BITS;
    return u.f;
}
#else
# define ECL_SINGLE_FLOAT_P(o) ECL_BOXED_TYPEP_CONST((o),t_singlefloat)
# define ecl_single_float(o) (ECL_IMMEDIATE(o)                  \
                              ? decode_const_single_float(o)    \
                              : (o)->SF.SFVAL)
#endif

#define ecl_set_single_float(o,v) ((o)->SF.SFVAL=(v))

/* -- short float ----------------------------------------------------------- */
/* Currently short floats are just single floats. */

#define ECL_LEAST_POSITIVE_SHORT_FLOAT ECL_ENUMERATE(0x11, t_singlefloat)
#define ECL_LEAST_NEGATIVE_SHORT_FLOAT ECL_ENUMERATE(0x12, t_singlefloat)
#define ECL_LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT ECL_ENUMERATE(0x13, t_singlefloat)
#define ECL_LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT ECL_ENUMERATE(0x14, t_singlefloat)
#define ECL_MOST_POSITIVE_SHORT_FLOAT ECL_ENUMERATE(0x15, t_singlefloat)
#define ECL_MOST_NEGATIVE_SHORT_FLOAT ECL_ENUMERATE(0x16, t_singlefloat)

#define ECL_NEGATIVE_ZERO_SHORT_FLOAT ECL_ENUMERATE(0x17, t_singlefloat)
#define ECL_POSITIVE_ZERO_SHORT_FLOAT ECL_ENUMERATE(0x18, t_singlefloat)

#define ECL_SHORT_FLOAT_P(o) ECL_SINGLE_FLOAT_P(o)
#define ecl_short_float(o) ecl_single_float(o)

#define ecl_set_short_float(o,v) ecl_set_single_float(o,v)

/* -- double float ---------------------------------------------------------- */

#define ECL_LEAST_POSITIVE_DOUBLE_FLOAT ECL_ENUMERATE(0x11, t_doublefloat)
#define ECL_LEAST_NEGATIVE_DOUBLE_FLOAT ECL_ENUMERATE(0x12, t_doublefloat)
#define ECL_LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT ECL_ENUMERATE(0x13, t_doublefloat)
#define ECL_LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT ECL_ENUMERATE(0x14, t_doublefloat)
#define ECL_MOST_POSITIVE_DOUBLE_FLOAT ECL_ENUMERATE(0x15, t_doublefloat)
#define ECL_MOST_NEGATIVE_DOUBLE_FLOAT ECL_ENUMERATE(0x16, t_doublefloat)

#define ECL_DFLOAT_NEGATIVE_ZERO ECL_ENUMERATE(0x17, t_doublefloat)
#define ECL_DFLOAT_POSITIVE_ZERO ECL_ENUMERATE(0x18, t_doublefloat)
#define ECL_DFLOAT_ONE ECL_ENUMERATE(0x19, t_doublefloat)
#define ECL_DFLOAT_TWO ECL_ENUMERATE(0x1a, t_doublefloat)

static inline double
decode_const_double_float(cl_object o) {
    switch((cl_index)o) {
    case (cl_index)ECL_LEAST_NEGATIVE_DOUBLE_FLOAT: return -DBL_TRUE_MIN;
    case (cl_index)ECL_LEAST_POSITIVE_DOUBLE_FLOAT: return +DBL_TRUE_MIN;
    case (cl_index)ECL_LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT: return -DBL_MIN;
    case (cl_index)ECL_LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT: return +DBL_MIN;
    case (cl_index)ECL_MOST_NEGATIVE_DOUBLE_FLOAT: return -DBL_MAX;
    case (cl_index)ECL_MOST_POSITIVE_DOUBLE_FLOAT: return +DBL_MAX;
    case (cl_index)ECL_DFLOAT_NEGATIVE_ZERO: return -0.0;
    case (cl_index)ECL_DFLOAT_POSITIVE_ZERO: return +0.0;
    case (cl_index)ECL_DFLOAT_ONE: return 1.0;
    case (cl_index)ECL_DFLOAT_TWO: return 2.0;
    default: return NAN;
    }
}

struct ecl_doublefloat {
        _ECL_HDR;
        double DFVAL;   /*  doublefloat value  */
};
#define ecl_set_double_float(o,v) ((o)->DF.DFVAL=(v))

#ifdef ECL_IMMEDIATE_DFLOAT

# define ECL_DOUBLE_FLOAT_TAG0 0b100
# define ECL_DOUBLE_FLOAT_TAG1 0b101

/* The exponent high bits are either 011 or 100 to match biased exponent
   [-255,0] and [1,256]. We encode the double as d[0:59] | sign | tag:

     ((d<<4) | d>>59&(2^4-1) | tag)

   where tag depends on the exponent high bits. */
static inline cl_object
ecl_encode_dfloat(double f)
{
    union {double f; uint64_t i;} u;
    uint64_t tag;
    u.f=f;
    tag = ((u.i>>60 & 0b111) == 0b100)
        ? ECL_DOUBLE_FLOAT_TAG0
        : ECL_DOUBLE_FLOAT_TAG1;
    return (cl_object)(u.i<<4 | (u.i>>60 & 0b1000) | tag);
}

static inline double
ecl_decode_dfloat(cl_object o)
{
    union {double f; uint64_t i;} u;
    cl_index bits = (cl_index)o;
    u.i = !(bits&1)
        ? ((bits<<60)                     | bits>>4)
        : ((bits & 0b1000) | 0b011) << 60 | bits>>4;
    return u.f;
}

# define ECL_DOUBLE_FLOAT_P(o) ((ECL_IMMEDIATE(o) == ECL_DOUBLE_FLOAT_TAG0) \
                                || (ECL_IMMEDIATE(o) == ECL_DOUBLE_FLOAT_TAG1) \
                                || ECL_BOXED_TYPEP_CONST((o),t_doublefloat))
# define ecl_double_float(o)   (((ECL_IMMEDIATE(o) == ECL_DOUBLE_FLOAT_TAG0) \
                                 || (ECL_IMMEDIATE(o) == ECL_DOUBLE_FLOAT_TAG1)) \
                                ? ecl_decode_dfloat(o)                  \
                                : ((ECL_IMMEDIATE(o)                    \
                                    ? decode_const_double_float(o)      \
                                    : (o)->DF.DFVAL)))
#else

struct ecl_doublefloat {
        _ECL_HDR;
        double DFVAL;   /*  doublefloat value  */
};

# define ECL_DOUBLE_FLOAT_P(o) ECL_BOXED_TYPEP_CONST((o),t_doublefloat)
# define ecl_double_float(o)   (ECL_IMMEDIATE(o)                   \
                               ? decode_const_double_float(o)     \
                               : (o)->DF.DFVAL)
#endif



/* -- long float ------------------------------------------------------------ */

#define ECL_LEAST_POSITIVE_LONG_FLOAT ECL_ENUMERATE(0x11, t_longfloat)
#define ECL_LEAST_NEGATIVE_LONG_FLOAT ECL_ENUMERATE(0x12, t_longfloat)
#define ECL_LEAST_POSITIVE_NORMALIZED_LONG_FLOAT ECL_ENUMERATE(0x13, t_longfloat)
#define ECL_LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT ECL_ENUMERATE(0x14, t_longfloat)
#define ECL_MOST_POSITIVE_LONG_FLOAT ECL_ENUMERATE(0x15, t_longfloat)
#define ECL_MOST_NEGATIVE_LONG_FLOAT ECL_ENUMERATE(0x16, t_longfloat)

#define ECL_LFLOAT_NEGATIVE_ZERO ECL_ENUMERATE(0x17, t_longfloat)
#define ECL_LFLOAT_POSITIVE_ZERO ECL_ENUMERATE(0x18, t_longfloat)
#define ECL_LFLOAT_ONE ECL_ENUMERATE(0x19, t_longfloat)
#define ECL_LFLOAT_TWO ECL_ENUMERATE(0x1a, t_longfloat)

#define ECL_PI ECL_ENUMERATE(0x20, t_longfloat)

static inline long double
decode_const_long_float(cl_object o) {
    switch((cl_index)o) {
    case (cl_index)ECL_LEAST_NEGATIVE_LONG_FLOAT: return -LDBL_TRUE_MIN;
    case (cl_index)ECL_LEAST_POSITIVE_LONG_FLOAT: return +LDBL_TRUE_MIN;
    case (cl_index)ECL_LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT: return -LDBL_MIN;
    case (cl_index)ECL_LEAST_POSITIVE_NORMALIZED_LONG_FLOAT: return +LDBL_MIN;
    case (cl_index)ECL_MOST_NEGATIVE_LONG_FLOAT: return -LDBL_MAX;
    case (cl_index)ECL_MOST_POSITIVE_LONG_FLOAT: return +LDBL_MAX;
    case (cl_index)ECL_LFLOAT_NEGATIVE_ZERO: return -0.0l;
    case (cl_index)ECL_LFLOAT_POSITIVE_ZERO: return +0.0l;
    case (cl_index)ECL_LFLOAT_ONE: return 1.0l;
    case (cl_index)ECL_LFLOAT_TWO: return 2.0l;
    case (cl_index)ECL_PI: return ECL_PI_L;
    default: return NAN;
    }
}

struct ecl_long_float {
        _ECL_HDR;
        long double value;
};

#define ECL_LONG_FLOAT_P(o) ECL_BOXED_TYPEP_CONST((o),t_longfloat)
#define ecl_long_float(o)   (ECL_IMMEDIATE(o)                   \
                             ? decode_const_long_float(o)       \
                             : (o)->longfloat.value)

#define ecl_set_long_float(o,v) ((o)->longfloat.value=(v))

/* ECL features two implementations of bignums. One (default) is based on GMP,
   and the other has no external dependencies. The latter is conforming but only
   superficially -- when we exceed the maximum size of the number, then we
   signal the condition STORAGE-EXHAUSTED.

   Both representations share the same structure. Note that operations may
   mutate numbers, so bignums always operate on registers. */

#ifdef ECL_GMPLIB
typedef mp_limb_t ecl_limb_t;

struct ecl_bignum {
        _ECL_HDR;
        mpz_t value;
};
#define ecl_bignum(o) ((o)->big.value)
#define ecl_bigsgn(o) mpz_sgn(ecl_bignum(o))
#else
typedef uintmax_t ubig_num_t;
typedef intmax_t  sbig_num_t;
typedef intmax_t  ecl_limb_t;

struct ecl_bignum {
        _ECL_HDR;
        sbig_num_t value;
};
#define ecl_bignum(o) ((o)->big.value)
#define ecl_bigsgn(o) ((o)->big.value<0 ? -1 : +1)
#endif

struct ecl_ratio {
        _ECL_HDR;
        cl_object den;          /*  denominator, must be an integer  */
        cl_object num;          /*  numerator, must be an integer  */
};
#define ecl_ratio_den(o) ((o)->ratio.den)
#define ecl_ratio_num(o) ((o)->ratio.num)

struct ecl_complex {
        _ECL_HDR;
        cl_object real;         /*  real part, must be a number  */
        cl_object imag;         /*  imaginary part, must be a number  */
};
#define ecl_complex_real(o) ((o)->gencomplex.real)
#define ecl_complex_imag(o) ((o)->gencomplex.imag)
#define ecl_complex_value(o) ((o)->gencomplex)

#ifdef ECL_COMPLEX_FLOAT
struct ecl_csfloat {
        _ECL_HDR;
        float _Complex value;
};
#define ecl_csfloat(o) ((o)->csfloat.value)

struct ecl_cdfloat {
        _ECL_HDR;
        double _Complex value;
};
#define ecl_cdfloat(o) ((o)->cdfloat.value)

struct ecl_clfloat {
        _ECL_HDR;
        long double _Complex value;
};
#define ecl_clfloat(o) ((o)->clfloat.value)
#endif

enum ecl_stype {                /*  symbol type  */
        ecl_stp_ordinary = 0,
        ecl_stp_constant = 1,
        ecl_stp_special = 2,
        ecl_stp_macro = 4,
        ecl_stp_special_form = 8
};

#define ECL_NIL                 ((cl_object)t_list)
#define ECL_PROTECT_TAG         ecl_ct_protect_tag
#define ECL_DUMMY_TAG           ecl_ct_dummy_tag

#define ECL_NIL_SYMBOL          ((cl_object)cl_symbols)
#define ECL_T                   ((cl_object)(cl_symbols+1))
#define ECL_UNBOUND             ((cl_object)(cl_symbols+2))
#define ECL_RESTART_CLUSTERS    ((cl_object)(cl_symbols+3))
#define ECL_HANDLER_CLUSTERS    ((cl_object)(cl_symbols+4))
#define ECL_INTERRUPTS_ENABLED  ((cl_object)(cl_symbols+5))
#define ECL_NO_TL_BINDING       ((cl_object)(1 << ECL_TAG_BITS))

struct ecl_symbol {
        _ECL_HDR1(stype);        /*  symbol type */
        cl_object value;         /*  global value of the symbol  */
        cl_object gfdef;         /*  global function definition  */
        cl_objectfn undef_entry; /*  entry point for not fboundp
                                  *  symbols (must match the position
                                  *  of cfun.entry); see below for
                                  *  more explanation */
        cl_object macfun;        /*  macro expansion function  */
        cl_object sfdef;         /*  global (setf f) definition */
        cl_object plist;         /*  property list  */
        cl_object name;          /*  print name  */
        cl_object cname;         /*  associated C name for function
                                  *  with exported C interface */
        cl_object hpack;         /*  home package  */
                                 /*  ECL_NIL for uninterned symbols  */
#ifdef ECL_THREADS
        cl_index binding;        /*  index into the bindings array  */
#endif
};
/*
 * Undefined functions are implemented as follows.
 *
 * For a symbols s with a global function binding: s->symbol.gfdef
 * points to a cfun object, f->symbol.undef_entry is unused.
 *
 * For a symbol without a global function binding: s->symbol.gfdef
 * points back to s itself, s->symbol.undef_entry is set. When an
 * attempt is made to funcall s, the entry point in
 * s->symbol.undef_entry is called which signals an undefined-function
 * condition. Since the_env->function is set to s->symbol.gfdef which
 * in this case is just s itself, we know which symbol caused the
 * error and can signal accordingly. This construction enables us to
 * skip the check for whether a function is bound or not when
 * funcalling.
 */
#define ECL_SYM_FUN(sym)        ((sym)->symbol.gfdef)
#define ECL_FBOUNDP(sym)        ((sym)->symbol.gfdef != (sym))
#define ECL_FMAKUNBOUND(sym)    ((sym)->symbol.gfdef = (sym))

struct ecl_package {
        _ECL_HDR1(locked);
        cl_object name;            /*  package name, a string  */
        cl_object nicknames;       /*  nicknames, list of strings  */
        cl_object local_nicknames; /*  local nicknames, assoc list */
        cl_object nicknamedby;  /*  nicknamed-by-list of packages */
        cl_object shadowings;   /*  shadowing symbol list  */
        cl_object uses;         /*  use-list of packages  */
        cl_object usedby;       /*  used-by-list of packages  */
        cl_object internal;     /*  hashtable for internal symbols  */
        cl_object external;     /*  hashtable for external symbols  */
};

/*
        The values returned by intern and find_symbol.
        File_symbol may return 0.
*/
enum {
        ECL_INTERNAL = 1,
        ECL_EXTERNAL,
        ECL_INHERITED
};

/*
 * CONSES
 *
 * We implement two variants. The "small cons" type carries the type
 * information in the least significant bits of the pointer. We have
 * to do some pointer arithmetics to find out the CAR / CDR of the
 * cons but the overall result is faster and memory efficient, only
 * using two words per cons.
 *
 * The other scheme stores conses as three-words objects, the first
 * word carrying the type information. This is kept for backward
 * compatibility and also because the oldest garbage collector does
 * not yet support the smaller datatype.
 *
 * To make code portable and independent of the representation, only
 * access the objects using the common macros below (that is all
 * except ECL_CONS_PTR or ECL_PTR_CONS).
 */

#ifdef ECL_SMALL_CONS
#define ECL_LISTP(x)    (ECL_IMMEDIATE(x) == t_list)
#define ECL_CONSP(x)    (LISTP(x) && !Null(x))
#define ECL_ATOM(x)     (Null(x) || !LISTP(x))
#define ECL_SYMBOLP(x)  (Null(x) || ((ECL_IMMEDIATE(x) == 0) && ((x)->d.t == t_symbol)))

#define ECL_PTR_CONS(x) (cl_object)((char*)(x) + t_list)
#define ECL_CONS_PTR(x) ((struct ecl_cons *)((char *)(x) - t_list))
#define ECL_CONS_CAR(x) (*(cl_object*)((char *)(x) - t_list))
#define ECL_CONS_CDR(x) (*(cl_object*)((char *)(x) + sizeof(cl_object) - t_list))
#define ECL_RPLACA(x,v) (ECL_CONS_CAR(x)=(v))
#define ECL_RPLACD(x,v) (ECL_CONS_CDR(x)=(v))

struct ecl_cons {
        cl_object car;          /*  car  */
        cl_object cdr;          /*  cdr  */
};
#else
#define ECL_LISTP(x)    (ECL_IMMEDIATE(x)? Null(x) : ((x)->d.t == t_list))
#define ECL_CONSP(x)    ((ECL_IMMEDIATE(x) == 0) && ((x)->d.t == t_list))
#define ECL_ATOM(x)     (ECL_IMMEDIATE(x) || ((x)->d.t != t_list))
#define ECL_SYMBOLP(x)  (Null(x) || ((ECL_IMMEDIATE(x) == 0) && ((x)->d.t == t_symbol)))

#define ECL_CONS_CAR(x) ((x)->cons.car)
#define ECL_CONS_CDR(x) ((x)->cons.cdr)
#define ECL_RPLACA(x,v) (ECL_CONS_CAR(x)=(v))
#define ECL_RPLACD(x,v) (ECL_CONS_CDR(x)=(v))

struct ecl_cons {
        _ECL_HDR;
        cl_object car;          /*  car  */
        cl_object cdr;          /*  cdr  */
};
#endif

enum ecl_httest {               /*  hash table key test function  */
        ecl_htt_eq,             /*  eq  */
        ecl_htt_eql,            /*  eql  */
        ecl_htt_equal,          /*  equal  */
        ecl_htt_equalp,         /*  equalp  */
        ecl_htt_pack,           /*  symbol hash  */
        ecl_htt_generic         /*  generic user-supplied test  */
};

enum ecl_htweak {
        ecl_htt_not_weak = 0,
        ecl_htt_weak_key,
        ecl_htt_weak_value,
        ecl_htt_weak_key_and_value,
        ecl_htt_weak_key_or_value
};

struct ecl_hashtable_entry {    /*  hash table entry  */
        cl_object key;          /*  key  */
        cl_object value;        /*  value  */
};

struct ecl_hashtable {          /*  hash table header  */
        _ECL_HDR2(test,weak);
        struct ecl_hashtable_entry *data; /*  pointer to the hash table  */
        cl_object sync_lock;              /*  synchronization lock  */
        cl_object generic_test;           /*  generic test function */
        cl_object generic_hash;           /*  generic hashing function */
        cl_index entries;       /*  number of entries  */
        cl_index size;          /*  hash table size  */
        cl_index limit;         /*  hash table threshold (integer value)  */
        cl_object rehash_size;  /*  rehash size  */
        cl_object threshold;    /*  rehash threshold  */
        double factor;          /*  cached value of threshold  */
        cl_object (*get)(cl_object, cl_object, cl_object);
        cl_object (*set)(cl_object, cl_object, cl_object);
        bool (*rem)(cl_object, cl_object);
        /* Unsafe variants are used to store the real accessors when
           the synchronized variant is bound to get/set/rem. */
        cl_object (*get_unsafe)(cl_object, cl_object, cl_object);
        cl_object (*set_unsafe)(cl_object, cl_object, cl_object);
        bool (*rem_unsafe)(cl_object, cl_object);
};

typedef enum {                  /*  array element type  */
        ecl_aet_object,         /*  t                */
        ecl_aet_sf,             /*  single-float     */
        ecl_aet_df,             /*  double-float     */
        ecl_aet_lf,             /*  long-float       */
#ifdef ECL_COMPLEX_FLOAT
        ecl_aet_csf,            /*  complex-single-float */
        ecl_aet_cdf,            /*  complex-double-float */
        ecl_aet_clf,            /*  complex-long-float */
#endif
        ecl_aet_bit,            /*  bit              */
        ecl_aet_fix,            /*  cl_fixnum        */
        ecl_aet_index,          /*  cl_index         */
        /* Below here, list types accepted by streams (i.e. OPEN) */
        ecl_aet_b8,             /*  byte8            */
        ecl_aet_i8,             /*  integer8         */
#ifdef ecl_uint16_t
        ecl_aet_b16,
        ecl_aet_i16,
#endif
#ifdef ecl_uint32_t
        ecl_aet_b32,
        ecl_aet_i32,
#endif
#ifdef ecl_uint64_t
        ecl_aet_b64,
        ecl_aet_i64,
#endif
#ifdef ECL_UNICODE
        ecl_aet_ch,                     /*  character        */
#endif
        ecl_aet_bc,                     /*  base-char        */
        ecl_aet_last_type = ecl_aet_bc
} cl_elttype;

union ecl_array_data {
        cl_object     *t;
        ecl_base_char *bc;
#ifdef ECL_UNICODE
        ecl_character *c;
#endif
        uint8_t       *b8;
        int8_t        *i8;
#ifdef ecl_uint16_t
        ecl_uint16_t  *b16;
        ecl_int16_t   *i16;
#endif
#ifdef ecl_uint32_t
        ecl_uint32_t  *b32;
        ecl_int32_t   *i32;
#endif
#ifdef ecl_uint64_t
        ecl_uint64_t  *b64;
        ecl_int64_t   *i64;
#endif
        float         *sf;
        double        *df;
        long double   *lf;
#ifdef ECL_COMPLEX_FLOAT
        float _Complex *csf;
        double _Complex *cdf;
        long double _Complex *clf;
#endif
        cl_fixnum     *fix;
        cl_index      *index;
        byte          *bit;
};

#define ECL_FLAG_HAS_FILL_POINTER 1
#define ECL_FLAG_ADJUSTABLE       2
#define ECL_ADJUSTABLE_ARRAY_P(x) ((x)->array.flags & ECL_FLAG_ADJUSTABLE)
#define ECL_ARRAY_HAS_FILL_POINTER_P(x) ((x)->array.flags & ECL_FLAG_HAS_FILL_POINTER)

struct ecl_array {              /*  array header  */
                                /*  adjustable flag  */
                                /*  has-fill-pointer flag  */
        _ECL_HDR2(elttype,flags); /*  array element type, has fill ptr, adjustable-p */
        cl_object displaced;    /*  displaced  */
        cl_index dim;           /*  dimension  */
        cl_index *dims;         /*  table of dimensions  */
        union ecl_array_data self;      /*  pointer to the array  */
        byte    offset;         /*  bitvector offset  */
        byte    rank;           /*  rank of array = # of dimensions  */
};

struct ecl_vector {             /*  vector header  */
                                /*  adjustable flag  */
                                /*  has-fill-pointer flag  */
        _ECL_HDR2(elttype,flags);        /*  array element type, has fill ptr, adjustable-p */
        cl_object displaced;    /*  displaced  */
        cl_index dim;           /*  dimension  */
        cl_index fillp;         /*  fill pointer  */
                                /*  For simple vectors, fillp is equal to dim. */
        union ecl_array_data self; /*  pointer to the vector  */
        byte    offset;
};

struct ecl_base_string {        /*  string header  */
                                /*  adjustable flag  */
                                /*  has-fill-pointer flag  */
        _ECL_HDR2(elttype,flags);       /*  array element type, has fill ptr, adjustable-p */
        cl_object displaced;    /*  displaced  */
        cl_index dim;           /*  dimension  */
                                /*  string length  */
        cl_index fillp;         /*  fill pointer  */
                                /*  For simple strings,  */
                                /*  st_fillp is equal to st_dim-1.  */
        ecl_base_char *self;    /*  pointer to the string  */
};

#ifdef ECL_UNICODE
struct ecl_string {             /*  string header  */
                                /*  adjustable flag  */
                                /*  has-fill-pointer flag  */
        _ECL_HDR2(elttype,flags);       /*  array element type, has fill ptr, adjustable-p */
        cl_object displaced;    /*  displaced  */
        cl_index dim;           /*  dimension, string length  */
        cl_index fillp;         /*  fill pointer  */
                                /*  For simple strings, fillp is equal to dim-1. */
        ecl_character *self;    /*  pointer to the string  */
};
#endif

#define T_STRUCTURE     t_instance
#define ECL_STRUCT_TYPE(x)      ECL_CLASS_OF(x)
#define ECL_STRUCT_SLOTS(x)     (x)->instance.slots
#define ECL_STRUCT_LENGTH(x)    (x)->instance.length
#define ECL_STRUCT_SLOT(x,i)    (x)->instance.slots[i]
#define ECL_STRUCT_NAME(x)      ECL_CLASS_NAME(ECL_CLASS_OF(x))

enum ecl_smmode {               /*  stream mode  */
        ecl_smm_input,          /*  input  */
        ecl_smm_input_file,     /*  input  */
        ecl_smm_output,         /*  output  */
        ecl_smm_output_file,    /*  output  */
        ecl_smm_io,             /*  input-output  */
        ecl_smm_io_file,        /*  input-output  */
        ecl_smm_synonym,        /*  synonym  */
        ecl_smm_broadcast,      /*  broadcast  */
        ecl_smm_concatenated,   /*  concatenated  */
        ecl_smm_two_way,        /*  two way  */
        ecl_smm_echo,           /*  echo  */
        ecl_smm_string_input,   /*  string input  */
        ecl_smm_string_output,  /*  string output  */
        ecl_smm_probe,          /*  probe (only used in open_stream())  */
#if defined(ECL_WSOCK)
        ecl_smm_input_wsock,    /*  input socket (Win32) */
        ecl_smm_output_wsock,   /*  output socket (Win32) */
        ecl_smm_io_wsock,       /*  input/output socket (Win32) */
#endif
#if defined(ECL_MS_WINDOWS_HOST)
        ecl_smm_io_wcon,        /*  windows console (Win32) */
#endif
        ecl_smm_sequence_input, /*  sequence input  */
        ecl_smm_sequence_output /*  sequence output  */
};

struct ecl_file_ops {
        cl_index (*read_byte8)(cl_object strm, unsigned char *c, cl_index n);
        cl_index (*write_byte8)(cl_object strm, unsigned char *c, cl_index n);

        cl_object (*read_byte)(cl_object strm);
        void (*write_byte)(cl_object strm, cl_object byte);
        void (*unread_byte)(cl_object strm, cl_object byte);
        cl_object (*peek_byte)(cl_object strm);

        ecl_character (*read_char)(cl_object strm);
        ecl_character (*write_char)(cl_object strm, ecl_character c);
        void (*unread_char)(cl_object strm, ecl_character c);
        ecl_character (*peek_char)(cl_object strm);

        cl_index (*read_vector)(cl_object strm, cl_object data, cl_index start, cl_index end);
        cl_index (*write_vector)(cl_object strm, cl_object data, cl_index start, cl_index end);

        int (*listen)(cl_object strm);
        void (*clear_input)(cl_object strm);
        void (*clear_output)(cl_object strm);
        void (*finish_output)(cl_object strm);
        void (*force_output)(cl_object strm);

        int (*input_p)(cl_object strm);
        int (*output_p)(cl_object strm);
        int (*interactive_p)(cl_object strm);
        cl_object (*element_type)(cl_object strm);

        cl_object (*length)(cl_object strm);
        cl_object (*get_position)(cl_object strm);
        cl_object (*set_position)(cl_object strm, cl_object pos);
        cl_object (*string_length)(cl_object strm, cl_object string);
        int (*column)(cl_object strm);

        cl_object (*pathname)(cl_object strm);
        cl_object (*truename)(cl_object strm);

        cl_object (*close)(cl_object strm);
};

enum {
        ECL_STREAM_BINARY = 0,
        ECL_STREAM_FORMAT = 0xF,
#ifndef ECL_UNICODE
        ECL_STREAM_DEFAULT_FORMAT = 1,
#else
        ECL_STREAM_DEFAULT_FORMAT = 2,
        ECL_STREAM_ISO_8859_1 = 1,
        ECL_STREAM_LATIN_1 = 1,
        ECL_STREAM_UTF_8 = 2,
        ECL_STREAM_UCS_2 = 3,
        ECL_STREAM_UCS_2LE = 5 + 128,
        ECL_STREAM_UCS_2BE = 5,
        ECL_STREAM_UCS_4 = 6,
        ECL_STREAM_UCS_4LE = 7 + 128,
        ECL_STREAM_UCS_4BE = 7,
        ECL_STREAM_USER_FORMAT = 8,
        ECL_STREAM_US_ASCII = 10,
#endif
        /* External Format */
        ECL_STREAM_CR = 16,
        ECL_STREAM_LF = 32,
        ECL_STREAM_SIGNED_BYTES = 64,
        ECL_STREAM_LITTLE_ENDIAN = 128,
        /* OS Streams */
        ECL_STREAM_C_STREAM = 256,
        ECL_STREAM_MIGHT_SEEK = 512,
        ECL_STREAM_CLOSE_ON_EXEC = 1024,
        ECL_STREAM_NONBLOCK = 2048,
        /* Lisp Streams */
        ECL_STREAM_CLOSE_COMPONENTS = 4096,
        ECL_STREAM_USE_VECTOR_FILLP = 8192
};

/* buffer points to an array of bytes ending at buffer_end. Decode one
   character from this array and advance buffer by the number of bytes
   consumed. If not enough bytes are available, return EOF and don't
   advance buffer. */
typedef ecl_character (*cl_eformat_decoder)(cl_object stream, unsigned char **buffer, unsigned char *buffer_end);
/* Encode the character c and store the result in buffer. Return the
   number of bytes used */
typedef int (*cl_eformat_encoder)(cl_object stream, unsigned char *buffer, ecl_character c);

/* Buffer is assumed to be big enough to store whole byte. The byte size is
   stream->strm.byte_size. Decoder returns an object, encoder fills a buffer. */
typedef cl_object (*cl_binary_decoder)(cl_object stream, unsigned char *buf);
typedef void (*cl_binary_encoder)(cl_object stream, unsigned char *buf, cl_object byte);

#define ECL_ANSI_STREAM_P(o) ECL_BOXED_TYPEP(o, t_stream)
#define ECL_ANSI_STREAM_TYPE_P(o,m) \
        (ECL_ANSI_STREAM_P(o) && ((o)->stream.mode == (m)))

struct ecl_stream {
        _ECL_HDR2(mode,closed); /*  stream mode of enum smmode  */
                                /*  closed stream?  */
        struct ecl_file_ops *ops; /*  dispatch table  */
        union {
                FILE *stream;           /* ANSI C streams */
                cl_fixnum descriptor;   /* POSIX files */
        } file;
        cl_object object0;      /*  some object  */
        cl_object object1;      /*  some object */
        cl_object last_byte;    /*  last byte read  */
        cl_fixnum last_char;    /*  last character read  */
        cl_object byte_stack;   /*  buffer for unread bytes  */
        cl_index column;        /*  file column  */
        cl_fixnum int0;         /*  some int  */
        cl_fixnum int1;         /*  some int  */
        cl_index byte_size;     /*  size of byte in binary streams  */
        cl_fixnum last_op;      /*  0: unknown, 1: reading, -1: writing */
        char *buffer;           /*  buffer for FILE  */
        unsigned char *byte_buffer; /*  buffer for encoding and decoding */
        cl_object format;       /*  external format  */
        cl_eformat_encoder encoder;
        cl_eformat_decoder decoder;
        cl_binary_encoder byte_encoder;
        cl_binary_decoder byte_decoder;
        cl_object format_table;
        int flags;              /*  character table, flags, etc  */
        ecl_character eof_char;
};

struct ecl_random {
        _ECL_HDR;
        cl_object value;        /*  random state value  */
};

enum ecl_chattrib {             /*  character attribute  */
        cat_whitespace,         /*  whitespace  */
        cat_terminating,        /*  terminating macro  */
        cat_non_terminating,    /*  non-terminating macro  */
        cat_single_escape,      /*  single-escape  */
        cat_multiple_escape,    /*  multiple-escape  */
        cat_constituent         /*  constituent  */
};

struct ecl_readtable_entry {            /*  read table entry  */
        enum ecl_chattrib syntax_type;  /*  character attribute  */
        cl_object macro;                /*  character  macro  */
        cl_object table;                /*  dispatch table or NIL*/
};

enum ecl_readtable_case {
        ecl_case_upcase,
        ecl_case_downcase,
        ecl_case_invert,
        ecl_case_preserve
};

struct ecl_readtable {          /*  read table  */
        _ECL_HDR1(locked);
        enum ecl_readtable_case read_case; /*  readtable-case  */
        struct ecl_readtable_entry *table; /*  read table itself  */
        cl_objectfn_parse parse_token;     /*  token parser */
#ifdef ECL_UNICODE
        cl_object hash;         /* hash for values outside base-char range */
#endif
};

struct ecl_pathname {
        _ECL_HDR1(logical);     /*  logical pathname?  */
        cl_object host;         /*  host  */
        cl_object device;       /*  device  */
        cl_object directory;    /*  directory  */
        cl_object name;         /*  name  */
        cl_object type;         /*  type  */
        cl_object version;      /*  version  */
};

struct ecl_codeblock {
        _ECL_HDR2(self_destruct,locked);        /*  delete DLL after gc */
                                        /*  do not garbage collect this library */
        void    *handle;                /*  handle returned by dlopen  */
        void    *entry;                 /*  entry point  */
        cl_object *data;                /*  data vector  */
        int     data_size;
        cl_object *temp_data;           /*  data vector for toplevel forms */
        int     temp_data_size;
        const cl_object *data_text;     /*  strings with objects to be defined  */
        cl_object next;                 /*  next codeblock within same library */
        cl_object name;
        cl_object links;                /*  list of symbols with linking calls  */
        cl_index cfuns_size;            /*  number of functions defined  */
        const struct ecl_cfunfixed *cfuns;
        cl_object source;               /*  common debug information for this block  */
        cl_object refs;                 /*  reference counter for the library  */
        cl_object error;                /*  error message when loading */
};

struct ecl_bytecodes {
        _ECL_HDR;
        cl_object name;         /*  function name  */
        cl_object definition;   /*  function definition in list form  */
        cl_objectfn entry;      /*  entry address (must match the position of
                                 *  the equivalent field in cfun) */
        cl_index code_size;     /*  number of bytecodes  */
        char *code;             /*  the intermediate language  */
        cl_object data;         /*  non-inmediate constants used in the code  */
        cl_object flex;         /*  indexes of captured objects (vector) */
        cl_object nlcl;         /*  number of slots for local values */
        cl_object file;         /*  file where it was defined...  */
        cl_object file_position;/*  and where it was created  */
};

struct ecl_bclosure {
        _ECL_HDR;
        cl_object code;
        cl_object lex;          /*  captured objects (flat vector) */
        cl_objectfn entry;      /*  entry address  */
};

struct ecl_cfun {               /*  compiled function header  */
        _ECL_HDR1(narg);        /*  number of fixed arguments */
        cl_object name;         /*  compiled function name  */
        cl_object block;        /*  descriptor of C code block for GC  */
        cl_objectfn entry;      /*  entry address  */
#ifdef ECL_C_COMPATIBLE_VARIADIC_DISPATCH
        /* Some architectures (i.e. ARM64 on iOS/Mac OS X) use a different
         * calling convention for the fixed and variadic arguments of
         * a variadic function. The only portable way to allow for
         * function redefinitions during runtime in these
         * architectures is to set entry to a dispatch function
         * calling entry_variadic with the correct calling conventions
         * (see cfun.d). -- mg 2019-12-02 */
        cl_objectfn entry_variadic;
#endif
        cl_object file;         /*  file where it was defined...  */
        cl_object file_position;/*  and where it was created  */
};

struct ecl_cfunfixed {          /*  compiled function header  */
        _ECL_HDR1(narg);        /*  number of arguments */
        cl_object name;         /*  compiled function name  */
        cl_object block;        /*  descriptor of C code block for GC  */
        cl_objectfn entry;      /*  entry address (must match the position of
                                 *  the equivalent field in cfun) */
        cl_objectfn_fixed entry_fixed;  /*  entry address  */
        cl_object file;         /*  file where it was defined...  */
        cl_object file_position;/*  and where it was created  */
};

struct ecl_cclosure {           /*  compiled closure header  */
        _ECL_HDR1(narg);        /*  number of fixed arguments */
        cl_object env;          /*  environment  */
        cl_object block;        /*  descriptor of C code block for GC  */
        cl_objectfn entry;      /*  entry address (must match the position of
                                 *  the equivalent field in cfun) */
#ifdef ECL_C_COMPATIBLE_VARIADIC_DISPATCH
        cl_objectfn entry_variadic; /* see struct ecl_cfun above */
#endif
        cl_object file;         /*  file where it was defined...  */
        cl_object file_position;/*  and where it was created  */
};

#define ECL_FFICALL_LIMIT 256

enum ecl_ffi_tag {
        ECL_FFI_CHAR = 0,
        ECL_FFI_UNSIGNED_CHAR,
        ECL_FFI_BYTE,
        ECL_FFI_UNSIGNED_BYTE,
        ECL_FFI_SHORT,
        ECL_FFI_UNSIGNED_SHORT,
        ECL_FFI_INT,
        ECL_FFI_UNSIGNED_INT,
        ECL_FFI_LONG,
        ECL_FFI_UNSIGNED_LONG,
#ifdef ecl_uint8_t
        ECL_FFI_INT8_T,
        ECL_FFI_UINT8_T,
#endif
#ifdef ecl_uint16_t
        ECL_FFI_INT16_T,
        ECL_FFI_UINT16_T,
#endif
#ifdef ecl_uint32_t
        ECL_FFI_INT32_T,
        ECL_FFI_UINT32_T,
#endif
#ifdef ecl_uint64_t
        ECL_FFI_INT64_T,
        ECL_FFI_UINT64_T,
#endif
#ifdef ecl_long_long_t
        ECL_FFI_LONG_LONG,
        ECL_FFI_UNSIGNED_LONG_LONG,
#endif
        ECL_FFI_POINTER_VOID,
        ECL_FFI_CSTRING,
        ECL_FFI_OBJECT,
        ECL_FFI_FLOAT,
        ECL_FFI_DOUBLE,
        ECL_FFI_LONG_DOUBLE,
#ifdef ECL_COMPLEX_FLOAT
        ECL_FFI_CSFLOAT,
        ECL_FFI_CDFLOAT,
        ECL_FFI_CLFLOAT,
#endif
        ECL_FFI_VOID
};

union ecl_ffi_values {
        char c;
        unsigned char uc;
        int8_t b;
        uint8_t ub;
        int i;
        unsigned int ui;
        short s;
        unsigned short us;
        long l;
        unsigned long ul;
#ifdef ecl_uint8_t
        ecl_int8_t i8;
        ecl_uint8_t u8;
#endif
#ifdef ecl_uint16_t
        ecl_int16_t i16;
        ecl_uint16_t u16;
#endif
#ifdef ecl_uint32_t
        ecl_int32_t i32;
        ecl_uint32_t u32;
#endif
#ifdef ecl_uint64_t
        ecl_int64_t i64;
        ecl_uint64_t u64;
#endif
#ifdef ecl_long_long_t
        ecl_long_long_t ll;
        ecl_ulong_long_t ull;
        unsigned long l2[2];
#endif
        void *pv;
        char *pc;
        cl_object o;
        float f;
        double d;
        long double lf;
#ifdef ECL_COMPLEX_FLOAT
        float _Complex csf;
        double _Complex cdf;
        long double _Complex clf;
#endif
};

struct ecl_foreign {            /*  user defined datatype  */
        _ECL_HDR;
        cl_object tag;          /*  a tag identifying the type  */
        cl_index size;          /*  the amount of memory allocated  */
        char *data;             /*  the data itself  */
};

struct ecl_stack_frame {
        _ECL_HDR1(opened);
        cl_index base;          /*  Start of the stack frame */
        cl_index size;          /*  Number of arguments  */
        cl_index sp;            /*  Stack pointer  */
        struct cl_env_struct *env;
};

/* Token is constructed by the reader from constituents and then parsed. */
struct ecl_token {
        _ECL_HDR1(escaped);
        cl_object string;       /* the token string */
        cl_object escape;       /* ranges of escaped characters */
};

struct ecl_weak_pointer {       /*  weak pointer to value  */
        _ECL_HDR;
        cl_object value;
};
#define ecl_weak_pointer(o) ((o)->weak.value)

/*
        dummy type
*/
struct ecl_dummy {
        _ECL_HDR;
};

#ifdef ECL_THREADS

#ifdef ECL_WINDOWS_THREADS
typedef HANDLE ecl_thread_t;
typedef HANDLE ecl_mutex_t;
typedef struct ecl_cond_var_t {
        HANDLE broadcast_event;
        HANDLE signal_event;
        cl_index state;
} ecl_cond_var_t;
typedef SRWLOCK ecl_rwlock_t;
#else
typedef pthread_t ecl_thread_t;
typedef pthread_mutex_t ecl_mutex_t;
typedef pthread_cond_t ecl_cond_var_t;
# ifdef HAVE_POSIX_RWLOCK
typedef pthread_rwlock_t ecl_rwlock_t;
# else
typedef struct ecl_rwlock_t {
        pthread_mutex_t mutex;
        pthread_cond_t reader_cv;
        pthread_cond_t writer_cv;
        /* reader_count = 0: lock is free
         * reader_count =-1: writer holds the lock
         * reader_count > 0: number of readers */
        cl_fixnum reader_count;
} ecl_rwlock_t;
# endif
#endif

enum {
        ECL_PROCESS_INACTIVE = 0,
        ECL_PROCESS_BOOTING,
        ECL_PROCESS_ACTIVE,
        ECL_PROCESS_EXITING
};

struct ecl_process {
        _ECL_HDR;
        cl_object name;
        cl_object function;
        cl_object args;
        struct cl_env_struct *env;
        cl_object interrupt;
        cl_object inherit_bindings_p;
        cl_object parent;
        cl_object exit_values;
        cl_object woken_up;
        ecl_mutex_t start_stop_lock; /* phase is updated only when we hold this lock */
        ecl_cond_var_t exit_barrier; /* process-join waits on this barrier */
        cl_index phase;
        ecl_thread_t thread;
        int trap_fpe_bits;
};

enum {
        ECL_WAKEUP_ONE = 0,
        ECL_WAKEUP_ALL = 1,
        ECL_WAKEUP_RESET_FLAG = 2,
        ECL_WAKEUP_KILL = 4,
        ECL_WAKEUP_DELETE = 8
};

struct ecl_semaphore {
        _ECL_HDR;
        cl_object name;
        cl_fixnum counter;
        cl_fixnum wait_count;
        ecl_mutex_t mutex;
        ecl_cond_var_t cv;
};

#define ECL_BARRIER_WAKEUP_NORMAL 1
#define ECL_BARRIER_WAKEUP_KILL 2

struct ecl_barrier {
        _ECL_HDR2(disabled,wakeup);
        cl_object name;
        cl_index count;
        cl_index arrivers_count;
        ecl_mutex_t mutex;
        ecl_cond_var_t cv;
};

struct ecl_lock {
        _ECL_HDR1(recursive);
        ecl_mutex_t mutex;
        cl_object owner;       /* thread holding the lock or NIL */
        cl_object name;
        cl_fixnum counter;
};

struct ecl_mailbox {
        _ECL_HDR;
        cl_object name;
        cl_object data;
        ecl_mutex_t mutex;
        ecl_cond_var_t reader_cv;
        ecl_cond_var_t writer_cv;
        cl_index message_count;
        cl_index read_pointer;
        cl_index write_pointer;
};

struct ecl_rwlock {
        _ECL_HDR;
        cl_object name;
        ecl_rwlock_t mutex;
};

struct ecl_condition_variable {
        _ECL_HDR;
        ecl_cond_var_t cv;
};
#endif /* ECL_THREADS */

#define ECL_INSTANCEP(x)        ECL_BOXED_TYPEP(x, t_instance)
#define ECL_FUNCALLABLE_P(x)    (ECL_INSTANCEP(x) && (x)->instance.isgf)
#define ECL_CLASS_OF(x)         (x)->instance.clas

#define ECL_SPEC_FLAG(x)        (x)->instance.slots[0]
#define ECL_SPEC_OBJECT(x)      (x)->instance.slots[3]
#define ECL_CLASS_NAME(x)       (x)->instance.slots[3+0]
#define ECL_CLASS_SUPERIORS(x)  (x)->instance.slots[3+1]
#define ECL_CLASS_INFERIORS(x)  (x)->instance.slots[3+2]
#define ECL_CLASS_SLOTS(x)      (x)->instance.slots[3+3]
#define ECL_CLASS_CPL(x)        (x)->instance.slots[3+4]

#define ECL_NOT_FUNCALLABLE     0
#define ECL_STANDARD_DISPATCH   1
#define ECL_RESTRICTED_DISPATCH 2
#define ECL_READER_DISPATCH     3
#define ECL_WRITER_DISPATCH     4
#define ECL_USER_DISPATCH       5
#define ECL_NULL_DISPATCH       6 /* funcallable, but gfdef is null */

struct ecl_instance {            /* -- instance header -- */
        _ECL_HDR1(isgf);         /*  gf type              */
        cl_object clas;          /*  instance class       */
        cl_object gfdef;         /*  generic function definition */
        cl_objectfn entry;       /*  entry address (matches cfun.entry) */
        cl_index length;         /*  instance length      */
        cl_object slotds;        /*  slot definitions     */
        cl_object *slots;        /*  instance slots       */
        cl_index stamp;          /*  instance stamp       */
        cl_index class_stamp;    /*  class stamp          */
};

#ifdef ECL_SSE2
union ecl_sse_data {
        /* This member must be first in order for
           ecl_def_ct_sse_pack to work properly. */
        uint8_t       b8[16];
        int8_t        i8[16];

        __m128        vf;
        __m128i       vi;
        __m128d       vd;

#ifdef ecl_uint16_t
        ecl_uint16_t  b16[8];
        ecl_int16_t   i16[8];
#endif
#ifdef ecl_uint32_t
        ecl_uint32_t  b32[4];
        ecl_int32_t   i32[4];
#endif
#ifdef ecl_uint64_t
        ecl_uint64_t  b64[2];
        ecl_int64_t   i64[2];
#endif

        float         sf[4];
        double        df[2];
};

struct ecl_sse_pack {
        _ECL_HDR1(elttype);
        union ecl_sse_data data;
};

#define ecl_sse_elttype(o) ((o)->sse.elttype)
#define ecl_sse_data(o)    ((o)->sse.data)
#endif

/*
        Definition of lispunion.
*/
union cl_lispunion {
#ifndef ECL_SMALL_CONS
        struct ecl_cons         cons;           /*  unoptimized cons  */
#endif
        struct ecl_bignum       big;            /*  bignum  */
        struct ecl_ratio        ratio;          /*  ratio  */
/* #ifndef ECL_IMMEDIATE_SFLOAT */
        struct ecl_singlefloat  SF;             /*  single floating-point number  */
/* #endif */
        struct ecl_doublefloat  DF;             /*  double floating-point number  */
        struct ecl_long_float   longfloat;      /*  long-float */
        struct ecl_complex      gencomplex;     /*  generic complex number  */
#ifdef ECL_COMPLEX_FLOAT
        struct ecl_csfloat      csfloat;        /*  complex single float */
        struct ecl_cdfloat      cdfloat;        /*  complex double float */
        struct ecl_clfloat      clfloat;        /*  complex long float */
#endif
        struct ecl_symbol       symbol;         /*  symbol  */
        struct ecl_package      pack;           /*  package  */
        struct ecl_hashtable    hash;           /*  hash table  */
        struct ecl_array        array;          /*  array  */
        struct ecl_vector       vector;         /*  vector  */
        struct ecl_base_string  base_string;    /*  base-string  */
#ifdef ECL_UNICODE
        struct ecl_string       string;         /*  string  */
#endif
        struct ecl_stream       stream;         /*  stream  */
        struct ecl_random       random;         /*  random-states  */
        struct ecl_readtable    readtable;      /*  read table  */
        struct ecl_pathname     pathname;       /*  path name  */
        struct ecl_bytecodes    bytecodes;      /*  bytecompiled function / code */
        struct ecl_bclosure     bclosure;       /*  bytecompiled closure */
        struct ecl_cfun         cfun;           /*  compiled function  */
        struct ecl_cfunfixed    cfunfixed;      /*  compiled function  */
        struct ecl_cclosure     cclosure;       /*  compiled closure  */
        struct ecl_dummy        d;              /*  dummy  */
        struct ecl_instance     instance;       /*  clos instance */
        struct ecl_token        token;          /*  token */
#ifdef ECL_THREADS
        struct ecl_process      process;        /*  process  */
        struct ecl_lock         lock;           /*  lock  */
        struct ecl_rwlock       rwlock;         /*  read/write lock  */
        struct ecl_condition_variable condition_variable; /*  condition-variable */
        struct ecl_semaphore    semaphore;      /*  semaphore  */
        struct ecl_barrier      barrier;        /*  barrier  */
        struct ecl_mailbox      mailbox;        /*  mailbox  */
#endif
        struct ecl_codeblock    cblock;         /*  codeblock  */
        struct ecl_foreign      foreign;        /*  user defined data type */
        struct ecl_stack_frame  frame;          /*  stack frame  */
        struct ecl_weak_pointer weak;           /*  weak pointers  */
#ifdef ECL_SSE2
        struct ecl_sse_pack     sse;
#endif
};

/*
        Type_of.
*/

static inline cl_type ecl_t_of(cl_object o) {
    int i = ECL_IMMEDIATE(o);
    if (!i) return (cl_type)(o->d.t);
    if (ECL_FIXNUMP(o))    return (cl_type)t_fixnum;
    if (ECL_LISTP(o))      return (cl_type)t_list;
    if (ECL_CHARACTERP(o)) return (cl_type)t_character;
#ifdef ECL_IMMEDIATE_DFLOAT
    if (i & 0b100)
        return (cl_type)t_doublefloat;
#endif
#ifdef ECL_IMMEDIATE_SFLOAT
    if (ECL_XMMEDIATE(o) == ECL_SINGLE_FLOAT_TAG)
        return (cl_type)t_singlefloat;
#endif
    return (cl_type)ECL_CONSTANT_TYPE(o);
}

/*
        This is used to retrieve optional arguments
*/
typedef struct {
        va_list args;
        cl_object *sp;
        int narg;
} ecl_va_list[1];

#ifdef __cplusplus
}
#endif
