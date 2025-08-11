/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
 * Copyright (c) 2001, Juan Jose Garcia Ripoll.
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/* internal.h -- Structures and functions that are not meant for the end user */
#ifndef ECL_INTERNAL_H
#define ECL_INTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif

/* -------------------------------------------------------------------- *
 *      FUNCTIONS, VARIABLES AND TYPES NOT FOR GENERAL USE              *
 * -------------------------------------------------------------------- */

#define unlikely_if(x) if (ecl_unlikely(x))

/* booting */
extern void init_all_symbols(void);
extern void init_alloc(int pass);
extern void init_backq(void);
extern void init_big();
extern void init_clos(void);
extern void init_error(void);
extern void init_eval(void);
extern void init_file(void);
#ifndef GBC_BOEHM
extern void init_GC(void);
#endif
extern void init_macros(void);
extern void init_read(void);

extern cl_object init_stacks(cl_env_ptr);
extern cl_object free_stacks(cl_env_ptr);

extern void init_unixint(int pass);
extern void init_unixtime(void);
extern void init_compiler(void);
extern void init_process(void);
#ifdef ECL_THREADS
extern void init_threads(void);
#endif
extern void ecl_init_env(cl_env_ptr);
extern void init_lib_LSP(cl_object);

extern cl_env_ptr _ecl_alloc_env(cl_env_ptr parent);
extern void _ecl_dealloc_env(cl_env_ptr);

/* alloc.d/alloc_2.d */

#ifdef GBC_BOEHM
#define ECL_COMPACT_OBJECT_EXTRA(x) ((void*)((x)->array.displaced))
#endif
extern void _ecl_set_max_heap_size(size_t new_size);
extern cl_object ecl_alloc_bytecodes(cl_index data_size, cl_index code_size);
extern cl_index ecl_object_byte_size(cl_type t);
extern cl_index ecl_next_stamp();

/* array.d */

#ifdef ECL_DEFINE_AET_SIZE
#undef ECL_DEFINE_AET_SIZE
static const cl_index ecl_aet_size[] = {
  sizeof(cl_object),          /* ecl_aet_object */
  sizeof(float),              /* ecl_aet_sf */
  sizeof(double),             /* ecl_aet_df */
  sizeof(long double),        /* ecl_aet_lf */
#ifdef ECL_COMPLEX_FLOAT
  sizeof(_Complex float),        /* ecl_aet_csf */
  sizeof(_Complex double),       /* ecl_aet_cdf */
  sizeof(_Complex long double),  /* ecl_aet_clf */
#endif
  0,                          /* ecl_aet_bit: cannot be handled with this code */
  sizeof(cl_fixnum),          /* ecl_aet_fix */
  sizeof(cl_index),           /* ecl_aet_index */
  sizeof(uint8_t),            /* ecl_aet_b8 */
  sizeof(int8_t),             /* ecl_aet_i8 */
#ifdef ecl_uint16_t
  sizeof(ecl_uint16_t),
  sizeof(ecl_int16_t),
#endif
#ifdef ecl_uint32_t
  sizeof(ecl_uint32_t),
  sizeof(ecl_int32_t),
#endif
#ifdef ecl_uint64_t
  sizeof(ecl_uint64_t),
  sizeof(ecl_int64_t),
#endif
#ifdef ECL_UNICODE
  sizeof(ecl_character),      /* ecl_aet_ch */
#endif
  sizeof(unsigned char)       /* ecl_aet_bc */
};
#endif /* ECL_DEFINE_AET_SIZE */

extern void ecl_displace(cl_object from, cl_object to, cl_object offset);

/* cfun_dispatch.d */

typedef cl_object (*cl_objectfn_fixed0)(void);
typedef cl_object (*cl_objectfn_fixed1)(cl_object);
typedef cl_object (*cl_objectfn_fixed2)(cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed3)(cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed4)(cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed5)(cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed6)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed7)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed8)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed9)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed10)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed11)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed12)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed13)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed14)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed15)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed16)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed17)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed18)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed19)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed20)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed21)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed22)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed23)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed24)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed25)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed26)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed27)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed28)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed29)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed30)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed31)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed32)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed33)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed34)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed35)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed36)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed37)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed38)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed39)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed40)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed41)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed42)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed43)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed44)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed45)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed46)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed47)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed48)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed49)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed50)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed51)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed52)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed53)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed54)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed55)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed56)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed57)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed58)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed59)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed60)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed61)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed62)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed63)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);

typedef cl_object (*cl_objectfn1)(cl_narg narg, cl_object, ...);
typedef cl_object (*cl_objectfn2)(cl_narg narg, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn3)(cl_narg narg, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn4)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn5)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn6)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn7)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn8)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn9)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn10)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn11)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn12)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn13)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn14)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn15)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn16)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn17)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn18)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn19)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn20)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn21)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn22)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn23)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn24)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn25)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn26)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn27)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn28)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn29)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn30)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn31)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn32)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn33)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn34)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn35)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn36)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn37)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn38)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn39)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn40)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn41)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn42)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn43)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn44)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn45)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn46)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn47)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn48)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn49)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn50)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn51)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn52)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn53)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn54)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn55)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn56)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn57)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn58)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn59)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn60)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn61)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn62)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn63)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);

/* compiler.d */

typedef struct cl_compiler_env *cl_compiler_env_ptr;

struct cl_compiler_env {
        cl_object variables;            /* the env: vars, tags, funs, etc */
        cl_object macros;               /* Macros and function bindings */
        cl_fixnum lexical_level;        /* =0 if toplevel form */
        cl_object captured;             /* Captured objects from the parent */
        cl_object constants;            /* Constants for this form */
        cl_object load_time_forms;      /* Constants that have to be rebuilt */
        cl_object ltf_being_created;    /* Load time objects being compiled */
        cl_object ltf_defer_init_until; /* Defer evaluation of current
                                         * load time init form until
                                         * this object has been created */
        cl_object ltf_locations;        /* Locations of constants externalized
                                         * with make-load-form */
        cl_object lex_env;              /* Lexical env. for eval-when */
        cl_object code_walker;          /* Value of SI:*CODE-WALKER* */
        cl_index env_depth;             /* How many environments is above us */
        cl_index env_width;             /* The maximum size of locals environment */
        cl_index env_size;              /* The current size of locals environment */
        int mode;
        bool stepping;
        cl_compiler_env_ptr parent_env;
};

enum ecl_cmpref_tag {
        ECL_CMPREF_LOCAL,
        ECL_CMPREF_CLOSE,
        ECL_CMPREF_UNDEFINED,
};

enum ecl_cmpvar_tag {
        ECL_CMPVAR_UNDEFINED,
        ECL_CMPVAR_SYM_MACRO,
        ECL_CMPVAR_SPECIAL,
        ECL_CMPVAR_LEXICAL,
};

struct cl_compiler_ref {
        enum ecl_cmpref_tag place;
        cl_object entry;        /* entry in c_env->variables (if any) */
        cl_fixnum index;        /* index in the corresponding location */
        cl_fixnum label;        /* index of a label (tagbody specific) */
        cl_object location;     /* (cons env-depth env-size) */
};

/* character.d */

#ifdef ECL_UNICODE
#define ECL_UCS_NONCHARACTER(c) \
        (((c) >= 0xFDD0 && (c) <= 0xFDEF) || \
         (((c) & 0xFFFF) >= 0xFFFE && (((c) & 0xFFFF) <= 0xFFFF)))
#define ECL_UCS_PRIVATE(c) \
        (((c) >= 0xE000 && (c) <= 0xF8FF) || \
         ((c) >= 0xF0000 && (c) <= 0xFFFD) || \
         ((c) >= 0x100000 && (c) <= 0x10FFFD))
#define ECL_UCS_HIGH_SURROGATE(c) ((c) >= 0xD800 && (c) <= 0xDBFF)
#define ECL_UCS_LOW_SURROGATE(c) ((c) >= 0xDC00 && (c) <= 0xDFFF)
#endif

/* error.d */

extern void _ecl_unexpected_return() ecl_attr_noreturn;
extern cl_object _ecl_strerror(int code);

/* eval.d */

#define _ecl_funcall5(fun, a, b, c, d) \
        ecl_function_dispatch(ecl_process_env(), (fun))(4, (a),(b),(c),(d))
#define _ecl_funcall4(fun, a, b, c) \
        ecl_function_dispatch(ecl_process_env(), (fun))(3, (a),(b),(c))
#define _ecl_funcall3(fun, a, b) \
        ecl_function_dispatch(ecl_process_env(), (fun))(2, (a),(b))
#define _ecl_funcall2(fun, a) \
        ecl_function_dispatch(ecl_process_env(), (fun))(1, (a))
#define _ecl_funcall1(fun) \
        ecl_function_dispatch(ecl_process_env(), (fun))(0)

extern cl_object ecl_undefined_function_entry _ECL_ARGS((cl_narg narg, ...));

extern cl_object si_constantp_inner _ECL_ARGS((cl_narg narg, cl_object form, ...));
extern cl_object si_constant_form_value _ECL_ARGS((cl_narg narg, cl_object form, ...));

/* interpreter.d */

#if __GNUC__>=12
/* GCC starting from 12.0 (still present in GCC 14.2.1) issues a warning for
   conforming code. The warning is based on a dead code generated by its IR:

   warning: array subscript ‘union cl_lispunion[0]’ is partly outside array
   bounds of ‘struct ecl_stack_frame[1]’ [-Warray-bounds=]

   Numerous issues reported in GCC tracker, i.e:
   https://gcc.gnu.org/bugzilla/show_bug.cgi?id=106274
   https://gcc.gnu.org/bugzilla/show_bug.cgi?id=105523

   -- jd 2025-05-02 */
#pragma GCC diagnostic ignored "-Warray-bounds"
#endif

#define ECL_BUILD_STACK_FRAME(env,name,frame)   \
        struct ecl_stack_frame frame;\
        cl_object name = ecl_stack_frame_open(env, (cl_object)&frame, 0);

#define ECL_STACK_FRAME_FROM_VA_LIST(e,f,va) do {                       \
                const cl_object __frame = (f);                          \
                cl_object *base;                                        \
                cl_index i, __nargs = va[0].narg;                       \
                ecl_stack_frame_open((e), __frame, __nargs);            \
                base = ECL_STACK_FRAME_PTR(__frame);                    \
                for (i = 0; i < __nargs; i++) {                         \
                        base[i] = ecl_va_arg(va);                       \
                }                                                       \
        } while (0)

#define ECL_STACK_FRAME_VARARGS_BEGIN(narg,lastarg,frame)               \
        struct ecl_stack_frame __ecl_frame;                             \
        const cl_object frame = (cl_object)&__ecl_frame;                \
        const cl_env_ptr env = ecl_process_env();                       \
        if (narg <= ECL_C_ARGUMENTS_LIMIT) {                            \
                ecl_stack_frame_open(env, frame, narg);                 \
                cl_object *p = ECL_STACK_FRAME_PTR(frame);              \
                va_list args;                                           \
                va_start(args, lastarg);                                \
                while (narg--) {                                        \
                        *p = va_arg(args, cl_object);                   \
                        ++p;                                            \
                }                                                       \
                va_end(args);                                           \
        } else {                                                        \
                cl_index bindex = ECL_STACK_INDEX(env) - narg;          \
                frame->frame.t = t_frame;                               \
                frame->frame.opened = 0;                                \
                frame->frame.base = bindex;                             \
                frame->frame.size = narg;                               \
                frame->frame.sp = bindex;                               \
                frame->frame.env = env;                                 \
        }
#define ECL_STACK_FRAME_VARARGS_END(frame) ecl_stack_frame_close(frame)

extern cl_object _ecl_bytecodes_dispatch_vararg(cl_narg narg, ...);
extern cl_object _ecl_bclosure_dispatch_vararg(cl_narg narg, ...);
extern cl_object ecl_close_around(cl_object fun, cl_object env, cl_object flex);

/* ffi/backtrace.d */

extern void _ecl_dump_c_backtrace();

/* ffi.d */

extern enum ecl_ffi_tag ecl_foreign_type_code(cl_object type);

/* stream.d */
cl_object ecl_alloc_stream(void);
struct ecl_file_ops *ecl_duplicate_dispatch_table(const struct ecl_file_ops *ops);
const struct ecl_file_ops *ecl_stream_dispatch_table(cl_object strm);
cl_index ecl_read_byte8(cl_object stream, unsigned char *c, cl_index n);
cl_index ecl_write_byte8(cl_object stream, unsigned char *c, cl_index n);

cl_object si_read_char(cl_object strm, cl_object eof_value);
cl_object si_unread_char(cl_object strm, cl_object eof_value);
cl_object si_peek_char(cl_object strm, cl_object eof_value);
cl_object si_write_char(cl_object strm, cl_object c);

cl_object si_read_byte(cl_object strm, cl_object eof_value);
cl_object si_unread_byte(cl_object strm, cl_object byte);
cl_object si_peek_byte(cl_object strm, cl_object eof_value);
cl_object si_write_byte(cl_object strm, cl_object c);

cl_object si_listen(cl_object strm);
cl_object si_clear_input(cl_object strm);
cl_object si_finish_output(cl_object strm);
cl_object si_force_output(cl_object strm);
cl_object si_clear_output(cl_object strm);

#define ecl_unread_error(s) FEerror("Error when unreading to stream ~D", 1, s)
#define ecl_unread_twice(s) FEerror("Unread twice twice to stream ~D", 1, s)

/* streams/strm_common.d */
cl_object ecl_not_a_file_stream(cl_object strm);
void ecl_not_an_input_stream(cl_object strm);
void ecl_not_an_output_stream(cl_object strm);
cl_index ecl_not_output_write_byte8(cl_object strm, unsigned char *c, cl_index n);
cl_index ecl_not_input_read_byte8(cl_object strm, unsigned char *c, cl_index n);
cl_index ecl_not_binary_read_byte8(cl_object strm, unsigned char *c, cl_index n);
void ecl_not_output_write_byte(cl_object strm, cl_object byte);
cl_object ecl_not_input_read_byte(cl_object strm);
void ecl_not_binary_write_byte(cl_object strm, cl_object byte);
cl_object ecl_not_binary_read_byte(cl_object strm);
void ecl_not_input_unread_byte(cl_object strm, cl_object byte);
ecl_character ecl_not_input_read_char(cl_object strm);
ecl_character ecl_not_output_write_char(cl_object strm, ecl_character c);
void ecl_not_input_unread_char(cl_object strm, ecl_character c);
int ecl_not_input_listen(cl_object strm);
ecl_character ecl_not_character_read_char(cl_object strm);
ecl_character ecl_not_character_write_char(cl_object strm, ecl_character c);
ecl_character ecl_not_character_decoder(cl_object stream, unsigned char **buffer, unsigned char *buffer_end);
int ecl_not_character_encoder(cl_object stream, unsigned char *buffer, ecl_character c);
void ecl_not_input_clear_input(cl_object strm);
void ecl_not_output_clear_output(cl_object strm);
void ecl_not_output_force_output(cl_object strm);
void ecl_not_output_finish_output(cl_object strm);
cl_object ecl_not_output_string_length(cl_object strm, cl_object string);
cl_object ecl_not_file_string_length(cl_object strm, cl_object string);
int ecl_unknown_column(cl_object strm);

cl_object ecl_generic_peek_byte(cl_object strm);
ecl_character ecl_generic_peek_char(cl_object strm);
void ecl_generic_void(cl_object strm);
int ecl_generic_always_true(cl_object strm);
int ecl_generic_always_false(cl_object strm);
cl_object ecl_generic_always_nil(cl_object strm);
int ecl_generic_column(cl_object strm);
cl_object ecl_generic_set_position(cl_object strm, cl_object pos);
cl_object ecl_generic_close(cl_object strm);
cl_index ecl_generic_write_vector(cl_object strm, cl_object data, cl_index start, cl_index end);
cl_index ecl_generic_read_vector(cl_object strm, cl_object data, cl_index start, cl_index end);

/* streams/strm_binary.d */
cl_object ecl_binary_read_byte(cl_object strm);
void ecl_binary_write_byte(cl_object c, cl_object strm);
void ecl_binary_unread_byte(cl_object strm, cl_object byte);

cl_object ecl_binary_u8_decoder(cl_object strm, unsigned char *buf);
void ecl_binary_u8_encoder(cl_object strm, unsigned char *buf, cl_object byte);
cl_object ecl_binary_s8_decoder(cl_object strm, unsigned char *buf);
void ecl_binary_s8_encoder(cl_object strm, unsigned char *buf, cl_object byte);
cl_object ecl_binary_be_decoder(cl_object strm, unsigned char *buf);
void ecl_binary_be_encoder(cl_object strm, unsigned char *buf, cl_object byte);
cl_object ecl_binary_le_decoder(cl_object strm, unsigned char *buf);
void ecl_binary_le_encoder(cl_object strm, unsigned char *buf, cl_object byte);

/* streams/strm_eformat.d */
ecl_character ecl_eformat_read_char(cl_object strm);
void ecl_eformat_unread_char(cl_object strm, ecl_character c);
ecl_character ecl_eformat_write_char(cl_object strm, ecl_character c);
void ecl_set_stream_elt_type(cl_object stream, cl_fixnum byte_size, int flags,
                             cl_object external_format);
cl_object ecl_eformat_file_string_length(cl_object stream, cl_object string);

static inline void
write_char_increment_column(cl_object strm, ecl_character c)
{
  if (c == '\n')
    strm->stream.column = 0;
  else if (c == '\t')
    strm->stream.column = (strm->stream.column & ~((cl_index)07)) + 8;
  else
    strm->stream.column++;
}

/* Maximum number of octets required to encode a char or a byte. This currently
 * corresponds to:
 * - (4 + 4) for the UCS-4 with 4 being the byte-order mark, 4 for the char
 * - (64/ 8) for the EXT:BYTE64 which is the biggest array integer type */
#define ENCODING_BUFFER_MAX_SIZE 8

/* file.d */

/* Windows does not have this flag (POSIX thing) */
#ifndef __COSMOPOLITAN__
# ifndef O_CLOEXEC
#  define O_CLOEXEC 0
# endif
# ifndef O_NONBLOCK
#  define O_NONBLOCK 0
# endif
#endif

/* Windows needs to be told explicitely to open files in binary mode */
#ifndef O_BINARY
#define O_BINARY 0
#endif

#define ECL_FILE_STREAM_P(strm) \
        (ECL_ANSI_STREAM_P(strm) && (strm)->stream.mode < ecl_smm_synonym)

#define STRING_OUTPUT_STRING(strm) (strm)->stream.object0
#define STRING_INPUT_STRING(strm) (strm)->stream.object0
#define STRING_INPUT_POSITION(strm) (strm)->stream.int0
#define STRING_INPUT_LIMIT(strm) (strm)->stream.int1

#define TWO_WAY_STREAM_INPUT(strm) (strm)->stream.object0
#define TWO_WAY_STREAM_OUTPUT(strm) (strm)->stream.object1

#define SYNONYM_STREAM_SYMBOL(strm) (strm)->stream.object0
#define SYNONYM_STREAM_STREAM(strm) ecl_symbol_value((strm)->stream.object0)
#define BROADCAST_STREAM_LIST(strm) (strm)->stream.object0
#define ECHO_STREAM_INPUT(strm) (strm)->stream.object0
#define ECHO_STREAM_OUTPUT(strm) (strm)->stream.object1
#define CONCATENATED_STREAM_LIST(strm) (strm)->stream.object0

#define IO_STREAM_FILE(strm) ((strm)->stream.file.stream)
#define IO_STREAM_ELT_TYPE(strm) (strm)->stream.object0
#define IO_STREAM_FILENAME(strm) (strm)->stream.object1

#define IO_FILE_DESCRIPTOR(strm) (strm)->stream.file.descriptor
#define IO_FILE_ELT_TYPE(strm) (strm)->stream.object0
#define IO_FILE_FILENAME(strm) (strm)->stream.object1

#define SEQ_STREAM_ELT_TYPE(strm) (strm)->stream.object0
#define SEQ_STREAM_VECTOR(strm) (strm)->stream.object1
#define SEQ_STREAM_POSITION(strm) (strm)->stream.int0

#define SEQ_INPUT_VECTOR_END(strm) (strm)->stream.int1
#define SEQ_INPUT_LIMIT(strm)                                           \
        ((strm)->stream.flags & ECL_STREAM_USE_VECTOR_FILLP             \
         ? SEQ_STREAM_VECTOR(strm)->vector.fillp                        \
         : SEQ_INPUT_VECTOR_END(strm))


#ifndef HAVE_FSEEKO
#define ecl_off_t int
#define ecl_fseeko fseek
#define ecl_ftello ftell
#else
#define ecl_off_t off_t
#define ecl_fseeko fseeko
#define ecl_ftello ftello
#endif

extern cl_object ecl_off_t_to_integer(ecl_off_t offset);
extern ecl_off_t ecl_integer_to_off_t(cl_object offset);

/* format.d */

#ifndef ECL_CMU_FORMAT
extern cl_object si_formatter_aux _ECL_ARGS((cl_narg narg, cl_object strm, cl_object string, ...));
#endif

/* hash.d */
extern cl_object ecl_extend_hashtable(cl_object hashtable);
#ifdef ECL_EXTERNALIZABLE
extern void ecl_reconstruct_serialized_hashtable(cl_object h);
#endif

/* gfun.d, kernel.lsp */

#define GFUN_NAME(x) ((x)->instance.slots[0])
#define GFUN_SPEC(x) ((x)->instance.slots[1])
#define GFUN_COMB(x) ((x)->instance.slots[2])

extern cl_object FEnot_funcallable_vararg(cl_narg narg, ...);
extern cl_object ecl_slot_reader_dispatch(cl_narg narg, ... /* cl_object instance */);
extern cl_object ecl_slot_writer_dispatch(cl_narg narg, ... /* cl_object value, cl_object instance */);

/* load.d */

extern cl_object _ecl_library_init_prefix(void);
extern cl_object _ecl_library_default_entry(void);

/* number.d */

extern cl_object _ecl_double_to_integer(double d);
extern cl_object _ecl_float_to_integer(float d);
extern cl_object _ecl_long_double_to_integer(long double d);

/* main.d */

extern cl_fixnum ecl_option_values[ECL_OPT_LIMIT+1];

extern void ecl_init_bignum_registers(cl_env_ptr env);
extern void ecl_clear_bignum_registers(cl_env_ptr env);

/* threads/mutex.d */

extern cl_object si_mutex_timeout(cl_object process, cl_object lock, cl_object timeout);

/* print.d */

extern cl_object _ecl_stream_or_default_output(cl_object stream);
extern void _ecl_write_addr(void *x, cl_object stream);
extern void _ecl_write_array(cl_object o, cl_object stream);
extern void _ecl_write_vector(cl_object o, cl_object stream);
extern void _ecl_write_bitvector(cl_object o, cl_object stream);
extern void _ecl_write_string(cl_object o, cl_object stream);
extern void _ecl_write_base_string(cl_object o, cl_object stream);
extern void _ecl_write_list(cl_object o, cl_object stream);
extern void _ecl_write_bclosure(cl_object o, cl_object stream);
extern void _ecl_write_bytecodes(cl_object o, cl_object stream);
extern void _ecl_write_symbol(cl_object o, cl_object stream);
extern void _ecl_write_fixnum(cl_fixnum o, cl_object stream);
extern void _ecl_write_sse(cl_object o, cl_object stream);
extern void _ecl_write_unreadable(cl_object x, const char *prefix, cl_object name, cl_object stream);
extern bool _ecl_will_print_as_hash(cl_object o);
extern cl_object _ecl_ensure_buffer(cl_object buffer, cl_fixnum length);
extern void _ecl_string_push_c_string(cl_object s, const char *c);

#define ECL_PPRINT_QUEUE_SIZE                   128
#define ECL_PPRINT_INDENTATION_STACK_SIZE       256

extern void cl_write_object(cl_object x, cl_object stream);

/* threads/rwlock.d */

#ifdef ECL_THREADS
extern cl_object mp_get_rwlock_read_wait(cl_object lock);
extern cl_object mp_get_rwlock_write_wait(cl_object lock);
#endif

/* read.d */
#ifdef ECL_UNICODE
#define RTABSIZE        256             /*  read table size  */
#else
#define RTABSIZE        ECL_CHAR_CODE_LIMIT     /*  read table size  */
#endif

/* package.d */

extern cl_object _ecl_package_to_be_created(const cl_env_ptr env, cl_object name);

/* pathname.d */

extern bool ecl_wild_string_p(cl_object item);

/* sequence.d */
typedef struct { cl_index start, end, length; } cl_index_pair;
extern ECL_API cl_index_pair ecl_sequence_start_end(cl_object fun, cl_object s, cl_object start, cl_object end);

#ifdef ECL_EXTERNALIZABLE
/* serialize.d */

extern cl_object ecl_deserialize(uint8_t *data);
#endif

/* string.d */
#define ecl_vector_start_end ecl_sequence_start_end

/* stacks.d */
#define CL_NEWENV_BEGIN {\
        const cl_env_ptr the_env = ecl_process_env(); \
        cl_index __i = ecl_data_stack_push_values(the_env); \

#define CL_NEWENV_END \
        ecl_data_stack_pop_values(the_env,__i); }

extern void ecl_cs_init(cl_env_ptr env);
extern void ecl_frs_set_limit(cl_env_ptr env, cl_index n);
extern void ecl_bds_set_limit(cl_env_ptr env, cl_index n);
extern void ecl_data_stack_set_limit(cl_env_ptr env, cl_index n);
extern void ecl_cs_set_size(cl_env_ptr env, cl_index n);

#ifndef RLIM_SAVED_MAX
# define RLIM_SAVED_MAX RLIM_INFINITY
#endif

#ifndef RLIM_SAVED_CUR
# define RLIM_SAVED_CUR RLIM_INFINITY
#endif

/* threads.d */

#ifdef ECL_THREADS
extern ECL_API cl_object mp_suspend_loop();
extern ECL_API cl_object mp_break_suspend_loop();

# ifdef ECL_WINDOWS_THREADS
#  define ecl_thread_exit() ExitThread(0);
# else
#  define ecl_thread_exit() pthread_exit(NULL);
# endif  /* ECL_WINDOWS_THREADS */

#endif

/* time.d */

struct ecl_timeval {
        cl_index tv_usec;
        cl_index tv_sec;
};

extern void ecl_get_internal_real_time(struct ecl_timeval *time);
extern void ecl_get_internal_run_time(struct ecl_timeval *time);
extern void ecl_musleep(double time);

#define UTC_time_to_universal_time(x) ecl_plus(ecl_make_integer(x),cl_core.Jan1st1970UT)
extern cl_fixnum ecl_runtime(void);

/* unixfsys.d */

/* Filename encodings: on Unix we use ordinary chars encoded in a user
 * specified format (usually utf8), while on Windows we use a wchar_t
 * type.
 *
 * Naming conventions:
 *  fstr: null-terminated raw C array with element type char or wchar_t
 *  filename: Lisp base string or vector with element type byte16,
 *            also null-terminated
 */
#if defined(ECL_MS_WINDOWS_HOST) && defined(ECL_UNICODE)
#include <wchar.h>

typedef wchar_t ecl_filename_char;
#define ecl_fstrlen(x) wcslen(x)
#define ecl_fstrcpy(x,y) wcscpy(x,y)
#define ecl_fstrcat(x,y) wcscat(x,y)
#define ecl_fstr(x) L ## x      /* wchar_t string constructor prefixed with L */

cl_object ecl_make_simple_filename(const ecl_filename_char *x, cl_fixnum size);
#define ecl_make_constant_filename(x,y) ecl_make_simple_filename(x,y)
cl_object ecl_alloc_filename(cl_index len, cl_object adjustable);
#define ecl_alloc_adjustable_filename(len) ecl_alloc_filename(len, ECL_T)
#define ecl_alloc_simple_filename(len) ecl_alloc_filename(len, ECL_NIL)
cl_object ecl_concatenate_filename(cl_object x, cl_object y);
#define ecl_filename_self(x) ((ecl_filename_char*)((x)->vector.self.b16))

#define ecl_chdir _wchdir
#define ecl_stat _wstat64
#define ecl_fstat _fstat64
typedef struct __stat64 ecl_stat_struct;
#define ecl_getcwd _wgetcwd
#define ecl_access _waccess
#define ecl_unlink _wunlink
#define ecl_rename _wrename
#define ecl_open _wopen
#define ecl_fopen _wfopen
#define ecl_fdopen _wfdopen
#define ecl_rmdir _wrmdir
#define ecl_mkdir _wmkdir
#define ecl_chmod _wchmod
#define ecl_getenv _wgetenv
#define ecl_GetFileAttributes GetFileAttributesW
#define ecl_MoveFile MoveFileW
#define ecl_MoveFileEx MoveFileExW
#define ecl_DeleteFile DeleteFileW
#define ecl_FindFirstFile FindFirstFileW
#define ecl_FindNextFile FindNextFileW
#define ecl_WIN32_FIND_DATA WIN32_FIND_DATAW
#define ecl_GetTempFileName GetTempFileNameW
#define ecl_CopyFile CopyFileW
#define ecl_LoadLibrary LoadLibraryW
#define ecl_GetModuleFileName GetModuleFileNameW

#else

typedef char ecl_filename_char;
#define ecl_fstrlen(x) strlen(x)
#define ecl_fstrcpy(x,y) strcpy(x,y)
#define ecl_fstrcat(x,y) strcat(x,y)
#define ecl_fstr(x) x

#define ecl_make_simple_filename(x,y) ecl_make_simple_base_string((char *)x,y)
#define ecl_make_constant_filename(x,y) ecl_make_constant_base_string((char *)x,y)
#define ecl_alloc_adjustable_filename(len) ecl_alloc_adjustable_base_string(len)
#define ecl_alloc_simple_filename(len) ecl_alloc_simple_base_string(len)
#define ecl_concatenate_filename(x,y) si_base_string_concatenate(2,x,y)
#define ecl_filename_self(x) ((ecl_filename_char*)((x)->base_string.self))

#define ecl_chdir chdir
#define ecl_stat stat
#define ecl_fstat fstat
typedef struct stat ecl_stat_struct;
#define ecl_getcwd getcwd
#define ecl_access access
#define ecl_unlink unlink
#define ecl_rename rename
#define ecl_open open
#define ecl_fopen fopen
#define ecl_fdopen fdopen
#define ecl_rmdir rmdir
#define ecl_mkdir mkdir
#define ecl_chmod chmod
#define ecl_getenv getenv
#define ecl_GetFileAttributes GetFileAttributesA
#define ecl_MoveFile MoveFileA
#define ecl_MoveFileEx MoveFileExA
#define ecl_DeleteFile DeleteFileA
#define ecl_FindFirstFile FindFirstFileA
#define ecl_FindNextFile FindNextFileA
#define ecl_WIN32_FIND_DATA WIN32_FIND_DATAA
#define ecl_GetTempFileName GetTempFileNameA
#define ecl_CopyFile CopyFileA
#define ecl_LoadLibrary LoadLibraryA
#define ecl_GetModuleFileName GetModuleFileNameA

#endif

/*
 * POSIX specifies that the "b" flag is ignored. This is good, because
 * under MSDOS and Apple's OS we need to open text files in binary mode,
 * so that we get both the carriage return and the linefeed characters.
 * Otherwise, it would be complicated to implement file-position and
 * seek operations.
 */
#define OPEN_R  ecl_fstr("rb")
#define OPEN_W  ecl_fstr("wb")
#define OPEN_RW ecl_fstr("r+b")
#define OPEN_A  ecl_fstr("ab")
#define OPEN_RA ecl_fstr("a+b")

int ecl_backup_open(const ecl_filename_char *filename, int option, int mode);
cl_object ecl_decode_filename(cl_object x, cl_object len);
cl_object ecl_encode_filename(cl_object x, cl_object len);


/* unixint.d */

#define ECL_PI_D 3.14159265358979323846264338327950288
#define ECL_PI_L 3.14159265358979323846264338327950288l
#define ECL_PI2_D 1.57079632679489661923132169163975144
#define ECL_PI2_L 1.57079632679489661923132169163975144l

extern void ecl_interrupt_process(cl_object process, cl_object function);

/* disabling interrupts on the lisp side */

#define ECL_WITHOUT_INTERRUPTS_BEGIN(the_env) do {                \
        cl_env_ptr __the_env = (the_env);                         \
        ecl_bds_bind(__the_env, ECL_INTERRUPTS_ENABLED, ECL_NIL);

#define ECL_WITHOUT_INTERRUPTS_END                 \
        ecl_bds_unwind1(__the_env);                \
        ecl_check_pending_interrupts(__the_env); } while(0)

/* unixsys.d */

/* Some old BSD systems don't have WCONTINUED / WIFCONTINUED */

#if !defined(ECL_MS_WINDOWS_HOST) && !defined(__COSMOPOLITAN__)
# ifndef WCONTINUED
#  define WCONTINUED 0
# endif
# ifndef WIFCONTINUED
#  define WIFCONTINUED(x) 0
# endif
#endif /* ECL_MS_WINDOWS_HOST */

/* global locks */

#include <ecl/threads.h>

#ifdef ECL_THREADS
# define ECL_WITH_GLOBAL_LOCK_BEGIN(the_env)                    \
        ECL_WITH_NATIVE_LOCK_BEGIN(the_env, &cl_core.global_lock)
# define ECL_WITH_GLOBAL_LOCK_END               \
        ECL_WITH_NATIVE_LOCK_END
# define ECL_WITH_LOCK_BEGIN(the_env,lock) {             \
        const cl_env_ptr __ecl_the_env = the_env;        \
        const cl_object __ecl_the_lock = lock;           \
        ecl_disable_interrupts_env(__ecl_the_env);       \
        mp_get_lock_wait(__ecl_the_lock);                \
        ECL_UNWIND_PROTECT_BEGIN(__ecl_the_env);         \
        ecl_enable_interrupts_env(__ecl_the_env);
# define ECL_WITH_LOCK_END                               \
        ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT {            \
                mp_giveup_lock(__ecl_the_lock);          \
        } ECL_UNWIND_PROTECT_THREAD_SAFE_END; }
# define ECL_WITH_NATIVE_LOCK_BEGIN(the_env,lock) {      \
        const cl_env_ptr __ecl_the_env = (the_env);      \
        ecl_mutex_t* __ecl_the_lock = (lock);            \
        ecl_disable_interrupts_env(__ecl_the_env);       \
        ecl_mutex_lock(__ecl_the_lock);                  \
        ECL_UNWIND_PROTECT_BEGIN(__ecl_the_env);         \
        ecl_enable_interrupts_env(__ecl_the_env);
# define ECL_WITH_NATIVE_LOCK_END                        \
        ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT {            \
                ecl_mutex_unlock(__ecl_the_lock);        \
        } ECL_UNWIND_PROTECT_THREAD_SAFE_END; }
# define ECL_WITH_GLOBAL_ENV_RDLOCK_BEGIN(the_env) {      \
        const cl_env_ptr __ecl_pack_env = the_env;        \
        ecl_bds_bind(__ecl_pack_env, ECL_INTERRUPTS_ENABLED, ECL_NIL);  \
        ecl_rwlock_lock_read(&cl_core.global_env_lock);
# define ECL_WITH_GLOBAL_ENV_RDLOCK_END                   \
        ecl_rwlock_unlock_read(&cl_core.global_env_lock); \
        ecl_bds_unwind1(__ecl_pack_env);                  \
        ecl_check_pending_interrupts(__ecl_pack_env); }
# define ECL_WITH_GLOBAL_ENV_WRLOCK_BEGIN(the_env) {      \
        const cl_env_ptr __ecl_pack_env = the_env;        \
        ecl_bds_bind(__ecl_pack_env, ECL_INTERRUPTS_ENABLED, ECL_NIL);  \
        ecl_rwlock_lock_write(&cl_core.global_env_lock);
# define ECL_WITH_GLOBAL_ENV_WRLOCK_END                    \
        ecl_rwlock_unlock_write(&cl_core.global_env_lock); \
        ecl_bds_unwind1(__ecl_pack_env);                   \
        ecl_check_pending_interrupts(__ecl_pack_env); }
#else
# define ECL_WITH_GLOBAL_LOCK_BEGIN(the_env)
# define ECL_WITH_GLOBAL_LOCK_END
# define ECL_WITH_LOCK_BEGIN(the_env,lock)
# define ECL_WITH_LOCK_END
# define ECL_WITH_NATIVE_LOCK_BEGIN(the_env,lock)
# define ECL_WITH_NATIVE_LOCK_END
# define ECL_WITH_GLOBAL_ENV_RDLOCK_BEGIN(the_env)
# define ECL_WITH_GLOBAL_ENV_RDLOCK_END
# define ECL_WITH_GLOBAL_ENV_WRLOCK_BEGIN(the_env)
# define ECL_WITH_GLOBAL_ENV_WRLOCK_END
#endif /* ECL_THREADS */

#include <ecl/ecl_atomics.h>

/*
 * Fake several ISO C99 mathematical functions if not available
 */
#include <math.h>

#ifndef HAVE_EXPF
# ifdef expf
#  undef expf
# endif
# define expf(x) exp((float)x)
#endif
#ifndef HAVE_POWF
# ifdef powf
#  undef powf
# endif
# define powf(x,y) pow((float)x,(float)y)
#endif
#ifndef HAVE_LOGF
# ifdef logf
#  undef logf
# endif
# define logf(x) log((float)x)
#endif
#ifndef HAVE_SQRTF
# ifdef sqrtf
#  undef sqrtf
# endif
# define sqrtf(x) sqrt((float)x)
#endif
#ifndef HAVE_SINF
# ifdef sinf
#  undef sinf
# endif
# define sinf(x) sin((float)x)
#endif
#ifndef HAVE_COSF
# ifdef cosf
#  undef cosf
# endif
# define cosf(x) cos((float)x)
#endif
#ifndef HAVE_TANF
# ifdef tanf
#  undef tanf
# endif
# define tanf(x) tan((float)x)
#endif
#ifndef HAVE_SINHF
# ifdef sinhf
#  undef sinhf
# endif
# define sinhf(x) sinh((float)x)
#endif
#ifndef HAVE_COSHF
# ifdef coshf
#  undef coshf
# endif
# define coshf(x) cosh((float)x)
#endif
#ifndef HAVE_TANHF
# ifdef tanhf
#  undef tanhf
# endif
# define tanhf(x) tanh((float)x)
#endif

#ifndef HAVE_CEILF
# define ceilf(x) ceil((float)x)
#endif
#ifndef HAVE_FLOORF
# define floorf(x) floor((float)x)
#endif
#ifndef HAVE_FABSF
# define fabsf(x) fabs((float)x)
#endif
#ifndef HAVE_FREXPF
# define frexpf(x,y) frexp((float)x,y)
#endif
#ifndef HAVE_LDEXPF
# define ldexpf(x,y) ldexp((float)x,y)
#endif

#ifndef HAVE_LOG1PF
# ifdef log1pf
#  undef log1pf
# endif
# define log1pf(x) logf(x+1.0f)
#endif
#ifndef HAVE_LOG1P
# ifdef log1p
#  undef log1p
# endif
# define log1p(x) log(x+1.0)
#endif
#ifndef HAVE_LOG1PL
# ifdef log1pl
#  undef log1pl
# endif
# define log1pl(x) logl(x+1.0l)
#endif

/*
 * Fake INFINITY and NAN defined in ISO C99 (portably)
 */

#ifndef INFINITY
# if _MSC_VER == 1600
static union {
    uint8_t bytes [ sizeof ( float ) ];
    float inf;
} __ecl_inf = {
    { 0, 0, 0xf0, 0x7f }
};
#  define INFINITY (__ecl_inf.inf)
# else
#  define INFINITY (1.0/0.0)
# endif  /* _MSC_VER == 1600 */
#endif /* INFINITY */

#ifndef NAN
# if _MSC_VER == 1600
static union {
    uint8_t bytes [ sizeof ( float ) ];
    float nan;
} __ecl_nan = {
    { 0, 0, 0xc0, 0x7f }
};
#  define NAN (__ecl_nan.nan)
# else
#  define NAN (0.0/0.0)
# endif  /* _MSC_VER == 1600 */
#endif /* ~NAN */

#ifdef ECL_COMPLEX_FLOAT
#include <complex.h>
#ifndef CMPLXF
# define CMPLXF(x, y) ((float complex)((float)(x) + I * (float)(y)))
#endif
#ifndef CMPLX
# define CMPLX(x, y) ((double complex)((double)(x) + I * (double)(y)))
#endif
#ifndef CMPLXL
# define CMPLXL(x, y) ((long double complex)((long double)(x) + I * (long double)(y)))
#endif
#endif

#ifdef __cplusplus
}
#endif

#endif /* ECL_INTERNAL_H */
