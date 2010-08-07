/* -*- mode: c; c-basic-offset: 8 -*- */
/*
 * Loops over a proper list. Complains on circularity
 */
#define loop_for_in_no_circle(list) { \
  cl_object __slow; \
  bool __flag = TRUE; \
  for (__slow = list; !ecl_endp(list); list = ECL_CONS_CDR(list)) { \
    if ((__flag = !__flag)) { \
      if (__slow == list) FEcircular_list(list); \
      __slow = ECL_CONS_CDR(__slow); \
    }

/*
 * Loops over a proper list
 */
#define loop_for_in(list) { \
  const cl_object __ecl_l0 = list; \
  for (; list != Cnil; list = ECL_CONS_CDR(list)) { \
    if (ecl_unlikely(!ECL_LISTP(list))) FEtype_error_proper_list(__ecl_l0);

#define end_loop_for_in }}

/*
 * Loops over a dotted list. Complains on circularity.
 */
#define loop_for_on_no_circle(list) \
  if (!CONSP(list)) { \
    if (list != Cnil) FEtype_error_list(list); \
  }else { \
    cl_object __slow; \
    bool __flag = TRUE; \
    for (__slow = list; CONSP(list); list = ECL_CONS_CDR(list)) { \
      if ((__flag = !__flag)) { \
        if (__slow == list) FEcircular_list(list); \
        __slow = CDR(__slow); \
      }

/*
 * Loops over a list. Ignores errors.
 */
#define loop_for_on_unsafe(list) \
  for (; ECL_CONSP(list); list = ECL_CONS_CDR(list)) {
#define end_loop_for_on_unsafe(list) }

/*
 * Loops over a dotted list
 */
#define loop_for_on(list) \
  if (Null(list)) { \
    (void)0; \
  } else if (ecl_unlikely(!ECL_LISTP(list))) { \
    FEtype_error_list(list); \
  } else do {
#define end_loop_for_on(list) } while (list = ECL_CONS_CDR(list), ECL_CONSP(list))

#ifdef __cplusplus
#define ecl_cast_ptr(type,n) reinterpret_cast<type>((void*)n)
#else
#define ecl_cast_ptr(type,n) ((type)(n))
#endif

#define ecl_def_ct_base_string(name,chars,len,static,const)     \
        static const struct ecl_base_string name ## data = {    \
                (int8_t)t_base_string, 0, aet_bc, 0,            \
                Cnil, (cl_index)(len), (cl_index)(len),         \
                (ecl_base_char*)(chars) };                      \
        static const cl_object name = (cl_object)(& name ## data)

#define ecl_def_ct_single_float(name,f,static,const)            \
        static const struct ecl_singlefloat name ## data = {    \
                (int8_t)t_singlefloat, 0, 0, 0,                 \
                (float)(f) };                                   \
        static const cl_object name = (cl_object)(& name ## data)

#define ecl_def_ct_double_float(name,f,static,const)            \
        static const struct ecl_doublefloat name ## data = {    \
                (int8_t)t_doublefloat, 0, 0, 0,                 \
                (double)(f) };                                  \
        static const cl_object name = (cl_object)(& name ## data)

#define ecl_def_ct_long_float(name,f,static,const)			\
        static const struct ecl_long_float name ## data = {		\
                (int8_t)t_longfloat, 0, 0, 0,				\
                (long double)(f) };					\
        static const cl_object name = (cl_object)(& name ## data)

#define ecl_def_ct_ratio(name,num,den,static,const)			\
        static const struct ecl_ratio name ## data = {			\
                (int8_t)t_ratio, 0, 0, 0,				\
                den, num };						\
        static const cl_object name = (cl_object)(& name ## data)

#define ecl_def_ct_complex(name,real,imag,static,const)			\
        static const struct ecl_complex name ## data = {		\
                (int8_t)t_complex, 0, 0, 0,				\
                (cl_object)real, (cl_object)imag };			\
        static const cl_object name = (cl_object)(& name ## data)

#define ecl_def_ct_vector(name,type,raw,len,static,const)               \
        static const struct ecl_vector name ## data = {                 \
                (int8_t)t_vector, 0, (type), 0,                         \
                Cnil, (cl_index)(len), (cl_index)(len),                 \
                ecl_cast_ptr(cl_object*,raw), 0 };                      \
        static const cl_object name = (cl_object)(& name ## data)

#ifdef ECL_SSE2
#define ecl_def_ct_sse_pack(name,type,v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15) \
        static const struct ecl_sse_pack name ## data = {                 \
                (int8_t)t_sse_pack, 0, (type), 0,                         \
                {{v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15}} \
        }; \
        static const cl_object name = (cl_object)(& name ## data)
#endif
