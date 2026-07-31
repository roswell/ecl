
@ECL_STDINT_HEADER@

#cmakedefine ECL_LONG_LONG_BITS @ECL_LONG_LONG_BITS@
#cmakedefine ecl_long_long_t    @ecl_long_long_t@
#cmakedefine ecl_ulong_long_t   @ecl_ulong_long_t@

#cmakedefine ECL_UINT8_T        @ECL_UINT8_T@
#cmakedefine ECL_UINT16_T       @ECL_UINT16_T@
#cmakedefine ECL_UINT32_T       @ECL_UINT32_T@
#cmakedefine ECL_UINT64_T       @ECL_UINT64_T@

#cmakedefine ecl_uint8_t        @ecl_uint8_t@
#cmakedefine ecl_uint16_t       @ecl_uint16_t@
#cmakedefine ecl_uint32_t       @ecl_uint32_t@
#cmakedefine ecl_uint64_t       @ecl_uint64_t@

#cmakedefine ECL_INT8_T         @ECL_INT8_T@
#cmakedefine ECL_INT16_T        @ECL_INT16_T@
#cmakedefine ECL_INT32_T        @ECL_INT32_T@
#cmakedefine ECL_INT64_T        @ECL_INT64_T@

#cmakedefine ecl_int8_t         @ecl_int8_t@
#cmakedefine ecl_int16_t        @ecl_int16_t@
#cmakedefine ecl_int32_t        @ecl_int32_t@
#cmakedefine ecl_int64_t        @ecl_int64_t@

#cmakedefine CL_FIXNUM_MIN     @CL_FIXNUM_MIN@
#cmakedefine CL_FIXNUM_MAX      @CL_FIXNUM_MAX@
#cmakedefine CL_FIXNUM_TYPE     @CL_FIXNUM_TYPE@
#cmakedefine CL_FIXNUM_BITS     @CL_FIXNUM_BITS@

#cmakedefine CL_SHORT_BITS      @CL_SHORT_BITS@
#cmakedefine CL_INT_BITS        @CL_INT_BITS@
#cmakedefine CL_LONG_BITS       @CL_LONG_BITS@

#cmakedefine HAVE_POSIX_RWLOCK  @HAVE_POSIX_RWLOCK@

#cmakedefine ECL_IEEE_FP        @ECL_IEEE_FP@
#cmakedefine ECL_SIGNED_ZERO    @ECL_SIGNED_ZERO@
#cmakedefine ECL_HAS_BIGNUM     @ECL_HAS_BIGNUM@

typedef @CL_FIXNUM_TYPE@ cl_fixnum;
typedef unsigned @CL_FIXNUM_TYPE@ cl_index;
typedef unsigned @CL_FIXNUM_TYPE@ cl_hashkey;

#cmakedefine ECL_BIGNUM_REGISTER_NUMBER @ECL_BIGNUM_REGISTER_NUMBER@

#cmakedefine ECL_INT_BITS            @ECL_INT_BITS@
#cmakedefine ECL_LONG_BITS           @ECL_LONG_BITS@
#cmakedefine ECL_FIXNUM_BITS         @ECL_FIXNUM_BITS@

#cmakedefine MOST_POSITIVE_FIXNUM      ((cl_fixnum)@MOST_POSITIVE_FIXNUM@)
#cmakedefine MOST_NEGATIVE_FIXNUM      ((cl_fixnum)@MOST_NEGATIVE_FIXNUM@)
#cmakedefine MOST_POSITIVE_FIXNUM_VAL  @MOST_POSITIVE_FIXNUM_VAL@
#cmakedefine MOST_NEGATIVE_FIXNUM_VAL @MOST_NEGATIVE_FIXNUM_VAL@
