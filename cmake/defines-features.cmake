#cmakedefine ECL_VERSION_NUMBER       @ECL_VERSION_NUMBER@
#cmakedefine ECL_THREADS              @ECL_THREADS@

#cmakedefine ECL_NEWLINE             "@ECL_NEWLINE@"
#cmakedefine ECL_NEWLINE_IS_CRLF      @ECL_NEWLINE_IS_CRLF@
#cmakedefine ECL_NEWLINE_IS_LFCR      @ECL_NEWLINE_IS_LFCR@

#cmakedefine ECL_MS_WINDOWS_HOST      @ECL_MS_WINDOWS_HOST@

#cmakedefine ECL_BIGENDIAN            @ECL_BIGENDIAN@
#cmakedefine WORDS_BIGENDIAN          @WORDS_BIGENDIAN@

#cmakedefine ECL_DOWN_STACK           @ECL_DOWN_STACK@
#cmakedefine ECL_CAN_SET_STACK_SIZE   @ECL_CAN_SET_STACK_SIZE@

#if defined(ECL_CAN_SET_STACK_SIZE)
#define ECL_DEFAULT_C_STACK_SIZE 0 /* Use the stack size provided by the OS */
#else
#define ECL_DEFAULT_C_STACK_SIZE @ECL_DEFAULT_C_STACK_SIZE@
#endif

#cmakedefine ECL_SSE2                 @ECL_SSE2@

#cmakedefine ECL_FILE_CNT             @ECL_FILE_CNT@

#cmakedefine ECL_AVOID_FPE_H          @ECL_AVOID_FPE_H@

#cmakedefine ECL_USE_DBGHELP          @ECL_USE_DBGHELP@
#cmakedefine ECL_WINDOWS_BACKTRACE    @ECL_WINDOWS_BACKTRACE@

#ifdef ECL_AVOID_FPE_H
# define ecl_detect_fpe()
#else
# include "@ECL_FPE_CODE@"
#endif

#cmakedefine HAVE_FEENABLEEXCEPT      @HAVE_FEENABLEEXCEPT@

#cmakedefine ECL_SLOTS_LIMIT          @ECL_SLOTS_LIMIT@

#cmakedefine LISP_PAGESIZE            @LISP_PAGESIZE@
#cmakedefine MAXPAGE                  @MAXPAGE@
#cmakedefine ECL_MAX_STRING_POOL_SIZE @ECL_MAX_STRING_POOL_SIZE@
#cmakedefine ECL_BUFFER_STRING_SIZE   @ECL_BUFFER_STRING_SIZE@
#cmakedefine ECL_ARRAY_RANK_LIMIT     @ECL_ARRAY_RANK_LIMIT@

#cmakedefine ECL_C_COMPATIBLE_VARIADIC_DISPATCH @ECL_C_COMPATIBLE_VARIADIC_DISPATCH@

#cmakedefine ECL_CALL_ARGUMENTS_LIMIT    @ECL_CALL_ARGUMENTS_LIMIT@
#cmakedefine ECL_LAMBDA_PARAMETERS_LIMIT @ECL_LAMBDA_PARAMETERS_LIMIT@
#cmakedefine ECL_C_ARGUMENTS_LIMIT       @ECL_C_ARGUMENTS_LIMIT@
#cmakedefine ECL_MULTIPLE_VALUES_LIMIT   @ECL_MULTIPLE_VALUES_LIMIT@

#cmakedefine ECL_WSOCK                @ECL_WSOCK@
#cmakedefine ECL_CMU_FORMAT           @ECL_CMU_FORMAT@
#cmakedefine ENABLE_DLOPEN            @ENABLE_DLOPEN@

#cmakedefine ECL_CLOS_STREAMS         @ECL_CLOS_STREAMS@
