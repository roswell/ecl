#cmakedefine GBC_BOEHM_GENGC      @GBC_BOEHM_GENGC@
#cmakedefine ECL_SMALL_CONS       @ECL_SMALL_CONS@
#cmakedefine GBC_BOEHM            @GBC_BOEHM@
#cmakedefine GBC_BOEHM_PRECISE    @GBC_BOEHM_PRECISE@
#cmakedefine GC_THREADS           @GC_THREADS@
#cmakedefine ECL_DYNAMIC_VV       @ECL_DYNAMIC_VV@

#ifdef GBC_BOEHM
# include "@ECL_BOEHM_GC_HEADER@"
/* GC >= 7.2 defines these macros to intercept thread functions, but
 * in doing so it breaks mingw. */
# if defined(ECL_MS_WINDOWS_HOST) && defined(_beginthreadex)
#  undef _beginthread
#  undef _endthread
#  undef _beginthreadex
#  undef _endthreadex
# endif
#endif

#ifdef GBC_BOEHM
# define ECL_ARRAY_DIMENSION_LIMIT @CL_FIXNUM_MAX@
# define ECL_ARRAY_TOTAL_LIMIT     @CL_FIXNUM_MAX@
#else
# define ECL_ARRAY_DIMENSION_LIMIT 16 * 1024 * 1024
# define ECL_ARRAY_TOTAL_LIMIT     16 * 1024 * 1024
#endif
