#include "ecl/defines-boehm.h"
#include "ecl/defines-complex.h"
#include "ecl/defines-feature.h"
#include "ecl/defines-function.h"
#include "ecl/defines-header.h"
#include "ecl/defines-types.h"
#include "ecl/defines-unicode.h"

#if defined(__cplusplus) || \
    (defined(__GNUC__) &&   \
    (defined(__GNUC_GNU_INLINE__) || defined(__GNUC_STDC_INLINE__)))
# define ECL_INLINE inline
# define ECL_CAN_INLINE 1
#else
# define ECL_INLINE
# define ECL_CAN_INLINE 0
#endif

#if !defined(__GNUC__)
# define ecl_likely(form) (form)
# define ecl_unlikely(form) (form)
# define ecl_attr_noreturn
#else
# if (__GNUC__ < 3)
#  define ecl_likely(form) (form)
#  define ecl_unlikely(form) (form)
# else
#  define ecl_likely(form) __builtin_expect(form,1)
#  define ecl_unlikely(form) __builtin_expect(form,0)
# endif
# if (__GNUC__ < 4)
#  define ecl_attr_noreturn
# else
#  define ecl_attr_noreturn __attribute__((noreturn))
# endif
#endif

/*
 * Macros that depend on these system features.
 */
#if defined(sparc) || defined(i386) || defined(mips)
#  define       stack_align(n)  (((n) + 0x7) & ~0x7)
#else
#  define       stack_align(n)  (((n) + 03) & ~03)
#endif


#if defined(cygwin) || defined(ECL_MS_WINDOWS_HOST)
#  define IS_DIR_SEPARATOR(x) ((x=='/')||(x=='\\'))
#  define DIR_SEPARATOR         '/'
#  define PATH_SEPARATOR        ';'
#else
#  define IS_DIR_SEPARATOR(x) (x=='/')
#  define DIR_SEPARATOR '/'
#  define PATH_SEPARATOR        ':'
#endif

#ifdef ECL_AVOID_FPE_H
# define ecl_detect_fpe()
#else
# include "@ECL_FPE_CODE@"
#endif

#ifdef ECL_INCLUDE_MATH_H
# include <math.h>
# ifdef _MSC_VER
#  undef complex
#  define signbit(x) (copysign(1.0,(x)))
# endif
# ifndef isfinite
#  error "Function isfinite() is missing"
# endif
# ifndef signbit
#  ifndef ECL_SIGNED_ZERO
#   define signbit(x) ((x) < 0)
#  else
#   ifdef HAVE_COPYSIGN
#    define signbit(x) (copysign(1.0,(x)) < 0)
#   else 
     /* Fall back to no signed zero */
#    undef \
     ECL_SIGNED_ZERO
#    define signbit(x) ((x) < 0)
#   endif
#  endif
# endif
/*
 * GCC fails to compile the following code
 * if (f == 0.0) { if (signbit(f)) ... }
 */
# if defined(__sun__) && defined(__GNUC__)
#  undef signbit
#  define signbit(x) (copysign(1.0,(x)) < 0)
# endif
#endif

/* Use mprotect for fast interrupt dispatch				*/
#ifndef NACL
#undef ECL_USE_MPROTECT
#endif

#if defined(ECL_MS_WINDOWS_HOST)
# define ECL_USE_GUARD_PAGE
#endif


#if defined(ECL_MS_WINDOWS_HOST)

# define strcasecmp _stricmp

# if defined(_MSC_VER) && (_MSC_VER < 1800)
#  define isnan _isnan
# endif
# define finite _finite
# define sleep _sleep

#endif

#if defined(ECL_MS_WINDOWS_HOST) || defined(cygwin)
# define ECL_DLLEXPORT __declspec(dllexport)
# ifdef ECL_API
#  undef \
   ECL_API /* Avoid autoconf removing this */
#  define ECL_API __declspec(dllexport)
# else
#  define ECL_API __declspec(dllimport)
# endif
#else
# define ECL_DLLEXPORT
# ifdef ECL_API
#  undef \
   ECL_API /* Avoid autoconf removing this */
# endif
# define ECL_API
#endif

