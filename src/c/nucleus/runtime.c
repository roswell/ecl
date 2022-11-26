
/* -- imports --------------------------------------------------------------- */

#include <limits.h>
#if defined(ECL_MS_WINDOWS_HOST)
# include <windows.h>
# include <shellapi.h>
# define MAXPATHLEN 512
#endif
#ifndef MAXPATHLEN
# ifdef PATH_MAX
#   define MAXPATHLEN PATH_MAX
# else
#   define MAXPATHLEN sysconf(_PC_PATH_MAX)
#   include <unistd.h>
# endif
#endif

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/external.h>

/* -- constants ----------------------------------------------------- */

const cl_object ecl_ct_Jan1st1970UT = ecl_make_fixnum(39052800);

ecl_def_ct_base_string(ecl_ct_null_string,"",0,,const);

ecl_def_ct_single_float(ecl_ct_default_rehash_size,1.5f,,const);
ecl_def_ct_single_float(ecl_ct_default_rehash_threshold,0.75f,,const);

ecl_def_ct_single_float(ecl_ct_singlefloat_zero,0,,const);
ecl_def_ct_double_float(ecl_ct_doublefloat_zero,0,,const);
ecl_def_ct_long_float(ecl_ct_longfloat_zero,0,,const);

ecl_def_ct_single_float(ecl_ct_singlefloat_minus_zero,-0.0,,const);
ecl_def_ct_double_float(ecl_ct_doublefloat_minus_zero,-0.0,,const);
ecl_def_ct_long_float(ecl_ct_longfloat_minus_zero,-0.0l,,const);

ecl_def_ct_ratio(ecl_ct_plus_half,ecl_make_fixnum(1),ecl_make_fixnum(2),,const);
ecl_def_ct_ratio(ecl_ct_minus_half,ecl_make_fixnum(-1),ecl_make_fixnum(2),,const);

/* These two tags have a special meaning for the frame stack. */

ecl_def_ct_base_string(ecl_ct_ptag_string,"PROTECT-TAG",11,static,const);
ecl_def_ct_base_string(ecl_ct_dtag_string,"DUMMY-TAG",9,static,const);

ecl_def_ct_token(ecl_ct_protect_tag,ecl_stp_constant,ecl_ct_ptag_string,ECL_NIL,,const);
ecl_def_ct_token(ecl_ct_dummy_tag  ,ecl_stp_constant,ecl_ct_dtag_string,ECL_NIL,,const);


/* -- implementation ------------------------------------------------ */

#if ECL_FIXNUM_BITS <= 32
/* 1GB */
#define ECL_DEFAULT_HEAP_SIZE 1073741824L
#else
/* 4GB */
#define ECL_DEFAULT_HEAP_SIZE 4294967296L
#endif

#ifndef ECL_DEFAULT_C_STACK_SIZE
#define ECL_DEFAULT_C_STACK_SIZE 0
#endif

#ifdef GBC_BOEHM_GENGC
#define ECL_INCREMENTAL_GC 1
#else
#define ECL_INCREMENTAL_GC 0
#endif

#if defined(ECL_THREADS) && defined(HAVE_SIGPROCMASK)
#define ECL_SIGNAL_HANDLING_THREAD 1
#else
#define ECL_SIGNAL_HANDLING_THREAD 0
#endif

/* INV: see ecl_option enum in external.h */
cl_fixnum ecl_option_values[ECL_OPT_LIMIT+1] = {
  /* ---------------------------------------------------------------- */
  ECL_INCREMENTAL_GC,           /* ECL_OPT_INCREMENTAL_GC             */
  1,                            /* ECL_OPT_TRAP_SIGSEGV               */
  1,                            /* ECL_OPT_TRAP_SIGFPE                */
  1,                            /* ECL_OPT_TRAP_SIGINT                */
  1,                            /* ECL_OPT_TRAP_SIGILL                */
  1,                            /* ECL_OPT_TRAP_SIGBUS                */
  1,                            /* ECL_OPT_TRAP_SIGPIPE               */
  1,                            /* ECL_OPT_TRAP_INTERRUPT_SIGNAL      */
  ECL_SIGNAL_HANDLING_THREAD,   /* ECL_OPT_SIGNAL_HANDLING_THREAD     */
  16,                           /* ECL_OPT_SIGNAL_QUEUE_SIZE          */
  0,                            /* ECL_OPT_BOOTED                     */
  /* ---------------------------------------------------------------- */
  8192,                         /* ECL_OPT_BIND_STACK_SIZE            */
  1024,                         /* ECL_OPT_BIND_STACK_SAFETY_AREA     */
  2048,                         /* ECL_OPT_FRAME_STACK_SIZE           */
  128,                          /* ECL_OPT_FRAME_STACK_SAFETY_AREA    */
  32768,                        /* ECL_OPT_LISP_STACK_SIZE            */
  128,                          /* ECL_OPT_LISP_STACK_SAFETY_AREA     */
  ECL_DEFAULT_C_STACK_SIZE,     /* ECL_OPT_C_STACK_SIZE               */
  4*sizeof(cl_index)*1024,      /* ECL_OPT_C_STACK_SAFETY_AREA        */
  ECL_DEFAULT_HEAP_SIZE,        /* ECL_OPT_HEAP_SIZE                  */
  1024*1024,                    /* ECL_OPT_HEAP_SAFETY_AREA           */
  0,                            /* ECL_OPT_THREAD_INTERRUPT_SIGNAL    */
  1,                            /* ECL_OPT_SET_GMP_MEMORY_FUNCTIONS   */
  1,                            /* ECL_OPT_USE_SETMODE_ON_FILES       */
  /* ---------------------------------------------------------------- */
  0};

cl_fixnum
ecl_get_option(int option)
{
  if (option >= ECL_OPT_LIMIT || option < 0) {
    return -1;
  }
  return ecl_option_values[option];
}

cl_fixnum
ecl_set_option(int option, cl_fixnum value)
{
  if (option > ECL_OPT_LIMIT || option < 0) {
    return -1;
  }
  if (option >= ECL_OPT_BOOTED || !ecl_option_values[ECL_OPT_BOOTED]) {
    ecl_option_values[option] = value;
  }
  return ecl_option_values[option];
}

/* -- core runtime ---------------------------------------------------------- */

/* The root environment is a default execution context. */
static struct cl_env_struct first_env;

struct ecl_core_struct ecl_core = {
  .first_env = &first_env,
  /* processes */
#ifdef ECL_THREADS
  .processes = ECL_NIL,
  .last_var_index = 0,
  .reused_indices = ECL_NIL,
#endif
  /* signals */
  .default_sigmask_bytes = 0,
  .known_signals = ECL_NIL,
  /* allocation */
  .max_heap_size = 0,
  .bytes_consed = ECL_NIL,
  .gc_counter = ECL_NIL,
  .gc_stats = 0,
  .safety_region = NULL,
  /* pathnames */
  .path_max = 0,
  .pathname_translations = ECL_NIL,
  /* LIBRARIES is a list of objects. It behaves as a sequence of weak pointers
     thanks to the magic in the garbage collector. */
  .libraries = ECL_NIL,
  .library_pathname = ECL_NIL
};

/* note that this function does not create any environment */
int
ecl_boot(void)
{
  int i;

  i = ecl_option_values[ECL_OPT_BOOTED];
  if (i) {
    if (i < 0) {
      /* We have called cl_shutdown and want to use ECL again. */
      ecl_set_option(ECL_OPT_BOOTED, 1);
    }
    return 1;
  }

  init_process();
  /* init_unixint(); */
  /* init_garbage(); */

  ecl_core.path_max = MAXPATHLEN;

  return 0;
}
