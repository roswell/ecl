
/* -- imports ------------------------------------------------------- */

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>

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
  1,                            /* ECL_OPT_SIGNAL_HANDLING_THREAD     */
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
