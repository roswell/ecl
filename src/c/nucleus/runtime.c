
/* -- imports ------------------------------------------------------- */

#include <ecl/ecl.h>
#include <ecl/internal.h>

/* -- implementation------------------------------------------------- */

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
