/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/* control.c - signaling conditions and transfering program control */

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/external.h>
#include <ecl/nucleus.h>

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#if defined(ECL_MS_WINDOWS_HOST) || defined(cygwin)
# include <windows.h>
#endif

#if defined(HAVE_BACKTRACE) && defined(HAVE_BACKTRACE_SYMBOLS)
# include <execinfo.h>
# define ECL_UNIX_BACKTRACE
#endif

#if defined(ECL_WINDOWS_BACKTRACE)
# include <windows.h>
# include <DbgHelp.h>
#endif

cl_objectfn
ecl_function_dispatch(cl_env_ptr env, cl_object x)
{
  cl_object fun = x;
  if (ecl_unlikely(fun == ECL_NIL))
    ecl_ferror(ECL_EX_F_UNDEF, fun, ECL_NIL);
  switch (ecl_t_of(fun)) {
  case t_cfunfixed:
    env->function = fun;
    return fun->cfunfixed.entry;
  case t_cfun:
    env->function = fun;
    return fun->cfun.entry;
  case t_cclosure:
    env->function = fun;
    return fun->cclosure.entry;
  case t_instance:
    env->function = fun;
    return fun->instance.entry;
  case t_symbol:
    fun = ECL_SYM_FUN(fun);
    env->function = fun;
    return fun->cfun.entry;
  case t_bytecodes:
    env->function = fun;
    return fun->bytecodes.entry;
  case t_bclosure:
    env->function = fun;
    return fun->bclosure.entry;
  default:
    ecl_ferror(ECL_EX_F_INVAL, fun, ECL_NIL);
  }
  _ecl_unexpected_return();
}

/* -- Escapes --------------------------------------------------------------- **

Non-local transfer of control. Practically this is like THROW, where
continuation is the exit point estabilished by an equivalent of CATCH.

** -------------------------------------------------------------------------- */

cl_object
ecl_escape(cl_object continuation)
{
  ecl_frame_ptr fr = frs_sch(continuation);
  if (!fr) ecl_internal_error("si_fear_handler: continuation not found!");
  ecl_unwind(ecl_process_env(), fr);
  _ecl_unexpected_return();
}

/* -- Signaling conditions -------------------------------------------------- **

Low level signals work slightly different from Common Lisp. There are no handler
clusters nor restarts. %signal is called with three arguments:

- condition   :: the signaled object (may be any cl_object)
- returns     :: the flag stating whether whether the function returns
- destination :: the thread the condition is delivered to (implementme!)

The signal invokes all handlers bound with with-handler in LIFO order and call
them with the condition. The handler may take do one of the following:

- decline :: return, then signal proceeds to the next handler
- escape  :: perform non-local transfer of control
- defer   :: signal a condition, invoke a debugger, ...

The called handler is not bound as an active signal handler during its execution
to avoid an infinite recursion while resignaling. When all handlers decline and
the CONTINUABLE is ECL_NIL, then we abort the program by invoking the function
_ecl_unexpected_return().

** -------------------------------------------------------------------------- */

cl_object
ecl_signal(cl_object condition, cl_object returns, cl_object thread) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object symbol, cluster, handler;
  symbol = ECL_SIGNAL_HANDLERS;
  cluster = ECL_SYM_VAL(the_env, symbol);
  ecl_bds_bind(the_env, symbol, cluster);
  while(!Null(cluster)) {
    handler = ECL_CONS_CAR(cluster);
    cluster = ECL_CONS_CDR(cluster);
    ECL_SETQ(the_env, symbol, cluster);
    _ecl_funcall2(handler, condition);
  }
  if (returns == ECL_NIL)
    _ecl_unexpected_return();
  ecl_bds_unwind1(the_env);
  return ECL_NIL;
}

cl_object
ecl_call_with_handler(cl_object handler, cl_object continuation)
{
  cl_env_ptr the_env = ecl_process_env();
  cl_object symbol = ECL_SIGNAL_HANDLERS;
  cl_object cluster = ECL_SYM_VAL(the_env, symbol);
  cl_object result;
  /* Binding a handler conses a new list, but at this stage we don't assume the
     the garbage collector to work! Luckily the extent of the binding is dynamic
     and we can allocate cons on the stack. */
  struct ecl_cons hnds = { .t = t_list, .car = handler, .cdr = cluster };
  ecl_bds_bind(the_env, symbol, ecl_cast_ptr(cl_object,&hnds));
  result = _ecl_funcall1(continuation);
  ecl_bds_unwind1(the_env);
  return result;
}

/* -- Exceptions ------------------------------------------------------------ **

Conditions in Common Lisp are instances of STANDARD-CLASS. While eventually I'd
like to include classes to the early environment, that would be too much work at
one go. This is also the reason why ecl_signal accepts all kinds of objects.

In order to signal conditions in the early environment we use a trick: we pass
to ecl_signal objects of type ecl_exception that are recognized by a Common Lisp
handler, and that handler resignals proper conditions. Exceptions are allocated
on the stack and capturing them is prohibited.

ecl_raise is very similar to ecl_signal with an exception that it does not pop
the current handler from the stack. This is to ensure, that the condition
handler is invoked despite being "above" the exception handler on the stack. To
avoid infinite recursion it is prohibited to resignal the exception itself.

** ---------------------------------------------------------------------------*/

cl_object
ecl_raise(ecl_ex_type type, bool returns, cl_object arg1, cl_object arg2)
{
  const cl_env_ptr the_env = ecl_process_env();
  struct ecl_exception ex =
    { .t = t_exception, .ex_type = type, .arg1 = arg1, .arg2 = arg2 };
  cl_object symbol, cluster, handler;
  cl_object exception = ecl_cast_ptr(cl_object,&ex);
  symbol = ECL_SIGNAL_HANDLERS;
  cluster = ECL_SYM_VAL(the_env, symbol);
  ecl_bds_bind(the_env, symbol, cluster);
  while(!Null(cluster)) {
    handler = ECL_CONS_CAR(cluster);
    cluster = ECL_CONS_CDR(cluster);
    _ecl_funcall2(handler, exception);
  }
  if (!returns)
    _ecl_unexpected_return();
  ecl_bds_unwind1(the_env);
  return ECL_NIL;
}

cl_object
ecl_ferror(ecl_ex_type extype, cl_object type, cl_object args)
{
  ecl_raise(extype, 0, type, args);
  _ecl_unexpected_return();
}

cl_object
ecl_cerror(ecl_ex_type extype, cl_object type, cl_object args)
{
  ecl_raise(extype, 1, type, args);
  return ECL_NIL;
}

/* -- Fatal errors ---------------------------------------------------------- **

Fatal errors that can't be recovered from and result in the program abortion.

** ---------------------------------------------------------------------------*/

void
ecl_internal_error(const char *s)
{
  int saved_errno = errno;
  fprintf(stderr, "\nInternal or unrecoverable error in:\n%s\n", s);
  if (saved_errno) {
    fprintf(stderr, "  [%d: %s]\n", saved_errno, strerror(saved_errno));
  }
  fflush(stderr);
  _ecl_dump_c_backtrace();
#ifdef SIGIOT
  signal(SIGIOT, SIG_DFL); /* avoid getting into a loop with abort */
#endif
  abort();
}

#ifdef ECL_THREADS
void
ecl_thread_internal_error(const char *s)
{
  int saved_errno = errno;
  fprintf(stderr, "\nInternal thread error in:\n%s\n", s);
  if (saved_errno) {
    fprintf(stderr, "  [%d: %s]\n", saved_errno, strerror(saved_errno));
  }
  _ecl_dump_c_backtrace();
  fprintf(stderr, "\nDid you forget to call `ecl_import_current_thread'?\n"
                  "Exitting thread.\n");
  fflush(stderr);
  ecl_thread_exit();
}
#endif

void
_ecl_unexpected_return()
{
  ecl_internal_error("*** \n"
                     "*** A call to ERROR returned without handling the error.\n"
                     "*** This should have never happened and is usually a signal\n"
                     "*** that the debugger or the universal error handler were\n"
                     "*** improperly coded or altered. Please contact the maintainers\n"
                     "*** \n");
}

void
ecl_miscompilation_error()
{
  ecl_internal_error("*** \n"
                     "*** Encountered a code path that should have never been taken.\n"
                     "*** This likely indicates a bug in the ECL compiler. Please contact\n"
                     "*** the maintainers.\n"
                     "*** \n");
}

/* Max number of frames dumped by _ecl_dump_c_backtrace */
#define MAX_BACKTRACE_SIZE 128
/* Max length of symbols printed */
#define MAX_SYMBOL_LENGTH 256

void
_ecl_dump_c_backtrace()
{
#if defined(ECL_UNIX_BACKTRACE) || defined(ECL_WINDOWS_BACKTRACE)
  {
    void **pointers = malloc(sizeof(void*) * MAX_BACKTRACE_SIZE);
# if defined(ECL_UNIX_BACKTRACE)
    int nframes = backtrace(pointers, MAX_BACKTRACE_SIZE);
    char **names = backtrace_symbols(pointers, nframes);
# elif defined(ECL_WINDOWS_BACKTRACE)
    HANDLE process = GetCurrentProcess();
    if (!SymInitialize(process, NULL, TRUE)) {
      return;
    }
    int nframes = CaptureStackBackTrace(0, MAX_BACKTRACE_SIZE, pointers, NULL);
    char buffer[sizeof(SYMBOL_INFO) + MAX_SYMBOL_LENGTH * sizeof(TCHAR)];
    PSYMBOL_INFO pSymbol = (PSYMBOL_INFO)buffer;
    pSymbol->SizeOfStruct = sizeof(SYMBOL_INFO);
    pSymbol->MaxNameLen = MAX_SYMBOL_LENGTH;
# endif
    int i;
    fprintf(stderr, "\n;;; ECL C Backtrace\n");
    for (i = 0; i < nframes; i++) {
# if defined(ECL_UNIX_BACKTRACE)
      fprintf(stderr, ";;; %s\n", names[i]);
# elif defined(ECL_WINDOWS_BACKTRACE)
      DWORD64 displacement;
      if (SymFromAddr(process, (DWORD64) pointers[i], &displacement, pSymbol)) {
        fprintf(stderr, ";;; (%s+0x%llx) [0x%p]\n", pSymbol->Name, displacement, pointers[i]);
      } else {
        fprintf(stderr, ";;; (unknown) [0x%p]\n", pointers[i]);
      }
# endif
    }
    fflush(stderr);
    free(pointers);
# if defined(ECL_UNIX_BACKTRACE)
    free(names);
# elif defined(ECL_WINDOWS_BACKTRACE)
    SymCleanup(process);
# endif
  }
#endif  /* defined(ECL_UNIX_BACKTRACE) || defined(ECL_WINDOWS_BACKTRACE) */
}
