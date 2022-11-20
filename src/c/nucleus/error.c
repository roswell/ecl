
/* -- imports ------------------------------------------------------- */

#include <ecl/ecl.h>
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>

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

/* -- implementation------------------------------------------------- */

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
