
/* -- imports ------------------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <ecl/ecl.h>

#if defined(HAVE_BACKTRACE) && defined(HAVE_BACKTRACE_SYMBOLS)
# include <execinfo.h>
# define ECL_UNIX_BACKTRACE
#endif

#if defined(ECL_WINDOWS_BACKTRACE)
# include <windows.h>
# include <DbgHelp.h>
#endif

/* -- implementation------------------------------------------------- */

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
