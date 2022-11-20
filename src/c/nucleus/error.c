
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

/* -- implementation------------------------------------------------- */

void
_ecl_unexpected_return()
{
  ecl_internal_error("*** \n"
                     "*** A call to ERROR returned without handling the error.\n"
                     "*** This should have never happened and is usually a signal\n"
                     "*** that the debugger or the universal error handler were\n"
                     "*** improperly coded or altered. Please contact the maintainers\n"
                     "***\n");
}

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
