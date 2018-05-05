/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * backtrace.d - C backtraces
 *
 * Copyright (c) 2010 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <ecl/ecl.h>

#if defined(HAVE_BACKTRACE) || defined(HAVE_BACKTRACE_SYMBOLS)
# include <execinfo.h>
#endif

#define MAX_BACKTRACE_SIZE 128

void
_ecl_dump_c_backtrace()
{
#ifdef HAVE_BACKTRACE_SYMBOLS
  {
    void **pointers = malloc(sizeof(void*) * MAX_BACKTRACE_SIZE);
    int nframes = backtrace(pointers, MAX_BACKTRACE_SIZE);
    char **names = backtrace_symbols(pointers, nframes);
    int i;
    fprintf(stderr, "\n;;; ECL C Backtrace\n");
    for (i = 0; i < nframes; i++) {
      fprintf(stderr, ";;; %s\n", names[i]);
    }
    fflush(stderr);
    free(names);
    free(pointers);
  }
#endif
}

cl_object
si_dump_c_backtrace(cl_object size)
{
  cl_env_ptr the_env = ecl_process_env();
#ifdef HAVE_BACKTRACE_SYMBOLS
  {
    cl_index nsize = ecl_to_unsigned_integer(size);
    void **pointers = malloc(sizeof(void*) * nsize);
    int nframes = backtrace(pointers, nsize);
    char **names = backtrace_symbols(pointers, nframes);
    int i;
    cl_format(2, ECL_T, make_constant_base_string("~&C Backtrace:~%"));
    for (i = 0; i < nframes; i++) {
      cl_format(3, ECL_T, make_constant_base_string("  > ~a~%"),
                make_constant_base_string(names[i]));
    }
    free(names);
    free(pointers);
  }
  ecl_return1(the_env, ECL_T);
#else
  ecl_return1(the_env, ECL_NIL);
#endif
}
