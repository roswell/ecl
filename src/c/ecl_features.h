/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * features.h - names of features compiled into ECL
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

ecl_def_string_array(feature_names,static,const) = {
  ecl_def_string_array_elt("ECL"),
  ecl_def_string_array_elt("COMMON"),
  ecl_def_string_array_elt(ECL_ARCHITECTURE),
  ecl_def_string_array_elt("FFI"),
  ecl_def_string_array_elt("PREFIXED-API"),
  ecl_def_string_array_elt("CDR-14"),
  ecl_def_string_array_elt("PACKAGE-LOCAL-NICKNAMES"),
#ifdef ECL_IEEE_FP
  ecl_def_string_array_elt("IEEE-FLOATING-POINT"),
#endif
  ecl_def_string_array_elt("COMMON-LISP"),
  ecl_def_string_array_elt("ANSI-CL"),
#if defined(GBC_BOEHM)
  ecl_def_string_array_elt("BOEHM-GC"),
#endif
#ifdef ECL_THREADS
  ecl_def_string_array_elt("THREADS"),
#endif
  ecl_def_string_array_elt("CLOS"),
#ifdef ENABLE_DLOPEN
  ecl_def_string_array_elt("DLOPEN"),
#endif
  ecl_def_string_array_elt("ECL-PDE"),
#if defined(unix) || defined(netbsd) || defined(openbsd) || defined(linux) || defined(darwin) || \
  defined(freebsd) || defined(dragonfly) || defined(kfreebsd) || defined(gnu) || defined(nsk) || defined(aix)
  ecl_def_string_array_elt("UNIX"),
#endif
#ifdef BSD
  ecl_def_string_array_elt("BSD"),
#endif
#ifdef SYSV
  ecl_def_string_array_elt("SYSTEM-V"),
#endif
#if defined(__MINGW32__)
  ecl_def_string_array_elt("MINGW32"),
  ecl_def_string_array_elt("WIN32"),
#endif
#if defined(__WIN64__)
  ecl_def_string_array_elt("WIN64"),
#endif
#ifdef _MSC_VER
  ecl_def_string_array_elt("MSVC"),
#endif
#if defined(ECL_MS_WINDOWS_HOST)
  ecl_def_string_array_elt("WINDOWS"),
#endif
#ifdef ECL_CMU_FORMAT
  ecl_def_string_array_elt("CMU-FORMAT"),
#endif
#ifdef ECL_CLOS_STREAMS
  ecl_def_string_array_elt("CLOS-STREAMS"),
#endif
#if defined(HAVE_LIBFFI)
  ecl_def_string_array_elt("DFFI"),
#endif
#ifdef ECL_UNICODE
  ecl_def_string_array_elt("UNICODE"),
#endif
#ifdef ECL_LONG_FLOAT
  ecl_def_string_array_elt("LONG-FLOAT"),
#endif
#ifdef ECL_RELATIVE_PACKAGE_NAMES
  ecl_def_string_array_elt("RELATIVE-PACKAGE-NAMES"),
#endif
#ifdef ecl_uint16_t
  ecl_def_string_array_elt("UINT16-T"),
#endif
#ifdef ecl_uint32_t
  ecl_def_string_array_elt("UINT32-T"),
#endif
#ifdef ecl_uint64_t
  ecl_def_string_array_elt("UINT64-T"),
#endif
#ifdef ecl_long_long_t
  ecl_def_string_array_elt("LONG-LONG"),
#endif
#ifdef ECL_EXTERNALIZABLE
  ecl_def_string_array_elt("EXTERNALIZABLE"),
#endif
#ifdef ECL_CXX_CORE
  ecl_def_string_array_elt("CXX-CORE"),
#endif
#ifdef ECL_SSE2
  ecl_def_string_array_elt("SSE2"),
#endif
#ifdef ECL_SEMAPHORES
  ecl_def_string_array_elt("SEMAPHORES"),
#endif
#ifdef ECL_RWLOCK
  ecl_def_string_array_elt("ECL-READ-WRITE-LOCK"),
#endif
#ifdef WORDS_BIGENDIAN
  ecl_def_string_array_elt("BIG-ENDIAN"),
#else
  ecl_def_string_array_elt("LITTLE-ENDIAN"),
#endif
#ifdef ECL_WEAK_HASH
  ecl_def_string_array_elt("ECL-WEAK-HASH"),
#endif
  ecl_def_string_array_elt(0)
};

