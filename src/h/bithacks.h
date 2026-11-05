/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=2 expandtab: */

/* This is a header implementation of utilities that operate on bits in C that
   are reused across the codebase. We can incorporate C compiler builtins, but
   there must always be a fallback portable C99 implementation in place. This
   file should contain only things that we use. This file should be independent
   of ECL abstractions (no cl_fixnum etc, no cl_object etc).

   Here are listed and explained many portable C bithacks (public domain):
   https://graphics.stanford.edu/%7Eseander/bithacks.html

   GCC extensions: https://gcc.gnu.org/onlinedocs/gcc/Bit-Operation-Builtins.html
   Clang extensions: https://clang.llvm.org/docs/LanguageExtensions.html
*/

#ifndef ECL_BITHACKS_H
#define ECL_BITHACKS_H

#include <stdint.h>

/* This will work also on smaller ints because the widening cast is safehere. */
/* Calling with x=0 has undefined consequences. */
static inline int ecl_ctz(unsigned long long x) {
#if defined(__GNUC__) || defined(__clang__)
  return __builtin_ctzll(x);
#else
  int count = 0;
  while(x && !(x&1)) { count++; x>>=1; }
  return count;
#endif
}

static inline int ecl_clz(unsigned long long x) {
#if defined(__GNUC__) || defined(__clang__)
  return __builtin_clzll(x);
#else
  int count = 0;
  while (x>>=1) count++;
  return count;
#endif
}

static inline uint64_t
ecl_bswap64(uint64_t x) {
#if defined(__GNUC__) || defined(__clang__)
  return __builtin_bswap64(x);
#else
  return ((x & 0x00000000000000ff) << 56)
    |    ((x & 0x000000000000ff00) << 40)
    |    ((x & 0x0000000000ff0000) << 24)
    |    ((x & 0x00000000ff000000) <<  8)
    |    ((x & 0x000000ff00000000) >>  8)
    |    ((x & 0x0000ff0000000000) >> 24)
    |    ((x & 0x00ff000000000000) >> 40)
    |    ((x & 0xff00000000000000) >> 56);
#endif
}

static inline uint32_t
ecl_bswap32(uint32_t x) {
#if defined(__GNUC__) || defined(__clang__)
  return __builtin_bswap32(x);
#else
  return ((x & 0x000000ff) << 24)
    |    ((x & 0x0000ff00) <<  8)
    |    ((x & 0x00ff0000) >>  8)
    |    ((x & 0xff000000) >> 24);
#endif
}

#endif
