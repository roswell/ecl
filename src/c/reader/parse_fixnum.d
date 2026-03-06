/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 * Copyright (c) 2026 Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/number.h>
#include <ecl/internal.h>

#define basep(d) (d <= 36)

static ecl_character
_ecl_char(cl_object object, cl_index index)
{
  switch(ecl_t_of(object)) {
#ifdef ECL_UNICODE
  case t_string:
    return object->string.self[index];
#endif
  case t_base_string:
    return object->base_string.self[index];
  default:
    ecl_internal_error("ecl_parse_fixnum: source is not a string.");
  }
}

static int
_ecl_digitp(ecl_character i, int r)
{
  if (('0' <= i) && (i <= '9') && (i < '0' + r))
    return i - '0';
  if (('A' <= i) && (10 < r) && (i < 'A' + (r - 10)))
    return i - 'A' + 10;
  if (('a' <= i) && (10 < r) && (i < 'a' + (r - 10)))
    return i - 'a' + 10;
/* #ifdef ECL_UNICODE */
/*   if (i > 255) { */
/*     int number = ucd_decimal_digit(i); */
/*     if (number < r) */
/*       return number; */
/*   } */
/* #endif */
  return -1;
}

/* This is like ecl_parse_integer (including the API), but returns OBJNULL also
   when the result does not fit in a fixnum. */
cl_object
ecl_parse_fixnum(cl_object str, cl_index start, cl_index end,
                 cl_index *ep, unsigned int radix)
{
  int sign, d;
  cl_fixnum aux, integer_part;
  cl_object output;
  cl_index i, c;

  if (start >= end || !basep(radix)) {
    *ep = start;
    return OBJNULL;
  }
  sign = 1;
  c = _ecl_char(str, start);
  if (c == '+') {
    start++;
  } else if (c == '-') {
    sign = -1;
    start++;
  }
  integer_part = 0;
  for (i = start; i < end; i++) {
    c = _ecl_char(str, i);
    d = _ecl_digitp(c, radix);
    if (d < 0) {
      break;
    }
    aux = integer_part;
    integer_part *= radix;
    if (integer_part/radix != aux) {
      *ep = i-1;
      return OBJNULL;
    }
    integer_part += d;
    if (integer_part > MOST_POSITIVE_FIXNUM) {
      *ep = i-1;
      return OBJNULL;;
    }
  }
  if (sign < 0) {
    integer_part = -integer_part;
  }
  *ep = i;
  return (i == start)? OBJNULL : ecl_make_fixnum(integer_part);
}
