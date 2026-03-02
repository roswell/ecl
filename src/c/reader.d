/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * read.d - reader
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 * Copyright (c) 2016 Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#include <ecl/number.h>
#include <assert.h>  /* for assert() */
#include <stdio.h>
#include <limits.h>
#include <float.h>
#include <string.h>
#include <stdlib.h>
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>
#include <ecl/bytecodes.h>

#define read_suppress (ecl_symbol_value(@'*read-suppress*') != ECL_NIL)

static cl_object
ecl_make_token()
{
  cl_object o = ecl_alloc_object(t_token);
  o->token.escaped = 0;
  o->token.string = si_get_buffer_string();
  /* To keep looping code simple, we insert an empty interval at the end. */
  o->token.escape = ecl_make_stack(2);
  return o;
}

/* FIXME pools should be resizeable stacks. */
cl_object
ecl_get_reader_token(void)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object pool = the_env->token_pool;
  cl_object aux;
  if (pool != ECL_NIL) {
    aux = CAR(pool);
    the_env->token_pool = CDR(pool);
    return aux;
  }
  return ecl_make_token();
}

void
ecl_put_reader_token(cl_object token)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object pool = the_env->token_pool;
  ecl_wipe_stack(token->token.escape);
  TOKEN_STRING_FILLP(token->token.string) = 0;
  the_env->token_pool = CONS(token, pool);
}

/*
 * This routine inverts the case of the characters in the buffer which were not
 * escaped. ESCAPE_INTERVALS is a vector of intevals of characters that were
 * escaped, as in ({(low-limit . high-limit)}*).
 */
static void
invert_buffer_case(cl_object o, int sign)
{
  cl_object string = o->token.string;
  cl_object escape = o->token.escape;
  cl_fixnum str_i = 0;
  int c;
  /* see whether we have a bug with reversed beginning/end */
  loop_across_eints(low_limit, high_limit, escape) {
    for(; str_i<low_limit; str_i++) {
      c = TOKEN_STRING_CHAR(string, str_i);
      if (ecl_upper_case_p(c) && (sign < 0)) {
        c = ecl_char_downcase(c);
      } else if (ecl_lower_case_p(c) && (sign > 0)) {
        c = ecl_char_upcase(c);
      }
      TOKEN_STRING_CHAR_SET(string, str_i, c);
    }
    str_i=high_limit;
  } end_loop_across_eints();
}

/*
  Returns OBJNULL if no dispatch function is defined and signal_error is false.
 */
static cl_object
dispatch_macro_character(cl_object table, cl_object in, int c, bool signal_error)
{
  cl_object arg;
  int d;
  c = ecl_read_char_noeof(in);
  d = ecl_digitp(c, 10);
  if (d >= 0) {
    cl_fixnum i = 0;
    do {
      i = 10*i + d;
      c = ecl_read_char_noeof(in);
      d = ecl_digitp(c, 10);
    } while (d >= 0);
    arg = ecl_make_fixnum(i);
  } else {
    arg = ECL_NIL;
  }
  {
    cl_object dc = ECL_CODE_CHAR(c);
    cl_object fun = ecl_gethash_safe(dc, table, ECL_NIL);
    unlikely_if (Null(fun)) {
      if (signal_error) {
        FEreader_error("No dispatch function defined "
                       "for character ~S",
                       in, 1, dc);
      } else {
        return OBJNULL;
      }
    }
    return _ecl_funcall4(fun, in, dc, arg);
  }
}

cl_object
ecl_dispatch_reader_fun(cl_object in, cl_object dc)
{
  cl_object readtable = ecl_current_readtable();
  cl_object dispatch_table;
  int c = ecl_char_code(dc);
  ecl_readtable_get(readtable, c, &dispatch_table);
  unlikely_if (!ECL_HASH_TABLE_P(dispatch_table))
    FEreader_error("~C is not a dispatching macro character", in, 1, dc);
  return dispatch_macro_character(dispatch_table, in, c, TRUE);
}

cl_object
ecl_read_token(cl_object in, bool escape_first_p)
{
  int c;
  cl_object token, string, escape;
  cl_index length;
  enum ecl_chattrib a;
  cl_env_ptr the_env = ecl_process_env();
  cl_object rtbl = ecl_current_readtable();
  enum ecl_readtable_case read_case = rtbl->readtable.read_case;
  cl_fixnum upcase; /* # uppercase characters - # downcase characters */
  cl_fixnum count; /* number of unescaped characters */
  bool suppress = read_suppress;

  upcase = count = length = 0;

  token = ecl_get_reader_token();
  string = token->token.string;
  escape = token->token.escape;

  if (escape_first_p) {
    c = 0;
    a = cat_single_escape;
  } else {
    c = ecl_read_char_noeof(in);
    a = ecl_readtable_get(rtbl, c, NULL);
  }

  for (;;) {
    if (a == cat_single_escape) {
      c = ecl_read_char_noeof(in);
      a = cat_constituent;
      ecl_stack_push(escape, ecl_make_fixnum(length-1));
      ecl_stack_push(escape, ecl_make_fixnum(length));
      ecl_string_push_extend(string, c);
      length++;
      goto NEXT;
    }
    if (a == cat_multiple_escape) {
      cl_index begin = length;
      for (;;) {
        c = ecl_read_char_noeof(in);
        a = ecl_readtable_get(rtbl, c, NULL);
        if (a == cat_single_escape) {
          c = ecl_read_char_noeof(in);
          a = cat_constituent;
        } else if (a == cat_multiple_escape)
          break;
        ecl_string_push_extend(string, c);
        length++;
      }
      ecl_stack_push(escape, ecl_make_fixnum(begin));
      ecl_stack_push(escape, ecl_make_fixnum(length-1));
      goto NEXT;
    }
    if (a == cat_whitespace || a == cat_terminating) {
      ecl_unread_char(c, in);
      break;
    }
    unlikely_if (ecl_invalid_character_p(c) && !suppress) {
      FEreader_error("Found invalid character ~:C", in, 1, ECL_CODE_CHAR(c));
    }
    if (read_case != ecl_case_preserve) {
      if (ecl_upper_case_p(c)) {
        upcase++;
        count++;
        if (read_case == ecl_case_downcase)
          c = ecl_char_downcase(c);
      } else if (ecl_lower_case_p(c)) {
        upcase--;
        count++;
        if (read_case == ecl_case_upcase)
          c = ecl_char_upcase(c);
      }
    }
    ecl_string_push_extend(string, c);
    length++;
  NEXT:
    c = ecl_read_char(in);
    if (c == EOF)
      break;
    a = ecl_readtable_get(rtbl, c, NULL);
  }
  ecl_stack_push(escape, ecl_make_fixnum(length));
  ecl_stack_push(escape, ecl_make_fixnum(length));

  /*TOKEN_STRING_CHAR_SET(string,length,'\0');*/
  /* If the readtable case was :INVERT and all non-escaped characters
   * had the same case, we revert their case. */
  if (read_case == ecl_case_invert && count != 0) {
    if (upcase == count) {
      invert_buffer_case(token, -1);
    } else if (upcase == -count) {
      invert_buffer_case(token, +1);
    }
  }

  ecl_return1(the_env, token);
}

cl_object
ecl_read_object_with_delimiter(cl_object in, int delimiter, int flags)
{
  cl_object x, token;
  int c;
  enum ecl_chattrib a;
  cl_env_ptr the_env = ecl_process_env();
  cl_object rtbl = ecl_current_readtable();
  bool suppress = read_suppress;
 BEGIN:
  do {
    c = ecl_read_char(in);
    if (c == delimiter) {
      the_env->nvalues = 0;
      return OBJNULL;
    }
    if (c == EOF)
      FEend_of_file(in);
    a = ecl_readtable_get(rtbl, c, &x);
  } while (a == cat_whitespace);
  if ((a == cat_terminating || a == cat_non_terminating)) {
    cl_object o;
    if (ECL_HASH_TABLE_P(x)) {
      if (suppress) {
        o = dispatch_macro_character(x, in, c, FALSE);
        if (o == OBJNULL)
          goto BEGIN;
      } else {
        o = dispatch_macro_character(x, in, c, TRUE);
      }
    } else {
      o = _ecl_funcall3(x, in, ECL_CODE_CHAR(c));
    }
    if (the_env->nvalues == 0) {
      if (flags == ECL_READ_RETURN_IGNORABLE)
        return ECL_NIL;
      goto BEGIN;
    }
    unlikely_if (the_env->nvalues > 1) {
      FEerror("The readmacro ~S returned ~D values.",
              2, x, ecl_make_fixnum(the_env->nvalues));
    }
    return o;
  }
  ecl_unread_char(c, in);
  token = ecl_read_token(in, 0);
  if (suppress) {
    x = ECL_NIL;
  } else {
    x = ecl_parse_token(token, in, flags);
  }
  ecl_put_reader_token(token);
  return x;
}
