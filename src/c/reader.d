/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * read.d - reader
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
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

/*
 * This routine inverts the case of the characters in the buffer which were not
 * escaped. ESCAPE_INTERVALS is a vector of intevals of characters that were
 * escaped, as in ({(low-limit . high-limit)}*).
 */
static void
invert_buffer_case(cl_object x, cl_object escape_intervals, int sign)
{
  cl_fixnum high_limit, low_limit;
  cl_fixnum str_i = 0;
  int c;
  /* see whether we have a bug with reversed beginning/end */
  loop_across_stack_fifo(eint, escape_intervals) {
    low_limit = ecl_fixnum(CAR(eint));
    high_limit = ecl_fixnum(CDR(eint));
    for(; str_i<low_limit; str_i++) {
      c = TOKEN_STRING_CHAR(x,str_i);
      if (ecl_upper_case_p(c) && (sign < 0)) {
        c = ecl_char_downcase(c);
      } else if (ecl_lower_case_p(c) && (sign > 0)) {
        c = ecl_char_upcase(c);
      }
      TOKEN_STRING_CHAR_SET(x,str_i,c);
    }
    str_i=high_limit;
  } end_loop_across_stack();
  for (; str_i < ecl_length(x); str_i++) {
    c = TOKEN_STRING_CHAR(x,str_i);
    if (ecl_upper_case_p(c) && (sign < 0)) {
      c = ecl_char_downcase(c);
    } else if (ecl_lower_case_p(c) && (sign > 0)) {
      c = ecl_char_upcase(c);
    }
    TOKEN_STRING_CHAR_SET(x,str_i,c);
  }
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
ecl_read_only_token(cl_object in, bool escape_first_p)
{
  int c;
  cl_object token;
  cl_index length;
  enum ecl_chattrib a;
  cl_env_ptr the_env = ecl_process_env();
  cl_object rtbl = ecl_current_readtable();
  enum ecl_readtable_case read_case = rtbl->readtable.read_case;
  cl_object escape_intervals; /* intervals of escaped characters */
  cl_fixnum upcase; /* # uppercase characters - # downcase characters */
  cl_fixnum count; /* number of unescaped characters */
  bool suppress = read_suppress;

  upcase = count = length = 0;
  token = si_get_buffer_string();
  escape_intervals = ecl_make_stack(0);

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
      ecl_stack_push(escape_intervals, CONS(ecl_make_fixnum(length-1),
                                            ecl_make_fixnum(length)));
      ecl_string_push_extend(token, c);
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
        ecl_string_push_extend(token, c);
        length++;
      }
      ecl_stack_push(escape_intervals, CONS(ecl_make_fixnum(begin),
                                            ecl_make_fixnum(length-1)));
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
    ecl_string_push_extend(token, c);
    length++;
  NEXT:
    c = ecl_read_char(in);
    if (c == EOF)
      break;
    a = ecl_readtable_get(rtbl, c, NULL);
  }

  /*TOKEN_STRING_CHAR_SET(token,length,'\0');*/
  /* If the readtable case was :INVERT and all non-escaped characters
   * had the same case, we revert their case. */
  if (read_case == ecl_case_invert && count != 0) {
    if (upcase == count) {
      invert_buffer_case(token, escape_intervals, -1);
    } else if (upcase == -count) {
      invert_buffer_case(token, escape_intervals, +1);
    }
  }

  ecl_return2(the_env, token, escape_intervals);
}

cl_object
ecl_read_token(cl_object in, int flags)
{
  cl_fixnum high_limit, low_limit, str_i;
  cl_object x, token;
  int c, base;
  cl_object package, package_name, symbol_name;
  cl_index length, i, sym_start, pack_end;
  int colon, intern_flag;
  bool external_symbol;
  cl_env_ptr the_env = ecl_process_env();
  cl_object escape_intervals; /* intervals of escaped characters */
  bool suppress = read_suppress;
  token = ecl_read_only_token(in, 0);
  escape_intervals = ecl_nth_value(the_env,1);

  package = package_name = ECL_NIL;
  str_i = sym_start = pack_end = colon = 0;
  length = ecl_length(token);
  external_symbol = 0;

  if (suppress) {
    x = ECL_NIL;
    goto OUTPUT;
  }

  loop_across_stack_fifo(eint, escape_intervals) {
    low_limit = ecl_fixnum(CAR(eint));
    high_limit = ecl_fixnum(CDR(eint));
    for(; str_i<low_limit; str_i++) {
      c = ecl_char(token, str_i);
      if (c == ':') {
        if(!Null(package))
          FEreader_error("Unexpected colon character.", in, 0);
        if (colon > 1) FEreader_error("Too many colons.", in, 0);
        if (colon < 1) pack_end = str_i;
        colon++;
        sym_start = str_i+1;
        continue;
      } else if (colon) {
        external_symbol = (colon == 1);
        if (pack_end == 0) {
          package = cl_core.keyword_package;
        } else {
          package_name = ecl_subseq(token, 0, pack_end);
          package = ecl_find_package_nolock(package_name);
        }
        if (Null(package)) {
          /* When loading binary files, we sometimes must create
             symbols whose package has not yet been maked. We
             allow it, but later on in ecl_init_module we make sure that
             all referenced packages have been properly built.
          */
          unlikely_if (Null(the_env->packages_to_be_created_p)) {
            ecl_free_stack(escape_intervals);
            si_put_buffer_string(token);
            FEerror("There is no package with the name ~A.", 1, package_name);
          }
          package = _ecl_package_to_be_created(the_env, package_name);
        }
      }
    }
    str_i=high_limit;
  } end_loop_across_stack();
  for(; str_i<length; str_i++) {
    c = ecl_char(token, str_i);
    if (c == ':') {
      if(!Null(package))
        FEreader_error("Unexpected colon character.", in, 0);
      if (colon > 1) FEreader_error("Too many colons.", in, 0);
      if (colon < 1) pack_end = str_i;
      colon++;
      sym_start = str_i+1;
      continue;
    } else if (colon) {
      external_symbol = (colon == 1);
      if (pack_end == 0) {
        package = cl_core.keyword_package;
        external_symbol = 0;
      } else {
        package_name = ecl_subseq(token, 0, pack_end);
        package = ecl_find_package_nolock(package_name);
      }
      if (Null(package)) {
        /* When loading binary files, we sometimes must create
           symbols whose package has not yet been maked. We
           allow it, but later on in ecl_init_module we make sure that
           all referenced packages have been properly built.
        */
        unlikely_if (Null(the_env->packages_to_be_created_p)) {
          ecl_free_stack(escape_intervals);
          si_put_buffer_string(token);
          FEerror("There is no package with the name ~A.", 1, package_name);
        }
        package = _ecl_package_to_be_created(the_env, package_name);
      }
    }
  }

  /* If there are some escaped characters, it must be a symbol */
  if (package != ECL_NIL || ecl_length(escape_intervals) != 0 || length == 0)
    goto SYMBOL;

  /* The case in which the buffer is full of dots has to be especial cased */
  if (length == 1 && TOKEN_STRING_CHAR_CMP(token,0,'.')) {
    if (flags == ECL_READ_LIST_DOT) {
      x = @'si::.';
      goto OUTPUT;
    } else {
      ecl_free_stack(escape_intervals);
      si_put_buffer_string(token);
      FEreader_error("Dots appeared illegally.", in, 0);
    }
  } else {
    int i;
    for (i = 0;  i < length;  i++) {
      if (!TOKEN_STRING_CHAR_CMP(token,i,'.'))
        goto MAYBE_NUMBER;
    }
    ecl_free_stack(escape_intervals);
    si_put_buffer_string(token);
    FEreader_error("Dots appeared illegally.", in, 0);
  }

 MAYBE_NUMBER:
  /* Here we try to parse a number from the content of the buffer */
  base = ecl_current_read_base();
  if ((base <= 10) && ecl_alpha_char_p(TOKEN_STRING_CHAR(token,0)))
    goto SYMBOL;
  x = ecl_parse_number(token, 0, TOKEN_STRING_FILLP(token), &i, base);
  unlikely_if (x == ECL_NIL) {
    ecl_free_stack(escape_intervals);
    si_put_buffer_string(token);
    FEreader_error("Syntax error when reading number.~%Offending string: ~S.",
                   in, 1, token);
  }
  if (x != OBJNULL && length == i)
    goto OUTPUT;
 SYMBOL:
  symbol_name = ecl_subseq(token, sym_start, length);
  if (external_symbol) {
    x = ecl_find_symbol(symbol_name, package, &intern_flag);
    unlikely_if (intern_flag != ECL_EXTERNAL) {
      ecl_free_stack(escape_intervals);
      si_put_buffer_string(token);
      FEreader_error("Cannot find the external symbol ~A in ~S.", in,
                     2, symbol_name, package);
    }
  } else {
    if (package == ECL_NIL) {
      package = ecl_current_package();
    }
    /* INV: cl_make_symbol() copies the string */
    x = ecl_intern(symbol_name, package, &intern_flag);
  }
 OUTPUT:
  ecl_free_stack(escape_intervals);
  si_put_buffer_string(token);
  ecl_return1(the_env, x);
}

cl_object
ecl_read_object_with_delimiter(cl_object in, int delimiter, int flags)
{
  cl_object x;
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
  return ecl_read_token(in, flags);
}
