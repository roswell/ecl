/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
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
#include <ecl/internal.h>

cl_object
ecl_parse_token(cl_object token, cl_object in, int flags)
{
  cl_fixnum str_i;
  cl_index length, i, sym_start, pack_end;
  int colon, intern_flag, c, base;
  bool external_symbol;
  cl_object package, package_name, symbol_name, string, escape, x;
  cl_env_ptr the_env = ecl_process_env();
  string = token->token.string;
  escape = token->token.escape;
  package = package_name = symbol_name = x = ECL_NIL;
  str_i = sym_start = pack_end = colon = 0;
  length = ecl_length(string);
  external_symbol = 0;

  loop_across_eints(low_limit, high_limit, escape) {
    for(; str_i<low_limit; str_i++) {
      c = ecl_char(string, str_i);
      if (c == ':') {
        if(!Null(package))
          FEreader_error("Unexpected colon character.", in, 0);
        pack_end = str_i;
        /* Eat all ':' and advance the pointer after them. */
        while(c == ':') {
          colon++;
          if (colon > 2)
            FEreader_error("Too many colons.", in, 0);
          str_i++;
          if (str_i == low_limit) {
            break;
          }
          c = ecl_char(string, str_i);
        }
        sym_start = str_i;
        external_symbol = (colon == 1);
        if (pack_end == 0) {
          package = cl_core.keyword_package;
          external_symbol = 0;
        } else {
          package_name = ecl_subseq(string, 0, pack_end);
          package = ecl_find_package_nolock(package_name);
        }
        if (Null(package)) {
          /* When loading binary files, we sometimes must create symbols whose
             package has not yet been maked. We allow it, but later on in
             ecl_init_module we make sure that all referenced packages have been
             properly built. */
          unlikely_if (Null(the_env->packages_to_be_created_p)) {
            ecl_put_reader_token(token);
            FEerror("There is no package with the name ~A.", 1, package_name);
          }
          package = _ecl_package_to_be_created(the_env, package_name);
        }
      }
    }
    str_i=high_limit;
  } end_loop_across_eints();

  /* If there are some escaped characters, it must be a symbol.
     escape_intervals always has an empty interval pair at the end. */
  if (package != ECL_NIL || ecl_length(escape) > 2 || length == 0)
    goto SYMBOL;

  /* The case in which the buffer is full of dots has to be especial cased. */
  if (length == 1 && TOKEN_STRING_CHAR_CMP(string, 0, '.')) {
    if (flags == ECL_READ_LIST_DOT) {
      x = @'si::.';
      goto OUTPUT;
    } else {
      ecl_put_reader_token(token);
      FEreader_error("Dots appeared illegally.", in, 0);
    }
  } else {
    int i;
    for (i = 0;  i < length;  i++) {
      if (!TOKEN_STRING_CHAR_CMP(string,i,'.'))
        goto MAYBE_NUMBER;
    }
    ecl_put_reader_token(token);
    FEreader_error("Dots appeared illegally.", in, 0);
  }

 MAYBE_NUMBER:
  /* Here we try to parse a number from the content of the buffer */
  base = ecl_current_read_base();
  if ((base <= 10) && ecl_alpha_char_p(TOKEN_STRING_CHAR(string, 0)))
    goto SYMBOL;
  x = ecl_parse_number(string, 0, TOKEN_STRING_FILLP(string), &i, base);
  unlikely_if (x == ECL_NIL) {
    ecl_put_reader_token(token);
    FEreader_error("Syntax error when reading number.~%Offending string: ~S.",
                   in, 1, string);
  }
  if (x != OBJNULL && length == i)
    goto OUTPUT;
 SYMBOL:
  symbol_name = ecl_subseq(string, sym_start, length);
  if (external_symbol) {
    x = ecl_find_symbol(symbol_name, package, &intern_flag);
    unlikely_if (intern_flag != ECL_EXTERNAL) {
      ecl_put_reader_token(token);
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
  return x;
}

cl_object
si_parse_token(cl_object token)
{
  cl_env_ptr the_env = ecl_process_env();
  cl_object object = ecl_parse_token(token, ECL_NIL, 42);
  ecl_return1(the_env, object);
}
