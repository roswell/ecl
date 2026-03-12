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

int
ecl_readtable_get(cl_object readtable, int c, cl_object *macro, cl_object *table)
{
  cl_object m, t;
  enum ecl_chattrib cat;
#ifdef ECL_UNICODE
  if (c >= RTABSIZE) {
    cl_object hash = readtable->readtable.hash;
    cat = cat_constituent;
    m = ECL_NIL;
    if (!Null(hash)) {
      cl_object pair = ecl_gethash_safe(ECL_CODE_CHAR(c), hash, ECL_NIL);
      if (!Null(pair)) {
        cat = ecl_fixnum(ECL_CONS_CAR(pair));
        pair = ECL_CONS_CDR(pair);
        m = ECL_CONS_CAR(pair);
        pair = ECL_CONS_CDR(pair);
        t = ECL_CONS_CAR(pair);
      }
    }
  } else
#endif
    {
      m = readtable->readtable.table[c].macro;
      t = readtable->readtable.table[c].table;
      cat = readtable->readtable.table[c].syntax_type;
    }
  if (macro) *macro = m;
  if (table) *table = t;
  return cat;
}

void
ecl_readtable_set(cl_object readtable, int c, enum ecl_chattrib cat,
                  cl_object macro, cl_object table)
{
#ifdef ECL_UNICODE
  if (c >= RTABSIZE) {
    cl_object hash = readtable->readtable.hash;
    if (Null(hash)) {
      hash = ecl_make_hash_table(@'eql', ecl_make_fixnum(128),
                                 ecl_ct_default_rehash_size,
                                 ecl_ct_default_rehash_threshold);
      readtable->readtable.hash = hash;
    }
    _ecl_sethash(ECL_CODE_CHAR(c), hash,
                 CONS(ecl_make_fixnum(cat),
                      CONS(macro,
                           CONS(table, ECL_NIL))));
  } else
#endif
    {
      readtable->readtable.table[c].macro = macro;
      readtable->readtable.table[c].table = table;
      readtable->readtable.table[c].syntax_type = cat;
    }
}

/* FIXME unicode defines a range of "safe" characters, so that there are no
   misleading pseudo-spaces in symbols and such. Investigate that. */
bool
ecl_invalid_character_p(int c)
{
  return (c <= 32) || (c == 127);
}

/* -- buffer ---------------------------------------------------------------- */
cl_object
si_get_buffer_string()
{
  const cl_env_ptr env = ecl_process_env();
  cl_object pool = env->string_pool;
  cl_object output;
  if (pool == ECL_NIL) {
#ifdef ECL_UNICODE
    output = ecl_alloc_adjustable_extended_string(ECL_BUFFER_STRING_SIZE);
#else
    output = ecl_alloc_adjustable_base_string(ECL_BUFFER_STRING_SIZE);
#endif
  } else {
    output = CAR(pool);
    env->string_pool = CDR(pool);
  }
  TOKEN_STRING_FILLP(output) = 0;
  @(return output);
}

/* FIXME pools should be resizeable stacks. */
cl_object
si_put_buffer_string(cl_object string)
{
  if (string != ECL_NIL) {
    const cl_env_ptr env = ecl_process_env();
    cl_object pool = env->string_pool;
    cl_index l = 0;
    if (pool != ECL_NIL) {
      /* We store the size of the pool in the string index */
      l = TOKEN_STRING_FILLP(ECL_CONS_CAR(pool));
    }
    if (l < ECL_MAX_STRING_POOL_SIZE) {
      TOKEN_STRING_FILLP(string) = l+1;
      env->string_pool = CONS(string, pool);
    }
  }
  @(return);
}

/* INV the buffer is never simultenaously used nor does it have displacements,
   so we can open-code PUSH-EXTEND and eagerly deallocate the underlying array.
   -- jd 2026-03-08 */
static ecl_character
_ecl_string_push_extend(cl_object s, ecl_character c)
{
  cl_index dim, fillp;
  switch(ecl_t_of(s)) {
#ifdef ECL_UNICODE
  case t_string:
    fillp = s->string.fillp;
    dim = s->string.dim;
    if (fillp >= dim) {
      cl_index new_size = dim + dim/2 + 1;
      cl_index old_bytes = sizeof(ecl_character) * dim;
      cl_index new_bytes = sizeof(ecl_character) * new_size;
      ecl_character *old_self = s->string.self;
      ecl_character *new_self = (ecl_character *)ecl_alloc_atomic(new_bytes);
      ecl_copy(new_self, old_self, old_bytes);
      s->string.self = new_self;
      s->string.dim = new_size;
      ecl_dealloc(old_self);
    }
    s->string.fillp++;
    return s->string.self[fillp] = c;
#endif
  case t_base_string:
    fillp = s->base_string.fillp;
    dim = s->base_string.dim;
    if (fillp >= dim) {
      cl_index new_size = dim + dim/2 + 1;
      cl_index old_bytes = sizeof(ecl_base_char) * dim;
      cl_index new_bytes = sizeof(ecl_base_char) * new_size;
      ecl_base_char *old_self = s->base_string.self;
      ecl_base_char *new_self = (ecl_base_char *)ecl_alloc_atomic(new_bytes+1);
      new_self[new_bytes] = 0;
      ecl_copy(new_self, old_self, old_bytes);
      s->base_string.self = new_self;
      s->base_string.dim = new_size;
      ecl_dealloc(old_self);
    }
    s->base_string.fillp++;
    return s->base_string.self[fillp] = c;
  default:
    ecl_internal_error("BUFFER-STRING is not a string.");
  }
}

/* -- tokens ---------------------------------------------------------------- */

static cl_object
ecl_make_token()
{
  cl_object o = ecl_alloc_object(t_token);
  o->token.escaped = 0;
  o->token.string = si_get_buffer_string();
  o->token.escape = ecl_make_stack(0);
  return o;
}

cl_object
si_token_string(cl_object token)
{
  cl_env_ptr the_env = ecl_process_env();
  cl_object object = token->token.string;
  ecl_return1(the_env, object);
}

cl_object
si_token_escape(cl_object token)
{
  cl_env_ptr the_env = ecl_process_env();
  cl_object object = token->token.escape;
  ecl_return1(the_env, object);
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
  TOKEN_STRING_FILLP(token->token.string) = 0;
  TOKEN_ESCAPE_FILLP(token->token.escape) = 0;
  token->token.escaped = 0;
  the_env->token_pool = CONS(token, pool);
}

/*
 * This routine inverts the case of the characters in the buffer which were not
 * escaped.
 */
static void
invert_buffer_case(cl_object o, int sign)
{
  int c;
  loop_across_token(index, limit, string, o) {
    c = TOKEN_STRING_CHAR(string, index);
    if (ecl_upper_case_p(c) && (sign < 0)) {
      c = ecl_char_downcase(c);
    } else if (ecl_lower_case_p(c) && (sign > 0)) {
      c = ecl_char_upcase(c);
    }
    TOKEN_STRING_CHAR_SET(string, index, c);
  } end_loop_across_token();
}

cl_object
ecl_read_token(cl_object rtbl, cl_object in, int flags)
{
  int c;
  cl_object token, string, escape;
  cl_index length;
  enum ecl_chattrib a;
  cl_env_ptr the_env = ecl_process_env();
  enum ecl_readtable_case read_case = rtbl->readtable.read_case;
  cl_fixnum upcase; /* # uppercase characters - # downcase characters */
  cl_fixnum count; /* number of unescaped characters */
  bool suppress = flags & ECL_READ_SUPPRESS;
  bool escape_first_p = flags & ECL_READ_ESCAPE_FIRST;

  upcase = count = length = 0;

  token = ecl_get_reader_token();
  string = token->token.string;
  escape = token->token.escape;

  if (escape_first_p) {
    c = 0;
    a = cat_single_escape;
  } else {
    c = ecl_read_char_noeof(in);
    a = ecl_readtable_get(rtbl, c, NULL, NULL);
  }

  for (;;) {
    if (a == cat_single_escape) {
      c = ecl_read_char_noeof(in);
      a = cat_constituent;
      _ecl_string_push_extend(string, c);
      length++;
      ecl_stack_push(escape, ecl_make_fixnum(length-1));
      ecl_stack_push(escape, ecl_make_fixnum(length));
      goto NEXT;
    }
    if (a == cat_multiple_escape) {
      cl_index begin = length;
      for (;;) {
        c = ecl_read_char_noeof(in);
        a = ecl_readtable_get(rtbl, c, NULL, NULL);
        if (a == cat_single_escape) {
          c = ecl_read_char_noeof(in);
          a = cat_constituent;
        } else if (a == cat_multiple_escape)
          break;
        _ecl_string_push_extend(string, c);
        length++;
      }
      ecl_stack_push(escape, ecl_make_fixnum(begin));
      ecl_stack_push(escape, ecl_make_fixnum(length));
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
    _ecl_string_push_extend(string, c);
    length++;
  NEXT:
    c = ecl_read_char(in);
    if (c == EOF)
      break;
    a = ecl_readtable_get(rtbl, c, NULL, NULL);
  }
  token->token.escaped = (TOKEN_ESCAPE_FILLP(escape) > 0);

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
ecl_read_object_with_delimiter(cl_object rtbl, cl_object in, int delimiter, int flags)
{
  cl_object x, token;
  int c;
  enum ecl_chattrib a;
  cl_env_ptr the_env = ecl_process_env();
  bool suppress = flags & ECL_READ_SUPPRESS;
 BEGIN:
  do {
    c = ecl_read_char(in);
    if (c == delimiter) {
      the_env->nvalues = 0;
      return OBJNULL;
    }
    if (c == EOF)
      FEend_of_file(in);
    a = ecl_readtable_get(rtbl, c, &x, NULL);
  } while (a == cat_whitespace);
  if ((a == cat_terminating || a == cat_non_terminating)) {
    cl_object o = _ecl_funcall3(x, in, ECL_CODE_CHAR(c));
    if (the_env->nvalues == 0) {
      if (flags & ECL_READ_RETURN_IGNORABLE)
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
  token = ecl_read_token(rtbl, in, flags);
  if (suppress) {
    x = ECL_NIL;
  } else {
    x = rtbl->readtable.parse_token(token, in, flags);
  }
  ecl_put_reader_token(token);
  return x;
}
