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
      /* Ok, by ignoring the following code, here we
       * are doing like SBCL: we simply grow the
       * input buffer and do not care about its
       * size. */
#if 0
      if (TOKEN_STRING_DIM(string) > 32*ECL_BUFFER_STRING_SIZE) {
        /* String has been enlarged. Cut it. */
#ifdef ECL_UNICODE
        string = ecl_alloc_adjustable_extended_string(ECL_BUFFER_STRING_SIZE);
#else
        string = ecl_alloc_adjustable_base_string(ECL_BUFFER_STRING_SIZE);
#endif
      }
#endif
      TOKEN_STRING_FILLP(string) = l+1;
      env->string_pool = CONS(string, pool);
    }
  }
  @(return);
}

static cl_object patch_sharp(const cl_env_ptr env, cl_object x);

cl_object
ecl_read_eval(cl_object in)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object c = ecl_read_object(in);
  unlikely_if (c == OBJNULL)
    FEend_of_file(in);
  if (read_suppress) {
    @(return ECL_NIL);
  }
  unlikely_if (ecl_cmp_symbol_value(the_env, @'*read-eval*') == ECL_NIL)
    FEreader_error("Cannot evaluate the form #.~A", in, 1, c);
  /* FIXME! We should do something here to ensure that the #.
   * only uses the #n# that have been defined */
  c = patch_sharp(the_env, c);
  c = si_eval_with_env(1, c);
  return c;
}

cl_object
ecl_read_object_non_recursive(cl_object in)
{
  cl_object x;
  const cl_env_ptr env = ecl_process_env();

  ecl_bds_bind(env, @'si::*sharp-eq-context*', ECL_NIL);
  ecl_bds_bind(env, @'si::*backq-level*', ecl_make_fixnum(0));
  x = ecl_read_object(in);
  x = patch_sharp(env, x);
  ecl_bds_unwind_n(env, 2);
  return x;
}

/*
  ecl_read_object(in) reads an object from stream in.
  This routine corresponds to COMMON Lisp function READ.
*/
cl_object
ecl_read_object(cl_object in)
{
  return ecl_read_object_with_delimiter(in, EOF, 0);
}

cl_object
si_read_object_or_ignore(cl_object in, cl_object eof)
{
  cl_object x;
  const cl_env_ptr env = ecl_process_env();

  ecl_bds_bind(env, @'si::*sharp-eq-context*', ECL_NIL);
  ecl_bds_bind(env, @'si::*backq-level*', ecl_make_fixnum(0));
  x = ecl_read_object_with_delimiter(in, EOF, ECL_READ_RETURN_IGNORABLE);
  if (x == OBJNULL) {
    env->nvalues = 1;
    x = eof;
  } else if (env->nvalues) {
    x = patch_sharp(env, x);
  }
  ecl_bds_unwind_n(env, 2);
  return x;
}

/*
  ecl_read_constituent(in, 0) reads a sequence of constituent characters from
  stream in and places it in token.  As a help, it returns TRUE or FALSE
  depending on the value of *READ-SUPPRESS*.

  The flag not_first is used by some reader macros to signify, that it is not a
  standalone token. For example #x123 calls ecl_read_constituent(in, 1) for "123".
*/
cl_object
ecl_read_constituent(cl_object in, bool not_first)
{
  int store = !read_suppress;
  cl_object rtbl = ecl_current_readtable();
  cl_object token = si_get_buffer_string();
  do {
    int c = ecl_read_char(in);
    enum ecl_chattrib c_cat;
    if (c == EOF) {
      break;
    }
    c_cat = ecl_readtable_get(rtbl, c, NULL);
    if (c_cat == cat_constituent ||
        ((c_cat == cat_non_terminating) && not_first))
      {
        if (store) {
          ecl_string_push_extend(token, c);
        }
      } else {
      ecl_unread_char(c, in);
      break;
    }
    not_first = 1;
  } while(1);
  return (read_suppress)? ECL_NIL : token;
}

static cl_object
do_patch_sharp(cl_object x, cl_object table)
{
  /* The hash table maintains an association as follows:
   *
   * [1] object -> itself
   *      The object has been processed by patch_sharp, use as it is.
   * [2] object -> nothing
   *      The object has to be processed by do_patch_sharp.
   * [3] (# . object) -> object
   *      This is the value of a #n# statement. The object might
   *      or might not yet be processed by do_patch_sharp().
   */
  /* If x is a list, it is processed iteratively. For this, we store
   * the first and current cons cell */
  cl_object first_cons = OBJNULL;
  cl_object current_cons = OBJNULL;
 AGAIN:
  switch (ecl_t_of(x)) {
  case t_list: {
    cl_object y;
    if (Null(x))
      return (first_cons ? first_cons : x);
    y = ecl_gethash_safe(x, table, table);
    if (y == table) {
      /* case [2] */
      if (first_cons == OBJNULL)
        first_cons = x;
      break;
    } else if (y == x) {
      /* case [1] */
      return (first_cons ? first_cons : x);
    } else {
      /* case [3] */
      x = y;
      if (current_cons != OBJNULL)
        ECL_RPLACD(current_cons, x);
      goto AGAIN;
    }
  }
  case t_vector:
  case t_array:
  case t_complex:
  case t_bclosure:
  case t_bytecodes: {
    cl_object y = ecl_gethash_safe(x, table, table);
    if (y == table) {
      /* case [2] */
      break;
    }
    /* it can only be case [1] */
  }
  default:
    return (first_cons ? first_cons : x);
  }
  /* We eagerly mark the object as processed, to avoid infinite
   * recursion. */
  _ecl_sethash(x, table, x);
  switch (ecl_t_of(x)) {
  case t_list:
    current_cons = x;
    ECL_RPLACA(x, do_patch_sharp(ECL_CONS_CAR(x), table));
    cl_object rest = ECL_CONS_CDR(x);
    if (ecl_t_of(rest) == t_list) {
      x = rest;
      goto AGAIN;
    } else {
      ECL_RPLACD(x, do_patch_sharp(rest, table));
    }
    break;
  case t_vector:
    if (x->vector.elttype == ecl_aet_object) {
      cl_index i;
      for (i = 0;  i < x->vector.fillp;  i++)
        x->vector.self.t[i] =
          do_patch_sharp(x->vector.self.t[i], table);
    }
    break;
  case t_array:
    if (x->vector.elttype == ecl_aet_object) {
      cl_index i, j = x->array.dim;
      for (i = 0;  i < j;  i++)
        x->array.self.t[i] =
          do_patch_sharp(x->array.self.t[i], table);
    }
    break;
  case t_complex: {
    cl_object r = do_patch_sharp(x->gencomplex.real, table);
    cl_object i = do_patch_sharp(x->gencomplex.imag, table);
    if (r != x->gencomplex.real || i != x->gencomplex.imag) {
      cl_object c = ecl_make_complex(r, i);
      x->gencomplex = c->gencomplex;
    }
    break;
  }
  case t_bclosure: {
    x->bclosure.lex = do_patch_sharp(x->bclosure.lex, table);
    x->bclosure.code = do_patch_sharp(x->bclosure.code, table);
    break;
  }
  case t_bytecodes: {
    x->bytecodes.name = do_patch_sharp(x->bytecodes.name, table);
    x->bytecodes.definition = do_patch_sharp(x->bytecodes.definition, table);
    x->bytecodes.data = do_patch_sharp(x->bytecodes.data, table);
    x->bytecodes.flex = do_patch_sharp(x->bytecodes.flex, table);
    break;
  }
  default:;
  }
  return (first_cons ? first_cons : x);
}

static cl_object
patch_sharp(const cl_env_ptr the_env, cl_object x)
{
  cl_object pairs = ECL_SYM_VAL(the_env, @'si::*sharp-eq-context*');
  if (pairs == ECL_NIL) {
    return x;
  } else {
    cl_object table = 
      cl__make_hash_table(@'eq', ecl_make_fixnum(20), /* size */
                          ecl_ct_default_rehash_size,
                          ecl_ct_default_rehash_threshold);
    do {
      cl_object pair = ECL_CONS_CAR(pairs);
      _ecl_sethash(pair, table, ECL_CONS_CDR(pair));
      pairs = ECL_CONS_CDR(pairs);
    } while (pairs != ECL_NIL);
    return do_patch_sharp(x, table);
  }
}

int
ecl_current_read_base(void)
{
  const cl_env_ptr the_env = ecl_process_env();
  /* INV: *READ-BASE* always has a value */
  cl_object x = ECL_SYM_VAL(the_env, @'*read-base*');
  cl_fixnum b;

  unlikely_if (!ECL_FIXNUMP(x) || ((b = ecl_fixnum(x)) < 2) || (b > 36))
    {
      ECL_SETQ(the_env, @'*read-base*', ecl_make_fixnum(10));
      FEerror("The value of *READ-BASE*~&  ~S~%"
              "is not in the range (INTEGER 2 36)", 1, x);
    }
  return b;
}

char
ecl_current_read_default_float_format(void)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object x;

  /* INV: *READ-DEFAULT-FLOAT-FORMAT* is always bound to something */
  x = ECL_SYM_VAL(the_env, @'*read-default-float-format*');
  if (x == @'single-float' || x == @'short-float')
    return 'F';
  if (x == @'double-float')
    return 'D';
  if (x == @'long-float') {
    return 'L';
  }
  ECL_SETQ(the_env, @'*read-default-float-format*', @'single-float');
  FEerror("The value of *READ-DEFAULT-FLOAT-FORMAT*~& ~S~%"
          "is not one of (SINGLE-FLOAT SHORT-FLOAT DOUBLE-FLOAT LONG-FLOAT)",
          1, x);
}

static cl_object
stream_or_default_input(cl_object stream)
{
  const cl_env_ptr the_env = ecl_process_env();
  if (Null(stream))
    return ECL_SYM_VAL(the_env, @'*standard-input*');
  if (stream == ECL_T)
    return ECL_SYM_VAL(the_env, @'*terminal-io*');
  return stream;
}

@(defun read (&optional (strm ECL_NIL) (eof_errorp ECL_T) eof_value recursivep)
  cl_object x;
  @
  strm = stream_or_default_input(strm);
  if (Null(recursivep)) {
    x = ecl_read_object_non_recursive(strm);
  } else {
    x = ecl_read_object(strm);
  }
  if (x == OBJNULL) {
    if (Null(eof_errorp)) {
      @(return eof_value);
    }
    FEend_of_file(strm);
  }
  /* Skip whitespace characters, but stop at beginning of new line or token */
  if (Null(recursivep)) {
    cl_object rtbl = ecl_current_readtable();
    int c = ecl_read_char(strm);
    if (c != EOF && (ecl_readtable_get(rtbl, c, NULL) != cat_whitespace)) {
      ecl_unread_char(c, strm);
    }
  }
  @(return x);
  @)

@(defun read_preserving_whitespace
  (&optional (strm ECL_NIL)
   (eof_errorp ECL_T)
   eof_value
   recursivep)
  cl_object x;
  @
  strm = stream_or_default_input(strm);
  if (Null(recursivep)) {
    x = ecl_read_object_non_recursive(strm);
  } else {
    x = ecl_read_object(strm);
  }
  if (x == OBJNULL) {
    if (Null(eof_errorp))
      @(return eof_value);
    FEend_of_file(strm);
  }
  @(return x);
  @)

cl_object
ecl_read_delimited_list(int d, cl_object in, bool proper_list)
{
  int after_dot = 0;
  bool suppress = read_suppress;
  cl_object x, y = ECL_NIL;
  cl_object *p = &y;
  do {
    x = ecl_read_object_with_delimiter(in, d, ECL_READ_LIST_DOT);
    if (x == OBJNULL) {
      /* End of the list. */
      unlikely_if (after_dot == 1) {
        /* Something like (1 . ) */
        FEreader_error("Object missing after a list dot", in, 0);
      }
      return y;
    } else if (x == @'si::.') {
      unlikely_if (proper_list) {
        FEreader_error("A dotted list was found where a proper list was expected.", in, 0);
      }
      unlikely_if (p == &y) {
        /* Something like (. 2) */
        FEreader_error("A dot appeared after a left parenthesis.", in, 0);
      }
      unlikely_if (after_dot) {
        /* Something like (1 . . 2) */
        FEreader_error("Two dots appeared consecutively.", in, 0);
      }
      after_dot = 1;
    } else if (after_dot) {
      unlikely_if (after_dot++ > 1) {
        /* Something like (1 . 2 3) */
        FEreader_error("Too many objects after a list dot", in, 0);
      }
      *p = x;
    } else if (!suppress) {
      *p = ecl_list1(x);
      p = &ECL_CONS_CDR(*p);
    }
  } while (1);
}

@(defun read_delimited_list (d &optional (strm ECL_NIL) recursivep)
  cl_object l;
  int delimiter;
  @
  delimiter = ecl_char_code(d);
  strm = stream_or_default_input(strm);
  if (!Null(recursivep)) {
    l = ecl_read_delimited_list(delimiter, strm, 1);
  } else {
    ecl_bds_bind(the_env, @'si::*sharp-eq-context*', ECL_NIL);
    ecl_bds_bind(the_env, @'si::*backq-level*', ecl_make_fixnum(0));
    l = ecl_read_delimited_list(delimiter, strm, 1);
    l = patch_sharp(the_env, l);
    ecl_bds_unwind_n(the_env, 2);
  }
  @(return l);
  @)

@(defun read_line (&optional (strm ECL_NIL) (eof_errorp ECL_T) eof_value recursivep)
  int c;
  cl_object token, value0, value1;
  @
  strm = stream_or_default_input(strm);
#ifdef ECL_CLOS_STREAMS
  if (!ECL_ANSI_STREAM_P(strm)) {
    value0 = _ecl_funcall2(@'gray::stream-read-line', strm);
    value1 = ecl_nth_value(the_env, 1);
    if ((Null(value0) || (ECL_STRINGP(value0) && (ecl_length(value0) == 0))) &&
        !Null(value1)) {
      if (!Null(eof_errorp))
        FEend_of_file(strm);
      value0 = eof_value;
      value1 = ECL_T;
    }
    goto OUTPUT;
  }
#endif
  token = si_get_buffer_string();
  do {
    c = ecl_read_char(strm);
    if (c == EOF || c == '\n')
      break;
    ecl_string_push_extend(token, c);
  } while(1);
  if (c == EOF && TOKEN_STRING_FILLP(token) == 0) {
    if (!Null(eof_errorp))
      FEend_of_file(strm);
    value0 = eof_value;
    value1 = ECL_T;
  } else {
#ifdef ECL_NEWLINE_IS_CRLF      /* From \r\n, ignore \r */
    if (TOKEN_STRING_FILLP(token) > 0 &&
        TOKEN_STRING_CHAR_CMP(token,TOKEN_STRING_FILLP(token)-1,'\r'))
      TOKEN_STRING_FILLP(token)--;
#endif
#ifdef ECL_NEWLINE_IS_LFCR      /* From \n\r, ignore \r */
    ecl_read_char(strm);
#endif
    value0 = cl_copy_seq(token);
    value1 = (c == EOF? ECL_T : ECL_NIL);
  }
  si_put_buffer_string(token);
 OUTPUT:
  @(return value0 value1);
  @)

@(defun read-char (&optional (strm ECL_NIL) (eof_errorp ECL_T) eof_value recursivep)
  int c;
  cl_object output;
  @
  strm = stream_or_default_input(strm);
  c = ecl_read_char(strm);
  if (c != EOF)
    output = ECL_CODE_CHAR(c);
  else if (Null(eof_errorp))
    output = eof_value;
  else
    FEend_of_file(strm);
  @(return output);
  @)

@(defun unread_char (c &optional (strm ECL_NIL))
  @
  /* INV: unread_char() checks the type `c' */
  strm = stream_or_default_input(strm);
  ecl_unread_char(ecl_char_code(c), strm);
  @(return ECL_NIL);
  @)

@(defun peek-char (&optional peek_type (strm ECL_NIL) (eof_errorp ECL_T) eof_value recursivep)
  int c;
  cl_object rtbl = ecl_current_readtable();
  @
  strm = stream_or_default_input(strm);
  c = ecl_peek_char(strm);
  if (c != EOF && !Null(peek_type)) {
    if (peek_type == ECL_T) {
      do {
        /* If the character is not a whitespace, output */
        if (ecl_readtable_get(rtbl, c, NULL) != cat_whitespace)
          break;
        /* Otherwise, read the whitespace and peek the
         * next character */
        ecl_read_char(strm);
        c = ecl_peek_char(strm);
      } while (c != EOF);
    } else {
      do {
        /* If the character belongs to the given class,
         * we're done. */
        if (ecl_char_eq(ECL_CODE_CHAR(c), peek_type))
          break;
        /* Otherwise, consume the character and
         * peek the next one. */
        ecl_read_char(strm);
        c = ecl_peek_char(strm);
      } while (c != EOF);
    }
  }
  if (c != EOF) {
    eof_value = ECL_CODE_CHAR(c);
  } else if (!Null(eof_errorp)) {
    FEend_of_file(strm);
  }
  @(return eof_value);
  @)

@(defun listen (&optional (strm ECL_NIL))
  @
  strm = stream_or_default_input(strm);
  @(return ((ecl_listen_stream(strm) == ECL_LISTEN_AVAILABLE)? ECL_T : ECL_NIL));
  @)

@(defun read_char_no_hang (&optional (strm ECL_NIL) (eof_errorp ECL_T) eof_value recursivep)
  int f;
  @
  strm = stream_or_default_input(strm);
#ifdef ECL_CLOS_STREAMS
  if (!ECL_ANSI_STREAM_P(strm)) {
    cl_object output =
      _ecl_funcall2(@'gray::stream-read-char-no-hang', strm);
    if (output == @':eof')
      goto END_OF_FILE;
    @(return output);
  }
#endif
  f = ecl_listen_stream(strm);
  if (f == ECL_LISTEN_AVAILABLE) {
    int c = ecl_read_char(strm);
    if (c != EOF) {
      @(return ECL_CODE_CHAR(c));
    }
  } else if (f == ECL_LISTEN_NO_CHAR) {
    @(return @'nil');
  }
  /* We reach here if there was an EOF */
 END_OF_FILE:
  if (Null(eof_errorp)) {
    @(return eof_value);
  }
  else {
      FEend_of_file(strm);
  }
  @)

@(defun clear_input (&optional (strm ECL_NIL))
  @
  strm = stream_or_default_input(strm);
  ecl_clear_input(strm);
  @(return ECL_NIL);
  @)

@(defun read_byte (binary_input_stream &optional (eof_errorp ECL_T) eof_value)
  cl_object c;
  @
  c = ecl_read_byte(binary_input_stream);
  if (c == OBJNULL) {
    if (Null(eof_errorp)) {
      @(return eof_value);
    }
    else
      FEend_of_file(binary_input_stream);
  }
  @(return c);
  @)

@(defun read_sequence (sequence stream &key (start ecl_make_fixnum(0)) end)
  @
#ifdef ECL_CLOS_STREAMS
  if (!ECL_ANSI_STREAM_P(stream))
  return funcall(5, @'gray::stream-read-sequence', stream, sequence, start, end);
  else
#endif
	  @(return si_do_read_sequence(sequence, stream, start, end));
  @)

/*
 *----------------------------------------------------------------------
 *
 * ecl_init_module --
 *     reads the data vector from stream into vector VV
 *
 * Results:
 *      a vector.
 *
 *----------------------------------------------------------------------
 */
static cl_object
make_one_data_stream(const cl_object string)
{
#ifdef ECL_UNICODE
  return si_make_sequence_input_stream(3, string, @':external-format',
                                       @':utf-8');
#else
  return ecl_make_string_input_stream(string, 0, ecl_length(string));
#endif
}

static cl_object
make_data_stream(const cl_object *data)
{
  if (data == 0 || data[0] == NULL) {
    return cl_core.null_stream;
  }
  if (data[1] == NULL) {
    return make_one_data_stream(data[0]);
  } else {
    cl_object stream_list = ECL_NIL;
    cl_index i;
    for (i = 0; data[i]; i++) {
      cl_object s = make_one_data_stream(data[i]);
      stream_list = ecl_cons(s, stream_list);
    }
    return cl_apply(2, @'make-concatenated-stream',
                    cl_nreverse(stream_list));
  }
}

cl_object
ecl_init_module(cl_object block, void (*entry_point)(cl_object))
{
  const cl_env_ptr env = ecl_process_env();
  volatile cl_object old_eptbc = env->packages_to_be_created;
  volatile cl_object x;
  cl_index i, len, perm_len, temp_len;
  cl_object in;
  cl_object *VV = NULL, *VVtemp = NULL;

  if (block == NULL)
    block = ecl_make_codeblock();
  block->cblock.entry = entry_point;

  in = OBJNULL;
  ECL_UNWIND_PROTECT_BEGIN(env) {
    cl_index bds_ndx;
    cl_object progv_list;

    ecl_bds_bind(env, @'*package*', ecl_cmp_symbol_value(env, @'*package*'));
    ecl_bds_bind(env, @'*readtable*', ecl_cmp_symbol_value(env, @'*readtable*'));
    ecl_bds_bind(env, @'si::*cblock*', block);
    env->packages_to_be_created_p = ECL_T;

    /* Communicate the library which Cblock we are using, and get
     * back the amount of data to be processed.
     */
    (*entry_point)(block);
    perm_len = block->cblock.data_size;
    temp_len = block->cblock.temp_data_size;
    len = perm_len + temp_len;

    if (block->cblock.data_text == 0) {
      if (len) {
        /* Code from COMPILE uses data in *compiler-constants* */
        cl_object v = ECL_SYM_VAL(env,@'si::*compiler-constants*');
        unlikely_if (ecl_t_of(v) != t_vector ||
                     v->vector.dim != len ||
                     v->vector.elttype != ecl_aet_object)
          FEerror("Internal error: corrupted data in "
                  "si::*compiler-constants*", 0);
        VV = block->cblock.data = v->vector.self.t;
        VVtemp = block->cblock.temp_data = NULL;
      }
      goto NO_DATA_LABEL;
    }
    if (len == 0) {
      VV = VVtemp = NULL;
      goto NO_DATA_LABEL;
    }
#ifdef ECL_DYNAMIC_VV
    VV = block->cblock.data = perm_len? (cl_object *)ecl_alloc(perm_len * sizeof(cl_object)) : NULL;
#else
    VV = block->cblock.data;
#endif
    memset(VV, 0, perm_len * sizeof(*VV));

    VVtemp = block->cblock.temp_data = temp_len? (cl_object *)ecl_alloc(temp_len * sizeof(cl_object)) : NULL;
    memset(VVtemp, 0, temp_len * sizeof(*VVtemp));

    /* Read all data for the library */
#ifdef ECL_EXTERNALIZABLE
    {
      unlikely_if (block->cblock.data_text == NULL) {
        unlikely_if (len > 0)
          FEreader_error("Not enough data while loading binary file", in, 0);
      } else {
        cl_object v = si_deserialize(*(block->cblock.data_text));
        unlikely_if (v->vector.dim < len)
          FEreader_error("Not enough data while loading binary file", in, 0);
        memcpy(VV, v->vector.self.t, perm_len * sizeof(cl_object));
        memcpy(VVtemp, v->vector.self.t + perm_len, temp_len * sizeof(cl_object));
      }
    }
#else
    in = make_data_stream(block->cblock.data_text);
    progv_list = ECL_SYM_VAL(env, @'si::+ecl-syntax-progv-list+');
    bds_ndx = ecl_progv(env, ECL_CONS_CAR(progv_list),
                        ECL_CONS_CDR(progv_list));
    for (i = 0 ; i < len; i++) {
      x = ecl_read_object(in);
      if (x == OBJNULL)
        break;
      if (i < perm_len)
        VV[i] = x;
      else
        VVtemp[i-perm_len] = x;
    }
    if (!Null(ECL_SYM_VAL(env, @'si::*sharp-eq-context*'))) {
      while (i--) {
        if (i < perm_len) {
          VV[i] = patch_sharp(env, VV[i]);
        } else {
          VVtemp[i-perm_len] = patch_sharp(env, VVtemp[i-perm_len]);
        }
      }
    }
    ecl_bds_unwind(env, bds_ndx);
    unlikely_if (i < len)
      FEreader_error("Not enough data while loading binary file", in, 0);
    cl_close(1,in);
    in = OBJNULL;
#endif
  NO_DATA_LABEL:
    env->packages_to_be_created_p = ECL_NIL;

    assert(block->cblock.cfuns_size == 0 || VV != NULL);
    for (i = 0; i < block->cblock.cfuns_size; i++) {
      const struct ecl_cfunfixed *prototype = block->cblock.cfuns+i;
      cl_index fname_location = ecl_fixnum(prototype->block);
      cl_object fname = VV[fname_location];
      cl_index location = ecl_fixnum(prototype->name);
      cl_object position = prototype->file_position;
      int narg = prototype->narg;
      if (prototype->t == t_cfunfixed) {
        VV[location] = ecl_make_cfun((cl_objectfn_fixed)prototype->entry,
                                     fname, block, narg);
      } else {
        VV[location] = ecl_make_cfun_va((cl_objectfn)prototype->entry,
                                        fname, block, narg);
      }
      /* Add source file info */
      if (position != ecl_make_fixnum(-1)) {
        ecl_set_function_source_file_info(VV[location],
                                          block->cblock.source,
                                          position);
      }
    }
    /* Execute top-level code */
    (*entry_point)(OBJNULL);
    x = cl_set_difference(2, env->packages_to_be_created, old_eptbc);
    old_eptbc = env->packages_to_be_created;
    unlikely_if (!Null(x)) {
      CEerror(ECL_T,
              Null(ECL_CONS_CDR(x))?
              "Package ~A referenced in "
              "compiled file~&  ~A~&but has not been created":
              "The packages~&  ~A~&were referenced in "
              "compiled file~&  ~A~&but have not been created",
              2, x, block->cblock.name);
    }
    if (VVtemp) {
      block->cblock.temp_data = NULL;
      block->cblock.temp_data_size = 0;
      ecl_dealloc(VVtemp);
    }
    ecl_bds_unwind_n(env, 3);
  } ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT {
    if (in != OBJNULL)
      cl_close(1,in);
    env->packages_to_be_created = old_eptbc;
    env->packages_to_be_created_p = ECL_NIL;
  } ECL_UNWIND_PROTECT_THREAD_SAFE_END;

  return block;
}
