/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * rtab_cl.d -- readtable for Common Lisp + ECL extensions
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/number.h>
#include <ecl/internal.h>
#include <ecl/bytecodes.h>

#define read_suppress (ecl_symbol_value(@'*read-suppress*') != ECL_NIL)

static cl_object
right_parenthesis_reader(cl_object in, cl_object character)
{
  FEreader_error("Unmatched right parenthesis, #\\)", in, 0);
}

static cl_object
left_parenthesis_reader(cl_object in, cl_object character)
{
  const char c = ')';
  @(return ecl_read_delimited_list(c, in, 0));
}

/*
 * BACKQUOTE READER
 */

static
cl_object comma_reader(cl_object in, cl_object c)
{
  cl_object x, y;
  const cl_env_ptr env = ecl_process_env();
  cl_fixnum backq_level = ecl_fixnum(ECL_SYM_VAL(env, @'si::*backq-level*'));

  unlikely_if (backq_level <= 0 && !read_suppress)
    FEreader_error("A comma has appeared out of a backquote.", in, 0);
  /* Read character & complain at EOF */
  c = cl_peek_char(2,ECL_NIL,in);
  if (c == ECL_CODE_CHAR('@@')) {
    x = @'si::unquote-splice';
    ecl_read_char(in);
  } else if (c == ECL_CODE_CHAR('.')) {
    x = @'si::unquote-nsplice';
    ecl_read_char(in);
  } else {
    x = @'si::unquote';
  }
  ECL_SETQ(env, @'si::*backq-level*', ecl_make_fixnum(backq_level-1));
  y = ecl_read_object(in);
  ECL_SETQ(env, @'si::*backq-level*', ecl_make_fixnum(backq_level));
  return cl_list(2, x, y);
}

static
cl_object backquote_reader(cl_object in, cl_object c)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_fixnum backq_level = ecl_fixnum(ECL_SYM_VAL(the_env, @'si::*backq-level*'));
  ECL_SETQ(the_env, @'si::*backq-level*', ecl_make_fixnum(backq_level+1));
  c = ecl_read_object(in);
  ECL_SETQ(the_env, @'si::*backq-level*', ecl_make_fixnum(backq_level));
  unlikely_if (c == OBJNULL)
    FEend_of_file(in);
  unlikely_if (read_suppress)
    @(return ECL_NIL);
#if 0
  @(return cl_macroexpand_1(2, cl_list(2, @'si::quasiquote', in), ECL_NIL));;
#else
  @(return cl_list(2,@'si::quasiquote',c));
#endif
}

static void
read_string_into_buffer(cl_object in, cl_object c, cl_object buffer)
{
  int delim = ECL_CHAR_CODE(c);
  cl_object rtbl = ecl_current_readtable();
  for (;;) {
    int c = ecl_read_char_noeof(in);
    if (c == delim)
      break;
    else if (ecl_readtable_get(rtbl, c, NULL) == cat_single_escape)
      c = ecl_read_char_noeof(in);
    ecl_string_push_extend(buffer, c);
  }
}

static cl_object
double_quote_reader(cl_object in, cl_object c)
{
  cl_object output;
  cl_object token = si_get_buffer_string();
  read_string_into_buffer(in, c, token);
  /* Must be kept a SIMPLE-STRING, meaning a (SIMPLE-ARRAY CHARACTERS
   * (*)), see CLHS 2.4.5. We thus can't coerce to a BASE-STRING. */
  output = cl_copy_seq(token);
  si_put_buffer_string(token);
  @(return output);
}

static void
extra_argument(int c, cl_object stream, cl_object d)
{
  FEreader_error("~S is an extra argument for the #~C readmacro.",
                 stream, 2, d, ECL_CODE_CHAR(c));
}


static cl_object
sharp_double_quote_reader(cl_object in, cl_object c, cl_object d)
{
  /* Base string reader. Used for data in compiled files. */
  cl_object s, token;

  if (d != ECL_NIL && !read_suppress)
    extra_argument('"', in, d);

  token = si_get_buffer_string();
  read_string_into_buffer(in, c, token);
  s = si_copy_to_simple_base_string(token);
  si_put_buffer_string(token);

  if (read_suppress)
    @(return ECL_NIL);
  @(return s);
}

static cl_object
single_quote_reader(cl_object in, cl_object c)
{
  c = ecl_read_object(in);
  unlikely_if (c == OBJNULL)
    FEend_of_file(in);
  @(return cl_list(2, @'quote', c));
}

static cl_object
void_reader3(cl_object in, cl_object c, cl_object f)
{
  /*  no result  */
  @(return);
}

static cl_object
semicolon_reader(cl_object in, cl_object c)
{
  int auxc;

  do
    auxc = ecl_read_char(in);
  while (auxc != '\n' && auxc != EOF);
  /*  no result  */
  @(return);
}

/*
  sharpmacro routines
*/

static cl_object
sharp_generic_error(cl_object in, cl_object c, cl_object n)
{
  FEreader_error("The character ~:C is not a valid dispatch macro character",
                 in, 1, c);
}

static cl_object
sharp_C_reader(cl_object in, cl_object c, cl_object d)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object x, real, imag;

  if (d != ECL_NIL && !read_suppress)
    extra_argument('C', in, d);
  x = ecl_read_object(in);
  unlikely_if (x == OBJNULL)
    FEend_of_file(in);
  if (read_suppress)
    @(return ECL_NIL);
  unlikely_if (!ECL_CONSP(x) || ecl_length(x) != 2)
    FEreader_error("Reader macro #C should be followed by a list",
                   in, 0);
  real = CAR(x);
  imag = CADR(x);
  /* INV: ecl_make_complex() checks its types. When reading circular
     structures, we cannot check the types of the elements, and we
     must build the complex number by hand. */
  if ((CONSP(real) || CONSP(imag)) &&
      !Null(ECL_SYM_VAL(the_env, @'si::*sharp-eq-context*')))
    {
      x = ecl_alloc_object(t_complex);
      x->gencomplex.real = real;
      x->gencomplex.imag = imag;
    } else {
    x = ecl_make_complex(real, imag);
  }
  @(return x);
}

static cl_object
sharp_backslash_reader(cl_object in, cl_object c, cl_object d)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object token, string, escape;
  if (d != ECL_NIL && !read_suppress) {
    unlikely_if (!ECL_FIXNUMP(d) || d != ecl_make_fixnum(0)) {
      FEreader_error("~S is an illegal CHAR-FONT.", in, 1, d);
    }
  }
  token = ecl_read_only_token(in, 1);
  string = token->token.string;
  escape = token->token.escape;
  if (TOKEN_STRING_FILLP(string) == 1) {
    c = ECL_CODE_CHAR(TOKEN_STRING_CHAR(string,0));
  } else if (TOKEN_STRING_FILLP(string) == 2 && TOKEN_STRING_CHAR_CMP(string,0,'^')) {
    /*      #\^x    */
    c = ECL_CODE_CHAR(TOKEN_STRING_CHAR(string,1) & 037);
  } else {
    cl_object nc = cl_name_char(string);
    unlikely_if (Null(nc)) {
      FEreader_error("~S is an illegal character name.", in, 1, string);
    }
    c = nc;
  }
  ecl_put_reader_token(token);
  ecl_return1(the_env, c);
}

static cl_object
sharp_single_quote_reader(cl_object in, cl_object c, cl_object d)
{
  bool suppress = read_suppress;
  if(d != ECL_NIL && !suppress)
    extra_argument('\'', in, d);
  c = ecl_read_object(in);
  unlikely_if (c == OBJNULL) {
    FEend_of_file(in);
  } else if (suppress) {
    c = ECL_NIL;
  } else {
    c = cl_list(2, @'function', c);
  }
  @(return c);
}

static cl_object
sharp_Y_reader(cl_object in, cl_object c, cl_object d)
{
  cl_index i;
  cl_object x, rv, nth, lex;

  if (d != ECL_NIL && !read_suppress)
    extra_argument('Y', in, d);
  x = ecl_read_object(in);
  unlikely_if (x == OBJNULL) {
    FEend_of_file(in);
  }
  if (read_suppress) {
    @(return ECL_NIL);
  }

  unlikely_if (!ECL_CONSP(x) || ecl_length(x) < 5) {
    FEreader_error("Reader macro #Y should be followed by a list", in, 0);
  }

  rv = ecl_alloc_object(t_bytecodes);

  rv->bytecodes.name = ECL_CONS_CAR(x);
  x = ECL_CONS_CDR(x);

  lex = ECL_CONS_CAR(x);
  x = ECL_CONS_CDR(x);

  rv->bytecodes.definition = ECL_CONS_CAR(x);
  x = ECL_CONS_CDR(x);

  nth = ECL_CONS_CAR(x);
  x = ECL_CONS_CDR(x);
  rv->bytecodes.code_size = ecl_to_fix(cl_list_length(nth));
  rv->bytecodes.code = ecl_alloc_atomic(rv->bytecodes.code_size * sizeof(uint16_t));
  for ( i=0; !ecl_endp(nth) ; i++, nth=ECL_CONS_CDR(nth) )
    ((cl_opcode*)(rv->bytecodes.code))[i] = ecl_to_fix(ECL_CONS_CAR(nth));

  nth = ECL_CONS_CAR(x);
  x = ECL_CONS_CDR(x);
  rv->bytecodes.data = nth;

  nth = ECL_CONS_CAR(x);
  x = ECL_CONS_CDR(x);
  rv->bytecodes.flex = nth;

  nth = ECL_CONS_CAR(x);
  x = ECL_CONS_CDR(x);
  rv->bytecodes.nlcl = nth;

  if (ECL_ATOM(x)) {
    nth = ECL_NIL;
  } else {
    nth = ECL_CONS_CAR(x);
    x = ECL_CONS_CDR(x);
  }
  rv->bytecodes.file = nth;
  if (ECL_ATOM(x)) {
    nth = ecl_make_fixnum(0);
  } else {
    nth = ECL_CONS_CAR(x);
    x = ECL_CONS_CDR(x);
  }
  rv->bytecodes.file_position = nth;

  rv->bytecodes.entry = _ecl_bytecodes_dispatch_vararg;

  if (lex != ECL_NIL) {
    cl_object x = ecl_alloc_object(t_bclosure);
    x->bclosure.code = rv;
    x->bclosure.lex = lex;
    x->bclosure.entry = _ecl_bclosure_dispatch_vararg;
    rv = x;
  }
  @(return rv);
}

#define QUOTE   1
#define EVAL    2
#define LIST    3
#define LISTX   4
#define APPEND  5
#define NCONC   6

/*
 *----------------------------------------------------------------------
 *      Stack of unknown size
 *----------------------------------------------------------------------
 */

cl_object
si_make_backq_vector(cl_object d, cl_object data, cl_object in)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object v, last;
  cl_index dim, i;
  if (Null(d)) {
    dim = ecl_length(data);
  } else {
    dim = ecl_fixnum(d);
  }
  v = ecl_alloc_simple_vector(dim, ecl_aet_object);
  for (i = 0, last = ECL_NIL; i < dim; i++) {
    if (data == ECL_NIL) {
      /* ... we fill the vector with the last element read (or NIL). */
      for (; i < dim; i++) {
        ecl_aset_unsafe(v, i, last);
      }
      break;
    }
    ecl_aset_unsafe(v, i, last = ecl_car(data));
    data = ECL_CONS_CDR(data);
  }
  unlikely_if (data != ECL_NIL) {
    if (in != ECL_NIL) {
      FEreader_error("Vector larger than specified length,"
                     "~D.", in, 1, d);
    } else {
      FEerror("Vector larger than specified length, ~D", 1, d);
    }
  }
  ecl_return1(the_env, v);
}

static cl_object
sharp_left_parenthesis_reader(cl_object in, cl_object c, cl_object d)
{
  extern int _cl_backq_car(cl_object *);
  const cl_env_ptr the_env = ecl_process_env();
  cl_object v;
  unlikely_if (!Null(d) &&
               (!ECL_FIXNUMP(d) || ecl_fixnum_minusp(d) ||
                ecl_fixnum_greater(d, ecl_make_fixnum(ECL_ARRAY_DIMENSION_LIMIT))))
    {
      FEreader_error("Invalid dimension size ~D in #()", in, 1, d);
    }
  if (ecl_fixnum_plusp(ECL_SYM_VAL(the_env, @'si::*backq-level*'))) {
    /* First case: ther might be unquoted elements in the vector.
     * Then we just create a form that generates the vector.
     */
    cl_object x = ecl_read_delimited_list(')', in, 1);
    cl_index a = _cl_backq_car(&x);
    if (a != QUOTE) {
      v = cl_list(2, @'si::unquote', 
                  cl_list(4, @'si::make-backq-vector', d, x, ECL_NIL));
    } else {
      return si_make_backq_vector(d, x, in);
    }
  } else if (read_suppress) {
    /* Second case: *read-suppress* = t, we ignore the data */
    ecl_read_delimited_list(')', in, 1);
    v = ECL_NIL;
  } else if (Null(d)) {
    /* Third case: no dimension provided. Read a list and
       coerce it to vector. */
    return si_make_backq_vector(d, ecl_read_delimited_list(')', in, 1), in);
  } else {
    /* Finally: Both dimension and data are provided. The
       amount of data cannot exceed the length, but it may
       be smaller, and in that case...*/
    cl_object last;
    cl_index dim = ecl_fixnum(d), i;
    v = ecl_alloc_simple_vector(dim, ecl_aet_object);
    for (i = 0, last = ECL_NIL;; i++) {
      cl_object aux = ecl_read_object_with_delimiter(in, ')', 0);
      if (aux == OBJNULL)
        break;
      unlikely_if (i >= dim) {
        FEreader_error("Vector larger than specified length,"
                       "~D.", in, 1, d);
      }
      ecl_aset_unsafe(v, i, last = aux);
    }
    /* ... we fill the vector with the last element read (or NIL). */
    for (; i < dim; i++) {
      ecl_aset_unsafe(v, i, last);
    }
  }
  @(return v);
}

static cl_object
sharp_asterisk_reader(cl_object in, cl_object c, cl_object d)
{
  cl_env_ptr env = ecl_process_env();
  cl_index sp = ECL_STACK_INDEX(env);
  cl_object last, elt, x;
  cl_fixnum dim, dimcount, i;
  cl_object rtbl = ecl_current_readtable();
  enum ecl_chattrib a;

  if (read_suppress) {
    ecl_read_constituent(in, 1);
    @(return ECL_NIL);
  }
  for (dimcount = 0 ;; dimcount++) {
    int x = ecl_read_char(in);
    if (x == EOF)
      break;
    a = ecl_readtable_get(rtbl, x, NULL);
    if (a == cat_terminating || a == cat_whitespace) {
      ecl_unread_char(x, in);
      break;
    }
    unlikely_if (a == cat_single_escape || a == cat_multiple_escape ||
                 (x != '0' && x != '1'))
      {
        FEreader_error("Character ~:C is not allowed after #*",
                       in, 1, ECL_CODE_CHAR(x));
      }
    ECL_STACK_PUSH(env, ecl_make_fixnum(x == '1'));
  }
  if (Null(d)) {
    dim = dimcount;
  } else {
    unlikely_if (!ECL_FIXNUMP(d) || ((dim = ecl_fixnum(d)) < 0) ||
                 (dim > ECL_ARRAY_DIMENSION_LIMIT))
      {
        FEreader_error("Wrong vector dimension size ~D in #*.",
                       in, 1, d);
      }
    unlikely_if (dimcount > dim)
      FEreader_error("Too many elements in #*.", in, 0);
    unlikely_if (dim && (dimcount == 0))
      FEreader_error("Cannot fill the bit-vector #*.", in, 0);
  }
  last = ECL_STACK_REF(env,-1);
  x = ecl_alloc_simple_vector(dim, ecl_aet_bit);
  for (i = 0; i < dim; i++) {
    elt = (i < dimcount) ? env->run_stack.org[sp+i] : last;
    if (elt == ecl_make_fixnum(0))
      x->vector.self.bit[i/CHAR_BIT] &= ~(0200 >> i%CHAR_BIT);
    else
      x->vector.self.bit[i/CHAR_BIT] |= 0200 >> i%CHAR_BIT;
  }
  ECL_STACK_POP_N_UNSAFE(env, dimcount);
  @(return x);
}

static cl_object
sharp_colon_reader(cl_object in, cl_object ch, cl_object d)
{
  cl_object rtbl = ecl_current_readtable();
  enum ecl_chattrib a;
  int c;
  cl_object output, token;

  if (d != ECL_NIL && !read_suppress)
    extra_argument(':', in, d);
  c = ecl_read_char_noeof(in);
  a = ecl_readtable_get(rtbl, c, NULL);
  token = si_get_buffer_string();
  goto L;
  for (;;) {
    ecl_string_push_extend(token, c);
  K:
    c = ecl_read_char(in);
    if (c == EOF)
      goto M;
    a = ecl_readtable_get(rtbl, c, NULL);
  L:
    if (a == cat_single_escape) {
      c = ecl_read_char_noeof(in);
      a = cat_constituent;
    } else if (a == cat_multiple_escape) {
      for (;;) {
        c = ecl_read_char_noeof(in);
        a = ecl_readtable_get(rtbl, c, NULL);
        if (a == cat_single_escape) {
          c = ecl_read_char_noeof(in);
          a = cat_constituent;
        } else if (a == cat_multiple_escape)
          break;
        ecl_string_push_extend(token, c);
      }
      goto K;
    } else if (ecl_lower_case_p(c)) {
      c = ecl_char_upcase(c);
    } else if (c == ':' && !read_suppress) {
      FEreader_error("An uninterned symbol must not contain a package prefix", in, 0);
    }
    if (a == cat_whitespace || a == cat_terminating)
      break;
  }
  ecl_unread_char(c, in);

 M:
  if (read_suppress) {
    output = ECL_NIL;
  } else {
    output = cl_make_symbol(token);
  }
  si_put_buffer_string(token);
  @(return output);
}

static cl_object
sharp_dot_reader(cl_object in, cl_object c, cl_object d)
{
  const cl_env_ptr the_env = ecl_process_env();
  if (d != ECL_NIL && !read_suppress)
    extra_argument('.', in, d);
  c = ecl_read_eval(in);
  ecl_return1(the_env, c);
}

static cl_object
read_number(cl_object in, int radix, cl_object macro_char)
{
  cl_index i;
  cl_object x;
  /* ecl_read_constituent is called with not_first=true, because we are called
     by reader macros for composite tokens, like #x123. -- jd 2024-05-12 */
  cl_object token = ecl_read_constituent(in, 1);
  if (token == ECL_NIL) {
    x = ECL_NIL;
  } else {
    x = ecl_parse_number(token, 0, TOKEN_STRING_FILLP(token), &i, radix);
    unlikely_if (x == OBJNULL || x == ECL_NIL ||
                 i != TOKEN_STRING_FILLP(token))
      {
        FEreader_error("Cannot parse the #~A readmacro.", in, 1,
                       macro_char);
      }
    unlikely_if (cl_rationalp(x) == ECL_NIL) {
      FEreader_error("The float ~S appeared after the #~A readmacro.",
                     in, 2, x, macro_char);
    }
    si_put_buffer_string(token);
  }
  return x;
}

static cl_object
sharp_B_reader(cl_object in, cl_object c, cl_object d)
{
  if(d != ECL_NIL && !read_suppress)
    extra_argument('B', in, d);
  @(return (read_number(in, 2, ECL_CODE_CHAR('B'))));
}

static cl_object
sharp_O_reader(cl_object in, cl_object c, cl_object d)
{
  if(d != ECL_NIL && !read_suppress)
    extra_argument('O', in, d);
  @(return (read_number(in, 8, ECL_CODE_CHAR('O'))));
}

static cl_object
sharp_X_reader(cl_object in, cl_object c, cl_object d)
{
  if(d != ECL_NIL && !read_suppress)
    extra_argument('X', in, d);
  @(return (read_number(in, 16, ECL_CODE_CHAR('X'))));
}

static cl_object
sharp_R_reader(cl_object in, cl_object c, cl_object d)
{
  int radix;
  if (read_suppress) {
    radix = 10;
  } else unlikely_if (!ECL_FIXNUMP(d)) {
      FEreader_error("No radix was supplied in the #R readmacro.", in, 0);
    } else {
    radix = ecl_fixnum(d);
    unlikely_if (radix > 36 || radix < 2) {
      FEreader_error("~S is an illegal radix.", in, 1, d);
    }
  }
  @(return (read_number(in, radix, ECL_CODE_CHAR('R'))));
}

static cl_object
sharp_eq_reader(cl_object in, cl_object c, cl_object d)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object pair, value;
  cl_object sharp_eq_context = ECL_SYM_VAL(the_env, @'si::*sharp-eq-context*');

  if (read_suppress) {
    @(return);
  }
  unlikely_if (Null(d)) {
    FEreader_error("The #= readmacro requires an argument.", in, 0);
  }
  unlikely_if (ecl_assq(d, sharp_eq_context) != ECL_NIL) {
    FEreader_error("Duplicate definitions for #~D=.", in, 1, d);
  }
  pair = CONS(d, OBJNULL);
  ECL_SETQ(the_env, @'si::*sharp-eq-context*', CONS(pair, sharp_eq_context));
  value = ecl_read_object(in);
  unlikely_if (value == pair) {
    FEreader_error("#~D# is defined by itself.", in, 1, d);
  }
  ECL_RPLACD(pair, value);
  ecl_return1(the_env, value);
}

static cl_object
sharp_sharp_reader(cl_object in, cl_object c, cl_object d)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object pair;

  if (read_suppress)
    ecl_return1(the_env, ECL_NIL);
  unlikely_if (Null(d)) {
    FEreader_error("The ## readmacro requires an argument.", in, 0);
  }
  pair = ecl_assq(d, ECL_SYM_VAL(the_env, @'si::*sharp-eq-context*'));
  unlikely_if (pair == ECL_NIL) {
    FEreader_error("#~D# is undefined.", in, 1, d);
  } else {
    cl_object value = ECL_CONS_CDR(pair);
    ecl_return1(the_env, (value == OBJNULL)? pair : value);
  }
}

#define sharp_plus_reader void_reader3
#define sharp_minus_reader void_reader3

static cl_object
sharp_vertical_bar_reader(cl_object in, cl_object ch, cl_object d)
{
  int c;
  int level = 0;

  if (d != ECL_NIL && !read_suppress)
    extra_argument('|', in, d);
  for (;;) {
    c = ecl_read_char_noeof(in);
  L:
    if (c == '#') {
      c = ecl_read_char_noeof(in);
      if (c == '|')
        level++;
    } else if (c == '|') {
      c = ecl_read_char_noeof(in);
      if (c == '#') {
        if (level == 0)
          break;
        else
          --level;
      } else
        goto L;
    }
  }
  /*  no result  */
  @(return);
}

/*
  #P" ... " returns the pathname with namestring ... .
*/
static cl_object
sharp_P_reader(cl_object in, cl_object c, cl_object d)
{
  bool suppress = read_suppress;
  if (d != ECL_NIL && !suppress)
    extra_argument('P', in, d);
  d = ecl_read_object(in);
  if (suppress) {
    d = ECL_NIL;
  } else {
    d = cl_parse_namestring(3, d, ECL_NIL, ECL_NIL);
  }
  @(return d);
}

/*
  #$ fixnum returns a random-state with the fixnum
  as its content.
*/
static cl_object
sharp_dollar_reader(cl_object in, cl_object c, cl_object d)
{
  cl_object rs;
  if (d != ECL_NIL && !read_suppress)
    extra_argument('$', in, d);
  c = ecl_read_object(in);
  rs = ecl_make_random_state(c);

  @(return rs);
}

#define make_cf2(f)     ecl_make_cfun((cl_objectfn_fixed)(f), ECL_NIL, NULL, 2)
#define make_cf3(f)     ecl_make_cfun((cl_objectfn_fixed)(f), ECL_NIL, NULL, 3)

void
init_read(void)
{
  struct ecl_readtable_entry *rtab;
  cl_object r, r_cmp;
  int i;

  cl_core.standard_readtable = r = ecl_alloc_object(t_readtable);
  r->readtable.locked = 0;
  r->readtable.read_case = ecl_case_upcase;
  r->readtable.table = rtab
    = (struct ecl_readtable_entry *)
    ecl_alloc(RTABSIZE * sizeof(struct ecl_readtable_entry));
  for (i = 0;  i < RTABSIZE;  i++) {
    rtab[i].syntax_type = cat_constituent;
    rtab[i].dispatch = ECL_NIL;
  }
#ifdef ECL_UNICODE
  r->readtable.hash = ECL_NIL;
#endif

  cl_core.dispatch_reader = make_cf2(ecl_dispatch_reader_fun);

  ecl_readtable_set(r, '\t', cat_whitespace, ECL_NIL);
  ecl_readtable_set(r, '\n', cat_whitespace, ECL_NIL);
  ecl_readtable_set(r, '\f', cat_whitespace, ECL_NIL);
  ecl_readtable_set(r, '\r', cat_whitespace, ECL_NIL);
  ecl_readtable_set(r, ' ', cat_whitespace, ECL_NIL);

  ecl_readtable_set(r, '"', cat_terminating,
                    make_cf2(double_quote_reader));

  ecl_readtable_set(r, '\'', cat_terminating,
                    make_cf2(single_quote_reader));
  ecl_readtable_set(r, '(', cat_terminating,
                    make_cf2(left_parenthesis_reader));
  ecl_readtable_set(r, ')', cat_terminating,
                    make_cf2(right_parenthesis_reader));
  ecl_readtable_set(r, ',', cat_terminating,
                    make_cf2(comma_reader));
  ecl_readtable_set(r, ';', cat_terminating,
                    make_cf2(semicolon_reader));
  ecl_readtable_set(r, '\\', cat_single_escape, ECL_NIL);
  ecl_readtable_set(r, '`', cat_terminating,
                    make_cf2(backquote_reader));
  ecl_readtable_set(r, '|', cat_multiple_escape, ECL_NIL);

  cl_make_dispatch_macro_character(3, ECL_CODE_CHAR('#'),
                                   ECL_T /* non terminating */, r);

  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('C'),
                                  make_cf3(sharp_C_reader), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('\\'),
                                  make_cf3(sharp_backslash_reader), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('\''),
                                  make_cf3(sharp_single_quote_reader), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('('),
                                  make_cf3(sharp_left_parenthesis_reader), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('*'),
                                  make_cf3(sharp_asterisk_reader), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR(':'),
                                  make_cf3(sharp_colon_reader), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('.'),
                                  make_cf3(sharp_dot_reader), r);
  /*  Used for fasload only. */
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('B'),
                                  make_cf3(sharp_B_reader), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('O'),
                                  make_cf3(sharp_O_reader), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('X'),
                                  make_cf3(sharp_X_reader), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('R'),
                                  make_cf3(sharp_R_reader), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('A'),
                                  @'si::sharp-a-reader', r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('S'),
                                  @'si::sharp-s-reader', r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('P'),
                                  make_cf3(sharp_P_reader), r);

  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('='),
                                  make_cf3(sharp_eq_reader), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('#'),
                                  make_cf3(sharp_sharp_reader), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('+'),
                                  make_cf3(sharp_plus_reader), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('-'),
                                  make_cf3(sharp_minus_reader), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('|'),
                                  make_cf3(sharp_vertical_bar_reader), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('\b'),
                                  make_cf3(sharp_generic_error), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('\t'),
                                  make_cf3(sharp_generic_error), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR(ECL_CHAR_CODE_NEWLINE),
                                  make_cf3(sharp_generic_error), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR(ECL_CHAR_CODE_LINEFEED),
                                  make_cf3(sharp_generic_error), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('\f'),
                                  make_cf3(sharp_generic_error), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR(ECL_CHAR_CODE_RETURN),
                                  make_cf3(sharp_generic_error), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR(' '),
                                  make_cf3(sharp_generic_error), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR(')'),
                                  make_cf3(sharp_generic_error), r);
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('<'),
                                  make_cf3(sharp_generic_error), r);
  /*  This is specific to this implementation  */
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('$'),
                                  make_cf3(sharp_dollar_reader), r);
  /*  This is specific to this implementation  */
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('Y'),
                                  make_cf3(sharp_Y_reader), r);
  /*  This is specific to this implementation: ignore BOM  */
#ifdef ECL_UNICODE
  ecl_readtable_set(r, 0xfeff, cat_whitespace, ECL_NIL);
#endif

  /* Lock the standard read table so that we do not have to make copies
   * to keep it unchanged */
  r->readtable.locked = 1;

  r_cmp = ecl_copy_readtable(cl_core.standard_readtable, ECL_NIL);
  /*  This is specific to this implementation: syntax for base strings  */
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('"'),
                                  make_cf3(sharp_double_quote_reader), r_cmp);
  cl_core.compiler_readtable = r_cmp;

  init_backq();

  ECL_SET(@'*readtable*',
          r=ecl_copy_readtable(cl_core.standard_readtable, ECL_NIL));
  cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('!'),
                                  ECL_NIL, r);
  ECL_SET(@'*read-default-float-format*', @'single-float');

  {
    cl_object var, val;
    var = cl_list(25,
                  @'*print-pprint-dispatch*', /* See end of pprint.lsp */
                  @'*print-array*',
                  @'*print-base*',
                  @'*print-case*',
                  @'*print-circle*',
                  @'*print-escape*',
                  @'*print-gensym*',
                  @'*print-length*',
                  @'*print-level*',
                  @'*print-lines*',
                  @'*print-miser-width*',
                  @'*print-pretty*',
                  @'*print-radix*',
                  @'*print-readably*',
                  @'*print-right-margin*',
                  @'*read-base*',
                  @'*read-default-float-format*',
                  @'*read-eval*',
                  @'*read-suppress*',
                  @'*readtable*',
                  @'*package*',
                  @'si::*print-package*',
                  @'si::*print-structure*',
                  @'si::*sharp-eq-context*',
                  @'si::*circle-counter*');
    val = cl_list(25,
                  /**pprint-dispatch-table**/ ECL_NIL,
                  /**print-array**/ @'base-string', /* base string syntax */
                  /**print-base**/ ecl_make_fixnum(10),
                  /**print-case**/ @':downcase',
                  /**print-circle**/ ECL_T,
                  /**print-escape**/ ECL_T,
                  /**print-gensym**/ ECL_T,
                  /**print-length**/ ECL_NIL,
                  /**print-level**/ ECL_NIL,
                  /**print-lines**/ ECL_NIL,
                  /**print-miser-width**/ ECL_NIL,
                  /**print-pretty**/ ECL_NIL,
                  /**print-radix**/ ECL_NIL,
                  /**print-readably**/ ECL_T,
                  /**print-right-margin**/ ECL_NIL,
                  /**read-base**/ ecl_make_fixnum(10),
                  /**read-default-float-format**/ @'single-float',
                  /**read-eval**/ ECL_T,
                  /**read-suppress**/ ECL_NIL,
                  /**readtable**/ cl_core.compiler_readtable,
                  /**package**/ cl_core.lisp_package,
                  /*si::*print-package**/ cl_core.lisp_package,
                  /*si::*print-structure**/ ECL_T,
                  /*si::*sharp-eq-context**/ ECL_NIL,
                  /*si::*cicle-counter**/ ECL_NIL);
    ECL_SET(@'si::+ecl-syntax-progv-list+', CONS(var,val));
    var = cl_list(23,
                  @'*print-pprint-dispatch*', /* See end of pprint.lsp */
                  @'*print-array*',
                  @'*print-base*',
                  @'*print-case*',
                  @'*print-circle*',
                  @'*print-escape*',
                  @'*print-gensym*',
                  @'*print-length*',
                  @'*print-level*',
                  @'*print-lines*',
                  @'*print-miser-width*',
                  @'*print-pretty*',
                  @'*print-radix*',
                  @'*print-readably*',
                  @'*print-right-margin*',
                  @'*read-base*',
                  @'*read-default-float-format*',
                  @'*read-eval*',
                  @'*read-suppress*',
                  @'*readtable*',
                  @'*package*',
                  @'si::*sharp-eq-context*',
                  @'si::*circle-counter*');
    val = cl_list(23,
                  /**pprint-dispatch-table**/ ECL_NIL,
                  /**print-array**/ ECL_T,
                  /**print-base**/ ecl_make_fixnum(10),
                  /**print-case**/ @':upcase',
                  /**print-circle**/ ECL_NIL,
                  /**print-escape**/ ECL_T,
                  /**print-gensym**/ ECL_T,
                  /**print-length**/ ECL_NIL,
                  /**print-level**/ ECL_NIL,
                  /**print-lines**/ ECL_NIL,
                  /**print-miser-width**/ ECL_NIL,
                  /**print-pretty**/ ECL_NIL,
                  /**print-radix**/ ECL_NIL,
                  /**print-readably**/ ECL_T,
                  /**print-right-margin**/ ECL_NIL,
                  /**read-base**/ ecl_make_fixnum(10),
                  /**read-default-float-format**/ @'single-float',
                  /**read-eval**/ ECL_T,
                  /**read-suppress**/ ECL_NIL,
                  /**readtable**/ cl_core.standard_readtable,
                  /**package**/ cl_core.user_package,
                  /*si::*sharp-eq-context**/ ECL_NIL,
                  /*si::*cicle-counter**/ ECL_NIL);
    ECL_SET(@'si::+io-syntax-progv-list+', CONS(var,val));
  }
}
