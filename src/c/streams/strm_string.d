/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * strm_string.d - String Streams dispatch tables
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 * Copyright (c) 2025 Daniel Kochmanski
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/internal.h>

/**********************************************************************
 * STRING OUTPUT STREAMS
 */

static ecl_character
str_out_write_char(cl_object strm, ecl_character c)
{
  write_char_increment_column(strm, c);
  ecl_string_push_extend(STRING_OUTPUT_STRING(strm), c);
  return c;
}

static cl_object
str_out_element_type(cl_object strm)
{
  cl_object string = STRING_OUTPUT_STRING(strm);
  if (ECL_BASE_STRING_P(string))
    return @'base-char';
  return @'character';
}

static cl_object
str_out_get_position(cl_object strm)
{
  return ecl_make_unsigned_integer(STRING_OUTPUT_STRING(strm)->base_string.fillp);
}

static cl_object
str_out_string_length(cl_object strm, cl_object string)
{
  cl_fixnum l = 0;
  switch (ecl_t_of(string)) {
#ifdef ECL_UNICODE
  case t_string:
#endif
  case t_base_string:
    l = string->base_string.fillp;
    break;
  case t_character:
    l = 1;
    break;
  default:
    FEwrong_type_nth_arg(@[file-string-length], 2, string, @[string]);
  }
  return ecl_make_fixnum(l);
}

static cl_object
str_out_set_position(cl_object strm, cl_object pos)
{
  cl_object string = STRING_OUTPUT_STRING(strm);
  cl_fixnum disp;
  if (Null(pos)) {
    disp = strm->base_string.dim;
  } else {
    disp = ecl_to_size(pos);
  }
  if (disp < string->base_string.fillp) {
    string->base_string.fillp = disp;
  } else {
    disp -= string->base_string.fillp;
    while (disp-- > 0)
      ecl_write_char(' ', strm);
  }
  return ECL_T;
}

const struct ecl_file_ops str_out_ops = {
  ecl_not_binary_read_byte8,
  ecl_not_output_write_byte8,

  ecl_not_input_read_byte,
  ecl_not_binary_write_byte,
  ecl_not_input_unread_byte,
  ecl_generic_peek_byte,

  ecl_not_input_read_char,
  str_out_write_char,
  ecl_not_input_unread_char,
  ecl_generic_peek_char,

  ecl_generic_read_vector,
  ecl_generic_write_vector,

  ecl_not_input_listen,
  ecl_not_input_clear_input,
  ecl_generic_void, /* clear-output */
  ecl_generic_void, /* finish-output */
  ecl_generic_void, /* force-output */

  ecl_generic_always_false, /* input_p */
  ecl_generic_always_true, /* output_p */
  ecl_generic_always_false,
  str_out_element_type,

  ecl_not_a_file_stream, /* length */
  str_out_get_position,
  str_out_set_position,
  str_out_string_length,
  ecl_generic_column,

  ecl_not_a_file_stream,
  ecl_not_a_file_stream,

  ecl_generic_close
};


cl_object
si_make_string_output_stream_from_string(cl_object s)
{
  cl_object strm = ecl_alloc_stream();
  unlikely_if (!ECL_STRINGP(s) || !ECL_ARRAY_HAS_FILL_POINTER_P(s))
    FEerror("~S is not a -string with a fill-pointer.", 1, s);
  strm->stream.ops = ecl_duplicate_dispatch_table(&str_out_ops);
  strm->stream.mode = (short)ecl_smm_string_output;
  STRING_OUTPUT_STRING(strm) = s;
  strm->stream.column = 0;
#if !defined(ECL_UNICODE)
  strm->stream.format = @':pass-through';
  strm->stream.flags = ECL_STREAM_DEFAULT_FORMAT;
  strm->stream.byte_size = 8;
#else
  if (ECL_BASE_STRING_P(s)) {
    strm->stream.format = @':latin-1';
    strm->stream.flags = ECL_STREAM_LATIN_1;
    strm->stream.byte_size = 8;
  } else {
    strm->stream.format = @':ucs-4';
    strm->stream.flags = ECL_STREAM_UCS_4;
    strm->stream.byte_size = 32;
  }
#endif
  @(return strm);
}

cl_object
ecl_make_string_output_stream(cl_index line_length, int extended)
{
#ifdef ECL_UNICODE
  cl_object s = extended?
    ecl_alloc_adjustable_extended_string(line_length) :
    ecl_alloc_adjustable_base_string(line_length);
#else
  cl_object s = ecl_alloc_adjustable_base_string(line_length);
#endif
  return si_make_string_output_stream_from_string(s);
}

@(defun make-string-output-stream (&key (element_type @'character'))
  int extended = 0;
@
  if (element_type == @'base-char') {
    (void)0;
  } else if (element_type == @'character') {
#ifdef ECL_UNICODE
    extended = 1;
#endif
  } else if (!Null(_ecl_funcall3(@'subtypep', element_type, @'base-char'))) {
    (void)0;
  } else if (!Null(_ecl_funcall3(@'subtypep', element_type, @'character'))) {
#ifdef ECL_UNICODE
    extended = 1;
#endif
  } else {
    FEerror("In MAKE-STRING-OUTPUT-STREAM, the argument :ELEMENT-TYPE (~A) must be a subtype of character",
            1, element_type);
  }
  @(return ecl_make_string_output_stream(128, extended));
@)

cl_object
cl_get_output_stream_string(cl_object strm)
{
  cl_object strng;
  unlikely_if (!ECL_ANSI_STREAM_TYPE_P(strm, ecl_smm_string_output))
    FEwrong_type_only_arg(@[get-output-stream-string],
                          strm, @[string-stream]);
  strng = cl_copy_seq(STRING_OUTPUT_STRING(strm));
  STRING_OUTPUT_STRING(strm)->base_string.fillp = 0;
  @(return strng);
}

/**********************************************************************
 * STRING INPUT STREAMS
 */

static ecl_character
str_in_read_char(cl_object strm)
{
  cl_fixnum curr_pos = STRING_INPUT_POSITION(strm);
  ecl_character c;
  if (curr_pos >= STRING_INPUT_LIMIT(strm)) {
    c = EOF;
  } else {
    c = ecl_char(STRING_INPUT_STRING(strm), curr_pos);
    STRING_INPUT_POSITION(strm) = curr_pos+1;
  }
  return c;
}

static void
str_in_unread_char(cl_object strm, ecl_character c)
{
  cl_fixnum curr_pos = STRING_INPUT_POSITION(strm);
  unlikely_if (curr_pos <= 0) {
    ecl_unread_error(strm);
  }
  STRING_INPUT_POSITION(strm) = curr_pos - 1;
}

static ecl_character
str_in_peek_char(cl_object strm)
{
  cl_index pos = STRING_INPUT_POSITION(strm);
  if (pos >= STRING_INPUT_LIMIT(strm)) {
    return EOF;
  } else {
    return ecl_char(STRING_INPUT_STRING(strm), pos);
  }
}

static int
str_in_listen(cl_object strm)
{
  if (STRING_INPUT_POSITION(strm) < STRING_INPUT_LIMIT(strm))
    return ECL_LISTEN_AVAILABLE;
  else
    return ECL_LISTEN_EOF;
}

static cl_object
str_in_element_type(cl_object strm)
{
  cl_object string = STRING_INPUT_STRING(strm);
  if (ECL_BASE_STRING_P(string))
    return @'base-char';
  return @'character';
}

static cl_object
str_in_get_position(cl_object strm)
{
  return ecl_make_unsigned_integer(STRING_INPUT_POSITION(strm));
}

static cl_object
str_in_set_position(cl_object strm, cl_object pos)
{
  cl_fixnum disp;
  if (Null(pos)) {
    disp = STRING_INPUT_LIMIT(strm);
  }  else {
    disp = ecl_to_size(pos);
    if (disp >= STRING_INPUT_LIMIT(strm)) {
      disp = STRING_INPUT_LIMIT(strm);
    }
  }
  STRING_INPUT_POSITION(strm) = disp;
  return ECL_T;
}

const struct ecl_file_ops str_in_ops = {
  ecl_not_binary_read_byte8,
  ecl_not_output_write_byte8,

  ecl_not_binary_read_byte,
  ecl_not_output_write_byte,
  ecl_not_binary_write_byte,
  ecl_not_binary_read_byte,

  str_in_read_char,
  ecl_not_output_write_char,
  str_in_unread_char,
  str_in_peek_char,

  ecl_generic_read_vector,
  ecl_generic_write_vector,

  str_in_listen,
  ecl_generic_void, /* clear-input */
  ecl_not_output_clear_output,
  ecl_not_output_finish_output,
  ecl_not_output_force_output,

  ecl_generic_always_true, /* input_p */
  ecl_generic_always_false, /* output_p */
  ecl_generic_always_false,
  str_in_element_type,

  ecl_not_a_file_stream, /* length */
  str_in_get_position,
  str_in_set_position,
  ecl_not_output_string_length,
  ecl_unknown_column,

  ecl_not_a_file_stream,
  ecl_not_a_file_stream,

  ecl_generic_close
};

cl_object
ecl_make_string_input_stream(cl_object strng, cl_index istart, cl_index iend)
{
  cl_object strm;

  strm = ecl_alloc_stream();
  strm->stream.ops = ecl_duplicate_dispatch_table(&str_in_ops);
  strm->stream.mode = (short)ecl_smm_string_input;
  STRING_INPUT_STRING(strm) = strng;
  STRING_INPUT_POSITION(strm) = istart;
  STRING_INPUT_LIMIT(strm) = iend;
#if !defined(ECL_UNICODE)
  strm->stream.format = @':pass-through';
  strm->stream.flags = ECL_STREAM_DEFAULT_FORMAT;
  strm->stream.byte_size = 8;
#else
  if (ECL_BASE_STRING_P(strng)) {
    strm->stream.format = @':latin-1';
    strm->stream.flags = ECL_STREAM_LATIN_1;
    strm->stream.byte_size = 8;
  } else {
    strm->stream.format = @':ucs-4';
    strm->stream.flags = ECL_STREAM_UCS_4;
    strm->stream.byte_size = 32;
  }
#endif
  return strm;
}

@(defun make_string_input_stream (strng &o (istart ecl_make_fixnum(0)) iend)
  cl_index_pair p;
@
  strng = cl_string(strng);
  p = ecl_vector_start_end(@[make-string-input-stream], strng, istart, iend);
  @(return (ecl_make_string_input_stream(strng, p.start, p.end)));
@)
