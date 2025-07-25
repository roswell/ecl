/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * strm_common.d - common functions and helpers for streams
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
 * NOT A #<stream-type> STREAM
 */

cl_object
ecl_not_a_file_stream(cl_object strm)
{
  cl_error(9, @'simple-type-error', @':format-control',
           @"~A is not an file stream",
           @':format-arguments', cl_list(1, strm),
           @':expected-type', @'file-stream',
           @':datum', strm);
}

void
ecl_not_an_input_stream(cl_object strm)
{
  cl_error(9, @'simple-type-error', @':format-control',
           @"~A is not an input stream",
           @':format-arguments', cl_list(1, strm),
           @':expected-type',
           cl_list(2, @'satisfies', @'input-stream-p'),
           @':datum', strm);
}

void
ecl_not_an_output_stream(cl_object strm)
{
  cl_error(9, @'simple-type-error', @':format-control',
           @"~A is not an output stream",
           @':format-arguments', cl_list(1, strm),
           @':expected-type', cl_list(2, @'satisfies', @'output-stream-p'),
           @':datum', strm);
}

static void
not_a_character_stream(cl_object s)
{
  cl_error(9, @'simple-type-error', @':format-control',
           @"~A is not a character stream",
           @':format-arguments', cl_list(1, s),
           @':expected-type', @'character',
           @':datum', cl_stream_element_type(s));
}

static void
not_a_binary_stream(cl_object s)
{
  cl_error(9, @'simple-type-error', @':format-control',
           @"~A is not a binary stream",
           @':format-arguments', cl_list(1, s),
           @':expected-type', @'integer',
           @':datum', cl_stream_element_type(s));
}

/**********************************************************************
 * NOT IMPLEMENTED or NOT APPLICABLE OPERATIONS
 */

cl_index
ecl_not_output_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  ecl_not_an_output_stream(strm);
  return 0;
}

cl_index
ecl_not_input_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  ecl_not_an_input_stream(strm);
  return 0;
}

cl_index
ecl_not_binary_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  not_a_binary_stream(strm);
  return 0;
}

void
ecl_not_output_write_byte(cl_object strm, cl_object byte)
{
  ecl_not_an_output_stream(strm);
}

cl_object
ecl_not_input_read_byte(cl_object strm)
{
  ecl_not_an_input_stream(strm);
  return OBJNULL;
}

void
ecl_not_binary_write_byte(cl_object strm, cl_object byte)
{
  not_a_binary_stream(strm);
}

cl_object
ecl_not_binary_read_byte(cl_object strm)
{
  not_a_binary_stream(strm);
  return OBJNULL;
}

void
ecl_not_input_unread_byte(cl_object strm, cl_object byte)
{
  ecl_not_an_input_stream(strm);
}

ecl_character
ecl_not_input_read_char(cl_object strm)
{
  ecl_not_an_input_stream(strm);
  return -1;
}

ecl_character
ecl_not_output_write_char(cl_object strm, ecl_character c)
{
  ecl_not_an_output_stream(strm);
  return c;
}

void
ecl_not_input_unread_char(cl_object strm, ecl_character c)
{
  ecl_not_an_input_stream(strm);
}

int
ecl_not_input_listen(cl_object strm)
{
  ecl_not_an_input_stream(strm);
  return -1;
}

ecl_character
ecl_not_character_read_char(cl_object strm)
{
  not_a_character_stream(strm);
  return -1;
}

ecl_character
ecl_not_character_write_char(cl_object strm, ecl_character c)
{
  not_a_character_stream(strm);
  return c;
}

ecl_character
ecl_not_character_decoder(cl_object stream, unsigned char **buffer, unsigned char *buffer_end)
{
  not_a_character_stream(stream);
  return EOF;
}

int
ecl_not_character_encoder(cl_object stream, unsigned char *buffer, ecl_character c)
{
  not_a_character_stream(stream);
  return 0;
}

void
ecl_not_input_clear_input(cl_object strm)
{
  ecl_not_an_input_stream(strm);
  return;
}

void
ecl_not_output_clear_output(cl_object strm)
{
  ecl_not_an_output_stream(strm);
}

void
ecl_not_output_force_output(cl_object strm)
{
  ecl_not_an_output_stream(strm);
}

void
ecl_not_output_finish_output(cl_object strm)
{
  ecl_not_an_output_stream(strm);
}

cl_object
ecl_not_output_string_length(cl_object strm, cl_object string)
{
  ecl_not_an_output_stream(strm);
  return 0;
}

cl_object
ecl_not_file_string_length(cl_object strm, cl_object string)
{
  ecl_not_a_file_stream(strm);
  return 0;
}

int
ecl_unknown_column(cl_object strm)
{
  return -1;
}

/**********************************************************************
 * CLOSED STREAM OPS
 */

static cl_index
closed_stream_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  FEclosed_stream(strm);
  return 0;
}

static cl_index
closed_stream_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  FEclosed_stream(strm);
  return 0;
}

static cl_object
closed_stream_read_byte(cl_object strm)
{
  FEclosed_stream(strm);
  return ECL_NIL;
}

static void
closed_stream_write_byte(cl_object strm, cl_object byte)
{
  FEclosed_stream(strm);
}

static void
closed_stream_unread_byte(cl_object strm, cl_object byte)
{
  FEclosed_stream(strm);
}

static ecl_character
closed_stream_read_char(cl_object strm)
{
  FEclosed_stream(strm);
  return 0;
}

static ecl_character
closed_stream_write_char(cl_object strm, ecl_character c)
{
  FEclosed_stream(strm);
  return c;
}

static void
closed_stream_unread_char(cl_object strm, ecl_character c)
{
  FEclosed_stream(strm);
}

static int
closed_stream_listen(cl_object strm)
{
  FEclosed_stream(strm);
  return 0;
}

static void
closed_stream_clear_input(cl_object strm)
{
  FEclosed_stream(strm);
}

#define closed_stream_clear_output closed_stream_clear_input
#define closed_stream_force_output closed_stream_clear_input
#define closed_stream_finish_output closed_stream_clear_input

static cl_object
closed_stream_length(cl_object strm)
{
  FEclosed_stream(strm);
}

#define closed_stream_get_position closed_stream_length

static cl_object
closed_stream_set_position(cl_object strm, cl_object position)
{
  FEclosed_stream(strm);
}

/**********************************************************************
 * GENERIC OPERATIONS
 *
 * Versions of the methods which are defined in terms of others
 */

cl_object
ecl_generic_peek_byte(cl_object strm)
{
  cl_object out = ecl_read_byte(strm);
  if (out != OBJNULL) ecl_unread_byte(out, strm);
  return out;
}

ecl_character
ecl_generic_peek_char(cl_object strm)
{
  ecl_character out = ecl_read_char(strm);
  if (out != EOF) ecl_unread_char(out, strm);
  return out;
}

void
ecl_generic_void(cl_object strm)
{
}

int
ecl_generic_always_true(cl_object strm)
{
  return 1;
}

int
ecl_generic_always_false(cl_object strm)
{
  return 0;
}

cl_object
ecl_generic_always_nil(cl_object strm)
{
  return ECL_NIL;
}

int
ecl_generic_column(cl_object strm)
{
  return strm->stream.column;
}

cl_object
ecl_generic_set_position(cl_object strm, cl_object pos)
{
  return ECL_NIL;
}

cl_object
ecl_generic_close(cl_object strm)
{
  struct ecl_file_ops *ops = strm->stream.ops;
  if (ecl_input_stream_p(strm)) {
    ops->read_byte8 = closed_stream_read_byte8;
    ops->read_byte = closed_stream_read_byte;
    ops->peek_byte = closed_stream_read_byte;
    ops->unread_byte = closed_stream_unread_byte;
    ops->read_char = closed_stream_read_char;
    ops->peek_char = closed_stream_read_char;
    ops->unread_char = closed_stream_unread_char;
    ops->listen = closed_stream_listen;
    ops->clear_input = closed_stream_clear_input;
  }
  if (ecl_output_stream_p(strm)) {
    ops->write_byte8 = closed_stream_write_byte8;
    ops->write_byte = closed_stream_write_byte;
    ops->write_char = closed_stream_write_char;
    ops->clear_output = closed_stream_clear_output;
    ops->force_output = closed_stream_force_output;
    ops->finish_output = closed_stream_finish_output;
  }
  ops->get_position = closed_stream_get_position;
  ops->set_position = closed_stream_set_position;
  ops->length = closed_stream_length;
  ops->close = ecl_generic_close;
  strm->stream.last_byte = OBJNULL;
  strm->stream.byte_buffer = NULL;
  strm->stream.closed = 1;
  return ECL_T;
}

cl_index
ecl_generic_write_vector(cl_object strm, cl_object data, cl_index start, cl_index end)
{
  cl_elttype elttype;
  const struct ecl_file_ops *ops;
  if (start >= end)
    return start;
  ops = ecl_stream_dispatch_table(strm);
  elttype = ecl_array_elttype(data);
  if (elttype == ecl_aet_bc ||
#ifdef ECL_UNICODE
      elttype == ecl_aet_ch ||
#endif
      (elttype == ecl_aet_object && ECL_CHARACTERP(ecl_elt(data, 0)))) {
    ecl_character (*write_char)(cl_object, ecl_character) = ops->write_char;                        
    for (; start < end; start++) {
      write_char(strm, ecl_char_code(ecl_elt(data, start)));
    }
  } else {
    void (*write_byte)(cl_object, cl_object) = ops->write_byte;
    for (; start < end; start++) {
      write_byte(strm, ecl_elt(data, start));
    }
  }
  return start;
}

cl_index
ecl_generic_read_vector(cl_object strm, cl_object data, cl_index start, cl_index end)
{
  const struct ecl_file_ops *ops;
  cl_object expected_type;
  if (start >= end)
    return start;
  expected_type = ecl_stream_element_type(strm);
  ops = ecl_stream_dispatch_table(strm);
  if (expected_type == @'base-char' || expected_type == @'character') {
    ecl_character (*read_char)(cl_object) = ops->read_char;
    for (; start < end; start++) {
      ecl_character c = read_char(strm);
      if (c == EOF) break;
      ecl_elt_set(data, start, ECL_CODE_CHAR(c));
    }
  } else {
    cl_object (*read_byte)(cl_object) = ops->read_byte;
    for (; start < end; start++) {
      cl_object x = read_byte(strm);
      if (x == OBJNULL) break;
      ecl_elt_set(data, start, x);
    }
  }
  return start;
}

