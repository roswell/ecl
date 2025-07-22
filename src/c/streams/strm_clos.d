/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * strm_clos.d - Gray Streams dispatch table
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

#ifdef ECL_CLOS_STREAMS
static cl_index
clos_stream_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  cl_index i;
  for (i = 0; i < n; i++) {
    cl_object byte = _ecl_funcall2(@'gray::stream-read-byte', strm);
    if (!ECL_FIXNUMP(byte))
      break;
    c[i] = ecl_fixnum(byte);
  }
  return i;
}

static cl_index
clos_stream_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  cl_index i;
  for (i = 0; i < n; i++) {
    cl_object byte = _ecl_funcall3(@'gray::stream-write-byte', strm,
                                   ecl_make_fixnum(c[i]));
    if (!ECL_FIXNUMP(byte))
      break;
  }
  return i;
}

static cl_object
clos_stream_read_byte(cl_object strm)
{
  cl_object out = _ecl_funcall2(@'gray::stream-read-byte', strm);
  if (out == @':eof') out = OBJNULL;
  return out;
}

static void
clos_stream_write_byte(cl_object strm, cl_object byte)
{
  _ecl_funcall3(@'gray::stream-write-byte', strm, byte);
}

static void
clos_stream_unread_byte(cl_object strm, cl_object byte)
{
  _ecl_funcall3(@'gray::stream-unread-byte', strm, byte);
}

static cl_object
clos_stream_peek_byte(cl_object strm)
{
  cl_object out = _ecl_funcall2(@'gray::stream-peek-byte', strm);
  if (out == @':eof') return OBJNULL;
  return out;
}

static ecl_character
clos_stream_read_char(cl_object strm)
{
  cl_object output = _ecl_funcall2(@'gray::stream-read-char', strm);
  cl_fixnum value;
  if (ECL_CHARACTERP(output))
    value = ECL_CHAR_CODE(output);
  else if (ECL_FIXNUMP(output))
    value = ecl_fixnum(output);
  else if (output == ECL_NIL || output == @':eof')
    return EOF;
  else
    value = -1;
  unlikely_if (value < 0 || value > ECL_CHAR_CODE_LIMIT)
    FEerror("Unknown character ~A", 1, output);
  return value;
}

static ecl_character
clos_stream_write_char(cl_object strm, ecl_character c)
{
  _ecl_funcall3(@'gray::stream-write-char', strm, ECL_CODE_CHAR(c));
  return c;
}

static void
clos_stream_unread_char(cl_object strm, ecl_character c)
{
  _ecl_funcall3(@'gray::stream-unread-char', strm, ECL_CODE_CHAR(c));
}

static ecl_character
clos_stream_peek_char(cl_object strm)
{
  cl_object out = _ecl_funcall2(@'gray::stream-peek-char', strm);
  if (out == @':eof') return EOF;
  return ecl_char_code(out);
}

static cl_index
clos_stream_read_vector(cl_object strm, cl_object data, cl_index start, cl_index end)
{
  return fixnnint(_ecl_funcall5(@'gray::stream-read-sequence', strm, data, ecl_make_fixnum(start), ecl_make_fixnum(end)));
}

static cl_index
clos_stream_write_vector(cl_object strm, cl_object data, cl_index start, cl_index end)
{
  _ecl_funcall5(@'gray::stream-write-sequence', strm, data, ecl_make_fixnum(start), ecl_make_fixnum(end));
  if (start >= end)
    return start;
  return end;
}

static int
clos_stream_listen(cl_object strm)
{
  return !Null(_ecl_funcall2(@'gray::stream-listen', strm));
}

static void
clos_stream_clear_input(cl_object strm)
{
  _ecl_funcall2(@'gray::stream-clear-input', strm);
}

static void
clos_stream_clear_output(cl_object strm)
{
  _ecl_funcall2(@'gray::stream-clear-output', strm);
  return;
}

static void
clos_stream_force_output(cl_object strm)
{
  _ecl_funcall2(@'gray::stream-force-output', strm);
}

static void
clos_stream_finish_output(cl_object strm)
{
  _ecl_funcall2(@'gray::stream-finish-output', strm);
}

static int
clos_stream_input_p(cl_object strm)
{
  return !Null(_ecl_funcall2(@'gray::input-stream-p', strm));
}

static int
clos_stream_output_p(cl_object strm)
{
  return !Null(_ecl_funcall2(@'gray::output-stream-p', strm));
}

static int
clos_stream_interactive_p(cl_object strm)
{
  return !Null(_ecl_funcall2(@'gray::stream-interactive-p', strm));

}

static cl_object
clos_stream_element_type(cl_object strm)
{
  return _ecl_funcall2(@'gray::stream-element-type', strm);
}

static cl_object
clos_stream_length(cl_object strm)
{
  return _ecl_funcall2(@'gray::stream-file-length', strm);
}

static cl_object
clos_stream_get_position(cl_object strm)
{
  return _ecl_funcall2(@'gray::stream-file-position', strm);
}

static cl_object
clos_stream_set_position(cl_object strm, cl_object pos)
{
  return _ecl_funcall3(@'gray::stream-file-position', strm, pos);
}

static cl_object
clos_stream_string_length(cl_object strm, cl_object string)
{
  return _ecl_funcall3(@'gray::stream-file-string-length', strm, string);
}

static int
clos_stream_column(cl_object strm)
{
  cl_object col = _ecl_funcall2(@'gray::stream-line-column', strm);
  return Null(col)? -1 : ecl_to_size(ecl_floor1(col));
}

static cl_object
clos_stream_pathname(cl_object strm)
{
  return _ecl_funcall2(@'gray::pathname', strm);
}

static cl_object
clos_stream_truename(cl_object strm)
{
  return _ecl_funcall2(@'gray::truename', strm);
}

static cl_object
clos_stream_close(cl_object strm)
{
  return _ecl_funcall2(@'gray::close', strm);
}

const struct ecl_file_ops clos_stream_ops = {
  clos_stream_read_byte8,
  clos_stream_write_byte8,

  clos_stream_read_byte,
  clos_stream_write_byte,
  clos_stream_unread_byte,
  clos_stream_peek_byte,

  clos_stream_read_char,
  clos_stream_write_char,
  clos_stream_unread_char,
  clos_stream_peek_char,

  clos_stream_read_vector,
  clos_stream_write_vector,

  clos_stream_listen,
  clos_stream_clear_input,
  clos_stream_clear_output,
  clos_stream_finish_output,
  clos_stream_force_output,

  clos_stream_input_p,
  clos_stream_output_p,
  clos_stream_interactive_p,
  clos_stream_element_type,

  clos_stream_length,
  clos_stream_get_position,
  clos_stream_set_position,
  clos_stream_string_length,
  clos_stream_column,

  clos_stream_pathname,
  clos_stream_truename,

  clos_stream_close
};
#endif /* ECL_CLOS_STREAMS */
