/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * stream.d - stream interface
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 * Copyright (c) 2025 Daniel Kochmanski
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/* -- imports --------------------------------------------------------------- */

#include <ecl/ecl.h>
#include <ecl/internal.h>

#ifdef ECL_CLOS_STREAMS
extern const struct ecl_file_ops clos_stream_ops;
#endif

/* -- implementation -------------------------------------------------------- */

cl_object
ecl_alloc_stream(void)
{
  cl_object x = ecl_alloc_object(t_stream);
  x->stream.closed = 0;
  x->stream.file.descriptor = -1;
  x->stream.object0 =
    x->stream.object1 = OBJNULL;
  x->stream.int0 = x->stream.int1 = 0;
  x->stream.format = ECL_NIL;
  x->stream.flags = 0;
  x->stream.byte_size = 8;
  x->stream.last_byte = OBJNULL;
  x->stream.buffer = NULL;
  x->stream.encoder = NULL;
  x->stream.decoder = NULL;
  x->stream.byte_buffer = NULL;
  x->stream.byte_encoder = NULL;
  x->stream.byte_decoder = NULL;
  x->stream.last_char = EOF;
  x->stream.byte_stack = ECL_NIL;
  x->stream.eof_char = EOF;
  return x;
}

struct ecl_file_ops *
ecl_duplicate_dispatch_table(const struct ecl_file_ops *ops)
{
  struct ecl_file_ops *new_ops = ecl_alloc_atomic(sizeof(*ops));
  *new_ops = *ops;
  return new_ops;
}

const struct ecl_file_ops *
ecl_stream_dispatch_table(cl_object strm)
{
#ifdef ECL_CLOS_STREAMS
  if (ECL_INSTANCEP(strm)) {
    return &clos_stream_ops;
  }
#endif
  if (!ECL_ANSI_STREAM_P(strm))
    FEwrong_type_argument(@[stream], strm);
  return (const struct ecl_file_ops *)strm->stream.ops;
}

cl_index
ecl_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  return ecl_stream_dispatch_table(strm)->read_byte8(strm, c, n);
}

cl_index
ecl_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  return ecl_stream_dispatch_table(strm)->write_byte8(strm, c, n);
}

cl_object
ecl_read_byte(cl_object strm)
{
  return ecl_stream_dispatch_table(strm)->read_byte(strm);
}

void
ecl_write_byte(cl_object byte, cl_object strm)
{
  ecl_stream_dispatch_table(strm)->write_byte(strm, byte);
}

void
ecl_unread_byte(cl_object byte, cl_object strm)
{
  ecl_stream_dispatch_table(strm)->unread_byte(strm, byte);
}

cl_object
ecl_peek_byte(cl_object strm)
{
  return ecl_stream_dispatch_table(strm)->peek_byte(strm);
}

ecl_character
ecl_read_char(cl_object strm)
{
  return ecl_stream_dispatch_table(strm)->read_char(strm);
}

ecl_character
ecl_read_char_noeof(cl_object strm)
{
  ecl_character c = ecl_read_char(strm);
  if (c == EOF)
    FEend_of_file(strm);
  return c;
}

ecl_character
ecl_write_char(ecl_character c, cl_object strm)
{
  return ecl_stream_dispatch_table(strm)->write_char(strm, c);
}

void
ecl_unread_char(ecl_character c, cl_object strm)
{
  ecl_stream_dispatch_table(strm)->unread_char(strm, c);
}

ecl_character
ecl_peek_char(cl_object strm)
{
  return ecl_stream_dispatch_table(strm)->peek_char(strm);
}

int
ecl_listen_stream(cl_object strm)
{
  return ecl_stream_dispatch_table(strm)->listen(strm);
}

void
ecl_clear_input(cl_object strm)
{
  ecl_stream_dispatch_table(strm)->clear_input(strm);
}

void
ecl_clear_output(cl_object strm)
{
  ecl_stream_dispatch_table(strm)->clear_output(strm);
}

void
ecl_force_output(cl_object strm)
{
  ecl_stream_dispatch_table(strm)->force_output(strm);
}

void
ecl_finish_output(cl_object strm)
{
  ecl_stream_dispatch_table(strm)->finish_output(strm);
}

int
ecl_file_column(cl_object strm)
{
  return ecl_stream_dispatch_table(strm)->column(strm);
}

cl_object
ecl_file_length(cl_object strm)
{
  return ecl_stream_dispatch_table(strm)->length(strm);
}

cl_object
ecl_file_position(cl_object strm)
{
  return ecl_stream_dispatch_table(strm)->get_position(strm);
}

cl_object
ecl_file_position_set(cl_object strm, cl_object pos)
{
  return ecl_stream_dispatch_table(strm)->set_position(strm, pos);
}

cl_object
ecl_file_string_length(cl_object strm, cl_object string)
{
  return ecl_stream_dispatch_table(strm)->string_length(strm, string);
}

bool
ecl_input_stream_p(cl_object strm)
{
  return ecl_stream_dispatch_table(strm)->input_p(strm);
}

bool
ecl_output_stream_p(cl_object strm)
{
  return ecl_stream_dispatch_table(strm)->output_p(strm);
}

cl_object
ecl_stream_element_type(cl_object strm)
{
  return ecl_stream_dispatch_table(strm)->element_type(strm);
}

bool
ecl_interactive_stream_p(cl_object strm)
{
  return ecl_stream_dispatch_table(strm)->interactive_p(strm);
}

cl_object
ecl_stream_pathname(cl_object strm)
{
  return ecl_stream_dispatch_table(strm)->pathname(strm);
}

cl_object
ecl_stream_truename(cl_object strm)
{
  return ecl_stream_dispatch_table(strm)->truename(strm);
}

/* -- Lisp interface -------------------------------------------------------- */

cl_object
si_read_char(cl_object strm, cl_object eof_value)
{
  cl_env_ptr the_env = ecl_process_env();
  ecl_character c = ecl_read_char(strm);
  ecl_return1(the_env, (c==EOF) ? eof_value : ECL_CODE_CHAR(c));
}

cl_object
si_unread_char(cl_object strm, cl_object c)
{
  cl_env_ptr the_env = ecl_process_env();
  ecl_unread_char(ecl_char_code(c), strm);
  ecl_return1(the_env, ECL_NIL);
}

cl_object
si_peek_char(cl_object strm, cl_object eof_value)
{
  cl_env_ptr the_env = ecl_process_env();
  ecl_character c = ecl_peek_char(strm);
  ecl_return1(the_env, (c==EOF)? eof_value : ECL_CODE_CHAR(c));
}

cl_object
si_write_char(cl_object strm, cl_object c)
{
  cl_env_ptr the_env = ecl_process_env();
  ecl_write_char(ecl_char_code(c), strm);
  ecl_return1(the_env, c);
}

cl_object
si_read_byte(cl_object strm, cl_object eof_value)
{
  cl_env_ptr the_env = ecl_process_env();
  cl_object byte = ecl_read_byte(strm);
  ecl_return1(the_env, (byte == OBJNULL) ? eof_value : byte);
}

cl_object
si_unread_byte(cl_object strm, cl_object byte)
{
  cl_env_ptr the_env = ecl_process_env();
  ecl_unread_byte(byte, strm);
  ecl_return1(the_env, ECL_NIL);
}

cl_object
si_peek_byte(cl_object strm, cl_object eof_value)
{
  cl_env_ptr the_env = ecl_process_env();
  cl_object byte = ecl_peek_byte(strm);
  ecl_return1(the_env, (byte == OBJNULL) ? eof_value : byte);
}

cl_object
si_write_byte(cl_object strm, cl_object byte)
{
  cl_env_ptr the_env = ecl_process_env();
  ecl_write_byte(byte, strm);
  ecl_return1(the_env, byte);
}

cl_object
si_listen(cl_object strm)
{
  cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ((ecl_listen_stream(strm) == ECL_LISTEN_AVAILABLE)
                        ? ECL_T : ECL_NIL));
}

cl_object
si_clear_input(cl_object strm)
{
  cl_env_ptr the_env = ecl_process_env();
  ecl_clear_input(strm);
  ecl_return1(the_env, ECL_NIL);
}

cl_object
si_finish_output(cl_object strm)
{
  cl_env_ptr the_env = ecl_process_env();
  ecl_finish_output(strm);
  ecl_return1(the_env, ECL_NIL);
}

cl_object
si_force_output(cl_object strm)
{
  cl_env_ptr the_env = ecl_process_env();
  ecl_force_output(strm);
  ecl_return1(the_env, ECL_NIL);
}

cl_object
si_clear_output(cl_object strm)
{
  cl_env_ptr the_env = ecl_process_env();
  ecl_clear_output(strm);
  ecl_return1(the_env, ECL_NIL);
}

cl_object
si_file_column(cl_object strm)
{
  int column = ecl_file_column(strm);
  @(return (column >= 0 ? ecl_make_fixnum(column) : ECL_NIL));
}

cl_object
cl_file_length(cl_object strm)
{
  @(return ecl_file_length(strm));
}

@(defun file-position (file_stream &o position)
  cl_object output;
@
  if (Null(position)) {
    output = ecl_file_position(file_stream);
  } else {
    if (position == @':start') {
      position = ecl_make_fixnum(0);
    } else if (position == @':end') {
      position = ECL_NIL;
    }
    output = ecl_file_position_set(file_stream, position);
  }
  @(return output);
@)

cl_object
cl_file_string_length(cl_object stream, cl_object string)
{
  @(return ecl_file_string_length(stream, string));
}

cl_object
cl_input_stream_p(cl_object strm)
{
  @(return (ecl_input_stream_p(strm) ? ECL_T : ECL_NIL));
}

cl_object
cl_output_stream_p(cl_object strm)
{
  @(return (ecl_output_stream_p(strm) ? ECL_T : ECL_NIL));
}

cl_object
cl_interactive_stream_p(cl_object strm)
{
  @(return (ecl_stream_dispatch_table(strm)->interactive_p(strm)? ECL_T : ECL_NIL));
}

cl_object
cl_open_stream_p(cl_object strm)
{
  /* ANSI and Cltl2 specify that open-stream-p should work
     on closed streams, and that a stream is only closed
     when #'close has been applied on it */
#ifdef ECL_CLOS_STREAMS
  if (ECL_INSTANCEP(strm)) {
    return _ecl_funcall2(@'gray::open-stream-p', strm);
  }
#endif
  unlikely_if (!ECL_ANSI_STREAM_P(strm))
    FEwrong_type_only_arg(@'open-stream-p', strm, @'stream');
  @(return (strm->stream.closed ? ECL_NIL : ECL_T));
}

cl_object
cl_stream_element_type(cl_object strm)
{
  @(return ecl_stream_element_type(strm));
}

cl_object
cl_stream_external_format(cl_object strm)
{
  cl_object output;
  cl_type t;
 AGAIN:
  t= ecl_t_of(strm);
#ifdef ECL_CLOS_STREAMS
  if (t == t_instance)
    output = @':default';
  else
#endif
    unlikely_if (t != t_stream)
      FEwrong_type_only_arg(@[stream-external-format], strm, @[stream]);
  if (strm->stream.mode == ecl_smm_synonym) {
    strm = SYNONYM_STREAM_STREAM(strm);
    goto AGAIN;
  }
  output = strm->stream.format;
  @(return output);
}

cl_object
cl_streamp(cl_object strm)
{
#ifdef ECL_CLOS_STREAMS
  if (ECL_INSTANCEP(strm)) {
    return _ecl_funcall2(@'gray::streamp', strm);
  }
#endif
  @(return (ECL_ANSI_STREAM_P(strm) ? ECL_T : ECL_NIL));
}

/* -- Miscellaneous --------------------------------------------------------- */

cl_object
si_copy_stream(cl_object in, cl_object out, cl_object wait)
{
  ecl_character c;
  if ((wait == ECL_NIL) && !ecl_listen_stream(in)) {
    return ECL_NIL;
  }
  for (c = ecl_read_char(in); c != EOF; c = ecl_read_char(in)) {
    ecl_write_char(c, out);
    if ((wait == ECL_NIL) && !ecl_listen_stream(in)) {
      break;
    }
  }
  ecl_force_output(out);
  @(return ((c==EOF) ? ECL_T : ECL_NIL));
}

cl_object
si_file_stream_fd(cl_object s)
{
  cl_object ret;

  unlikely_if (!ECL_FILE_STREAM_P(s)) {
    ecl_not_a_file_stream(s);
  }

  switch ((enum ecl_smmode)s->stream.mode) {
  case ecl_smm_input:
  case ecl_smm_output:
  case ecl_smm_io:
    ret = ecl_make_fixnum(fileno(IO_STREAM_FILE(s)));
    break;
  case ecl_smm_input_file:
  case ecl_smm_output_file:
  case ecl_smm_io_file:
    ret = ecl_make_fixnum(IO_FILE_DESCRIPTOR(s));
    break;
  default:
    ecl_internal_error("not a file stream");
  }
  @(return ret);
}
