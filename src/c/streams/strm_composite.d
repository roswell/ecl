/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * strm_composite.d - Composite Streams dispatch tables
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
 * TWO WAY STREAM
 */

static cl_index
two_way_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  if (strm == cl_core.terminal_io)
    ecl_force_output(TWO_WAY_STREAM_OUTPUT(cl_core.terminal_io));
  return ecl_read_byte8(TWO_WAY_STREAM_INPUT(strm), c, n);
}

static cl_index
two_way_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  return ecl_write_byte8(TWO_WAY_STREAM_OUTPUT(strm), c, n);
}

static void
two_way_write_byte(cl_object strm, cl_object byte)
{
  ecl_write_byte(byte, TWO_WAY_STREAM_OUTPUT(strm));
}

static cl_object
two_way_read_byte(cl_object stream)
{
  return ecl_read_byte(TWO_WAY_STREAM_INPUT(stream));
}

static void
two_way_unread_byte(cl_object strm, cl_object byte)
{
  ecl_unread_byte(byte, TWO_WAY_STREAM_INPUT(strm));
}

static cl_object
two_way_peek_byte(cl_object strm)
{
  return ecl_peek_byte(TWO_WAY_STREAM_INPUT(strm));
}

static ecl_character
two_way_read_char(cl_object strm)
{
  return ecl_read_char(TWO_WAY_STREAM_INPUT(strm));
}

static ecl_character
two_way_write_char(cl_object strm, ecl_character c)
{
  return ecl_write_char(c, TWO_WAY_STREAM_OUTPUT(strm));
}

static void
two_way_unread_char(cl_object strm, ecl_character c)
{
  ecl_unread_char(c, TWO_WAY_STREAM_INPUT(strm));
}

static ecl_character
two_way_peek_char(cl_object strm)
{
  return ecl_peek_char(TWO_WAY_STREAM_INPUT(strm));
}

static cl_index
two_way_read_vector(cl_object strm, cl_object data, cl_index start, cl_index n)
{
  strm = TWO_WAY_STREAM_INPUT(strm);
  return ecl_stream_dispatch_table(strm)->read_vector(strm, data, start, n);
}

static cl_index
two_way_write_vector(cl_object strm, cl_object data, cl_index start, cl_index n)
{
  strm = TWO_WAY_STREAM_OUTPUT(strm);
  return ecl_stream_dispatch_table(strm)->write_vector(strm, data, start, n);
}

static int
two_way_listen(cl_object strm)
{
  return ecl_listen_stream(TWO_WAY_STREAM_INPUT(strm));
}

static void
two_way_clear_input(cl_object strm)
{
  ecl_clear_input(TWO_WAY_STREAM_INPUT(strm));
}

static void
two_way_clear_output(cl_object strm)
{
  ecl_clear_output(TWO_WAY_STREAM_OUTPUT(strm));
}

static void
two_way_force_output(cl_object strm)
{
  ecl_force_output(TWO_WAY_STREAM_OUTPUT(strm));
}

static void
two_way_finish_output(cl_object strm)
{
  ecl_finish_output(TWO_WAY_STREAM_OUTPUT(strm));
}

static int
two_way_interactive_p(cl_object strm)
{
  return ecl_interactive_stream_p(TWO_WAY_STREAM_INPUT(strm));
}

static cl_object
two_way_element_type(cl_object strm)
{
  return ecl_stream_element_type(TWO_WAY_STREAM_INPUT(strm));
}

static int
two_way_column(cl_object strm)
{
  return ecl_file_column(TWO_WAY_STREAM_OUTPUT(strm));
}

static cl_object
two_way_close(cl_object strm)
{
  if (strm->stream.flags & ECL_STREAM_CLOSE_COMPONENTS) {
    cl_close(1, TWO_WAY_STREAM_INPUT(strm));
    cl_close(1, TWO_WAY_STREAM_OUTPUT(strm));
  }
  return ecl_generic_close(strm);
}

const struct ecl_file_ops two_way_ops = {
  two_way_read_byte8,
  two_way_write_byte8,

  two_way_read_byte,
  two_way_write_byte,
  two_way_unread_byte,
  two_way_peek_byte,

  two_way_read_char,
  two_way_write_char,
  two_way_unread_char,
  two_way_peek_char,

  two_way_read_vector,
  two_way_write_vector,

  two_way_listen,
  two_way_clear_input,
  two_way_clear_output,
  two_way_finish_output,
  two_way_force_output,

  ecl_generic_always_true, /* input_p */
  ecl_generic_always_true, /* output_p */
  two_way_interactive_p,
  two_way_element_type,

  ecl_not_a_file_stream, /* length */
  ecl_generic_always_nil, /* get_position */
  ecl_generic_set_position,
  ecl_not_file_string_length,
  two_way_column,

  ecl_not_a_file_stream,
  ecl_not_a_file_stream,

  two_way_close
};


cl_object
cl_make_two_way_stream(cl_object istrm, cl_object ostrm)
{
  cl_object strm;
  if (!ecl_input_stream_p(istrm))
    ecl_not_an_input_stream(istrm);
  if (!ecl_output_stream_p(ostrm))
    ecl_not_an_output_stream(ostrm);
  strm = ecl_alloc_stream();
  strm->stream.format = cl_stream_external_format(istrm);
  strm->stream.mode = (short)ecl_smm_two_way;
  strm->stream.ops = ecl_duplicate_dispatch_table(&two_way_ops);
  TWO_WAY_STREAM_INPUT(strm) = istrm;
  TWO_WAY_STREAM_OUTPUT(strm) = ostrm;
  @(return strm);
}

cl_object
cl_two_way_stream_input_stream(cl_object strm)
{
  unlikely_if (!ECL_ANSI_STREAM_TYPE_P(strm,ecl_smm_two_way))
    FEwrong_type_only_arg(@[two-way-stream-input-stream],
                          strm, @[two-way-stream]);
  @(return TWO_WAY_STREAM_INPUT(strm));
}

cl_object
cl_two_way_stream_output_stream(cl_object strm)
{
  unlikely_if (!ECL_ANSI_STREAM_TYPE_P(strm, ecl_smm_two_way))
    FEwrong_type_only_arg(@[two-way-stream-output-stream],
                          strm, @[two-way-stream]);
  @(return TWO_WAY_STREAM_OUTPUT(strm));
}

/**********************************************************************
 * BROADCAST STREAM
 */

static cl_index
broadcast_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  cl_object l;
  cl_index out = n;
  for (l = BROADCAST_STREAM_LIST(strm); !Null(l); l = ECL_CONS_CDR(l)) {
    out = ecl_write_byte8(ECL_CONS_CAR(l), c, n);
  }
  return out;
}

static ecl_character
broadcast_write_char(cl_object strm, ecl_character c)
{
  cl_object l;
  for (l = BROADCAST_STREAM_LIST(strm); !Null(l); l = ECL_CONS_CDR(l)) {
    ecl_write_char(c, ECL_CONS_CAR(l));
  }
  return c;
}

static void
broadcast_write_byte(cl_object strm, cl_object byte)
{
  cl_object l;
  for (l = BROADCAST_STREAM_LIST(strm); !Null(l); l = ECL_CONS_CDR(l)) {
    ecl_write_byte(byte, ECL_CONS_CAR(l));
  }
}

static void
broadcast_clear_output(cl_object strm)
{
  cl_object l;
  for (l = BROADCAST_STREAM_LIST(strm); !Null(l); l = ECL_CONS_CDR(l)) {
    ecl_clear_output(ECL_CONS_CAR(l));
  }
}

static void
broadcast_force_output(cl_object strm)
{
  cl_object l;
  for (l = BROADCAST_STREAM_LIST(strm); !Null(l); l = ECL_CONS_CDR(l)) {
    ecl_force_output(ECL_CONS_CAR(l));
  }
}

static void
broadcast_finish_output(cl_object strm)
{
  cl_object l;
  for (l = BROADCAST_STREAM_LIST(strm); !Null(l); l = ECL_CONS_CDR(l)) {
    ecl_finish_output(ECL_CONS_CAR(l));
  }
}

static cl_object
broadcast_element_type(cl_object strm)
{
  cl_object l = BROADCAST_STREAM_LIST(strm);
  if (Null(l))
    return ECL_T;
  return ecl_stream_element_type(ECL_CONS_CAR(l));
}

static cl_object
broadcast_length(cl_object strm)
{
  cl_object l = BROADCAST_STREAM_LIST(strm);
  if (Null(l))
    return ecl_make_fixnum(0);
  return ecl_file_length(ECL_CONS_CAR(ecl_last(l, 1)));
}

static cl_object
broadcast_get_position(cl_object strm)
{
  cl_object l = BROADCAST_STREAM_LIST(strm);
  if (Null(l))
    return ecl_make_fixnum(0);
  return ecl_file_position(ECL_CONS_CAR(ecl_last(l, 1)));
}

static cl_object
broadcast_set_position(cl_object strm, cl_object pos)
{
  cl_object l = BROADCAST_STREAM_LIST(strm);
  if (Null(l))
    return ECL_NIL;
  return ecl_file_position_set(ECL_CONS_CAR(l), pos);
}

cl_object
broadcast_string_length(cl_object strm, cl_object string)
{
  cl_object l = BROADCAST_STREAM_LIST(strm);
  if (Null(l))
    return ecl_make_fixnum(1);
  return ecl_file_string_length(ECL_CONS_CAR(ecl_last(l, 1)), string);
}

static int
broadcast_column(cl_object strm)
{
  cl_object l = BROADCAST_STREAM_LIST(strm);
  if (Null(l))
    return 0;
  return ecl_file_column(ECL_CONS_CAR(l));
}

static cl_object
broadcast_close(cl_object strm)
{
  if (strm->stream.flags & ECL_STREAM_CLOSE_COMPONENTS) {
    cl_mapc(2, @'close', BROADCAST_STREAM_LIST(strm));
  }
  return ecl_generic_close(strm);
}

const struct ecl_file_ops broadcast_ops = {
  ecl_not_input_read_byte8,
  broadcast_write_byte8,

  ecl_not_input_read_byte,
  broadcast_write_byte,
  ecl_not_input_unread_byte,
  ecl_generic_peek_byte,

  ecl_not_input_read_char,
  broadcast_write_char,
  ecl_not_input_unread_char,
  ecl_generic_peek_char,

  ecl_generic_read_vector,
  ecl_generic_write_vector,

  ecl_not_input_listen,
  broadcast_force_output, /* clear_input */ /* FIXME! This is legacy behaviour */
  broadcast_clear_output,
  broadcast_finish_output,
  broadcast_force_output,

  ecl_generic_always_false, /* input_p */
  ecl_generic_always_true, /* output_p */
  ecl_generic_always_false,
  broadcast_element_type,

  broadcast_length,
  broadcast_get_position,
  broadcast_set_position,
  broadcast_string_length,
  broadcast_column,

  ecl_not_a_file_stream,
  ecl_not_a_file_stream,

  broadcast_close
};

@(defun make_broadcast_stream (&rest ap)
  cl_object x, streams;
  int i;
@
  streams = ECL_NIL;
  for (i = 0; i < narg; i++) {
    x = ecl_va_arg(ap);
    unlikely_if (!ecl_output_stream_p(x))
      ecl_not_an_output_stream(x);
    streams = CONS(x, streams);
  }
  x = ecl_alloc_stream();
  x->stream.format = @':default';
  x->stream.ops = ecl_duplicate_dispatch_table(&broadcast_ops);
  x->stream.mode = (short)ecl_smm_broadcast;
  BROADCAST_STREAM_LIST(x) = cl_nreverse(streams);
  @(return x);
@)

cl_object
cl_broadcast_stream_streams(cl_object strm)
{
  unlikely_if (!ECL_ANSI_STREAM_TYPE_P(strm, ecl_smm_broadcast))
    FEwrong_type_only_arg(@[broadcast-stream-streams],
                          strm, @[broadcast-stream]);
  return cl_copy_list(BROADCAST_STREAM_LIST(strm));
}

/**********************************************************************
 * ECHO STREAM
 */

static cl_index
echo_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  cl_index out = ecl_read_byte8(ECHO_STREAM_INPUT(strm), c, n);
  return ecl_write_byte8(ECHO_STREAM_OUTPUT(strm), c, out);
}

static cl_index
echo_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  return ecl_write_byte8(ECHO_STREAM_OUTPUT(strm), c, n);
}

static cl_object
echo_read_byte(cl_object strm)
{
  cl_object byte = strm->stream.last_byte;
  if (byte != OBJNULL) {
    strm->stream.last_byte = OBJNULL;
    byte = ecl_read_byte(ECHO_STREAM_INPUT(strm));
  } else {
    byte = ecl_read_byte(ECHO_STREAM_INPUT(strm));
    if (byte != OBJNULL)
      ecl_write_byte(byte, ECHO_STREAM_OUTPUT(strm));
  }
  return byte;
}

static void
echo_write_byte(cl_object strm, cl_object byte)
{
  ecl_write_byte(byte, ECHO_STREAM_OUTPUT(strm));
}

static void
echo_unread_byte(cl_object strm, cl_object byte)
{
  unlikely_if (strm->stream.last_byte != OBJNULL) {
    ecl_unread_twice(strm);
  }
  strm->stream.last_byte = ECL_T;
  ecl_unread_byte(byte, ECHO_STREAM_INPUT(strm));
}

static cl_object
echo_peek_byte(cl_object strm)
{
  return ecl_peek_byte(ECHO_STREAM_INPUT(strm));
}

static ecl_character
echo_read_char(cl_object strm)
{
  cl_object byte = strm->stream.last_byte;
  ecl_character c;
  if (byte != OBJNULL) {
    strm->stream.last_byte = OBJNULL;
    c = ecl_read_char(ECHO_STREAM_INPUT(strm));
  } else {
    c = ecl_read_char(ECHO_STREAM_INPUT(strm));
    if (c != EOF)
      ecl_write_char(c, ECHO_STREAM_OUTPUT(strm));
  }
  return c;
}

static ecl_character
echo_write_char(cl_object strm, ecl_character c)
{
  return ecl_write_char(c, ECHO_STREAM_OUTPUT(strm));
}

static void
echo_unread_char(cl_object strm, ecl_character c)
{
  unlikely_if (strm->stream.last_byte != OBJNULL) {
    ecl_unread_twice(strm);
  }
  strm->stream.last_byte = ECL_T;;
  ecl_unread_char(c, ECHO_STREAM_INPUT(strm));
}

static ecl_character
echo_peek_char(cl_object strm)
{
  return ecl_peek_char(ECHO_STREAM_INPUT(strm));
}

static int
echo_listen(cl_object strm)
{
  return ecl_listen_stream(ECHO_STREAM_INPUT(strm));
}

static void
echo_clear_input(cl_object strm)
{
  ecl_clear_input(ECHO_STREAM_INPUT(strm));
}

static void
echo_clear_output(cl_object strm)
{
  ecl_clear_output(ECHO_STREAM_OUTPUT(strm));
}

static void
echo_force_output(cl_object strm)
{
  ecl_force_output(ECHO_STREAM_OUTPUT(strm));
}

static void
echo_finish_output(cl_object strm)
{
  ecl_finish_output(ECHO_STREAM_OUTPUT(strm));
}

static cl_object
echo_element_type(cl_object strm)
{
  return ecl_stream_element_type(ECHO_STREAM_INPUT(strm));
}

static int
echo_column(cl_object strm)
{
  return ecl_file_column(ECHO_STREAM_OUTPUT(strm));
}

static cl_object
echo_close(cl_object strm)
{
  if (strm->stream.flags & ECL_STREAM_CLOSE_COMPONENTS) {
    cl_close(1, ECHO_STREAM_INPUT(strm));
    cl_close(1, ECHO_STREAM_OUTPUT(strm));
  }
  return ecl_generic_close(strm);
}

const struct ecl_file_ops echo_ops = {
  echo_read_byte8,
  echo_write_byte8,

  echo_read_byte,
  echo_write_byte,
  echo_unread_byte,
  echo_peek_byte,

  echo_read_char,
  echo_write_char,
  echo_unread_char,
  echo_peek_char,

  ecl_generic_read_vector,
  ecl_generic_write_vector,

  echo_listen,
  echo_clear_input,
  echo_clear_output,
  echo_finish_output,
  echo_force_output,

  ecl_generic_always_true, /* input_p */
  ecl_generic_always_true, /* output_p */
  ecl_generic_always_false,
  echo_element_type,

  ecl_not_a_file_stream, /* length */
  ecl_generic_always_nil, /* get_position */
  ecl_generic_set_position,
  ecl_not_file_string_length,
  echo_column,

  ecl_not_a_file_stream,
  ecl_not_a_file_stream,

  echo_close
};

cl_object
cl_make_echo_stream(cl_object strm1, cl_object strm2)
{
  cl_object strm;
  unlikely_if (!ecl_input_stream_p(strm1))
    ecl_not_an_input_stream(strm1);
  unlikely_if (!ecl_output_stream_p(strm2))
    ecl_not_an_output_stream(strm2);
  strm = ecl_alloc_stream();
  strm->stream.format = cl_stream_external_format(strm1);
  strm->stream.mode = (short)ecl_smm_echo;
  strm->stream.ops = ecl_duplicate_dispatch_table(&echo_ops);
  ECHO_STREAM_INPUT(strm) = strm1;
  ECHO_STREAM_OUTPUT(strm) = strm2;
  @(return strm);
}

cl_object
cl_echo_stream_input_stream(cl_object strm)
{
  unlikely_if (!ECL_ANSI_STREAM_TYPE_P(strm, ecl_smm_echo))
    FEwrong_type_only_arg(@[echo-stream-input-stream],
                          strm, @[echo-stream]);
  @(return ECHO_STREAM_INPUT(strm));
}

cl_object
cl_echo_stream_output_stream(cl_object strm)
{
  unlikely_if (!ECL_ANSI_STREAM_TYPE_P(strm, ecl_smm_echo))
    FEwrong_type_only_arg(@[echo-stream-output-stream],
                          strm, @[echo-stream]);
  @(return ECHO_STREAM_OUTPUT(strm));
}

/**********************************************************************
 * CONCATENATED STREAM
 */

static cl_index
concatenated_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  cl_object l = CONCATENATED_STREAM_LIST(strm);
  cl_index out = 0;
  while (out < n && !Null(l)) {
    cl_index delta = ecl_read_byte8(ECL_CONS_CAR(l), c + out, n - out);
    out += delta;
    if (out == n) break;
    CONCATENATED_STREAM_LIST(strm) = l = ECL_CONS_CDR(l);
  }
  return out;
}

static cl_object
concatenated_read_byte(cl_object strm)
{
  cl_object l = CONCATENATED_STREAM_LIST(strm);
  cl_object c = OBJNULL;
  while (!Null(l)) {
    c = ecl_read_byte(ECL_CONS_CAR(l));
    if (c != OBJNULL) break;
    CONCATENATED_STREAM_LIST(strm) = l = ECL_CONS_CDR(l);
  }
  return c;
}

static void
concatenated_unread_byte(cl_object strm, cl_object byte)
{
  cl_object l = CONCATENATED_STREAM_LIST(strm);
  unlikely_if (Null(l)) {
    ecl_unread_error(strm);
  }
  ecl_unread_byte(byte, ECL_CONS_CAR(l));
}

static ecl_character
concatenated_read_char(cl_object strm)
{
  cl_object l = CONCATENATED_STREAM_LIST(strm);
  ecl_character c = EOF;
  while (!Null(l)) {
    c = ecl_read_char(ECL_CONS_CAR(l));
    if (c != EOF) break;
    CONCATENATED_STREAM_LIST(strm) = l = ECL_CONS_CDR(l);
  }
  return c;
}

static void
concatenated_unread_char(cl_object strm, ecl_character c)
{
  cl_object l = CONCATENATED_STREAM_LIST(strm);
  unlikely_if (Null(l)) {
    ecl_unread_error(strm);
  }
  ecl_unread_char(c, ECL_CONS_CAR(l));
}

static int
concatenated_listen(cl_object strm)
{
  cl_object l = CONCATENATED_STREAM_LIST(strm);
  while (!Null(l)) {
    int f = ecl_listen_stream(ECL_CONS_CAR(l));
    l = ECL_CONS_CDR(l);
    if (f == ECL_LISTEN_EOF) {
      CONCATENATED_STREAM_LIST(strm) = l;
    } else {
      return f;
    }
  }
  return ECL_LISTEN_EOF;
}

static cl_object
concatenated_close(cl_object strm)
{
  if (strm->stream.flags & ECL_STREAM_CLOSE_COMPONENTS) {
    cl_mapc(2, @'close', CONCATENATED_STREAM_LIST(strm));
  }
  return ecl_generic_close(strm);
}

const struct ecl_file_ops concatenated_ops = {
  concatenated_read_byte8,
  ecl_not_output_write_byte8,

  concatenated_read_byte,
  ecl_not_output_write_byte,
  concatenated_unread_byte,
  ecl_generic_peek_byte,

  concatenated_read_char,
  ecl_not_output_write_char,
  concatenated_unread_char,
  ecl_generic_peek_char,

  ecl_generic_read_vector,
  ecl_generic_write_vector,

  concatenated_listen,
  ecl_generic_void, /* clear_input */
  ecl_not_output_clear_output,
  ecl_not_output_finish_output,
  ecl_not_output_force_output,

  ecl_generic_always_true, /* input_p */
  ecl_generic_always_false, /* output_p */
  ecl_generic_always_false,
  broadcast_element_type,

  ecl_not_a_file_stream, /* length */
  ecl_generic_always_nil, /* get_position */
  ecl_generic_set_position,
  ecl_not_output_string_length,
  ecl_unknown_column,

  ecl_not_a_file_stream,
  ecl_not_a_file_stream,

  concatenated_close
};

@(defun make_concatenated_stream (&rest ap)
  cl_object x, streams;
  int i;
@
  streams = ECL_NIL;
  for (i = 0; i < narg; i++) {
    x = ecl_va_arg(ap);
    unlikely_if (!ecl_input_stream_p(x))
      ecl_not_an_input_stream(x);
    streams = CONS(x, streams);
  }
  x = ecl_alloc_stream();
  if (Null(streams)) {
    x->stream.format = @':pass-through';
  } else {
    x->stream.format = cl_stream_external_format(ECL_CONS_CAR(streams));
  }
  x->stream.mode = (short)ecl_smm_concatenated;
  x->stream.ops = ecl_duplicate_dispatch_table(&concatenated_ops);
  CONCATENATED_STREAM_LIST(x) = cl_nreverse(streams);
  @(return x);
@)

cl_object
cl_concatenated_stream_streams(cl_object strm)
{
  unlikely_if (!ECL_ANSI_STREAM_TYPE_P(strm, ecl_smm_concatenated))
    FEwrong_type_only_arg(@[concatenated-stream-streams],
                          strm, @[concatenated-stream]);
  return cl_copy_list(CONCATENATED_STREAM_LIST(strm));
}

/**********************************************************************
 * SYNONYM STREAM
 */

static cl_index
synonym_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  return ecl_read_byte8(SYNONYM_STREAM_STREAM(strm), c, n);
}

static cl_index
synonym_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  return ecl_write_byte8(SYNONYM_STREAM_STREAM(strm), c, n);
}

static void
synonym_write_byte(cl_object strm, cl_object byte)
{
  ecl_write_byte(byte, SYNONYM_STREAM_STREAM(strm));
}

static cl_object
synonym_read_byte(cl_object strm)
{
  return ecl_read_byte(SYNONYM_STREAM_STREAM(strm));
}

static void
synonym_unread_byte(cl_object strm, cl_object byte)
{
  ecl_unread_byte(SYNONYM_STREAM_STREAM(strm), byte);
}

static cl_object
synonym_peek_byte(cl_object strm)
{
  return ecl_peek_byte(SYNONYM_STREAM_STREAM(strm));
}

static ecl_character
synonym_read_char(cl_object strm)
{
  return ecl_read_char(SYNONYM_STREAM_STREAM(strm));
}

static ecl_character
synonym_write_char(cl_object strm, ecl_character c)
{
  return ecl_write_char(c, SYNONYM_STREAM_STREAM(strm));
}

static void
synonym_unread_char(cl_object strm, ecl_character c)
{
  ecl_unread_char(c, SYNONYM_STREAM_STREAM(strm));
}

static ecl_character
synonym_peek_char(cl_object strm)
{
  return ecl_peek_char(SYNONYM_STREAM_STREAM(strm));
}

static cl_index
synonym_read_vector(cl_object strm, cl_object data, cl_index start, cl_index n)
{
  strm = SYNONYM_STREAM_STREAM(strm);
  return ecl_stream_dispatch_table(strm)->read_vector(strm, data, start, n);
}

static cl_index
synonym_write_vector(cl_object strm, cl_object data, cl_index start, cl_index n)
{
  strm = SYNONYM_STREAM_STREAM(strm);
  return ecl_stream_dispatch_table(strm)->write_vector(strm, data, start, n);
}

static int
synonym_listen(cl_object strm)
{
  return ecl_listen_stream(SYNONYM_STREAM_STREAM(strm));
}

static void
synonym_clear_input(cl_object strm)
{
  ecl_clear_input(SYNONYM_STREAM_STREAM(strm));
}

static void
synonym_clear_output(cl_object strm)
{
  ecl_clear_output(SYNONYM_STREAM_STREAM(strm));
}

static void
synonym_force_output(cl_object strm)
{
  ecl_force_output(SYNONYM_STREAM_STREAM(strm));
}

static void
synonym_finish_output(cl_object strm)
{
  ecl_finish_output(SYNONYM_STREAM_STREAM(strm));
}

static int
synonym_input_p(cl_object strm)
{
  return ecl_input_stream_p(SYNONYM_STREAM_STREAM(strm));
}

static int
synonym_output_p(cl_object strm)
{
  return ecl_output_stream_p(SYNONYM_STREAM_STREAM(strm));
}

static int
synonym_interactive_p(cl_object strm)
{
  return ecl_interactive_stream_p(SYNONYM_STREAM_STREAM(strm));
}

static cl_object
synonym_element_type(cl_object strm)
{
  return ecl_stream_element_type(SYNONYM_STREAM_STREAM(strm));
}

static cl_object
synonym_length(cl_object strm)
{
  return ecl_file_length(SYNONYM_STREAM_STREAM(strm));
}

static cl_object
synonym_get_position(cl_object strm)
{
  return ecl_file_position(SYNONYM_STREAM_STREAM(strm));
}

static cl_object
synonym_set_position(cl_object strm, cl_object pos)
{
  return ecl_file_position_set(SYNONYM_STREAM_STREAM(strm), pos);
}

static cl_object
synonym_string_length(cl_object strm, cl_object string)
{
  return ecl_file_string_length(SYNONYM_STREAM_STREAM(strm), string);
}

static int
synonym_column(cl_object strm)
{
  return ecl_file_column(SYNONYM_STREAM_STREAM(strm));
}

static cl_object
synonym_pathname(cl_object strm)
{
  return ecl_stream_pathname(SYNONYM_STREAM_STREAM(strm));
}

static cl_object
synonym_truename(cl_object strm)
{
  return ecl_stream_truename(SYNONYM_STREAM_STREAM(strm));
}

const struct ecl_file_ops synonym_ops = {
  synonym_read_byte8,
  synonym_write_byte8,

  synonym_read_byte,
  synonym_write_byte,
  synonym_unread_byte,
  synonym_peek_byte,

  synonym_read_char,
  synonym_write_char,
  synonym_unread_char,
  synonym_peek_char,

  synonym_read_vector,
  synonym_write_vector,

  synonym_listen,
  synonym_clear_input,
  synonym_clear_output,
  synonym_finish_output,
  synonym_force_output,

  synonym_input_p,
  synonym_output_p,
  synonym_interactive_p,
  synonym_element_type,

  synonym_length,
  synonym_get_position,
  synonym_set_position,
  synonym_string_length,
  synonym_column,

  synonym_pathname,
  synonym_truename,

  ecl_generic_close
};

cl_object
cl_make_synonym_stream(cl_object sym)
{
  cl_object x;

  sym = ecl_check_cl_type(@'make-synonym-stream',sym,t_symbol);
  x = ecl_alloc_stream();
  x->stream.ops = ecl_duplicate_dispatch_table(&synonym_ops);
  x->stream.mode = (short)ecl_smm_synonym;
  SYNONYM_STREAM_SYMBOL(x) = sym;
  @(return x);
}

cl_object
cl_synonym_stream_symbol(cl_object strm)
{
  unlikely_if (!ECL_ANSI_STREAM_TYPE_P(strm, ecl_smm_synonym))
    FEwrong_type_only_arg(@[synonym-stream-symbol],
                          strm, @[synonym-stream]);
  @(return SYNONYM_STREAM_SYMBOL(strm));
}
