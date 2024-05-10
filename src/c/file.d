/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * file.d - file interface
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/*
  IMPLEMENTATION-DEPENDENT

  The file contains code to reclaim the I/O buffer
  by accessing the FILE structure of C.
*/

#include <errno.h>
#include <sys/types.h>
#ifndef _MSC_VER
# include <unistd.h>
#endif
#include <fcntl.h>
#if !defined(_MSC_VER) && !defined(__MINGW32__)
# include <sys/stat.h>
/* it isn't pulled in by fcntl.h */
#endif
#include <string.h>
#include <stdio.h>
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#define ECL_DEFINE_AET_SIZE
#include <ecl/internal.h>

#ifdef HAVE_SELECT
# ifdef HAVE_SYS_SELECT_H
#  include <sys/select.h>
# endif
# include <sys/time.h>
# include <sys/types.h>
# include <unistd.h>
#elif defined(ECL_MS_WINDOWS_HOST)
# include <winsock.h>
# include <windows.h>
# include <sys/stat.h>
# define STDIN_FILENO 0
# define STDOUT_FILENO 1
# define STDERR_FILENO 2
# define HAVE_SELECT
#endif
#if defined(HAVE_SYS_IOCTL_H) && !defined(ECL_MS_WINDOWS_HOST) && !defined(cygwin)
# include <sys/ioctl.h>
#endif

/* Maximum number of bytes required to encode a character.
 * This currently corresponds to (4 + 4) for the UCS-4 encoding
 * with 4 being the byte-order mark, 4 for the character.
 */
#define ENCODING_BUFFER_MAX_SIZE 8
/* Size of the encoding buffer for vectors */
#define VECTOR_ENCODING_BUFFER_SIZE 2048

static cl_index ecl_read_byte8(cl_object stream, unsigned char *c, cl_index n);
static cl_index ecl_write_byte8(cl_object stream, unsigned char *c, cl_index n);

struct ecl_file_ops *duplicate_dispatch_table(const struct ecl_file_ops *ops);
const struct ecl_file_ops *stream_dispatch_table(cl_object strm);

static int file_listen(cl_object, FILE *);
static int fd_listen(cl_object, int);

static cl_object alloc_stream();

static void cannot_close(cl_object stream) ecl_attr_noreturn;
static void file_libc_error(cl_object error_type, cl_object stream, const char *msg, int narg, ...) ecl_attr_noreturn;
static cl_object not_a_file_stream(cl_object fn) ecl_attr_noreturn;
static void not_an_input_stream(cl_object fn) ecl_attr_noreturn;
static void not_an_output_stream(cl_object fn) ecl_attr_noreturn;
static void not_a_character_stream(cl_object s) ecl_attr_noreturn;
static void not_a_binary_stream(cl_object s) ecl_attr_noreturn;
static int restartable_io_error(cl_object strm, const char *s);
static void unread_error(cl_object strm);
static void unread_twice(cl_object strm);
static void io_error(cl_object strm) ecl_attr_noreturn;
#ifdef ECL_UNICODE
static cl_index encoding_error(cl_object strm, unsigned char *buffer, ecl_character c);
static ecl_character decoding_error(cl_object strm, unsigned char **buffer, int char_length, unsigned char *buffer_end);
#endif
static void wrong_file_handler(cl_object strm) ecl_attr_noreturn;
#if defined(ECL_WSOCK)
static void wsock_error( const char *err_msg, cl_object strm ) ecl_attr_noreturn;
#endif

/**********************************************************************
 * NOT IMPLEMENTED or NOT APPLICABLE OPERATIONS
 */

static cl_index
not_output_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  not_an_output_stream(strm);
  return 0;
}

static cl_index
not_input_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  not_an_input_stream(strm);
  return 0;
}

static cl_index
not_binary_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  not_a_binary_stream(strm);
  return 0;
}

static void
not_output_write_byte(cl_object c, cl_object strm)
{
  not_an_output_stream(strm);
}

static cl_object
not_input_read_byte(cl_object strm)
{
  not_an_input_stream(strm);
  return OBJNULL;
}

static void
not_binary_write_byte(cl_object c, cl_object strm)
{
  not_a_binary_stream(strm);
}

static cl_object
not_binary_read_byte(cl_object strm)
{
  not_a_binary_stream(strm);
  return OBJNULL;
}

static ecl_character
not_input_read_char(cl_object strm)
{
  not_an_input_stream(strm);
  return -1;
}

static ecl_character
not_output_write_char(cl_object strm, ecl_character c)
{
  not_an_output_stream(strm);
  return c;
}

static void
not_input_unread_char(cl_object strm, ecl_character c)
{
  not_an_input_stream(strm);
}

static int
not_input_listen(cl_object strm)
{
  not_an_input_stream(strm);
  return -1;
}

static ecl_character
not_character_read_char(cl_object strm)
{
  not_a_character_stream(strm);
  return -1;
}

static ecl_character
not_character_write_char(cl_object strm, ecl_character c)
{
  not_a_character_stream(strm);
  return c;
}

static ecl_character
not_character_decoder(cl_object stream, unsigned char **buffer, unsigned char *buffer_end) {
  not_a_character_stream(stream);
  return EOF;
}

static int
not_character_encoder(cl_object stream, unsigned char *buffer, ecl_character c) {
  not_a_character_stream(stream);
  return 0;
}

static void
not_input_clear_input(cl_object strm)
{
  not_an_input_stream(strm);
  return;
}

static void
not_output_clear_output(cl_object strm)
{
  not_an_output_stream(strm);
}

static void
not_output_force_output(cl_object strm)
{
  not_an_output_stream(strm);
}

static void
not_output_finish_output(cl_object strm)
{
  not_an_output_stream(strm);
}

static cl_object
not_output_string_length(cl_object strm, cl_object string)
{
  not_an_output_stream(strm);
  return 0;
}

static cl_object
not_file_string_length(cl_object strm, cl_object string)
{
  not_a_file_stream(strm);
  return 0;
}

static int
unknown_column(cl_object strm)
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
/*
 * Byte operations based on octet operators.
 */
static cl_object
generic_read_byte_unsigned8(cl_object strm)
{
  unsigned char c;
  if (strm->stream.ops->read_byte8(strm, &c, 1) < 1) {
    return ECL_NIL;
  }
  return ecl_make_fixnum(c);
}

static void
generic_write_byte_unsigned8(cl_object byte, cl_object strm)
{
  unsigned char c = ecl_to_uint8_t(byte);
  strm->stream.ops->write_byte8(strm, &c, 1);
}

static cl_object
generic_read_byte_signed8(cl_object strm)
{
  signed char c;
  if (strm->stream.ops->read_byte8(strm, (unsigned char *)&c, 1) < 1)
    return ECL_NIL;
  return ecl_make_fixnum(c);
}

static void
generic_write_byte_signed8(cl_object byte, cl_object strm)
{
  signed char c = ecl_to_int8_t(byte);
  strm->stream.ops->write_byte8(strm, (unsigned char *)&c, 1);
}

static cl_object
generic_read_byte_le(cl_object strm)
{
  cl_index (*read_byte8)(cl_object, unsigned char *, cl_index);
  unsigned char c;
  cl_index nb, bs;
  cl_object output = ecl_make_fixnum(0);
  read_byte8 = strm->stream.ops->read_byte8;
  bs = strm->stream.byte_size;
  for (nb = 0; bs >= 8; bs -= 8, nb += 8) {
    cl_object aux;
    if (read_byte8(strm, &c, 1) < 1)
      return ECL_NIL;
    if (bs <= 8 && (strm->stream.flags & ECL_STREAM_SIGNED_BYTES))
      aux = ecl_make_fixnum((signed char)c);
    else
      aux = ecl_make_fixnum((unsigned char)c);
    output = cl_logior(2, output, cl_ash(aux, ecl_make_fixnum(nb)));
  }
  return output;
}

static void
generic_write_byte_le(cl_object c, cl_object strm)
{
  cl_index (*write_byte8)(cl_object strm, unsigned char *c, cl_index n);
  cl_index bs;
  write_byte8 = strm->stream.ops->write_byte8;
  bs = strm->stream.byte_size;
  do {
    cl_object b = cl_logand(2, c, ecl_make_fixnum(0xFF));
    unsigned char aux = (unsigned char)ecl_fixnum(b);
    if (write_byte8(strm, &aux, 1) < 1)
      break;
    c = cl_ash(c, ecl_make_fixnum(-8));
    bs -= 8;
  } while (bs);
}

static cl_object
generic_read_byte(cl_object strm)
{
  cl_index (*read_byte8)(cl_object, unsigned char *, cl_index);
  unsigned char c;
  cl_object output = NULL;
  cl_index bs;
  read_byte8 = strm->stream.ops->read_byte8;
  bs = strm->stream.byte_size;
  for (; bs >= 8; bs -= 8) {
    if (read_byte8(strm, &c, 1) < 1)
      return ECL_NIL;
    if (output) {
      output = cl_logior(2, ecl_make_fixnum(c),
                         cl_ash(output, ecl_make_fixnum(8)));
    } else if (strm->stream.flags & ECL_STREAM_SIGNED_BYTES) {
      output = ecl_make_fixnum((signed char)c);
    } else {
      output = ecl_make_fixnum((unsigned char)c);
    }
  }
  return output;
}

static void
generic_write_byte(cl_object c, cl_object strm)
{
  cl_index (*write_byte8)(cl_object strm, unsigned char *c, cl_index n);
  cl_index bs;
  write_byte8 = strm->stream.ops->write_byte8;
  bs = strm->stream.byte_size;
  do {
    unsigned char aux;
    cl_object b;
    bs -= 8;
    b = cl_logand(2, ecl_make_fixnum(0xFF), bs? cl_ash(c, ecl_make_fixnum(-bs)) : c);
    aux = (unsigned char)ecl_fixnum(b);
    if (write_byte8(strm, &aux, 1) < 1)
      break;
  } while (bs);
}

static ecl_character
generic_peek_char(cl_object strm)
{
  ecl_character out = ecl_read_char(strm);
  if (out != EOF) ecl_unread_char(out, strm);
  return out;
}

static void
generic_void(cl_object strm)
{
}

static int
generic_always_true(cl_object strm)
{
  return 1;
}

static int
generic_always_false(cl_object strm)
{
  return 0;
}

static cl_object
generic_always_nil(cl_object strm)
{
  return ECL_NIL;
}

static int
generic_column(cl_object strm)
{
  return strm->stream.column;
}

static cl_object
generic_set_position(cl_object strm, cl_object pos)
{
  return ECL_NIL;
}

static cl_object
generic_close(cl_object strm)
{
  struct ecl_file_ops *ops = strm->stream.ops;
  if (ecl_input_stream_p(strm)) {
    ops->read_byte8 = closed_stream_read_byte8;
    ops->read_char = closed_stream_read_char;
    ops->unread_char = closed_stream_unread_char;
    ops->listen = closed_stream_listen;
    ops->clear_input = closed_stream_clear_input;
  }
  if (ecl_output_stream_p(strm)) {
    ops->write_byte8 = closed_stream_write_byte8;
    ops->write_char = closed_stream_write_char;
    ops->clear_output = closed_stream_clear_output;
    ops->force_output = closed_stream_force_output;
    ops->finish_output = closed_stream_finish_output;
  }
  ops->get_position = closed_stream_get_position;
  ops->set_position = closed_stream_set_position;
  ops->length = closed_stream_length;
  ops->close = generic_close;
  strm->stream.closed = 1;
  return ECL_T;
}

static cl_index
generic_write_vector(cl_object strm, cl_object data, cl_index start, cl_index end)
{
  cl_elttype elttype;
  const struct ecl_file_ops *ops;
  if (start >= end)
    return start;
  ops = stream_dispatch_table(strm);
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
      write_byte(ecl_elt(data, start), strm);
    }
  }
  return start;
}

static cl_index
generic_read_vector(cl_object strm, cl_object data, cl_index start, cl_index end)
{
  const struct ecl_file_ops *ops;
  cl_object expected_type;
  if (start >= end)
    return start;
  expected_type = ecl_stream_element_type(strm);
  ops = stream_dispatch_table(strm);
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
      if (Null(x)) break;
      ecl_elt_set(data, start, x);
    }
  }
  return start;
}

/**********************************************************************
 * CHARACTER AND EXTERNAL FORMAT SUPPORT
 */

static void
eformat_unread_char(cl_object strm, ecl_character c)
{
  unlikely_if (c != strm->stream.last_char) {
    unread_twice(strm);
  }
  {
    unsigned char buffer[2*ENCODING_BUFFER_MAX_SIZE];
    int ndx = 0;
    cl_object l = strm->stream.byte_stack;
    cl_fixnum i = strm->stream.last_code[0];
    if (i != EOF) {
      ndx += strm->stream.encoder(strm, buffer, i);
    }
    i = strm->stream.last_code[1];
    if (i != EOF) {
      ndx += strm->stream.encoder(strm, buffer+ndx, i);
    }
    while (ndx != 0) {
      l = CONS(ecl_make_fixnum(buffer[--ndx]), l);
    }
    strm->stream.byte_stack = l;
    strm->stream.last_char = EOF;
  }
}

static ecl_character
eformat_read_char(cl_object strm)
{
  unsigned char buffer[ENCODING_BUFFER_MAX_SIZE];
  ecl_character c;
  unsigned char *buffer_pos = buffer;
  unsigned char *buffer_end = buffer;
  cl_index byte_size = (strm->stream.byte_size / 8);
  do {
    if (ecl_read_byte8(strm, buffer_end, byte_size) < byte_size) {
      c = EOF;
      break;
    }
    buffer_end += byte_size;
    c = strm->stream.decoder(strm, &buffer_pos, buffer_end);
  } while(c == EOF && (buffer_end - buffer) < ENCODING_BUFFER_MAX_SIZE);
  unlikely_if (c == strm->stream.eof_char)
    return EOF;
  if (c != EOF) {
    strm->stream.last_char = c;
    strm->stream.last_code[0] = c;
    strm->stream.last_code[1] = EOF;
  }
  return c;
}

static inline void
write_char_increment_column(cl_object strm, ecl_character c)
{
  if (c == '\n')
    strm->stream.column = 0;
  else if (c == '\t')
    strm->stream.column = (strm->stream.column & ~((cl_index)07)) + 8;
  else
    strm->stream.column++;
}

static ecl_character
eformat_write_char(cl_object strm, ecl_character c)
{
  unsigned char buffer[ENCODING_BUFFER_MAX_SIZE];
  ecl_character nbytes;
  nbytes = strm->stream.encoder(strm, buffer, c);
  strm->stream.ops->write_byte8(strm, buffer, nbytes);
  write_char_increment_column(strm, c);
  return c;
}

static ecl_character
eformat_read_char_cr(cl_object strm)
{
  ecl_character c = eformat_read_char(strm);
  if (c == ECL_CHAR_CODE_RETURN) {
    c = ECL_CHAR_CODE_NEWLINE;
    strm->stream.last_char = c;
  }
  return c;
}

static ecl_character
eformat_write_char_cr(cl_object strm, ecl_character c)
{
  if (c == ECL_CHAR_CODE_NEWLINE) {
    eformat_write_char(strm, ECL_CHAR_CODE_RETURN);
    strm->stream.column = 0;
    return c;
  }
  return eformat_write_char(strm, c);
}

static ecl_character
eformat_read_char_crlf(cl_object strm)
{
  ecl_character c = eformat_read_char(strm);
  if (c == ECL_CHAR_CODE_RETURN) {
    c = eformat_read_char(strm);
    if (c == ECL_CHAR_CODE_LINEFEED) {
      strm->stream.last_code[0] = ECL_CHAR_CODE_RETURN;
      strm->stream.last_code[1] = c;
      c = ECL_CHAR_CODE_NEWLINE;
    } else {
      eformat_unread_char(strm, c);
      c = ECL_CHAR_CODE_RETURN;
      strm->stream.last_code[0] = c;
      strm->stream.last_code[1] = EOF;
    }
    strm->stream.last_char = c;
  }
  return c;
}

static ecl_character
eformat_write_char_crlf(cl_object strm, ecl_character c)
{
  if (c == ECL_CHAR_CODE_NEWLINE) {
    eformat_write_char(strm, ECL_CHAR_CODE_RETURN);
    eformat_write_char(strm, ECL_CHAR_CODE_LINEFEED);
    strm->stream.column = 0;
    return c;
  }
  return eformat_write_char(strm, c);
}

/*
 * If we use Unicode, this is LATIN-1, ISO-8859-1, that is the 256
 * lowest codes of Unicode. Otherwise, we simply assume the file and
 * the strings use the same format.
 */

static ecl_character
passthrough_decoder(cl_object stream, unsigned char **buffer, unsigned char *buffer_end)
{
  if (*buffer >= buffer_end)
    return EOF;
  return *((*buffer)++);
}

static int
passthrough_encoder(cl_object stream, unsigned char *buffer, ecl_character c)
{
#ifdef ECL_UNICODE
  unlikely_if (c > 0xFF) {
    return encoding_error(stream, buffer, c);
  }
#endif
  buffer[0] = c;
  return 1;
}

#ifdef ECL_UNICODE
/*
 * US ASCII, that is the 128 (0-127) lowest codes of Unicode
 */

static ecl_character
ascii_decoder(cl_object stream, unsigned char **buffer, unsigned char *buffer_end)
{
  if (*buffer >= buffer_end)
    return EOF;
  if (**buffer > 127) {
    return decoding_error(stream, buffer, 1, buffer_end);
  } else {
    return *((*buffer)++);
  }
}

static int
ascii_encoder(cl_object stream, unsigned char *buffer, ecl_character c)
{
  unlikely_if (c > 127) {
    return encoding_error(stream, buffer, c);
  }
  buffer[0] = c;
  return 1;
}

/*
 * UCS-4 BIG ENDIAN
 */

static ecl_character
ucs_4be_decoder(cl_object stream, unsigned char **buffer, unsigned char *buffer_end)
{
  ecl_character aux;
  if ((*buffer)+3 >= buffer_end)
    return EOF;
  aux = (*buffer)[3]+((*buffer)[2]<<8)+((*buffer)[1]<<16)+((*buffer)[0]<<24);
  *buffer += 4;
  return aux;
}

static int
ucs_4be_encoder(cl_object stream, unsigned char *buffer, ecl_character c)
{
  buffer[3] = c & 0xFF; c >>= 8;
  buffer[2] = c & 0xFF; c >>= 8;
  buffer[1] = c & 0xFF; c >>= 8;
  buffer[0] = c;
  return 4;
}

/*
 * UCS-4 LITTLE ENDIAN
 */

static ecl_character
ucs_4le_decoder(cl_object stream, unsigned char **buffer, unsigned char *buffer_end)
{
  ecl_character aux;
  if ((*buffer)+3 >= buffer_end)
    return EOF;
  aux = (*buffer)[0]+((*buffer)[1]<<8)+((*buffer)[2]<<16)+((*buffer)[3]<<24);
  *buffer += 4;
  return aux;
}

static int
ucs_4le_encoder(cl_object stream, unsigned char *buffer, ecl_character c)
{
  buffer[0] = c & 0xFF; c >>= 8;
  buffer[1] = c & 0xFF; c >>= 8;
  buffer[2] = c & 0xFF; c >>= 8;
  buffer[3] = c;
  return 4;
}

/*
 * UCS-4 BOM ENDIAN
 */

static ecl_character
ucs_4_decoder(cl_object stream, unsigned char **buffer, unsigned char *buffer_end)
{
  cl_fixnum c = ucs_4be_decoder(stream, buffer, buffer_end);
  if (c == EOF)
    return c;
  if (c == 0xFEFF) {
    stream->stream.decoder = ucs_4be_decoder;
    stream->stream.encoder = ucs_4be_encoder;
    return ucs_4be_decoder(stream, buffer, buffer_end);
  } else if (c == 0xFFFE0000) {
    stream->stream.decoder = ucs_4le_decoder;
    stream->stream.encoder = ucs_4le_encoder;
    return ucs_4le_decoder(stream, buffer, buffer_end);
  } else {
    stream->stream.decoder = ucs_4be_decoder;
    stream->stream.encoder = ucs_4be_encoder;
    return c;
  }
}

static int
ucs_4_encoder(cl_object stream, unsigned char *buffer, ecl_character c)
{
  stream->stream.decoder = ucs_4be_decoder;
  stream->stream.encoder = ucs_4be_encoder;
  buffer[0] = buffer[1] = 0;
  buffer[2] = 0xFE;
  buffer[3] = 0xFF;
  return 4 + ucs_4be_encoder(stream, buffer+4, c);
}


/*
 * UTF-16 BIG ENDIAN
 */

static ecl_character
ucs_2be_decoder(cl_object stream, unsigned char **buffer, unsigned char *buffer_end)
{
  if ((*buffer)+1 >= buffer_end) {
    return EOF;
  } else {
    ecl_character c = ((ecl_character)(*buffer)[0] << 8) | (*buffer)[1];
    if (((*buffer)[0] & 0xFC) == 0xD8) {
      if ((*buffer)+3 >= buffer_end) {
        return EOF;
      } else {
        ecl_character aux;
        if (((*buffer)[2] & 0xFC) != 0xDC) {
          return decoding_error(stream, buffer, 4, buffer_end);
        }
        aux = ((ecl_character)(*buffer)[2] << 8) | (*buffer)[3];
        *buffer += 4;
        return ((c & 0x3FF) << 10) + (aux & 0x3FF) + 0x10000;
      }
    }
    *buffer += 2;
    return c;
  }
}

static int
ucs_2be_encoder(cl_object stream, unsigned char *buffer, ecl_character c)
{
  if (c >= 0x10000) {
    c -= 0x10000;
    ucs_2be_encoder(stream, buffer, (c >> 10) | 0xD800);
    ucs_2be_encoder(stream, buffer+2, (c & 0x3FF) | 0xDC00);
    return 4;
  } else {
    buffer[1] = c & 0xFF; c >>= 8;
    buffer[0] = c;
    return 2;
  }
}

/*
 * UTF-16 LITTLE ENDIAN
 */

static ecl_character
ucs_2le_decoder(cl_object stream, unsigned char **buffer, unsigned char *buffer_end)
{
  if ((*buffer)+1 >= buffer_end) {
    return EOF;
  } else {
    ecl_character c = ((ecl_character)(*buffer)[1] << 8) | (*buffer)[0];
    if (((*buffer)[1] & 0xFC) == 0xD8) {
      if ((*buffer)+3 >= buffer_end) {
        return EOF;
      } else {
        ecl_character aux;
        if (((*buffer)[3] & 0xFC) != 0xDC) {
          return decoding_error(stream, buffer, 4, buffer_end);
        }
        aux = ((ecl_character)(*buffer)[3] << 8) | (*buffer)[2];
        *buffer += 4;
        return ((c & 0x3FF) << 10) + (aux & 0x3FF) + 0x10000;
      }
    }
    *buffer += 2;
    return c;
  }
}

static int
ucs_2le_encoder(cl_object stream, unsigned char *buffer, ecl_character c)
{
  if (c >= 0x10000) {
    c -= 0x10000;
    ucs_2le_encoder(stream, buffer, (c >> 10) | 0xD800);
    ucs_2le_encoder(stream, buffer+2, (c & 0x3FF) | 0xDC00);
    return 4;
  } else {
    buffer[0] = c & 0xFF; c >>= 8;
    buffer[1] = c & 0xFF;
    return 2;
  }
}

/*
 * UTF-16 BOM ENDIAN
 */

static ecl_character
ucs_2_decoder(cl_object stream, unsigned char **buffer, unsigned char *buffer_end)
{
  ecl_character c = ucs_2be_decoder(stream, buffer, buffer_end);
  if (c == EOF)
    return c;
  if (c == 0xFEFF) {
    stream->stream.decoder = ucs_2be_decoder;
    stream->stream.encoder = ucs_2be_encoder;
    return ucs_2be_decoder(stream, buffer, buffer_end);
  } else if (c == 0xFFFE) {
    stream->stream.decoder = ucs_2le_decoder;
    stream->stream.encoder = ucs_2le_encoder;
    return ucs_2le_decoder(stream, buffer, buffer_end);
  } else {
    stream->stream.decoder = ucs_2be_decoder;
    stream->stream.encoder = ucs_2be_encoder;
    return c;
  }
}

static int
ucs_2_encoder(cl_object stream, unsigned char *buffer, ecl_character c)
{
  stream->stream.decoder = ucs_2be_decoder;
  stream->stream.encoder = ucs_2be_encoder;
  buffer[0] = 0xFE;
  buffer[1] = 0xFF;
  return 2 + ucs_2be_encoder(stream, buffer+2, c);
}

/*
 * USER DEFINED ENCODINGS. SIMPLE CASE.
 */

static ecl_character
user_decoder(cl_object stream, unsigned char **buffer, unsigned char *buffer_end)
{
  cl_object table = stream->stream.format_table;
  cl_object character;
  if (*buffer >= buffer_end) {
    return EOF;
  }
  character = ecl_gethash_safe(ecl_make_fixnum((*buffer)[0]), table, ECL_NIL);
  unlikely_if (Null(character)) {
    return decoding_error(stream, buffer, 1, buffer_end);
  }
  if (character == ECL_T) {
    if ((*buffer)+1 >= buffer_end) {
      return EOF;
    } else {
      cl_fixnum byte = ((*buffer)[0]<<8) + (*buffer)[1];
      character = ecl_gethash_safe(ecl_make_fixnum(byte), table, ECL_NIL);
      unlikely_if (Null(character)) {
        return decoding_error(stream, buffer, 2, buffer_end);
      }
    }
    (*buffer)++;
  }
  (*buffer)++;
  return ECL_CHAR_CODE(character);
}

static int
user_encoder(cl_object stream, unsigned char *buffer, ecl_character c)
{
  cl_object byte = ecl_gethash_safe(ECL_CODE_CHAR(c), stream->stream.format_table, ECL_NIL);
  if (Null(byte)) {
    return encoding_error(stream, buffer, c);
  } else {
    cl_fixnum code = ecl_fixnum(byte);
    if (code > 0xFF) {
      buffer[1] = code & 0xFF; code >>= 8;
      buffer[0] = code;
      return 2;
    } else {
      buffer[0] = code;
      return 1;
    }
  }
}

/*
 * USER DEFINED ENCODINGS. SIMPLE CASE.
 */

static ecl_character
user_multistate_decoder(cl_object stream, unsigned char **buffer, unsigned char *buffer_end)
{
  cl_object table_list = stream->stream.format_table;
  cl_object table = ECL_CONS_CAR(table_list);
  cl_object character;
  cl_fixnum i, j;
  for (i = j = 0; i < ENCODING_BUFFER_MAX_SIZE; ) {
    if ((*buffer)+i >= buffer_end) {
      return EOF;
    }
    j = (j << 8) | (*buffer)[i];
    character = ecl_gethash_safe(ecl_make_fixnum(j), table, ECL_NIL);
    if (ECL_CHARACTERP(character)) {
      *buffer += i+1;
      return ECL_CHAR_CODE(character);
    }
    unlikely_if (Null(character)) {
      return decoding_error(stream, buffer, i+1, buffer_end);
    }
    if (character == ECL_T) {
      /* Need more characters */
      i++;
      continue;
    }
    if (CONSP(character)) {
      /* Changed the state. */
      stream->stream.format_table = table_list = character;
      table = ECL_CONS_CAR(table_list);
      *buffer += i+1;
      i = j = 0;
      continue;
    }
    break;
  }
  FEerror("Internal error in decoder table.", 0);
}

static int
user_multistate_encoder(cl_object stream, unsigned char *buffer, ecl_character c)
{
  cl_object table_list = stream->stream.format_table;
  cl_object p = table_list;
  do {
    cl_object table = ECL_CONS_CAR(p);
    cl_object byte = ecl_gethash_safe(ECL_CODE_CHAR(c), table, ECL_NIL);
    if (!Null(byte)) {
      cl_fixnum code = ecl_fixnum(byte);
      ecl_character n = 0;
      if (p != table_list) {
        /* Must output a escape sequence */
        cl_object x = ecl_gethash_safe(ECL_T, table, ECL_NIL);
        while (!Null(x)) {
          buffer[0] = ecl_fixnum(ECL_CONS_CAR(x));
          buffer++;
          x = ECL_CONS_CDR(x);
          n++;
        }
        stream->stream.format_table = p;
      }
      if (code > 0xFF) {
        buffer[1] = code & 0xFF; code >>= 8;
        buffer[0] = code;
        return n+2;
      } else {
        buffer[0] = code;
        return n+1;
      }
    }
    p = ECL_CONS_CDR(p);
  } while (p != table_list);
  /* Exhausted all lists */
  return encoding_error(stream, buffer, c);
}

/*
 * UTF-8
 */

static ecl_character
utf_8_decoder(cl_object stream, unsigned char **buffer, unsigned char *buffer_end)
{
  /* In understanding this code:
   * 0x8 = 1000, 0xC = 1100, 0xE = 1110, 0xF = 1111
   * 0x1 = 0001, 0x3 = 0011, 0x7 = 0111, 0xF = 1111
   */
  ecl_character cum = 0;
  int nbytes, i;
  unsigned char aux;
  if (*buffer >= buffer_end)
    return EOF;
  aux = (*buffer)[0];
  if ((aux & 0x80) == 0) {
    (*buffer)++;
    return aux;
  }
  unlikely_if ((aux & 0x40) == 0) {
    return decoding_error(stream, buffer, 1, buffer_end);
  }
  if ((aux & 0x20) == 0) {
    cum = aux & 0x1F;
    nbytes = 1;
  } else if ((aux & 0x10) == 0) {
    cum = aux & 0x0F;
    nbytes = 2;
  } else if ((aux & 0x08) == 0) {
    cum = aux & 0x07;
    nbytes = 3;
  } else {
    return decoding_error(stream, buffer, 1, buffer_end);
  }
  if ((*buffer)+nbytes >= buffer_end)
    return EOF;
  for (i = 1; i <= nbytes; i++) {
    unsigned char c = (*buffer)[i];
    unlikely_if ((c & 0xC0) != 0x80) {
      return decoding_error(stream, buffer, nbytes+1, buffer_end);
    }
    cum = (cum << 6) | (c & 0x3F);
    unlikely_if (cum == 0) {
      return decoding_error(stream, buffer, nbytes+1, buffer_end);
    }
  }
  if (cum >= 0xd800) {
    unlikely_if (cum <= 0xdfff) {
      return decoding_error(stream, buffer, nbytes+1, buffer_end);
    }
    unlikely_if (cum >= 0xFFFE && cum <= 0xFFFF) {
      return decoding_error(stream, buffer, nbytes+1, buffer_end);
    }
  }
  *buffer += nbytes+1;
  return cum;
}

static int
utf_8_encoder(cl_object stream, unsigned char *buffer, ecl_character c)
{
  int nbytes = 0;
  if (c <= 0x7F) {
    buffer[0] = c;
    nbytes = 1;
  } else if (c <= 0x7ff) {
    buffer[1] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[0] = c | 0xC0;
    /*printf("\n; %04x ;: %04x :: %04x :\n", c_orig, buffer[0], buffer[1]);*/
    nbytes = 2;
  } else if (c <= 0xFFFF) {
    buffer[2] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[1] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[0] = c | 0xE0;
    nbytes = 3;
  } else if (c <= 0x1FFFFFL) {
    buffer[3] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[2] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[1] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[0] = c | 0xF0;
    nbytes = 4;
  }
  return nbytes;
}
#endif

static cl_index
compute_char_size(cl_object stream, ecl_character c)
{
  unsigned char buffer[5];
  int l = 0;
  if (c == ECL_CHAR_CODE_NEWLINE) {
    int flags = stream->stream.flags;
    if (flags & ECL_STREAM_CR) {
      l += stream->stream.encoder(stream, buffer, ECL_CHAR_CODE_RETURN);
      if (flags & ECL_STREAM_LF)
        l += stream->stream.encoder(stream, buffer,
                                    ECL_CHAR_CODE_LINEFEED);
    } else {
      l += stream->stream.encoder(stream, buffer, ECL_CHAR_CODE_LINEFEED);
    }
  } else {
    l += stream->stream.encoder(stream, buffer, c);
  }
  return l;
}

cl_object
file_string_length(cl_object stream, cl_object string)
{
  cl_fixnum l = 0;
  switch (ecl_t_of(string)) {
#ifdef ECL_UNICODE
  case t_string:
#endif
  case t_base_string: {
    cl_index i;
    for (i = 0; i < string->base_string.fillp; i++) {
      l += compute_char_size(stream, ecl_char(string, i));
    }
    break;
  }
  case t_character:
    l = compute_char_size(stream, ECL_CHAR_CODE(string));
    break;
  default:
    FEwrong_type_nth_arg(@[file-string-length], 2, string, @[string]);
  }
  return ecl_make_fixnum(l);
}

/********************************************************************************
 * CLOS STREAMS
 */

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
  cl_object b = _ecl_funcall2(@'gray::stream-read-byte', strm);
  if (b == @':eof') b = ECL_NIL;
  return b;
}

static void
clos_stream_write_byte(cl_object c, cl_object strm)
{
  _ecl_funcall3(@'gray::stream-write-byte', strm, c);
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

static int
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
  clos_stream_write_byte8,
  clos_stream_read_byte8,

  clos_stream_write_byte,
  clos_stream_read_byte,

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
  not_output_write_byte8,
  not_binary_read_byte8,

  not_binary_write_byte,
  not_input_read_byte,

  not_input_read_char,
  str_out_write_char,
  not_input_unread_char,
  generic_peek_char,

  generic_read_vector,
  generic_write_vector,

  not_input_listen,
  not_input_clear_input,
  generic_void, /* clear-output */
  generic_void, /* finish-output */
  generic_void, /* force-output */

  generic_always_false, /* input_p */
  generic_always_true, /* output_p */
  generic_always_false,
  str_out_element_type,

  not_a_file_stream, /* length */
  str_out_get_position,
  str_out_set_position,
  str_out_string_length,
  generic_column,

  not_a_file_stream,
  not_a_file_stream,

  generic_close
};


cl_object
si_make_string_output_stream_from_string(cl_object s)
{
  cl_object strm = alloc_stream();
  unlikely_if (!ECL_STRINGP(s) || !ECL_ARRAY_HAS_FILL_POINTER_P(s))
    FEerror("~S is not a -string with a fill-pointer.", 1, s);
  strm->stream.ops = duplicate_dispatch_table(&str_out_ops);
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
  unlikely_if (c <= 0) {
    unread_error(strm);
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
  not_output_write_byte8,
  not_binary_read_byte8,

  not_output_write_byte,
  not_binary_read_byte,

  str_in_read_char,
  not_output_write_char,
  str_in_unread_char,
  str_in_peek_char,

  generic_read_vector,
  generic_write_vector,

  str_in_listen,
  generic_void, /* clear-input */
  not_output_clear_output,
  not_output_finish_output,
  not_output_force_output,

  generic_always_true, /* input_p */
  generic_always_false, /* output_p */
  generic_always_false,
  str_in_element_type,

  not_a_file_stream, /* length */
  str_in_get_position,
  str_in_set_position,
  not_output_string_length,
  unknown_column,

  not_a_file_stream,
  not_a_file_stream,

  generic_close
};

cl_object
ecl_make_string_input_stream(cl_object strng, cl_index istart, cl_index iend)
{
  cl_object strm;

  strm = alloc_stream();
  strm->stream.ops = duplicate_dispatch_table(&str_in_ops);
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
two_way_write_byte(cl_object byte, cl_object stream)
{
  ecl_write_byte(byte, TWO_WAY_STREAM_OUTPUT(stream));
}

static cl_object
two_way_read_byte(cl_object stream)
{
  return ecl_read_byte(TWO_WAY_STREAM_INPUT(stream));
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
  return stream_dispatch_table(strm)->read_vector(strm, data, start, n);
}

static cl_index
two_way_write_vector(cl_object strm, cl_object data, cl_index start, cl_index n)
{
  strm = TWO_WAY_STREAM_OUTPUT(strm);
  return stream_dispatch_table(strm)->write_vector(strm, data, start, n);
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
  return generic_close(strm);
}

const struct ecl_file_ops two_way_ops = {
  two_way_write_byte8,
  two_way_read_byte8,

  two_way_write_byte,
  two_way_read_byte,

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

  generic_always_true, /* input_p */
  generic_always_true, /* output_p */
  two_way_interactive_p,
  two_way_element_type,

  not_a_file_stream, /* length */
  generic_always_nil, /* get_position */
  generic_set_position,
  not_file_string_length,
  two_way_column,

  not_a_file_stream,
  not_a_file_stream,

  two_way_close
};


cl_object
cl_make_two_way_stream(cl_object istrm, cl_object ostrm)
{
  cl_object strm;
  if (!ecl_input_stream_p(istrm))
    not_an_input_stream(istrm);
  if (!ecl_output_stream_p(ostrm))
    not_an_output_stream(ostrm);
  strm = alloc_stream();
  strm->stream.format = cl_stream_external_format(istrm);
  strm->stream.mode = (short)ecl_smm_two_way;
  strm->stream.ops = duplicate_dispatch_table(&two_way_ops);
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
broadcast_write_byte(cl_object c, cl_object strm)
{
  cl_object l;
  for (l = BROADCAST_STREAM_LIST(strm); !Null(l); l = ECL_CONS_CDR(l)) {
    ecl_write_byte(c, ECL_CONS_CAR(l));
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
  return generic_close(strm);
}

const struct ecl_file_ops broadcast_ops = {
  broadcast_write_byte8,
  not_input_read_byte8,

  broadcast_write_byte,
  not_input_read_byte,

  not_input_read_char,
  broadcast_write_char,
  not_input_unread_char,
  generic_peek_char,

  generic_read_vector,
  generic_write_vector,

  not_input_listen,
  broadcast_force_output, /* clear_input */ /* FIXME! This is legacy behaviour */
  broadcast_clear_output,
  broadcast_finish_output,
  broadcast_force_output,

  generic_always_false, /* input_p */
  generic_always_true, /* output_p */
  generic_always_false,
  broadcast_element_type,

  broadcast_length,
  broadcast_get_position,
  broadcast_set_position,
  broadcast_string_length,
  broadcast_column,

  not_a_file_stream,
  not_a_file_stream,

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
      not_an_output_stream(x);
    streams = CONS(x, streams);
  }
  x = alloc_stream();
  x->stream.format = @':default';
  x->stream.ops = duplicate_dispatch_table(&broadcast_ops);
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

static void
echo_write_byte(cl_object c, cl_object strm)
{
  ecl_write_byte(c, ECHO_STREAM_OUTPUT(strm));
}

static cl_object
echo_read_byte(cl_object strm)
{
  cl_object out = ecl_read_byte(ECHO_STREAM_INPUT(strm));
  if (!Null(out)) ecl_write_byte(out, ECHO_STREAM_OUTPUT(strm));
  return out;
}

static ecl_character
echo_read_char(cl_object strm)
{
  ecl_character c = strm->stream.last_code[0];
  if (c == EOF) {
    c = ecl_read_char(ECHO_STREAM_INPUT(strm));
    if (c != EOF)
      ecl_write_char(c, ECHO_STREAM_OUTPUT(strm));
  } else {
    strm->stream.last_code[0] = EOF;
    ecl_read_char(ECHO_STREAM_INPUT(strm));
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
  unlikely_if (strm->stream.last_code[0] != EOF) {
    unread_twice(strm);
  }
  strm->stream.last_code[0] = c;
  ecl_unread_char(c, ECHO_STREAM_INPUT(strm));
}

static ecl_character
echo_peek_char(cl_object strm)
{
  ecl_character c = strm->stream.last_code[0];
  if (c == EOF) {
    c = ecl_peek_char(ECHO_STREAM_INPUT(strm));
  }
  return c;
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
  return generic_close(strm);
}

const struct ecl_file_ops echo_ops = {
  echo_write_byte8,
  echo_read_byte8,

  echo_write_byte,
  echo_read_byte,

  echo_read_char,
  echo_write_char,
  echo_unread_char,
  echo_peek_char,

  generic_read_vector,
  generic_write_vector,

  echo_listen,
  echo_clear_input,
  echo_clear_output,
  echo_finish_output,
  echo_force_output,

  generic_always_true, /* input_p */
  generic_always_true, /* output_p */
  generic_always_false,
  echo_element_type,

  not_a_file_stream, /* length */
  generic_always_nil, /* get_position */
  generic_set_position,
  not_file_string_length,
  echo_column,

  not_a_file_stream,
  not_a_file_stream,

  echo_close
};

cl_object
cl_make_echo_stream(cl_object strm1, cl_object strm2)
{
  cl_object strm;
  unlikely_if (!ecl_input_stream_p(strm1))
    not_an_input_stream(strm1);
  unlikely_if (!ecl_output_stream_p(strm2))
    not_an_output_stream(strm2);
  strm = alloc_stream();
  strm->stream.format = cl_stream_external_format(strm1);
  strm->stream.mode = (short)ecl_smm_echo;
  strm->stream.ops = duplicate_dispatch_table(&echo_ops);
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
  cl_object c = ECL_NIL;
  while (!Null(l)) {
    c = ecl_read_byte(ECL_CONS_CAR(l));
    if (c != ECL_NIL) break;
    CONCATENATED_STREAM_LIST(strm) = l = ECL_CONS_CDR(l);
  }
  return c;
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
  unlikely_if (Null(l))
    unread_error(strm);
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
  return generic_close(strm);
}

const struct ecl_file_ops concatenated_ops = {
  not_output_write_byte8,
  concatenated_read_byte8,

  not_output_write_byte,
  concatenated_read_byte,

  concatenated_read_char,
  not_output_write_char,
  concatenated_unread_char,
  generic_peek_char,

  generic_read_vector,
  generic_write_vector,

  concatenated_listen,
  generic_void, /* clear_input */
  not_output_clear_output,
  not_output_finish_output,
  not_output_force_output,

  generic_always_true, /* input_p */
  generic_always_false, /* output_p */
  generic_always_false,
  broadcast_element_type,

  not_a_file_stream, /* length */
  generic_always_nil, /* get_position */
  generic_set_position,
  not_output_string_length,
  unknown_column,

  not_a_file_stream,
  not_a_file_stream,

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
      not_an_input_stream(x);
    streams = CONS(x, streams);
  }
  x = alloc_stream();
  if (Null(streams)) {
    x->stream.format = @':pass-through';
  } else {
    x->stream.format = cl_stream_external_format(ECL_CONS_CAR(streams));
  }
  x->stream.mode = (short)ecl_smm_concatenated;
  x->stream.ops = duplicate_dispatch_table(&concatenated_ops);
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
synonym_write_byte(cl_object c, cl_object strm)
{
  ecl_write_byte(c, SYNONYM_STREAM_STREAM(strm));
}

static cl_object
synonym_read_byte(cl_object strm)
{
  return ecl_read_byte(SYNONYM_STREAM_STREAM(strm));
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
  return stream_dispatch_table(strm)->read_vector(strm, data, start, n);
}

static cl_index
synonym_write_vector(cl_object strm, cl_object data, cl_index start, cl_index n)
{
  strm = SYNONYM_STREAM_STREAM(strm);
  return stream_dispatch_table(strm)->write_vector(strm, data, start, n);
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
  synonym_write_byte8,
  synonym_read_byte8,

  synonym_write_byte,
  synonym_read_byte,

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

  generic_close
};

cl_object
cl_make_synonym_stream(cl_object sym)
{
  cl_object x;

  sym = ecl_check_cl_type(@'make-synonym-stream',sym,t_symbol);
  x = alloc_stream();
  x->stream.ops = duplicate_dispatch_table(&synonym_ops);
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

/**********************************************************************
 * UNINTERRUPTED OPERATIONS
 */

#ifdef ECL_MS_WINDOWS_HOST
#define ecl_mode_t int
#else
#define ecl_mode_t mode_t
#endif

static int
safe_open(const ecl_filename_char *filename, int flags, ecl_mode_t mode)
{
  const cl_env_ptr the_env = ecl_process_env();
  int output;
  ecl_disable_interrupts_env(the_env);
  output = ecl_open(filename, flags, mode);
  ecl_enable_interrupts_env(the_env);
  return output;
}

static int
safe_close(int f)
{
  const cl_env_ptr the_env = ecl_process_env();
  int output;
  ecl_disable_interrupts_env(the_env);
  output = close(f);
  ecl_enable_interrupts_env(the_env);
  return output;
}

static FILE *
safe_fdopen(int fildes, const ecl_filename_char *mode)
{
  const cl_env_ptr the_env = ecl_process_env();
  FILE *output;
  ecl_disable_interrupts_env(the_env);
  output = ecl_fdopen(fildes, mode);
  ecl_enable_interrupts_env(the_env);
  return output;
}

static int
safe_fclose(FILE *stream)
{
  const cl_env_ptr the_env = ecl_process_env();
  int output;
  /* If someone have closed our fd, do nothing. See #267. */
  unlikely_if (fileno(stream) == -1) return 0;
  ecl_disable_interrupts_env(the_env);
  output = fclose(stream);
  ecl_enable_interrupts_env(the_env);
  return output;
}

/**********************************************************************
 * POSIX FILE STREAM
 */

static cl_index
consume_byte_stack(cl_object strm, unsigned char *c, cl_index n)
{
  cl_index out = 0;
  while (n) {
    cl_object l = strm->stream.byte_stack;
    if (l == ECL_NIL)
      return out + strm->stream.ops->read_byte8(strm, c, n);
    *(c++) = ecl_fixnum(ECL_CONS_CAR(l));
    out++;
    n--;
    strm->stream.byte_stack = l = ECL_CONS_CDR(l);
  }
  return out;
}

static cl_index
io_file_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  unlikely_if (strm->stream.byte_stack != ECL_NIL) {
    return consume_byte_stack(strm, c, n);
  } else {
    int f = IO_FILE_DESCRIPTOR(strm);
    cl_fixnum out = 0;
    ecl_disable_interrupts();
    do {
      out = read(f, c, sizeof(char)*n);
    } while (out < 0 && restartable_io_error(strm, "read"));
    ecl_enable_interrupts();
    return out;
  }
}

static cl_index
output_file_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  int f = IO_FILE_DESCRIPTOR(strm);
  cl_fixnum out;
  ecl_disable_interrupts();
  do {
    out = write(f, c, sizeof(char)*n);
  } while (out < 0 && restartable_io_error(strm, "write"));
  ecl_enable_interrupts();
  return out;
}

static cl_index
io_file_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  unlikely_if (strm->stream.byte_stack != ECL_NIL) {
    /* Try to move to the beginning of the unread characters */
    cl_object aux = ecl_file_position(strm);
    if (!Null(aux))
      ecl_file_position_set(strm, aux);
  }
  return output_file_write_byte8(strm, c, n);
}

static int
io_file_listen(cl_object strm)
{
  if (strm->stream.byte_stack != ECL_NIL)
    return ECL_LISTEN_AVAILABLE;
  if (strm->stream.flags & ECL_STREAM_MIGHT_SEEK) {
    cl_env_ptr the_env = ecl_process_env();
    int f = IO_FILE_DESCRIPTOR(strm);
    ecl_off_t disp, new;
    ecl_disable_interrupts_env(the_env);
    disp = lseek(f, 0, SEEK_CUR);
    ecl_enable_interrupts_env(the_env);
    if (disp != (ecl_off_t)-1) {
      ecl_disable_interrupts_env(the_env);
      new = lseek(f, 0, SEEK_END);
      ecl_enable_interrupts_env(the_env);
      lseek(f, disp, SEEK_SET);
      if (new == disp) {
        return ECL_LISTEN_NO_CHAR;
      } else if (new != (ecl_off_t)-1) {
        return ECL_LISTEN_AVAILABLE;
      }
    }
  }
  return fd_listen(strm, IO_FILE_DESCRIPTOR(strm));
}

#if defined(ECL_MS_WINDOWS_HOST)
static int
isaconsole(int i)
{
  HANDLE h = (HANDLE)_get_osfhandle(i);
  DWORD mode;
  return !!GetConsoleMode(h, &mode);
}
#define isatty isaconsole
#endif

static void
io_file_clear_input(cl_object strm)
{
  int f = IO_FILE_DESCRIPTOR(strm);
#if defined(ECL_MS_WINDOWS_HOST)
  if (isatty(f)) {
    /* Flushes Win32 console */
    if (!FlushConsoleInputBuffer((HANDLE)_get_osfhandle(f)))
      FEwin32_error("FlushConsoleInputBuffer() failed", 0);
    /* Do not stop here: the FILE structure needs also to be flushed */
  }
#endif
  while (fd_listen(strm, f) == ECL_LISTEN_AVAILABLE) {
    ecl_character c = eformat_read_char(strm);
    if (c == EOF) return;
  }
}

#define io_file_clear_output generic_void
#define io_file_force_output generic_void
#define io_file_finish_output io_file_force_output

static int
io_file_interactive_p(cl_object strm)
{
  int f = IO_FILE_DESCRIPTOR(strm);
  return isatty(f);
}

static cl_object
io_file_element_type(cl_object strm)
{
  return IO_FILE_ELT_TYPE(strm);
}

static cl_object
io_file_length(cl_object strm)
{
  int f = IO_FILE_DESCRIPTOR(strm);
  cl_object output = ecl_file_len(f);
  if (strm->stream.byte_size != 8) {
    const cl_env_ptr the_env = ecl_process_env();
    cl_index bs = strm->stream.byte_size;
    output = ecl_floor2(output, ecl_make_fixnum(bs/8));
    unlikely_if (ecl_nth_value(the_env, 1) != ecl_make_fixnum(0)) {
      FEerror("File length is not on byte boundary", 0);
    }
  }
  return output;
}

static cl_object
io_file_get_position(cl_object strm)
{
  cl_object output;
  ecl_off_t offset;

  int f = IO_FILE_DESCRIPTOR(strm);
  if (isatty(f)) return(ECL_NIL);

  ecl_disable_interrupts();
  offset = lseek(f, 0, SEEK_CUR);
  ecl_enable_interrupts();
  unlikely_if (offset < 0)
  {
    if (errno == ESPIPE)
      return(ECL_NIL);
    else
      io_error(strm);
  }
  if (sizeof(ecl_off_t) == sizeof(long)) {
    output = ecl_make_integer(offset);
  } else {
    output = ecl_off_t_to_integer(offset);
  }
  {
    /* If there are unread octets, we return the position at which
     * these bytes begin! */
    cl_object l = strm->stream.byte_stack;
    while (CONSP(l)) {
      output = ecl_one_minus(output);
      l = ECL_CONS_CDR(l);
    }
  }
  if (strm->stream.byte_size != 8) {
    output = ecl_floor2(output, ecl_make_fixnum(strm->stream.byte_size / 8));
  }
  return output;
}

static cl_object
io_file_set_position(cl_object strm, cl_object large_disp)
{
  ecl_off_t disp;
  int mode;
  int f = IO_FILE_DESCRIPTOR(strm);
  if (isatty(f)) return(ECL_NIL);
  strm->stream.byte_stack = ECL_NIL;
  if (Null(large_disp)) {
    disp = 0;
    mode = SEEK_END;
  } else {
    if (strm->stream.byte_size != 8) {
      large_disp = ecl_times(large_disp,
                             ecl_make_fixnum(strm->stream.byte_size / 8));
    }
    disp = ecl_integer_to_off_t(large_disp);
    mode = SEEK_SET;
  }
  disp = lseek(f, disp, mode);
  return (disp == (ecl_off_t)-1)? ECL_NIL : ECL_T;
}

static cl_object
io_file_close(cl_object strm)
{
  int f = IO_FILE_DESCRIPTOR(strm);
  int failed;
  unlikely_if (f == STDOUT_FILENO)
    FEerror("Cannot close the standard output", 0);
  unlikely_if (f == STDIN_FILENO)
    FEerror("Cannot close the standard input", 0);
  failed = safe_close(f);
  unlikely_if (failed < 0)
    cannot_close(strm);
  IO_FILE_DESCRIPTOR(strm) = -1;
  return generic_close(strm);
}

static ecl_character io_file_decode_char_from_buffer(cl_object strm, unsigned char *buffer, unsigned char **buffer_pos, unsigned char **buffer_end, bool seekable, cl_index min_needed_bytes) {
  bool crlf = 0;
  unsigned char *previous_buffer_pos;
  ecl_character c;
 AGAIN:
  previous_buffer_pos = *buffer_pos;
  c = strm->stream.decoder(strm, buffer_pos, *buffer_end);
  if (c != EOF) {
    /* Ugly handling of line breaks */
    if (crlf) {
      if (c == ECL_CHAR_CODE_LINEFEED) {
        strm->stream.last_code[1] = c;
        c = ECL_CHAR_CODE_NEWLINE;
      }
      else {
        *buffer_pos = previous_buffer_pos;
        c = ECL_CHAR_CODE_RETURN;
      }
    } else if (strm->stream.flags & ECL_STREAM_CR && c == ECL_CHAR_CODE_RETURN) {
      if (strm->stream.flags & ECL_STREAM_LF) {
        strm->stream.last_code[0] = c;
        crlf = 1;
        goto AGAIN;
      }
      else
        c = ECL_CHAR_CODE_NEWLINE;
    }
    if (!crlf) {
      strm->stream.last_code[0] = c;
      strm->stream.last_code[1] = EOF;
    }
    strm->stream.last_char = c;
    return c;
  } else {
    /* We need more bytes. First copy unconsumed bytes at the
     * beginning of buffer. */
    cl_index unconsumed_bytes = *buffer_end - *buffer_pos;
    memcpy(buffer, *buffer_pos, unconsumed_bytes);
    cl_index needed_bytes = VECTOR_ENCODING_BUFFER_SIZE;
    if (!seekable && min_needed_bytes < VECTOR_ENCODING_BUFFER_SIZE)
      needed_bytes = min_needed_bytes;
    *buffer_end = buffer + unconsumed_bytes
      + ecl_read_byte8(strm, buffer + unconsumed_bytes, needed_bytes);
    if (*buffer_end == buffer + unconsumed_bytes)
      return EOF;
    *buffer_pos = buffer;
    goto AGAIN;
  }
}

/* The following macros create a temporary buffer for efficient
 * reading of characters from file streams */
#define ECL_FILE_READ_BUFFER_CREATE(strm) \
  cl_object __stream = (strm); \
  unsigned char __buffer[VECTOR_ENCODING_BUFFER_SIZE + ENCODING_BUFFER_MAX_SIZE]; \
  unsigned char *__buffer_pos = __buffer; \
  unsigned char *__buffer_end = __buffer; \
  /* When we can't call lseek/fseek we have to be conservative and \
   * read only as many bytes as we actually need. Otherwise, we read \
   * more and later reposition the file offset. */ \
  bool __seekable = ecl_file_position(__stream) != ECL_NIL; \
  ECL_UNWIND_PROTECT_BEGIN(ecl_process_env()) {

#define ECL_FILE_READ_BUFFER_GET_CHAR(min_needed_bytes) \
  io_file_decode_char_from_buffer(__stream, __buffer, &__buffer_pos, &__buffer_end, __seekable, min_needed_bytes)

#define ECL_FILE_READ_BUFFER_DESTROY \
  } ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT { \
    if (__seekable) \
      /* INV: (buffer_end - buffer_pos) is divisible by \
       * (strm->stream.byte_size / 8) since VECTOR_ENCODING_BUFFER_SIZE \
       * is divisible by all byte sizes for character streams and all \
       * decoders consume bytes in multiples of the byte size. */ \
      ecl_file_position_set(__stream, ecl_minus(ecl_file_position(__stream), \
        ecl_make_fixnum((__buffer_end - __buffer_pos) / (__stream->stream.byte_size / 8)))); \
  } ECL_UNWIND_PROTECT_THREAD_SAFE_END

static cl_index
io_file_read_vector(cl_object strm, cl_object data, cl_index start, cl_index end)
{
  cl_elttype t = ecl_array_elttype(data);
  if (start >= end)
    return start;
  if (t == ecl_aet_b8 || t == ecl_aet_i8) {
    if (strm->stream.byte_size == 8) {
      void *aux = data->vector.self.bc + start;
      return start + strm->stream.ops->read_byte8(strm, aux, end-start);
    }
  } else if (t == ecl_aet_fix || t == ecl_aet_index) {
    if (strm->stream.byte_size == sizeof(cl_fixnum)*8) {
      void *aux = data->vector.self.fix + start;
      cl_index bytes = (end - start) * sizeof(cl_fixnum);
      bytes = strm->stream.ops->read_byte8(strm, aux, bytes);
      return start + bytes / sizeof(cl_fixnum);
    }
  } else if (ecl_stream_element_type(strm) == @'base-char'
             || ecl_stream_element_type(strm) == @'character') {
    ECL_FILE_READ_BUFFER_CREATE(strm) {
      while (start < end) {
        ecl_character c = ECL_FILE_READ_BUFFER_GET_CHAR((end-start) * (strm->stream.byte_size / 8));
        if (c != EOF)
          ecl_elt_set(data, start++, ECL_CODE_CHAR(c));
        else
          break;
      }
    } ECL_FILE_READ_BUFFER_DESTROY;
    return start;
  }
  return generic_read_vector(strm, data, start, end);
}

static cl_index
io_file_write_vector(cl_object strm, cl_object data, cl_index start, cl_index end)
{
  cl_elttype t = ecl_array_elttype(data);
  if (start >= end)
    return start;
  if (t == ecl_aet_b8 || t == ecl_aet_i8) {
    if (strm->stream.byte_size == 8) {
      void *aux = data->vector.self.bc + start;
      return strm->stream.ops->write_byte8(strm, aux, end-start);
    }
  } else if (t == ecl_aet_fix || t == ecl_aet_index) {
    if (strm->stream.byte_size == sizeof(cl_fixnum)*8) {
      void *aux = data->vector.self.fix + start;
      cl_index bytes = (end - start) * sizeof(cl_fixnum);
      bytes = strm->stream.ops->write_byte8(strm, aux, bytes);
      return start + bytes / sizeof(cl_fixnum);
    }
  } else if (t == ecl_aet_bc) {
    /* 1 extra byte for linefeed in crlf mode */
    unsigned char buffer[VECTOR_ENCODING_BUFFER_SIZE + ENCODING_BUFFER_MAX_SIZE + 1];
    cl_index nbytes = 0;
    cl_index i;
    for (i = start; i < end; i++) {
      ecl_character c = *(data->vector.self.bc + i);
      if (c == ECL_CHAR_CODE_NEWLINE) {
        if (strm->stream.flags & ECL_STREAM_CR &&
            strm->stream.flags & ECL_STREAM_LF)
          nbytes += strm->stream.encoder(strm, buffer + nbytes, ECL_CHAR_CODE_RETURN);
        else if (strm->stream.flags & ECL_STREAM_CR)
          c = ECL_CHAR_CODE_RETURN;
        strm->stream.column = 0;
      }
      nbytes += strm->stream.encoder(strm, buffer + nbytes, c);
      write_char_increment_column(strm, c);
      if (nbytes >= VECTOR_ENCODING_BUFFER_SIZE) {
        strm->stream.ops->write_byte8(strm, buffer, nbytes);
        nbytes = 0;
      }
    }
    strm->stream.ops->write_byte8(strm, buffer, nbytes);
    return end;
  }
#ifdef ECL_UNICODE
  else if (t == ecl_aet_ch) {
    /* 1 extra byte for linefeed in crlf mode */
    unsigned char buffer[VECTOR_ENCODING_BUFFER_SIZE + ENCODING_BUFFER_MAX_SIZE + 1];
    cl_index nbytes = 0;
    cl_index i;
    for (i = start; i < end; i++) {
      ecl_character c = *(data->vector.self.c + i);
      if (c == ECL_CHAR_CODE_NEWLINE) {
        if (strm->stream.flags & ECL_STREAM_CR &&
            strm->stream.flags & ECL_STREAM_LF)
          nbytes += strm->stream.encoder(strm, buffer + nbytes, ECL_CHAR_CODE_RETURN);
        else if (strm->stream.flags & ECL_STREAM_CR)
          c = ECL_CHAR_CODE_RETURN;
        strm->stream.column = 0;
      }
      nbytes += strm->stream.encoder(strm, buffer + nbytes, c);
      write_char_increment_column(strm, c);
      if (nbytes >= VECTOR_ENCODING_BUFFER_SIZE) {
        strm->stream.ops->write_byte8(strm, buffer, nbytes);
        nbytes = 0;
      }
    }
    strm->stream.ops->write_byte8(strm, buffer, nbytes);
    return end;
  }
#endif
  return generic_write_vector(strm, data, start, end);
}

static cl_object
io_file_pathname(cl_object strm)
{
  return IO_STREAM_FILENAME(strm);
}

static cl_object
io_file_truename(cl_object strm)
{
  return cl_truename(IO_STREAM_FILENAME(strm));
}

const struct ecl_file_ops io_file_ops = {
  io_file_write_byte8,
  io_file_read_byte8,

  generic_write_byte,
  generic_read_byte,

  eformat_read_char,
  eformat_write_char,
  eformat_unread_char,
  generic_peek_char,

  io_file_read_vector,
  io_file_write_vector,

  io_file_listen,
  io_file_clear_input,
  io_file_clear_output,
  io_file_finish_output,
  io_file_force_output,

  generic_always_true, /* input_p */
  generic_always_true, /* output_p */
  io_file_interactive_p,
  io_file_element_type,

  io_file_length,
  io_file_get_position,
  io_file_set_position,
  file_string_length,
  generic_column,

  io_file_pathname,
  io_file_truename,

  io_file_close
};

const struct ecl_file_ops output_file_ops = {
  output_file_write_byte8,
  not_input_read_byte8,

  generic_write_byte,
  not_input_read_byte,

  not_input_read_char,
  eformat_write_char,
  not_input_unread_char,
  not_input_read_char,

  generic_read_vector,
  io_file_write_vector,

  not_input_listen,
  not_input_clear_input,
  io_file_clear_output,
  io_file_finish_output,
  io_file_force_output,

  generic_always_false, /* input_p */
  generic_always_true, /* output_p */
  generic_always_false,
  io_file_element_type,

  io_file_length,
  io_file_get_position,
  io_file_set_position,
  file_string_length,
  generic_column,

  io_file_pathname,
  io_file_truename,

  io_file_close
};

const struct ecl_file_ops input_file_ops = {
  not_output_write_byte8,
  io_file_read_byte8,

  not_output_write_byte,
  generic_read_byte,

  eformat_read_char,
  not_output_write_char,
  eformat_unread_char,
  generic_peek_char,

  io_file_read_vector,
  generic_write_vector,

  io_file_listen,
  io_file_clear_input,
  not_output_clear_output,
  not_output_finish_output,
  not_output_force_output,

  generic_always_true, /* input_p */
  generic_always_false, /* output_p */
  io_file_interactive_p,
  io_file_element_type,

  io_file_length,
  io_file_get_position,
  io_file_set_position,
  not_output_string_length,
  unknown_column,

  io_file_pathname,
  io_file_truename,

  io_file_close
};


static int
parse_external_format(cl_object stream, cl_object format, int flags)
{
  if (format == @':default') {
    format = ecl_symbol_value(@'ext::*default-external-format*');
  }
  if (CONSP(format)) {
    flags = parse_external_format(stream, ECL_CONS_CDR(format), flags);
    format = ECL_CONS_CAR(format);
  }
  if (format == ECL_T) {
#ifdef ECL_UNICODE
    return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_UTF_8;
#else
    return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_DEFAULT_FORMAT;
#endif
  }
  if (format == ECL_NIL) {
    return flags;
  }
  if (format == @':CR') {
    return (flags | ECL_STREAM_CR) & ~ECL_STREAM_LF;
  }
  if (format == @':LF') {
    return (flags | ECL_STREAM_LF) & ~ECL_STREAM_CR;
  }
  if (format == @':CRLF') {
    return flags | (ECL_STREAM_CR+ECL_STREAM_LF);
  }
  if (format == @':LITTLE-ENDIAN') {
    return flags | ECL_STREAM_LITTLE_ENDIAN;
  }
  if (format == @':BIG-ENDIAN') {
    return flags & ~ECL_STREAM_LITTLE_ENDIAN;
  }
  if (format == @':pass-through') {
#ifdef ECL_UNICODE
    return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_LATIN_1;
#else
    return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_DEFAULT_FORMAT;
#endif
  }
#ifdef ECL_UNICODE
 PARSE_SYMBOLS:
  if (format == @':UTF-8') {
    return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_UTF_8; 
  }
  if (format == @':UCS-2') {
    return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_UCS_2;
  }
  if (format == @':UCS-2BE') {
    return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_UCS_2BE;
  }
  if (format == @':UCS-2LE') {
    return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_UCS_2LE;
  }
  if (format == @':UCS-4') {
    return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_UCS_4;
  }
  if (format == @':UCS-4BE') {
    return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_UCS_4BE;
  }
  if (format == @':UCS-4LE') {
    return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_UCS_4LE;
  }
  if (format == @':ISO-8859-1') {
    return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_ISO_8859_1;
  }
  if (format == @':LATIN-1') {
    return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_LATIN_1;
  }
  if (format == @':US-ASCII') {
    return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_US_ASCII; 
  }
  if (ECL_HASH_TABLE_P(format)) {
    stream->stream.format_table = format;
    return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_USER_FORMAT;
  }
  if (ECL_SYMBOLP(format)) {
    format = _ecl_funcall2(@'ext::make-encoding', format);
    if (ECL_SYMBOLP(format))
      goto PARSE_SYMBOLS;
    stream->stream.format_table = format;
    return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_USER_FORMAT;
  }
#endif
  FEerror("Unknown or unsupported external format: ~A", 1, format);
  return ECL_STREAM_DEFAULT_FORMAT;
}

static void
set_stream_elt_type(cl_object stream, cl_fixnum byte_size, int flags,
                    cl_object external_format)
{
  cl_object t;
  if (byte_size < 0) {
    byte_size = -byte_size;
    flags |= ECL_STREAM_SIGNED_BYTES;
    t = @'signed-byte';
  } else {
    flags &= ~ECL_STREAM_SIGNED_BYTES;
    t = @'unsigned-byte';
  }
  if (external_format != ECL_NIL) {
    flags = parse_external_format(stream, external_format, flags);
  }
  stream->stream.ops->read_char = eformat_read_char;
  stream->stream.ops->write_char = eformat_write_char;
  switch (flags & ECL_STREAM_FORMAT) {
  case ECL_STREAM_BINARY:
    IO_STREAM_ELT_TYPE(stream) = cl_list(2, t, ecl_make_fixnum(byte_size));
    stream->stream.format = t;
    stream->stream.ops->read_char = not_character_read_char;
    stream->stream.ops->write_char = not_character_write_char;
    stream->stream.decoder = not_character_decoder;
    stream->stream.encoder = not_character_encoder;
    break;
#ifdef ECL_UNICODE
    /*case ECL_ISO_8859_1:*/
  case ECL_STREAM_LATIN_1:
    IO_STREAM_ELT_TYPE(stream) = @'base-char';
    byte_size = 8;
    stream->stream.format = @':latin-1';
    stream->stream.encoder = passthrough_encoder;
    stream->stream.decoder = passthrough_decoder;
    break;
  case ECL_STREAM_UTF_8:
    IO_STREAM_ELT_TYPE(stream) = @'character';
    byte_size = 8;
    stream->stream.format = @':utf-8';
    stream->stream.encoder = utf_8_encoder;
    stream->stream.decoder = utf_8_decoder;
    break;
  case ECL_STREAM_UCS_2:
    IO_STREAM_ELT_TYPE(stream) = @'character';
    byte_size = 8*2;
    stream->stream.format = @':ucs-2';
    stream->stream.encoder = ucs_2_encoder;
    stream->stream.decoder = ucs_2_decoder;
    break;
  case ECL_STREAM_UCS_2BE:
    IO_STREAM_ELT_TYPE(stream) = @'character';
    byte_size = 8*2;
    if (flags & ECL_STREAM_LITTLE_ENDIAN) {
      stream->stream.format = @':ucs-2le';
      stream->stream.encoder = ucs_2le_encoder;
      stream->stream.decoder = ucs_2le_decoder;
    } else {
      stream->stream.format = @':ucs-2be';
      stream->stream.encoder = ucs_2be_encoder;
      stream->stream.decoder = ucs_2be_decoder;
    }
    break;
  case ECL_STREAM_UCS_4:
    IO_STREAM_ELT_TYPE(stream) = @'character';
    byte_size = 8*4;
    stream->stream.format = @':ucs-4be';
    stream->stream.encoder = ucs_4_encoder;
    stream->stream.decoder = ucs_4_decoder;
    break;
  case ECL_STREAM_UCS_4BE:
    IO_STREAM_ELT_TYPE(stream) = @'character';
    byte_size = 8*4;
    if (flags & ECL_STREAM_LITTLE_ENDIAN) {
      stream->stream.format = @':ucs-4le';
      stream->stream.encoder = ucs_4le_encoder;
      stream->stream.decoder = ucs_4le_decoder;
    } else {
      stream->stream.format = @':ucs-4be';
      stream->stream.encoder = ucs_4be_encoder;
      stream->stream.decoder = ucs_4be_decoder;
    }
    break;
  case ECL_STREAM_USER_FORMAT:
    IO_STREAM_ELT_TYPE(stream) = @'character';
    byte_size = 8;
    stream->stream.format = stream->stream.format_table;
    if (CONSP(stream->stream.format)) {
      stream->stream.encoder = user_multistate_encoder;
      stream->stream.decoder = user_multistate_decoder;
    } else {
      stream->stream.encoder = user_encoder;
      stream->stream.decoder = user_decoder;
    }
    break;
  case ECL_STREAM_US_ASCII:
    IO_STREAM_ELT_TYPE(stream) = @'base-char';
    byte_size = 8;
    stream->stream.format = @':us-ascii';
    stream->stream.encoder = ascii_encoder;
    stream->stream.decoder = ascii_decoder;
    break;
#else
  case ECL_STREAM_DEFAULT_FORMAT:
    IO_STREAM_ELT_TYPE(stream) = @'base-char';
    byte_size = 8;
    stream->stream.format = @':pass-through';
    stream->stream.encoder = passthrough_encoder;
    stream->stream.decoder = passthrough_decoder;
    break;
#endif
  default:
    FEerror("Invalid or unsupported external format ~A with code ~D",
            2, external_format, ecl_make_fixnum(flags));
  }
  t = @':LF';
  if (stream->stream.ops->write_char == eformat_write_char &&
      (flags & ECL_STREAM_CR)) {
    if (flags & ECL_STREAM_LF) {
      stream->stream.ops->read_char = eformat_read_char_crlf;
      stream->stream.ops->write_char = eformat_write_char_crlf;
      t = @':CRLF';
    } else {
      stream->stream.ops->read_char = eformat_read_char_cr;
      stream->stream.ops->write_char = eformat_write_char_cr;
      t = @':CR';
    }
  }
  stream->stream.format = cl_list(2, stream->stream.format, t);
  {
    cl_object (*read_byte)(cl_object);
    void (*write_byte)(cl_object,cl_object);
    byte_size = (byte_size+7)&(~(cl_fixnum)7);
    if (byte_size == 8) {
      if (flags & ECL_STREAM_SIGNED_BYTES) {
        read_byte = generic_read_byte_signed8;
        write_byte = generic_write_byte_signed8;
      } else {
        read_byte = generic_read_byte_unsigned8;
        write_byte = generic_write_byte_unsigned8;
      }
    } else if (flags & ECL_STREAM_LITTLE_ENDIAN) {
      read_byte = generic_read_byte_le;
      write_byte = generic_write_byte_le;
    } else {
      read_byte = generic_read_byte;
      write_byte = generic_write_byte;
    }
    if (ecl_input_stream_p(stream)) {
      stream->stream.ops->read_byte = read_byte;
    }
    if (ecl_output_stream_p(stream)) {
      stream->stream.ops->write_byte = write_byte;
    }
  }
  stream->stream.flags = flags;
  stream->stream.byte_size = byte_size;
}

cl_object
si_stream_external_format_set(cl_object stream, cl_object format)
{
#ifdef ECL_CLOS_STREAMS
  unlikely_if (ECL_INSTANCEP(stream)) {
    FEerror("Cannot change external format of stream ~A", 1, stream);
  }
#endif
  switch (stream->stream.mode) {
  case ecl_smm_input:
  case ecl_smm_input_file:
  case ecl_smm_output:
  case ecl_smm_output_file:
  case ecl_smm_io:
  case ecl_smm_io_file:
#ifdef ECL_WSOCK
  case ecl_smm_input_wsock:
  case ecl_smm_output_wsock:
  case ecl_smm_io_wsock:
  case ecl_smm_io_wcon:
#endif
    {
      cl_object elt_type = ecl_stream_element_type(stream);
      unlikely_if (elt_type != @'character' && elt_type != @'base-char') {
        FEerror("Cannot change external format of binary stream ~A", 1, stream);
      }
      set_stream_elt_type(stream, stream->stream.byte_size, stream->stream.flags, format);
    }
    break;
  default:
    FEerror("Cannot change external format of stream ~A", 1, stream);
  }
  @(return);
}

cl_object
ecl_make_file_stream_from_fd(cl_object fname, int fd, enum ecl_smmode smm,
                             cl_fixnum byte_size, int flags, cl_object external_format)
{
  cl_object stream = alloc_stream();
  switch(smm) {
  case ecl_smm_input:
    smm = ecl_smm_input_file;
  case ecl_smm_input_file:
  case ecl_smm_probe:
    stream->stream.ops = duplicate_dispatch_table(&input_file_ops);
    break;
  case ecl_smm_output:
    smm = ecl_smm_output_file;
  case ecl_smm_output_file:
    stream->stream.ops = duplicate_dispatch_table(&output_file_ops);
    break;
  case ecl_smm_io:
    smm = ecl_smm_io_file;
  case ecl_smm_io_file:
    stream->stream.ops = duplicate_dispatch_table(&io_file_ops);
    break;
  default:
    FEerror("make_stream: wrong mode", 0);
  }
  stream->stream.mode = (short)smm;
  stream->stream.closed = 0;
  set_stream_elt_type(stream, byte_size, flags, external_format);
  IO_FILE_FILENAME(stream) = fname; /* used in cl:pathname */
  stream->stream.column = 0;
  IO_FILE_DESCRIPTOR(stream) = fd;
  stream->stream.last_op = 0;
  si_set_finalizer(stream, ECL_T);
  return stream;
}

/**********************************************************************
 * C STREAMS
 */

static cl_index
input_stream_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  unlikely_if (strm->stream.byte_stack != ECL_NIL) {
    return consume_byte_stack(strm, c, n);
  } else {
    FILE *f = IO_STREAM_FILE(strm);
    cl_fixnum out = 0;
    ecl_disable_interrupts();
#ifdef FILE_CNT
    do {
      out = fread(c, sizeof(char), n, f);
    } while (out < n && ferror(f) && restartable_io_error(strm, "fread"));
#else
    /* We can't use fread here due to the buffering. It makes
       impossible checking if we have some data available in the
       buffer what renders listen returning incorrect result. */
    do {
      out = read(fileno(f), c, sizeof(char)*n);
    } while (out < 0 && restartable_io_error(strm, "read"));
#endif
    ecl_enable_interrupts();
    return out;
  }
}

static cl_index
output_stream_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  cl_index out;
  ecl_disable_interrupts();
  do {
    out = fwrite(c, sizeof(char), n, IO_STREAM_FILE(strm));
  } while (out < n && restartable_io_error(strm, "fwrite"));
  ecl_enable_interrupts();
  return out;
}

static cl_index
io_stream_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  /* When using the same stream for input and output operations, we have to
   * use some file position operation before reading again. Besides this, if
   * there were unread octets, we have to move to the position at the
   * begining of them.
   */
  if (strm->stream.byte_stack != ECL_NIL) {
    cl_object aux = ecl_file_position(strm);
    if (!Null(aux))
      ecl_file_position_set(strm, aux);
  } else if (strm->stream.last_op > 0) {
    ecl_fseeko(IO_STREAM_FILE(strm), 0, SEEK_CUR);
  }
  strm->stream.last_op = -1;
  return output_stream_write_byte8(strm, c, n);
}

static void io_stream_force_output(cl_object strm);

static cl_index
io_stream_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  /* When using the same stream for input and output operations, we have to
   * flush the stream before reading.
   */
  if (strm->stream.last_op < 0) {
    io_stream_force_output(strm);
  }
  strm->stream.last_op = +1;
  return input_stream_read_byte8(strm, c, n);
}

static int
io_stream_listen(cl_object strm)
{
  if (strm->stream.byte_stack != ECL_NIL)
    return ECL_LISTEN_AVAILABLE;
  return file_listen(strm, IO_STREAM_FILE(strm));
}

static void
io_stream_clear_input(cl_object strm)
{
  FILE *fp = IO_STREAM_FILE(strm);
#if defined(ECL_MS_WINDOWS_HOST)
  int f = fileno(fp);
  if (isatty(f)) {
    /* Flushes Win32 console */
    unlikely_if (!FlushConsoleInputBuffer((HANDLE)_get_osfhandle(f)))
      FEwin32_error("FlushConsoleInputBuffer() failed", 0);
    /* Do not stop here: the FILE structure needs also to be flushed */
  }
#endif
  while (file_listen(strm, fp) == ECL_LISTEN_AVAILABLE) {
    ecl_disable_interrupts();
    getc(fp);
    ecl_enable_interrupts();
  }
}

#define io_stream_clear_output generic_void

static void
io_stream_force_output(cl_object strm)
{
  FILE *f = IO_STREAM_FILE(strm);
  ecl_disable_interrupts();
  while ((fflush(f) == EOF) && restartable_io_error(strm, "fflush"))
    (void)0;
  ecl_enable_interrupts();
}

#define io_stream_finish_output io_stream_force_output

static int
io_stream_interactive_p(cl_object strm)
{
  FILE *f = IO_STREAM_FILE(strm);
  return isatty(fileno(f));
}

static cl_object
io_stream_length(cl_object strm)
{
  FILE *f = IO_STREAM_FILE(strm);
  cl_object output = ecl_file_len(fileno(f));
  if (strm->stream.byte_size != 8) {
    const cl_env_ptr the_env = ecl_process_env();
    cl_index bs = strm->stream.byte_size;
    output = ecl_floor2(output, ecl_make_fixnum(bs/8));
    unlikely_if (ecl_nth_value(the_env, 1) != ecl_make_fixnum(0)) {
      FEerror("File length is not on byte boundary", 0);
    }
  }
  return output;
}

static cl_object
io_stream_get_position(cl_object strm)
{
  FILE *f = IO_STREAM_FILE(strm);
  cl_object output;
  ecl_off_t offset;

  ecl_disable_interrupts();
  offset = ecl_ftello(f);
  ecl_enable_interrupts();
  unlikely_if (offset < 0) {
    if (errno == ESPIPE)
      return(ECL_NIL);
    else
      io_error(strm);
  }
  if (sizeof(ecl_off_t) == sizeof(long)) {
    output = ecl_make_integer(offset);
  } else {
    output = ecl_off_t_to_integer(offset);
  }
  {
    /* If there are unread octets, we return the position at which
     * these bytes begin! */
    cl_object l = strm->stream.byte_stack;
    while (CONSP(l)) {
      output = ecl_one_minus(output);
      l = ECL_CONS_CDR(l);
    }
  }
  if (strm->stream.byte_size != 8) {
    output = ecl_floor2(output, ecl_make_fixnum(strm->stream.byte_size / 8));
  }
  return output;
}

static cl_object
io_stream_set_position(cl_object strm, cl_object large_disp)
{
  FILE *f = IO_STREAM_FILE(strm);
  ecl_off_t disp;
  int mode;
  strm->stream.byte_stack = ECL_NIL;
  if (Null(large_disp)) {
    disp = 0;
    mode = SEEK_END;
  } else {
    if (strm->stream.byte_size != 8) {
      large_disp = ecl_times(large_disp,
                             ecl_make_fixnum(strm->stream.byte_size / 8));
    }
    disp = ecl_integer_to_off_t(large_disp);
    mode = SEEK_SET;
  }
  ecl_disable_interrupts();
  mode = ecl_fseeko(f, disp, mode);
  ecl_enable_interrupts();
  return mode? ECL_NIL : ECL_T;
}

static cl_object
io_stream_close(cl_object strm)
{
  FILE *f = IO_STREAM_FILE(strm);
  int failed;
  unlikely_if (f == stdout)
    FEerror("Cannot close the standard output", 0);
  unlikely_if (f == stdin)
    FEerror("Cannot close the standard input", 0);
  unlikely_if (f == NULL)
    wrong_file_handler(strm);
  if (ecl_output_stream_p(strm)) {
    ecl_force_output(strm);
  }
  failed = safe_fclose(f);
  unlikely_if (failed)
    cannot_close(strm);
#if !defined(GBC_BOEHM)
  ecl_dealloc(strm->stream.buffer);
  IO_STREAM_FILE(strm) = NULL;
#endif
  return generic_close(strm);
}

/*
 * Specialized sequence operations
 */

#define io_stream_read_vector io_file_read_vector
#define io_stream_write_vector io_file_write_vector

const struct ecl_file_ops io_stream_ops = {
  io_stream_write_byte8,
  io_stream_read_byte8,

  generic_write_byte,
  generic_read_byte,

  eformat_read_char,
  eformat_write_char,
  eformat_unread_char,
  generic_peek_char,

  io_file_read_vector,
  io_file_write_vector,

  io_stream_listen,
  io_stream_clear_input,
  io_stream_clear_output,
  io_stream_finish_output,
  io_stream_force_output,

  generic_always_true, /* input_p */
  generic_always_true, /* output_p */
  io_stream_interactive_p,
  io_file_element_type,

  io_stream_length,
  io_stream_get_position,
  io_stream_set_position,
  file_string_length,
  generic_column,

  io_file_pathname,
  io_file_truename,

  io_stream_close
};

const struct ecl_file_ops output_stream_ops = {
  output_stream_write_byte8,
  not_input_read_byte8,

  generic_write_byte,
  not_input_read_byte,

  not_input_read_char,
  eformat_write_char,
  not_input_unread_char,
  not_input_read_char,

  generic_read_vector,
  io_file_write_vector,

  not_input_listen,
  generic_void,
  io_stream_clear_output,
  io_stream_finish_output,
  io_stream_force_output,

  generic_always_false, /* input_p */
  generic_always_true, /* output_p */
  generic_always_false,
  io_file_element_type,

  io_stream_length,
  io_stream_get_position,
  io_stream_set_position,
  file_string_length,
  generic_column,

  io_file_pathname,
  io_file_truename,

  io_stream_close
};

const struct ecl_file_ops input_stream_ops = {
  not_output_write_byte8,
  input_stream_read_byte8,

  not_output_write_byte,
  generic_read_byte,

  eformat_read_char,
  not_output_write_char,
  eformat_unread_char,
  generic_peek_char,

  io_file_read_vector,
  generic_write_vector,

  io_stream_listen,
  io_stream_clear_input,
  generic_void,
  generic_void,
  generic_void,

  generic_always_true, /* input_p */
  generic_always_false, /* output_p */
  io_stream_interactive_p,
  io_file_element_type,

  io_stream_length,
  io_stream_get_position,
  io_stream_set_position,
  not_output_string_length,
  unknown_column,

  io_file_pathname,
  io_file_truename,

  io_stream_close
};

/**********************************************************************
 * WINSOCK STREAMS  
 */

#if defined(ECL_WSOCK)

#define winsock_stream_element_type io_file_element_type

static cl_index
winsock_stream_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  cl_index len = 0;

  unlikely_if (strm->stream.byte_stack != ECL_NIL) {
    return consume_byte_stack(strm, c, n);
  }
  if(n > 0) {
    SOCKET s = (SOCKET)IO_FILE_DESCRIPTOR(strm);
    unlikely_if (INVALID_SOCKET == s) {
      wrong_file_handler(strm);
    } else {
      ecl_disable_interrupts();
      len = recv(s, c, n, 0);
      unlikely_if (len == SOCKET_ERROR)
        wsock_error("Cannot read bytes from Windows "
                    "socket ~S.~%~A", strm);
      ecl_enable_interrupts();
    }
  }
  return (len > 0) ? len : EOF;
}

static cl_index
winsock_stream_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  cl_index out = 0;
  unsigned char *endp;
  unsigned char *p;
  SOCKET s = (SOCKET)IO_FILE_DESCRIPTOR(strm);
  unlikely_if (INVALID_SOCKET == s) {
    wrong_file_handler(strm);
  } else {
    ecl_disable_interrupts();
    do {
      cl_index res = send(s, c + out, n, 0);
      unlikely_if (res == SOCKET_ERROR) {
        wsock_error("Cannot write bytes to Windows"
                    " socket ~S.~%~A", strm);
        break; /* stop writing */
      } else {                        
        out += res;
        n -= res;
      }
    } while (n > 0);
    ecl_enable_interrupts();
  }
  return out;
}

static int
winsock_stream_listen(cl_object strm) 
{
  SOCKET s;
  unlikely_if (strm->stream.byte_stack != ECL_NIL) {
    return ECL_LISTEN_AVAILABLE;
  }
  s = (SOCKET)IO_FILE_DESCRIPTOR(strm);
  unlikely_if (INVALID_SOCKET == s) {
    wrong_file_handler(strm);
  }
  {
    struct timeval tv = { 0, 0 };
    fd_set fds;
    cl_index result;
                        
    FD_ZERO( &fds );
    FD_SET(s, &fds);
    ecl_disable_interrupts();
    result = select( 0, &fds, NULL, NULL,  &tv );
    unlikely_if (result == SOCKET_ERROR)
      wsock_error("Cannot listen on Windows "
                  "socket ~S.~%~A", strm );
    ecl_enable_interrupts();
    return ( result > 0 
             ? ECL_LISTEN_AVAILABLE 
             : ECL_LISTEN_NO_CHAR );
  }
}

static void
winsock_stream_clear_input(cl_object strm)
{
  while (winsock_stream_listen(strm) == ECL_LISTEN_AVAILABLE) {
    eformat_read_char(strm);
  }
}

static cl_object
winsock_stream_close(cl_object strm)
{
  SOCKET s = (SOCKET) IO_FILE_DESCRIPTOR(strm);
  int failed;
  ecl_disable_interrupts();
  failed = closesocket(s);
  ecl_enable_interrupts();
  unlikely_if (failed < 0)
    cannot_close(strm);
  IO_FILE_DESCRIPTOR(strm) = (int)INVALID_SOCKET;
  return generic_close(strm);
}

const struct ecl_file_ops winsock_stream_io_ops = {
  winsock_stream_write_byte8,
  winsock_stream_read_byte8,

  generic_write_byte,
  generic_read_byte,

  eformat_read_char,
  eformat_write_char,
  eformat_unread_char,
  generic_peek_char,

  generic_read_vector,
  generic_write_vector,

  winsock_stream_listen,
  winsock_stream_clear_input,
  generic_void,
  generic_void,
  generic_void,

  generic_always_true, /* input_p */
  generic_always_true, /* output_p */
  generic_always_false,
  winsock_stream_element_type,

  not_a_file_stream,
  generic_always_nil, /* get_position */
  generic_set_position,
  file_string_length,
  generic_column,

  not_a_file_stream,
  not_a_file_stream,

  winsock_stream_close
};

const struct ecl_file_ops winsock_stream_output_ops = {
  winsock_stream_write_byte8,
  not_input_read_byte8,

  generic_write_byte,
  not_input_read_byte,

  not_input_read_char,
  eformat_write_char,
  not_input_unread_char,
  generic_peek_char,

  generic_read_vector,
  generic_write_vector,

  not_input_listen,
  not_input_clear_input,
  generic_void,
  generic_void,
  generic_void,

  generic_always_false, /* input_p */
  generic_always_true, /* output_p */
  generic_always_false,
  winsock_stream_element_type,

  not_a_file_stream,
  generic_always_nil, /* get_position */
  generic_set_position,
  file_string_length,
  generic_column,

  not_a_file_stream,
  not_a_file_stream,

  winsock_stream_close
};

const struct ecl_file_ops winsock_stream_input_ops = {
  not_output_write_byte8,
  winsock_stream_read_byte8,

  not_output_write_byte,
  generic_read_byte,

  eformat_read_char,
  not_output_write_char,
  eformat_unread_char,
  generic_peek_char,

  generic_read_vector,
  generic_write_vector,

  winsock_stream_listen,
  winsock_stream_clear_input,
  not_output_clear_output,
  not_output_finish_output,
  not_output_force_output,

  generic_always_true, /* input_p */
  generic_always_false, /* output_p */
  generic_always_false,
  winsock_stream_element_type,

  not_a_file_stream,
  generic_always_nil, /* get_position */
  generic_set_position,
  not_output_string_length,
  unknown_column,

  not_a_file_stream,
  not_a_file_stream,

  winsock_stream_close
};
#endif

/**********************************************************************
 * WINCONSOLE STREAM
 */

#if defined(ECL_MS_WINDOWS_HOST)

#define wcon_stream_element_type io_file_element_type

static cl_index
wcon_stream_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  unlikely_if (strm->stream.byte_stack != ECL_NIL) {
    return consume_byte_stack(strm, c, n);
  } else {
    cl_env_ptr the_env = ecl_process_env();
    HANDLE h = (HANDLE)IO_FILE_DESCRIPTOR(strm);
    DWORD nchars;
    int ok;
    ecl_disable_interrupts_env(the_env);
    ok = ReadConsoleA(h, c, n, &nchars, NULL);
    ecl_enable_interrupts_env(the_env);
    unlikely_if (!ok) {
      FEwin32_error("Cannot read from console", 0);
    }
    return (nchars > 0) ? nchars : EOF;
  }
}

static cl_index
wcon_stream_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  HANDLE h = (HANDLE)IO_FILE_DESCRIPTOR(strm);
  DWORD nchars;
  unlikely_if(!WriteConsoleA(h, c, n, &nchars, NULL)) {
    FEwin32_error("Cannot write to console.", 0);
  }
  return nchars;
}

static int
wcon_stream_listen(cl_object strm) 
{
  HANDLE h = (HANDLE)IO_FILE_DESCRIPTOR(strm);
  INPUT_RECORD aux;
  DWORD nevents;
  do {
    unlikely_if(!PeekConsoleInput(h, &aux, 1, &nevents))
      FEwin32_error("Cannot read from console.", 0);
    if (nevents == 0)
      return 0;
    if (aux.EventType == KEY_EVENT)
      return 1;
    unlikely_if(!ReadConsoleInput(h, &aux, 1, &nevents))
      FEwin32_error("Cannot read from console.", 0);
  } while (1);
}

static void
wcon_stream_clear_input(cl_object strm)
{
  FlushConsoleInputBuffer((HANDLE)IO_FILE_DESCRIPTOR(strm));
}

static void
wcon_stream_force_output(cl_object strm)
{
  DWORD nchars;
  WriteConsole((HANDLE)IO_FILE_DESCRIPTOR(strm), 0, 0, &nchars, NULL);
}

const struct ecl_file_ops wcon_stream_io_ops = {
  wcon_stream_write_byte8,
  wcon_stream_read_byte8,

  generic_write_byte,
  generic_read_byte,

  eformat_read_char,
  eformat_write_char,
  eformat_unread_char,
  generic_peek_char,

  generic_read_vector,
  generic_write_vector,

  wcon_stream_listen,
  wcon_stream_clear_input,
  generic_void,
  wcon_stream_force_output,
  wcon_stream_force_output,

  generic_always_true, /* input_p */
  generic_always_true, /* output_p */
  generic_always_false,
  wcon_stream_element_type,

  not_a_file_stream,
  generic_always_nil, /* get_position */
  generic_set_position,
  file_string_length,
  generic_column,

  io_file_pathname,
  io_file_truename,

  generic_close,
};

#define CONTROL_Z 26

static cl_object
maybe_make_windows_console_FILE(cl_object fname, FILE *f, enum ecl_smmode smm,
                                cl_fixnum byte_size, int flags,
                                cl_object external_format)
{
  int desc = fileno(f);
  cl_object output;
  if (isatty(desc)) {
    output = ecl_make_stream_from_FILE
      (fname,
       (void*)_get_osfhandle(desc),
       ecl_smm_io_wcon,
       byte_size, flags,
       external_format);
    output->stream.eof_char = CONTROL_Z;
  } else {
    output = ecl_make_stream_from_FILE
      (fname, f, smm, byte_size, flags,
       external_format);
  }
  return output;
}

static cl_object
maybe_make_windows_console_fd(cl_object fname, int desc, enum ecl_smmode smm,
                              cl_fixnum byte_size, int flags,
                              cl_object external_format)
{
  cl_object output;
  if (isatty(desc)) {
    output = ecl_make_stream_from_FILE
      (fname,
       (void*)_get_osfhandle(desc),
       ecl_smm_io_wcon,
       byte_size, flags,
       external_format);
    output->stream.eof_char = CONTROL_Z;
  } else {
    /* Windows changes the newline characters for \r\n
     * even when using read()/write() */
    if (ecl_option_values[ECL_OPT_USE_SETMODE_ON_FILES]) {
      _setmode(desc, _O_BINARY);
    } else {
      external_format = ECL_CONS_CDR(external_format);
    }
    output = ecl_make_file_stream_from_fd
      (fname, desc, smm,
       byte_size, flags,
       external_format);
  }
  return output;
}

cl_object
si_windows_codepage_encoding()
{
  /* Mapping from windows codepages to encoding names used by ECL */
  DWORD cp = GetConsoleCP();
  cl_object encoding;
  switch (cp) {
#ifdef ECL_UNICODE
  case 437: return ecl_make_keyword("DOS-CP437");
  case 708: return ecl_make_keyword("ISO-8859-6");
  case 850: return ecl_make_keyword("DOS-CP850");
  case 852: return ecl_make_keyword("DOS-CP852");
  case 855: return ecl_make_keyword("DOS-CP855");
  case 857: return ecl_make_keyword("DOS-CP857");
  case 858: return ecl_make_keyword("DOS-CP858");
  case 860: return ecl_make_keyword("DOS-CP860");
  case 861: return ecl_make_keyword("DOS-CP861");
  case 862: return ecl_make_keyword("DOS-CP862");
  case 863: return ecl_make_keyword("DOS-CP863");
  case 864: return ecl_make_keyword("DOS-CP864");
  case 865: return ecl_make_keyword("DOS-CP865");
  case 866: return ecl_make_keyword("DOS-CP866");
  case 869: return ecl_make_keyword("DOS-CP869");
  case 932: return ecl_make_keyword("WINDOWS-CP932");
  case 936: return ecl_make_keyword("WINDOWS-CP936");
  case 949: return ecl_make_keyword("WINDOWS-CP949");
  case 950: return ecl_make_keyword("WINDOWS-CP950");
  case 1200: return ecl_make_keyword("UCS-2LE");
  case 1201: return ecl_make_keyword("UCS-2BE");
  case 1250: return ecl_make_keyword("WINDOWS-CP1250");
  case 1251: return ecl_make_keyword("WINDOWS-CP1251");
  case 1252: return ecl_make_keyword("WINDOWS-CP1252");
  case 1253: return ecl_make_keyword("WINDOWS-CP1253");
  case 1254: return ecl_make_keyword("WINDOWS-CP1254");
  case 1255: return ecl_make_keyword("WINDOWS-CP1255");
  case 1256: return ecl_make_keyword("WINDOWS-CP1256");
  case 1257: return ecl_make_keyword("WINDOWS-CP1257");
  case 1258: return ecl_make_keyword("WINDOWS-CP1258");
  case 12000: return ecl_make_keyword("UCS-4LE");
  case 12001: return ecl_make_keyword("UCS-4BE");
  case 20932: return ecl_make_keyword("JISX0212");
  case 21866: return ecl_make_keyword("KOI8-U");
  case 28591: return ecl_make_keyword("ISO-8859-1");
  case 28592: return ecl_make_keyword("ISO-8859-2");
  case 28593: return ecl_make_keyword("ISO-8859-3");
  case 28594: return ecl_make_keyword("ISO-8859-4");
  case 28595: return ecl_make_keyword("ISO-8859-5");
  case 28596: return ecl_make_keyword("ISO-8859-6");
  case 28597: return ecl_make_keyword("ISO-8859-7");
  case 28598: return ecl_make_keyword("ISO-8859-8");
  case 28599: return ecl_make_keyword("ISO-8859-9");
  case 28603: return ecl_make_keyword("ISO-8859-13");
  case 28605: return ecl_make_keyword("ISO-8859-15");
  case 50220: return ecl_make_keyword("ISO-2022-JP");
  case 65001: return ecl_make_keyword("UTF-8");
#endif
    /* Nothing we can do here, try our best with :pass-through */
  default: return @':pass-through';
  }
}
#else
#define maybe_make_windows_console_FILE ecl_make_stream_from_FILE
#define maybe_make_windows_console_fd ecl_make_file_stream_from_fd
#endif

cl_object
si_set_buffering_mode(cl_object stream, cl_object buffer_mode_symbol)
{
  enum ecl_smmode mode = stream->stream.mode;
  int buffer_mode;

  unlikely_if (!ECL_ANSI_STREAM_P(stream)) {
    FEerror("Cannot set buffer of ~A", 1, stream);
  }

  if (buffer_mode_symbol == @':none' || Null(buffer_mode_symbol))
    buffer_mode = _IONBF;
  else if (buffer_mode_symbol == @':line' || buffer_mode_symbol == @':line-buffered')
    buffer_mode = _IOLBF;
  else if (buffer_mode_symbol == @':full' || buffer_mode_symbol == @':fully-buffered')
    buffer_mode = _IOFBF;
  else
    FEerror("Not a valid buffering mode: ~A", 1, buffer_mode_symbol);

  if (mode == ecl_smm_output || mode == ecl_smm_io || mode == ecl_smm_input) {
    FILE *fp = IO_STREAM_FILE(stream);

    if (buffer_mode != _IONBF) {
      cl_index buffer_size = BUFSIZ;
      char *new_buffer = ecl_alloc_atomic(buffer_size);
      stream->stream.buffer = new_buffer;
      setvbuf(fp, new_buffer, buffer_mode, buffer_size);
    } else
      setvbuf(fp, NULL, _IONBF, 0);
  }
  @(return stream);
}

cl_object
ecl_make_stream_from_FILE(cl_object fname, void *f, enum ecl_smmode smm,
                          cl_fixnum byte_size, int flags, cl_object external_format)
{
  cl_object stream;
  stream = alloc_stream();
  stream->stream.mode = (short)smm;
  stream->stream.closed = 0;
  switch (smm) {
  case ecl_smm_io:
    stream->stream.ops = duplicate_dispatch_table(&io_stream_ops);
    break;
  case ecl_smm_probe:
  case ecl_smm_input:
    stream->stream.ops = duplicate_dispatch_table(&input_stream_ops);
    break;
  case ecl_smm_output:
    stream->stream.ops = duplicate_dispatch_table(&output_stream_ops);
    break;
#if defined(ECL_WSOCK)
  case ecl_smm_input_wsock:
    stream->stream.ops = duplicate_dispatch_table(&winsock_stream_input_ops);
    break;
  case ecl_smm_output_wsock:
    stream->stream.ops = duplicate_dispatch_table(&winsock_stream_output_ops);
    break;
  case ecl_smm_io_wsock:
    stream->stream.ops = duplicate_dispatch_table(&winsock_stream_io_ops);
    break;
  case ecl_smm_io_wcon:
    stream->stream.ops = duplicate_dispatch_table(&wcon_stream_io_ops);
    break;
#endif
  default:
    FEerror("Not a valid mode ~D for ecl_make_stream_from_FILE", 1, ecl_make_fixnum(smm));
  }
  set_stream_elt_type(stream, byte_size, flags, external_format);
  IO_STREAM_FILENAME(stream) = fname;
  stream->stream.column = 0;
  IO_STREAM_FILE(stream) = f;
  stream->stream.last_op = 0;
  si_set_finalizer(stream, ECL_T);
  return stream;
}

cl_object
ecl_make_stream_from_fd(cl_object fname, int fd, enum ecl_smmode smm,
                        cl_fixnum byte_size, int flags, cl_object external_format)
{
  ecl_filename_char *mode;        /* file open mode */
  FILE *fp;                       /* file pointer */
  switch(smm) {
  case ecl_smm_input:
    mode = OPEN_R;
    break;
  case ecl_smm_output:
    mode = OPEN_W;
    break;
  case ecl_smm_io:
    mode = OPEN_RW;
    break;
#if defined(ECL_WSOCK)
  case ecl_smm_input_wsock:
  case ecl_smm_output_wsock:
  case ecl_smm_io_wsock:
  case ecl_smm_io_wcon:
    break;
#endif
  default:
    FEerror("make_stream: wrong mode", 0);
  }
#if defined(ECL_WSOCK)
  if (smm == ecl_smm_input_wsock || smm == ecl_smm_output_wsock || smm == ecl_smm_io_wsock || smm == ecl_smm_io_wcon)
    fp = (FILE*)fd;
  else
    fp = safe_fdopen(fd, mode);
#else
  fp = safe_fdopen(fd, mode);
#endif
  if (fp == NULL) {
    FElibc_error("Unable to create stream for file descriptor ~D",
                 1, ecl_make_integer(fd));
  }
  return ecl_make_stream_from_FILE(fname, fp, smm, byte_size, flags,
                                   external_format);
}

int
ecl_stream_to_handle(cl_object s, bool output)
{
 BEGIN:
  if (ecl_unlikely(!ECL_ANSI_STREAM_P(s)))
    return -1;
  switch ((enum ecl_smmode)s->stream.mode) {
  case ecl_smm_input:
    if (output) return -1;
    return fileno(IO_STREAM_FILE(s));
  case ecl_smm_input_file:
    if (output) return -1;
    return IO_FILE_DESCRIPTOR(s);
  case ecl_smm_output:
    if (!output) return -1;
    return fileno(IO_STREAM_FILE(s));
  case ecl_smm_output_file:
    if (!output) return -1;
    return IO_FILE_DESCRIPTOR(s);
  case ecl_smm_io:
    return fileno(IO_STREAM_FILE(s));
  case ecl_smm_io_file:
    return IO_FILE_DESCRIPTOR(s);
  case ecl_smm_synonym:
    s = SYNONYM_STREAM_STREAM(s);
    goto BEGIN;
  case ecl_smm_two_way:
    s = output? TWO_WAY_STREAM_OUTPUT(s) : TWO_WAY_STREAM_INPUT(s);
    goto BEGIN;
#if defined(ECL_WSOCK)
  case ecl_smm_input_wsock:
  case ecl_smm_output_wsock:
  case ecl_smm_io_wsock:
#endif
#if defined(ECL_MS_WINDOWS_HOST)
  case ecl_smm_io_wcon:
#endif
  default:
    return -1;
  }
}

cl_object
si_file_stream_fd(cl_object s)
{
  cl_object ret;

  unlikely_if (!ECL_FILE_STREAM_P(s)) {
    not_a_file_stream(s);
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

/**********************************************************************
 * SEQUENCE INPUT STREAMS
 */

static cl_index
seq_in_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  cl_fixnum curr_pos = SEQ_INPUT_POSITION(strm);
  cl_fixnum last = SEQ_INPUT_LIMIT(strm);
  cl_fixnum delta = last - curr_pos;
  if (delta > 0) {
    cl_object vector = SEQ_INPUT_VECTOR(strm);
    if (delta > n) delta = n;
    memcpy(c, vector->vector.self.bc + curr_pos, delta);
    SEQ_INPUT_POSITION(strm) += delta;
    return delta;
  }
  return 0;
}

static void
seq_in_unread_char(cl_object strm, ecl_character c)
{
  eformat_unread_char(strm, c);
  SEQ_INPUT_POSITION(strm) -= ecl_length(strm->stream.byte_stack);
  strm->stream.byte_stack = ECL_NIL;
}

#ifdef ecl_uint16_t
static ecl_character
seq_in_ucs2_read_char(cl_object strm)
{
  cl_fixnum curr_pos = SEQ_INPUT_POSITION(strm);
  cl_fixnum last = SEQ_INPUT_LIMIT(strm);
  if (curr_pos >= last) {
    return EOF;
  }
  cl_object vector = SEQ_INPUT_VECTOR(strm);
  ecl_character c = vector->vector.self.b16[curr_pos++];
  cl_object err;
  if (c >= 0xD800 && c <= 0xDBFF) {
    if (curr_pos >= last) {
      err = ecl_list1(ecl_make_fixnum(c));
      goto DECODING_ERROR;
    }
    ecl_character aux = vector->vector.self.b16[curr_pos++];
    if (aux < 0xDC00 || aux > 0xDFFF) {
      err = cl_list(2, ecl_make_fixnum(c), ecl_make_fixnum(aux));
      goto DECODING_ERROR;
    }
    c = ((c & 0x3FF) << 10) + (aux & 0x3FF) + 0x10000;
  }
  SEQ_INPUT_POSITION(strm) = curr_pos;
  return c;
  cl_object code;
 DECODING_ERROR:
  code = _ecl_funcall4(@'ext::decoding-error', strm,
                       cl_stream_external_format(strm),
                       err);
  if (Null(code)) {
    /* Go for next character */
    return seq_in_ucs2_read_char(strm);
  } else {
    /* Return supplied character */
    return ecl_char_code(code);
  }
}

static void
seq_in_ucs2_unread_char(cl_object strm, ecl_character c)
{
  if (c >= 0x10000) {
    SEQ_INPUT_POSITION(strm) -= 2;
  } else {
    SEQ_INPUT_POSITION(strm) -= 1;
  }
}
#endif

#ifdef ecl_uint32_t
static ecl_character
seq_in_ucs4_read_char(cl_object strm)
{
  cl_fixnum curr_pos = SEQ_INPUT_POSITION(strm);
  if (curr_pos >= SEQ_INPUT_LIMIT(strm)) {
    return EOF;
  }
  cl_object vector = SEQ_INPUT_VECTOR(strm);
  SEQ_INPUT_POSITION(strm) += 1;
  return vector->vector.self.b32[curr_pos];
}

static void
seq_in_ucs4_unread_char(cl_object strm, ecl_character c)
{
  SEQ_INPUT_POSITION(strm) -= 1;
}
#endif

static int
seq_in_listen(cl_object strm)
{
  if (SEQ_INPUT_POSITION(strm) < SEQ_INPUT_LIMIT(strm))
    return ECL_LISTEN_AVAILABLE;
  else
    return ECL_LISTEN_EOF;
}

static cl_object
seq_in_get_position(cl_object strm)
{
  return ecl_make_unsigned_integer(SEQ_INPUT_POSITION(strm));
}

static cl_object
seq_in_set_position(cl_object strm, cl_object pos)
{
  cl_fixnum disp;
  if (Null(pos)) {
    disp = SEQ_INPUT_LIMIT(strm);
  }  else {
    disp = ecl_to_size(pos);
    if (disp >= SEQ_INPUT_LIMIT(strm)) {
      disp = SEQ_INPUT_LIMIT(strm);
    }
  }
  SEQ_INPUT_POSITION(strm) = disp;
  return ECL_T;
}

const struct ecl_file_ops seq_in_ops = {
  not_output_write_byte8,
  seq_in_read_byte8,

  not_output_write_byte,
  generic_read_byte,

  eformat_read_char,
  not_output_write_char,
  seq_in_unread_char,
  generic_peek_char,

  generic_read_vector,
  generic_write_vector,

  seq_in_listen,
  generic_void, /* clear-input */
  not_output_clear_output,
  not_output_finish_output,
  not_output_force_output,

  generic_always_true, /* input_p */
  generic_always_false, /* output_p */
  generic_always_false,
  io_file_element_type,

  not_a_file_stream, /* length */
  seq_in_get_position,
  seq_in_set_position,
  not_output_string_length,
  unknown_column,

  not_a_file_stream,
  not_a_file_stream,

  generic_close
};

static cl_object
make_sequence_input_stream(cl_object vector, cl_index istart, cl_index iend,
                           cl_object external_format)
{
  cl_object strm;
  cl_elttype type;
  cl_object type_name;
  int byte_size;
  int flags = 0;
  if (!ECL_VECTORP(vector)) {
    FEwrong_type_nth_arg(@[ext::make-sequence-input-stream], 1, vector, @[vector]);
  }
  type = ecl_array_elttype(vector);
  type_name = ecl_elttype_to_symbol(type);
  byte_size = ecl_normalize_stream_element_type(type_name);
  /* Character streams always get some external format. For binary
   * sequences it has to be explicitly mentioned. */
  strm = alloc_stream();
  strm->stream.ops = duplicate_dispatch_table(&seq_in_ops);
  strm->stream.mode = (short)ecl_smm_sequence_input;
  if (!byte_size && Null(external_format)) {
    external_format = @':default';
  }
  if (ecl_aet_size[type] == 1) {
    set_stream_elt_type(strm, byte_size, flags, external_format);
    /* Override byte size */
    if (byte_size) strm->stream.byte_size = 8;
  }
#ifdef ecl_uint16_t
  else if (ecl_aet_size[type] == 2 && external_format == @':ucs-2') {
    IO_STREAM_ELT_TYPE(strm) = @'character';
    strm->stream.format = @':ucs-2';
    strm->stream.byte_size = 2*8;
    strm->stream.ops->read_char = seq_in_ucs2_read_char;
    strm->stream.ops->unread_char = seq_in_ucs2_unread_char;
  }
#endif
#ifdef ecl_uint32_t
  else if (ecl_aet_size[type] == 4 && external_format == @':ucs-4') {
    IO_STREAM_ELT_TYPE(strm) = @'character';
    strm->stream.format = @':ucs-4';
    strm->stream.byte_size = 4*8;
    strm->stream.ops->read_char = seq_in_ucs4_read_char;
    strm->stream.ops->unread_char = seq_in_ucs4_unread_char;
  }
#endif
  else {
    FEerror("Illegal combination of external-format ~A and input vector ~A for MAKE-SEQUENCE-INPUT-STREAM.~%", 2, external_format, vector);
  }
  SEQ_INPUT_VECTOR(strm) = vector;
  SEQ_INPUT_POSITION(strm) = istart;
  SEQ_INPUT_LIMIT(strm) = iend;
  return strm;
}

@(defun ext::make_sequence_input_stream (vector &key
                                         (start ecl_make_fixnum(0))
                                         (end ECL_NIL)
                                         (external_format ECL_NIL))
  cl_index_pair p;
  @
  p = ecl_vector_start_end(@[ext::make-sequence-input-stream],
                           vector, start, end);
  @(return make_sequence_input_stream(vector, p.start, p.end,
                                      external_format))
  @)

/**********************************************************************
 * SEQUENCE OUTPUT STREAMS
 */

static void
seq_out_enlarge_vector(cl_object strm)
{
  cl_object vector = SEQ_OUTPUT_VECTOR(strm);
  si_adjust_vector(vector, ecl_ash(ecl_make_fixnum(vector->vector.dim), 1));
  SEQ_OUTPUT_VECTOR(strm) = vector;
}

static cl_index
seq_out_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
 AGAIN:
  {
    cl_object vector = SEQ_OUTPUT_VECTOR(strm);
    cl_fixnum curr_pos = SEQ_OUTPUT_POSITION(strm);
    cl_fixnum last = vector->vector.dim;
    cl_fixnum delta = last - curr_pos;
    if (delta < n) {
      seq_out_enlarge_vector(strm);
      goto AGAIN;
    }
    memcpy(vector->vector.self.bc + curr_pos, c, n);
    SEQ_OUTPUT_POSITION(strm) = curr_pos += n;
    if (vector->vector.fillp < curr_pos)
      vector->vector.fillp = curr_pos;
  }
  return n;
}

#ifdef ecl_uint16_t
static ecl_character
seq_out_ucs2_write_char(cl_object strm, ecl_character c)
{
 AGAIN:
  {
    cl_object vector = SEQ_OUTPUT_VECTOR(strm);
    cl_fixnum curr_pos = SEQ_OUTPUT_POSITION(strm);
    cl_fixnum n = (c >= 0x10000) ? 2 : 1;
    if (vector->vector.dim - curr_pos < n) {
      seq_out_enlarge_vector(strm);
      goto AGAIN;
    }
    if (c >= 0x10000) {
      c -= 0x10000;
      vector->vector.self.b16[curr_pos++] = (c >> 10) | 0xD800;
      vector->vector.self.b16[curr_pos++] = (c & 0x3FF) | 0xDC00;
    } else {
      vector->vector.self.b16[curr_pos++] = c;
    }
    SEQ_OUTPUT_POSITION(strm) = curr_pos;
    if (vector->vector.fillp < curr_pos)
      vector->vector.fillp = curr_pos;
  }
  return c;
}
#endif

#ifdef ecl_uint32_t
static ecl_character
seq_out_ucs4_write_char(cl_object strm, ecl_character c)
{
 AGAIN:
  {
    cl_object vector = SEQ_OUTPUT_VECTOR(strm);
    cl_fixnum curr_pos = SEQ_OUTPUT_POSITION(strm);
    if (vector->vector.dim - curr_pos < 1) {
      seq_out_enlarge_vector(strm);
      goto AGAIN;
    }
    vector->vector.self.b32[curr_pos++] = c;
    SEQ_OUTPUT_POSITION(strm) = curr_pos;
    if (vector->vector.fillp < curr_pos)
      vector->vector.fillp = curr_pos;
  }
  return c;
}
#endif

static cl_object
seq_out_get_position(cl_object strm)
{
  return ecl_make_unsigned_integer(SEQ_OUTPUT_POSITION(strm));
}

static cl_object
seq_out_set_position(cl_object strm, cl_object pos)
{
  cl_object vector = SEQ_OUTPUT_VECTOR(strm);
  cl_fixnum disp;
  if (Null(pos)) {
    disp = vector->vector.fillp;
  } else {
    disp = ecl_to_size(pos);
    if (disp >= vector->vector.dim) {
      disp = vector->vector.fillp;
    }
  }
  SEQ_OUTPUT_POSITION(strm) = disp;
  return ECL_T;
}

const struct ecl_file_ops seq_out_ops = {
  seq_out_write_byte8,
  not_input_read_byte8,

  generic_write_byte,
  not_input_read_byte,

  not_input_read_char,
  eformat_write_char,
  not_input_unread_char,
  generic_peek_char,

  generic_read_vector,
  generic_write_vector,

  not_input_listen,
  not_input_clear_input,
  generic_void, /* clear-output */
  generic_void, /* finish-output */
  generic_void, /* force-output */

  generic_always_false, /* input_p */
  generic_always_true, /* output_p */
  generic_always_false,
  io_file_element_type,

  not_a_file_stream, /* length */
  seq_out_get_position,
  seq_out_set_position,
  not_output_string_length,
  generic_column,

  not_a_file_stream,
  not_a_file_stream,

  generic_close
};

static cl_object
make_sequence_output_stream(cl_object vector, cl_object external_format)
{
  cl_object strm;
  cl_elttype type;
  cl_object type_name;
  int byte_size;
  int flags = 0;
  if (!ECL_VECTORP(vector)) {
    FEwrong_type_nth_arg(@[ext::make-sequence-output-stream], 1, vector, @[vector]);
  }
  type = ecl_array_elttype(vector);
  type_name = ecl_elttype_to_symbol(type);
  byte_size = ecl_normalize_stream_element_type(type_name);
  /* Character streams always get some external format. For binary
   * sequences it has to be explicitly mentioned. */
  strm = alloc_stream();
  strm->stream.ops = duplicate_dispatch_table(&seq_out_ops);
  strm->stream.mode = (short)ecl_smm_sequence_output;
  if (!byte_size && Null(external_format)) {
    external_format = @':default';
  }
  if (ecl_aet_size[type] == 1) {
    set_stream_elt_type(strm, byte_size, flags, external_format);
    /* Override byte size */
    if (byte_size) strm->stream.byte_size = 8;
  }
#ifdef ecl_uint16_t
  else if (ecl_aet_size[type] == 2 && external_format == @':ucs-2') {
    IO_STREAM_ELT_TYPE(strm) = @'character';
    strm->stream.format = @':ucs-2';
    strm->stream.byte_size = 2*8;
    strm->stream.ops->write_char = seq_out_ucs2_write_char;
  }
#endif
#ifdef ecl_uint32_t
  else if (ecl_aet_size[type] == 4 && external_format == @':ucs-4') {
    IO_STREAM_ELT_TYPE(strm) = @'character';
    strm->stream.format = @':ucs-4';
    strm->stream.byte_size = 4*8;
    strm->stream.ops->write_char = seq_out_ucs4_write_char;
  }
#endif
  else {
    FEerror("Illegal combination of external-format ~A and output vector ~A for MAKE-SEQUENCE-OUTPUT-STREAM.~%", 2, external_format, vector);
  }
  SEQ_OUTPUT_VECTOR(strm) = vector;
  SEQ_OUTPUT_POSITION(strm) = vector->vector.fillp;
  return strm;
}

@(defun ext::make_sequence_output_stream (vector &key (external_format ECL_NIL))
@
  @(return make_sequence_output_stream(vector, external_format));
@)

/**********************************************************************
 * MEDIUM LEVEL INTERFACE
 */

struct ecl_file_ops *
duplicate_dispatch_table(const struct ecl_file_ops *ops)
{
  struct ecl_file_ops *new_ops = ecl_alloc_atomic(sizeof(*ops));
  *new_ops = *ops;
  return new_ops;
}

const struct ecl_file_ops *
stream_dispatch_table(cl_object strm)
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

static cl_index
ecl_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  return stream_dispatch_table(strm)->read_byte8(strm, c, n);
}

static cl_index
ecl_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  return stream_dispatch_table(strm)->write_byte8(strm, c, n);
}

ecl_character
ecl_read_char(cl_object strm)
{
  return stream_dispatch_table(strm)->read_char(strm);
}

ecl_character
ecl_read_char_noeof(cl_object strm)
{
  ecl_character c = ecl_read_char(strm);
  if (c == EOF)
    FEend_of_file(strm);
  return c;
}

cl_object
ecl_read_byte(cl_object strm)
{
  return stream_dispatch_table(strm)->read_byte(strm);
}

void
ecl_write_byte(cl_object c, cl_object strm)
{
  stream_dispatch_table(strm)->write_byte(c, strm);
}

ecl_character
ecl_write_char(ecl_character c, cl_object strm)
{
  return stream_dispatch_table(strm)->write_char(strm, c);
}

void
ecl_unread_char(ecl_character c, cl_object strm)
{
  stream_dispatch_table(strm)->unread_char(strm, c);
}

bool
ecl_listen_stream(cl_object strm)
{
  return stream_dispatch_table(strm)->listen(strm);
}

void
ecl_clear_input(cl_object strm)
{
  stream_dispatch_table(strm)->clear_input(strm);
}

void
ecl_clear_output(cl_object strm)
{
  stream_dispatch_table(strm)->clear_output(strm);
}

void
ecl_force_output(cl_object strm)
{
  stream_dispatch_table(strm)->force_output(strm);
}

void
ecl_finish_output(cl_object strm)
{
  stream_dispatch_table(strm)->finish_output(strm);
}

int
ecl_file_column(cl_object strm)
{
  return stream_dispatch_table(strm)->column(strm);
}

cl_object
ecl_file_length(cl_object strm)
{
  return stream_dispatch_table(strm)->length(strm);
}

cl_object
ecl_file_position(cl_object strm)
{
  return stream_dispatch_table(strm)->get_position(strm);
}

cl_object
ecl_file_position_set(cl_object strm, cl_object pos)
{
  return stream_dispatch_table(strm)->set_position(strm, pos);
}

cl_object
ecl_file_string_length(cl_object strm, cl_object string)
{
  return stream_dispatch_table(strm)->string_length(strm, string);
}

bool
ecl_input_stream_p(cl_object strm)
{
  return stream_dispatch_table(strm)->input_p(strm);
}

bool
ecl_output_stream_p(cl_object strm)
{
  return stream_dispatch_table(strm)->output_p(strm);
}

cl_object
ecl_stream_element_type(cl_object strm)
{
  return stream_dispatch_table(strm)->element_type(strm);
}

bool
ecl_interactive_stream_p(cl_object strm)
{
  return stream_dispatch_table(strm)->interactive_p(strm);
}

cl_object
ecl_stream_pathname(cl_object strm)
{
  return stream_dispatch_table(strm)->pathname(strm);
}

cl_object
ecl_stream_truename(cl_object strm)
{
  return stream_dispatch_table(strm)->truename(strm);
}

/*
 * ecl_read_char(s) tries to read a character from the stream S. It outputs
 * either the code of the character read, or EOF. Whe compiled with
 * CLOS-STREAMS and S is an instance object, STREAM-READ-CHAR is invoked
 * to retrieve the character. Then STREAM-READ-CHAR should either
 * output the character, or NIL, indicating EOF.
 *
 * INV: ecl_read_char(strm) checks the type of STRM.
 */
ecl_character
ecl_peek_char(cl_object strm)
{
  return stream_dispatch_table(strm)->peek_char(strm);
}

/*******************************tl***************************************
 * SEQUENCES I/O
 */

void
writestr_stream(const char *s, cl_object strm)
{
  cl_object buffer = si_get_buffer_string();
  cl_index size = ecl_fixnum(cl_array_total_size(buffer));
  cl_index i = 0;
  while (*s != '\0') {
    ecl_char_set(buffer, i++, (ecl_character) *s++);
    if (i >= size) {
      si_fill_pointer_set(buffer, ecl_make_fixnum(size));
      si_do_write_sequence(buffer, strm, ecl_make_fixnum(0), ECL_NIL);
      i = 0;
    }
  }
  si_fill_pointer_set(buffer, ecl_make_fixnum(i));
  si_do_write_sequence(buffer, strm, ecl_make_fixnum(0), ECL_NIL);
  si_put_buffer_string(buffer);
}

cl_object
cl_file_string_length(cl_object stream, cl_object string)
{
  @(return ecl_file_string_length(stream, string));
}

cl_object
si_do_write_sequence(cl_object seq, cl_object stream, cl_object s, cl_object e)
{
  const struct ecl_file_ops *ops;
  cl_fixnum start,limit,end;

  /* Since we have called ecl_length(), we know that SEQ is a valid
     sequence. Therefore, we only need to check the type of the
     object, and seq == ECL_NIL i.f.f. t = t_symbol */
  limit = ecl_length(seq);
  if (ecl_unlikely(!ECL_FIXNUMP(s) ||
                   ((start = ecl_fixnum(s)) < 0) ||
                   (start > limit))) {
    FEwrong_type_key_arg(@[write-sequence], @[:start], s,
                         ecl_make_integer_type(ecl_make_fixnum(0),
                                               ecl_make_fixnum(limit-1)));
  }
  if (e == ECL_NIL) {
    end = limit;
  } else if (ecl_unlikely(!ECL_FIXNUMP(e) ||
                          ((end = ecl_fixnum(e)) < 0) ||
                          (end > limit))) {
    FEwrong_type_key_arg(@[write-sequence], @[:end], e,
                         ecl_make_integer_type(ecl_make_fixnum(0),
                                               ecl_make_fixnum(limit)));
  }
  if (end <= start) {
    goto OUTPUT;
  }
  ops = stream_dispatch_table(stream);
  if (LISTP(seq)) {
    cl_object elt_type = cl_stream_element_type(stream);
    bool ischar = (elt_type == @'base-char') || (elt_type == @'character');
    cl_object s = ecl_nthcdr(start, seq);
    loop_for_in(s) {
      if (start < end) {
        cl_object elt = CAR(s);
        if (ischar)
          ops->write_char(stream, ecl_char_code(elt));
        else
          ops->write_byte(elt, stream);
        start++;
      } else {
        goto OUTPUT;
      }
    } end_loop_for_in;
  } else {
    ops->write_vector(stream, seq, start, end);
  }
 OUTPUT:
  @(return seq);
}

cl_object
si_do_read_sequence(cl_object seq, cl_object stream, cl_object s, cl_object e)
{
  const struct ecl_file_ops *ops;
  cl_fixnum start,limit,end;

  /* Since we have called ecl_length(), we know that SEQ is a valid
     sequence. Therefore, we only need to check the type of the
     object, and seq == ECL_NIL i.f.f. t = t_symbol */
  limit = ecl_length(seq);
  if (ecl_unlikely(!ECL_FIXNUMP(s) ||
                   ((start = ecl_fixnum(s)) < 0) ||
                   (start > limit))) {
    FEwrong_type_key_arg(@[read-sequence], @[:start], s,
                         ecl_make_integer_type(ecl_make_fixnum(0),
                                               ecl_make_fixnum(limit-1)));
  }
  if (e == ECL_NIL) {
    end = limit;
  } else if (ecl_unlikely(!ECL_FIXNUMP(e) ||
                          ((end = ecl_fixnum(e)) < 0) ||
                          (end > limit))) {
    FEwrong_type_key_arg(@[read-sequence], @[:end], e,
                         ecl_make_integer_type(ecl_make_fixnum(0),
                                               ecl_make_fixnum(limit)));
  }
  if (end <= start) {
    goto OUTPUT;
  }
  ops = stream_dispatch_table(stream);
  if (LISTP(seq)) {
    cl_object elt_type = cl_stream_element_type(stream);
    bool ischar = (elt_type == @'base-char') || (elt_type == @'character');
    seq = ecl_nthcdr(start, seq);
    loop_for_in(seq) {
      if (start >= end) {
        goto OUTPUT;
      } else {
        cl_object c;
        if (ischar) {
          int i = ops->read_char(stream);
          if (i < 0) goto OUTPUT;
          c = ECL_CODE_CHAR(i);
        } else {
          c = ops->read_byte(stream);
          if (c == ECL_NIL) goto OUTPUT;
        }
        ECL_RPLACA(seq, c);
        start++;
      }
    } end_loop_for_in;
  } else {
    start = ops->read_vector(stream, seq, start, end);
  }
 OUTPUT:
  @(return ecl_make_fixnum(start));
}

/**********************************************************************
 * LISP LEVEL INTERFACE
 */

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
  @(return (stream_dispatch_table(strm)->interactive_p(strm)? ECL_T : ECL_NIL));
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

/**********************************************************************
 * OTHER TOOLS
 */

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


/**********************************************************************
 * FILE OPENING AND CLOSING
 */

cl_fixnum
ecl_normalize_stream_element_type(cl_object element_type)
{
  cl_fixnum sign = 0;
  cl_index size;
  if (element_type == @'signed-byte' || element_type == @'ext::integer8') {
    return -8;
  } else if (element_type == @'unsigned-byte' || element_type == @'ext::byte8') {
    return 8;
  }
#ifdef ecl_uint16_t
  else if (element_type == @'ext::integer16') {
    return -16;
  } else if (element_type == @'ext::byte16') {
    return 16;
  }
#endif
#ifdef ecl_uint32_t
  else if (element_type == @'ext::integer32') {
    return -32;
  } else if (element_type == @'ext::byte32') {
    return 32;
  }
#endif
#ifdef ecl_uint64_t
  else if (element_type == @'ext::integer64') {
    return -64;
  } else if (element_type == @'ext::byte64') {
    return 64;
  }
#endif
  else if (element_type == @':default') {
    return 0;
  } else if (element_type == @'base-char' || element_type == @'character') {
    return 0;
  } else if (_ecl_funcall3(@'subtypep', element_type, @'character') != ECL_NIL) {
    return 0;
  } else if (_ecl_funcall3(@'subtypep', element_type, @'unsigned-byte') != ECL_NIL) {
    sign = +1;
  } else if (_ecl_funcall3(@'subtypep', element_type, @'signed-byte') != ECL_NIL) {
    sign = -1;
  } else {
    FEerror("Not a valid stream element type: ~A", 1, element_type);
  }
  if (CONSP(element_type)) {
    if (CAR(element_type) == @'unsigned-byte')
      return ecl_to_size(cl_cadr(element_type));
    if (CAR(element_type) == @'signed-byte')
      return -ecl_to_size(cl_cadr(element_type));
  }
  for (size = 8; 1; size++) {
    cl_object type;
    type = cl_list(2, sign>0? @'unsigned-byte' : @'signed-byte',
                   ecl_make_fixnum(size));
    if (_ecl_funcall3(@'subtypep', element_type, type) != ECL_NIL) {
      return size * sign;
    }
  }
  FEerror("Not a valid stream element type: ~A", 1, element_type);
}

static void
FEinvalid_option(cl_object option, cl_object value)
{
  FEerror("Invalid value op option ~A: ~A", 2, option, value);
}

static int
smmode_to_open_flag(enum ecl_smmode smm)
{
  switch (smm) {
  case ecl_smm_probe:
  case ecl_smm_input:
    return O_RDONLY;
  case ecl_smm_output:
    return O_WRONLY;
  case ecl_smm_io:
    return O_RDWR;
  default:
    FEerror("Illegal stream mode ~S", 1, ecl_make_fixnum(smm));
  }
}

cl_object
ecl_open_stream(cl_object fn, enum ecl_smmode smm, cl_object if_exists,
                cl_object if_does_not_exist, cl_fixnum byte_size,
                int flags, cl_object external_format)
{
  cl_object output;
  int fd, open_flags = smmode_to_open_flag(smm) | O_BINARY;
  bool appending = 0;
#if defined(ECL_MS_WINDOWS_HOST)
  ecl_mode_t mode = _S_IREAD | _S_IWRITE;
#else
  ecl_mode_t mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
#endif
  /* FILENAME is used only to access the actual file while a stream
     remembers the original pathname FN. -- jd 2020-03-27 */
  cl_object filename = si_coerce_to_filename(fn);
  ecl_filename_char *fname = ecl_filename_self(filename);

  if (if_does_not_exist == @':create') {
    open_flags |= O_CREAT;
    if ((smm == ecl_smm_output || smm == ecl_smm_io) &&
        (if_exists == ECL_NIL || if_exists == @':error' || if_exists == @':rename')) {
      open_flags |= O_EXCL;
    }
  } else if (if_does_not_exist != ECL_NIL && if_does_not_exist != @':error') {
    FEinvalid_option(@':if-does-not-exist', if_does_not_exist);
  }
  if (if_exists == @':rename_and_delete' ||
      if_exists == @':new_version' ||
      if_exists == @':supersede' ||
      if_exists == @':truncate') {
    if (smm == ecl_smm_output || smm == ecl_smm_io) {
      open_flags |= O_TRUNC;
    }
  } else if (if_exists == @':append') {
    if (smm == ecl_smm_output || smm == ecl_smm_io) {
      appending = 1;
    }
  } else if (if_exists != ECL_NIL &&
             if_exists != @':error' &&
             if_exists != @':rename' &&
             if_exists != @':overwrite') {
    FEinvalid_option(@':if-exists', if_exists);
  }
  if (flags & ECL_STREAM_CLOSE_ON_EXEC) {
    open_flags |= O_CLOEXEC;
  }
  if (flags & ECL_STREAM_NONBLOCK) {
    open_flags |= O_NONBLOCK;
  }

  fd = safe_open(fname, open_flags, mode);
  if (fd < 0) {
    if (errno == ENOENT && if_does_not_exist == ECL_NIL) {
      return ECL_NIL;
    } else if (errno == EEXIST) {
      if (if_exists == ECL_NIL) {
        return ECL_NIL;
      } else if (if_exists == @':error') {
        FEcannot_open(fn);
      } else if (if_exists == @':rename') {
        fd = ecl_backup_open(fname, smmode_to_open_flag(smm)|O_CREAT, mode);
        unlikely_if (fd < 0) FEcannot_open(fn);
      }
    } else {
      FEcannot_open(fn);
    }
  }

  if (smm == ecl_smm_probe) {
    safe_close(fd);
    output = ecl_make_file_stream_from_fd(fn, -1, smm, byte_size, flags, external_format);
    generic_close(output);
    return output;
  }
  if (flags & ECL_STREAM_C_STREAM) {
    FILE *fp = 0;
    switch (smm) {
    case ecl_smm_probe:
      /* never happens (returns earlier) */
    case ecl_smm_input:
      fp = safe_fdopen(fd, OPEN_R);
      break;
    case ecl_smm_output:
      fp = safe_fdopen(fd, OPEN_W);
      break;
    case ecl_smm_io:
      fp = safe_fdopen(fd, OPEN_RW);
      break;
    default:;
      /* never reached (errors earlier) */
    }
    if (fp == NULL) {
      FEcannot_open(fn);
    }
    output = ecl_make_stream_from_FILE(fn, fp, smm, byte_size, flags, external_format);
    si_set_buffering_mode(output, byte_size? @':full' : @':line');
  } else {
    output = ecl_make_file_stream_from_fd(fn, fd, smm, byte_size, flags, external_format);
  }

  output->stream.flags |= ECL_STREAM_MIGHT_SEEK;
  si_set_finalizer(output, ECL_T);
  /* Set file pointer to the correct position */
  ecl_file_position_set(output, appending? ECL_NIL : ecl_make_fixnum(0));
  return output;
}

@(defun open (filename
              &key (direction @':input')
              (element_type @'character')
              (if_exists ECL_NIL iesp)
              (if_does_not_exist ECL_NIL idnesp)
              (external_format @':default')
              (cstream ECL_T)
              (close_on_exec ECL_T)
              (nonblock ECL_NIL)
              &aux strm)
  enum ecl_smmode smm;
  int flags = 0;
  cl_fixnum byte_size;
@
  /* INV: ecl_open_stream() checks types */
  if (direction == @':input') {
    smm = ecl_smm_input;
    if (!idnesp)
      if_does_not_exist = @':error';
  } else if (direction == @':output') {
    smm = ecl_smm_output;
    if (!iesp)
      if_exists = @':new_version';
    if (!idnesp) {
      if (if_exists == @':overwrite' ||
          if_exists == @':append')
        if_does_not_exist = @':error';
      else
        if_does_not_exist = @':create';
    }
  } else if (direction == @':io') {
    smm = ecl_smm_io;
    if (!iesp)
      if_exists = @':new_version';
    if (!idnesp) {
      if (if_exists == @':overwrite' ||
          if_exists == @':append')
        if_does_not_exist = @':error';
      else
        if_does_not_exist = @':create';
    }
  } else if (direction == @':probe') {
    smm = ecl_smm_probe;
    if (!idnesp)
      if_does_not_exist = ECL_NIL;
  } else {
    FEerror("~S is an illegal DIRECTION for OPEN.",
            1, direction);
  }
  byte_size = ecl_normalize_stream_element_type(element_type);
  if (byte_size != 0) {
    external_format = ECL_NIL;
  }
  if (!Null(cstream)) {
    flags |= ECL_STREAM_C_STREAM;
  }
  if (!Null(close_on_exec)) {
    flags |= ECL_STREAM_CLOSE_ON_EXEC;
  }
  if (!Null(nonblock)) {
    flags |= ECL_STREAM_NONBLOCK;
  }
  strm = ecl_open_stream(filename, smm, if_exists, if_does_not_exist,
                         byte_size, flags, external_format);
  @(return strm);
@)

@(defun close (strm &key (abort @'nil'))
@
  @(return stream_dispatch_table(strm)->close(strm));
@)

/**********************************************************************
 * BACKEND
 */

#if defined(ECL_MS_WINDOWS_HOST)
static int
fd_listen(cl_object stream, int fileno)
{
  HANDLE hnd = (HANDLE)_get_osfhandle(fileno);
  switch (GetFileType(hnd)) {
  case FILE_TYPE_CHAR: {
    DWORD dw, dw_read, cm;
    if (GetNumberOfConsoleInputEvents(hnd, &dw)) {
      unlikely_if (!GetConsoleMode(hnd, &cm))
        FEwin32_error("GetConsoleMode() failed", 0);
      if (dw > 0) {
        PINPUT_RECORD recs = (PINPUT_RECORD)GC_MALLOC(sizeof(INPUT_RECORD)*dw);
        int i;
        unlikely_if (!PeekConsoleInput(hnd, recs, dw, &dw_read))
          FEwin32_error("PeekConsoleInput failed()", 0);
        if (dw_read > 0) {
          if (cm & ENABLE_LINE_INPUT) {
            for (i=0; i<dw_read; i++)
              if (recs[i].EventType == KEY_EVENT &&
                  recs[i].Event.KeyEvent.bKeyDown &&
                  recs[i].Event.KeyEvent.uChar.AsciiChar == 13)
                return ECL_LISTEN_AVAILABLE;
          } else {
            for (i=0; i<dw_read; i++)
              if (recs[i].EventType == KEY_EVENT &&
                  recs[i].Event.KeyEvent.bKeyDown &&
                  recs[i].Event.KeyEvent.uChar.AsciiChar != 0)
                return ECL_LISTEN_AVAILABLE;
          }
        }
      }
      return ECL_LISTEN_NO_CHAR;
    } else {
      FEwin32_error("GetNumberOfConsoleInputEvents() failed", 0);
    }
    break;
  }
  case FILE_TYPE_DISK:
    /* use regular file code below */
    break;
  case FILE_TYPE_PIPE: {
    DWORD dw;
    if (PeekNamedPipe(hnd, NULL, 0, NULL, &dw, NULL))
      return (dw > 0 ? ECL_LISTEN_AVAILABLE : ECL_LISTEN_NO_CHAR);
    else if (GetLastError() == ERROR_BROKEN_PIPE)
      return ECL_LISTEN_EOF;
    else
      FEwin32_error("PeekNamedPipe() failed", 0);
    break;
  }
  default:
    FEerror("Unsupported Windows file type: ~A", 1, ecl_make_fixnum(GetFileType(hnd)));
    break;
  }
  return ECL_LISTEN_FALLBACK;
}
#else
static int
fd_listen(cl_object stream, int fileno)
{
  /* Method 1: poll, see POLL(2)
     Method 2: select, see SELECT(2)
     Method 3: ioctl FIONREAD, see FILIO(4)
     Method 4: read a byte. Use non-blocking I/O if poll or select were not
               available. */
  int result;
#if defined(HAVE_POLL)
  struct pollfd fd = {fileno, POLLIN, 0};
restart_poll:
  result = poll(&fd, 1, 0);
  if (ecl_unlikely(result < 0)) {
    if (errno == EINTR)
      goto restart_poll;
    else
      goto listen_error;
  }
  if (fd.revents == 0) {
    return ECL_LISTEN_NO_CHAR;
  }
  /* When read() returns a result without blocking, this can also be
     EOF! (Example: Linux and pipes.) We therefore refrain from simply
     doing  { return ECL_LISTEN_AVAILABLE; }  and instead try methods
     3 and 4. */
#elif defined(HAVE_SELECT)
  fd_set fds;
  struct timeval tv = {0, 0};
  FD_ZERO(&fds);
  FD_SET(fileno, &fds);
restart_select:
  result = select(fileno + 1, &fds, NULL, NULL, &tv);
  if (ecl_unlikely(result < 0)) {
    if (errno == EINTR)
      goto restart_select;
    if (errno != EBADF) /* UNIX_LINUX returns EBADF for files! */
      goto listen_error;
  } else if (result == 0) {
    return ECL_LISTEN_NO_CHAR;
  }
#endif
#ifdef FIONREAD
  long c = 0;
  result = ioctl(fileno, FIONREAD, &c);
  if (result == 0) {
    return (c > 0) ? ECL_LISTEN_AVAILABLE : ECL_LISTEN_EOF;
  } else if (result < 0 && errno != ENOTTY && errno != EINVAL) {
    goto listen_error;
  }
#endif
  cl_index byte;
restart_read:
#if !defined(HAVE_POLL) && !defined(HAVE_SELECT)
  {
    int flags = fcntl(fileno, F_GETFL, 0);
    int read_errno;
    fcntl(fileno, F_SETFL, flags | O_NONBLOCK);
    result = read(fileno, &byte, 1);
    read_errno = errno;
    fcntl(fileno, F_SETFL, flags);
    errno = read_errno;
  }
#else
  result = read(fileno, &byte, 1);
#endif
  if (result < 0) {
    if (errno == EINTR)
      goto restart_read;
    if (errno == EAGAIN || errno == EWOULDBLOCK)
      return ECL_LISTEN_NO_CHAR;
    else
      goto listen_error;
  }

  if (result == 0) {
    return ECL_LISTEN_EOF;
  }

  stream->stream.byte_stack = CONS(ecl_make_fixnum(byte), stream->stream.byte_stack);
  return ECL_LISTEN_AVAILABLE;
listen_error:
  file_libc_error(@[stream-error], stream, "Error while listening to stream.", 0);
}
#endif

static int
file_listen(cl_object stream, FILE *fp)
{
  int aux;
  if (feof(fp) || ferror(fp))
    return ECL_LISTEN_EOF;
#ifdef FILE_CNT
  if (FILE_CNT(fp) > 0)
    return ECL_LISTEN_AVAILABLE;
#endif
  aux = fd_listen(stream, fileno(fp));
  if (aux != ECL_LISTEN_FALLBACK)
    return aux;
  /* This code is portable, and implements the expected behavior for regular files.
     It will fail on noninteractive streams. */
  {
    /* regular file */
    ecl_off_t old_pos = ecl_ftello(fp), end_pos;
    unlikely_if (old_pos == -1 ||
                 ecl_fseeko(fp, 0, SEEK_END) != 0)
      file_libc_error(@[file-error], stream,
                      "Unable to check file position", 0);
    end_pos = ecl_ftello(fp);
    unlikely_if (ecl_fseeko(fp, old_pos, SEEK_SET) != 0)
      file_libc_error(@[file-error], stream,
                      "Unable to check file position", 0);
    return (end_pos > old_pos ? ECL_LISTEN_AVAILABLE : ECL_LISTEN_EOF);
  }
  return !ECL_LISTEN_AVAILABLE;
}

/* Compilation of this function on some platforms may give a warning:
   "right shift count >= width of type [-Werror=shift-count-overflow]"
   but on these platforms this branch is never encountered.

   FIXME: this can probably be conditionaly defined in #if … #endif */
cl_object
ecl_off_t_to_integer(ecl_off_t offset)
{
  cl_object output;
  if (sizeof(ecl_off_t) == sizeof(cl_fixnum)) {
    output = ecl_make_integer(offset);
  } else if (offset <= MOST_POSITIVE_FIXNUM) {
    output = ecl_make_fixnum((cl_fixnum)offset);
  } else {
    cl_object y = _ecl_big_register0();
    if (sizeof(ECL_BIGNUM_LIMBS(y)[0]) == sizeof(cl_index)) {
      ECL_BIGNUM_LIMBS(y)[0] = (cl_index)offset;
      offset >>= ECL_FIXNUM_BITS;
      ECL_BIGNUM_LIMBS(y)[1] = offset;
      ECL_BIGNUM_SIZE(y) = offset? 2 : 1;
    } else if (sizeof(ECL_BIGNUM_LIMBS(y)[0]) >= sizeof(ecl_off_t)) {
      ECL_BIGNUM_LIMBS(y)[0] = offset;
      ECL_BIGNUM_SIZE(y) = 1;
    }
    output = _ecl_big_register_normalize(y);
  }
  return output;
}

/* Compilation of this function on some platforms may give a warning:
   "left shift count >= width of type [-Werror=shift-count-overflow]"
   but on these platforms this branch is never encountered.

   FIXME: this can probably be conditionaly defined in #if … #endif */
ecl_off_t
ecl_integer_to_off_t(cl_object offset)
{
  ecl_off_t output = 0;
  if (sizeof(ecl_off_t) == sizeof(cl_fixnum)) {
    output = fixint(offset);
  } else if (ECL_FIXNUMP(offset)) {
    output = fixint(offset);
  } else if (ECL_BIGNUMP(offset)) {
    if (sizeof(ECL_BIGNUM_LIMBS(offset)[0]) == sizeof(cl_index)) {
      if (ECL_BIGNUM_SIZE(offset) > 2) {
        goto ERROR;
      }
      if (ECL_BIGNUM_SIZE(offset) == 2) {
        output = ECL_BIGNUM_LIMBS(offset)[1];
        output <<= ECL_FIXNUM_BITS;
      }
      output += ECL_BIGNUM_LIMBS(offset)[0];
    } else if (sizeof(ECL_BIGNUM_LIMBS(offset)[0]) >= sizeof(ecl_off_t)) {
      if (ECL_BIGNUM_SIZE(offset) > 1) {
        goto ERROR;
      }
      output = ECL_BIGNUM_LIMBS(offset)[0];
    }
  } else {
  ERROR:    FEerror("Not a valid file offset: ~S", 1, offset);
  }
  return output;
}

static cl_object
alloc_stream()
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
  x->stream.buffer = NULL;
  x->stream.encoder = NULL;
  x->stream.decoder = NULL;
  x->stream.last_char = EOF;
  x->stream.byte_stack = ECL_NIL;
  x->stream.last_code[0] = x->stream.last_code[1] = EOF;
  x->stream.eof_char = EOF;
  return x;
}

/**********************************************************************
 * ERROR MESSAGES
 */

static cl_object
not_a_file_stream(cl_object strm)
{
  cl_error(9, @'simple-type-error', @':format-control',
           @"~A is not an file stream",
           @':format-arguments', cl_list(1, strm),
           @':expected-type', @'file-stream',
           @':datum', strm);
}

static void
not_an_input_stream(cl_object strm)
{
  cl_error(9, @'simple-type-error', @':format-control',
           @"~A is not an input stream",
           @':format-arguments', cl_list(1, strm),
           @':expected-type',
           cl_list(2, @'satisfies', @'input-stream-p'),
           @':datum', strm);
}

static void
not_an_output_stream(cl_object strm)
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

static void
cannot_close(cl_object stream)
{
  file_libc_error(@[file-error], stream, "Stream cannot be closed", 0);
}

static void
file_libc_error(cl_object error_type, cl_object stream,
                const char *msg, int narg, ...)
{
  ecl_va_list args;
  cl_object rest, error = _ecl_strerror(errno);

  ecl_va_start(args, narg, narg, 0);
  rest = cl_grab_rest_args(args);
  ecl_va_end(args);

  si_signal_simple_error(4, (cl_object)(cl_symbols + ecl_fixnum(error_type)), ECL_NIL,
                         @"~?~%C library explanation: ~A.",
                         cl_list(3, ecl_make_constant_base_string(msg,-1), rest,
                                 error));
  _ecl_unexpected_return();
}

static void
unread_error(cl_object s)
{
  CEerror(ECL_T, "Error when using UNREAD-CHAR on stream ~D", 1, s);
}

static void
unread_twice(cl_object s)
{
  CEerror(ECL_T, "Used UNREAD-CHAR twice on stream ~D", 1, s);
}

static void
maybe_clearerr(cl_object strm)
{
  int t = strm->stream.mode;
  if (t == ecl_smm_io || t == ecl_smm_output || t == ecl_smm_input) {
    FILE *f = IO_STREAM_FILE(strm);
    if (f != NULL) clearerr(f);
  }
}

static int
restartable_io_error(cl_object strm, const char *s)
{
  cl_env_ptr the_env = ecl_process_env();
  volatile int old_errno = errno;
  /* ecl_disable_interrupts(); ** done by caller */
  maybe_clearerr(strm);
  ecl_enable_interrupts_env(the_env);
  if (old_errno == EINTR) {
    return 1;
  } else {
    file_libc_error(@[stream-error], strm,
                    "C operation (~A) signaled an error.",
                    1, ecl_make_constant_base_string(s, strlen(s)));
    return 0;
  }
}

static void
io_error(cl_object strm)
{
  cl_env_ptr the_env = ecl_process_env();
  /* ecl_disable_interrupts(); ** done by caller */
  maybe_clearerr(strm);
  ecl_enable_interrupts_env(the_env);
  file_libc_error(@[stream-error], strm,
                  "Read or write operation signaled an error", 0);
}

static void
wrong_file_handler(cl_object strm)
{
  FEerror("Internal error: stream ~S has no valid C file handler.", 1, strm);
}

#ifdef ECL_UNICODE
static cl_index
encoding_error(cl_object stream, unsigned char *buffer, ecl_character c)
{
  cl_object code = _ecl_funcall4(@'ext::encoding-error', stream,
                                 cl_stream_external_format(stream),
                                 ecl_make_integer(c));
  if (Null(code)) {
    /* Output nothing */
    return 0;
  } else {
    /* Try with supplied character */
    return stream->stream.encoder(stream, buffer, ecl_char_code(code));
  }
}

static ecl_character
decoding_error(cl_object stream, unsigned char **buffer, int char_length, unsigned char *buffer_end)
{
  cl_object octets = ECL_NIL, code;
  for (; char_length > 0; char_length--) {
    octets = CONS(ecl_make_fixnum(*((*buffer)++)), octets);
  }
  code = _ecl_funcall4(@'ext::decoding-error', stream,
                       cl_stream_external_format(stream),
                       octets);
  if (Null(code)) {
    /* Go for next character */
    return stream->stream.decoder(stream, buffer, buffer_end);
  } else {
    /* Return supplied character */
    return ecl_char_code(code);
  }
}
#endif

#if defined(ECL_WSOCK)
static void
wsock_error( const char *err_msg, cl_object strm )
{
  char *msg;
  cl_object msg_obj;
  /* ecl_disable_interrupts(); ** done by caller */
  {
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER,
                  0, WSAGetLastError(), 0, (void*)&msg, 0, NULL);
    msg_obj = ecl_make_simple_base_string(msg,-1);
    LocalFree(msg);
  }
  ecl_enable_interrupts();
  FEerror(err_msg, 2, strm, msg_obj);
}
#endif

void
init_file(void)
{
  int flags;
  cl_object standard_input;
  cl_object standard_output;
  cl_object error_output;
  cl_object aux;
  cl_object null_stream;
  cl_object external_format = ECL_NIL;
#if defined(ECL_MS_WINDOWS_HOST)
  /* We start out with :pass-through external format for standard
   * input/output for bootstrap reasons (some of the external format
   * support is implemented in lisp and not available on start of
   * ECL). The correct format is later on set using the encoding
   * specified by the current codepage. */
  external_format = cl_list(2, @':pass-through', @':crlf');
# ifdef ECL_UNICODE
  flags = 0;
# else
  flags = ECL_STREAM_DEFAULT_FORMAT;
# endif
#else
  flags = ECL_STREAM_DEFAULT_FORMAT;
#endif

  null_stream = ecl_make_stream_from_FILE(@"/dev/null",
                                          NULL, ecl_smm_io, 8, flags, external_format);
  generic_close(null_stream);
  null_stream = cl_make_two_way_stream(null_stream, cl_make_broadcast_stream(0));
  cl_core.null_stream = null_stream;

  /* We choose C streams by default only when _not_ using threads.
   * The reason is that C streams block on I/O operations. */
#if !defined(ECL_THREADS)
  standard_input = maybe_make_windows_console_FILE(@"stdin",
                                                   stdin, ecl_smm_input, 8, flags, external_format);
  standard_output = maybe_make_windows_console_FILE(@"stdout",
                                                    stdout, ecl_smm_output, 8, flags, external_format);
  error_output = maybe_make_windows_console_FILE(@"stderr",
                                                 stderr, ecl_smm_output, 8, flags, external_format);
#else
  standard_input = maybe_make_windows_console_fd(@"stdin",
                                                 STDIN_FILENO, ecl_smm_input_file, 8, flags,
                                                 external_format);
  standard_output = maybe_make_windows_console_fd(@"stdout",
                                                  STDOUT_FILENO, ecl_smm_output_file, 8, flags,
                                                  external_format);
  error_output = maybe_make_windows_console_fd(@"stderr",
                                               STDERR_FILENO, ecl_smm_output_file, 8, flags,
                                               external_format);
#endif
  cl_core.standard_input = standard_input;
  ECL_SET(@'ext::+process-standard-input+', standard_input);
  ECL_SET(@'*standard-input*', standard_input);
  cl_core.standard_output = standard_output;
  ECL_SET(@'ext::+process-standard-output+', standard_output);
  ECL_SET(@'*standard-output*', standard_output);
  ECL_SET(@'*trace-output*', standard_output);
  cl_core.error_output = error_output;
  ECL_SET(@'ext::+process-error-output+', error_output);
  ECL_SET(@'*error-output*', error_output);

  cl_core.terminal_io = aux 
    = cl_make_two_way_stream(standard_input, standard_output);

  ECL_SET(@'*terminal-io*', aux);
  aux = cl_make_synonym_stream(@'*terminal-io*');
  ECL_SET(@'*query-io*', aux);
  ECL_SET(@'*debug-io*', aux);
}
