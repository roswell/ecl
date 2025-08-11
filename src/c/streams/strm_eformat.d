/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * strm_eformat.d - External formats encoding/decoding for streams
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

/* -- errors ---------------------------------------------------------------- */

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

/**********************************************************************
 * CHARACTER AND EXTERNAL FORMAT SUPPORT
 */

ecl_character
ecl_eformat_read_char(cl_object strm)
{
  unsigned char *buffer = strm->stream.byte_buffer;
  ecl_character c;
  unsigned char *buffer_pos = buffer;
  unsigned char *buffer_end = buffer;
  cl_index byte_size = (strm->stream.byte_size / 8);
  strm->stream.last_char = EOF;
  strm->stream.last_byte = OBJNULL;
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
  return c;
}

void
ecl_eformat_unread_char(cl_object strm, ecl_character c)
{
  unlikely_if (strm->stream.last_char != EOF
               || strm->stream.last_byte != OBJNULL) {
    ecl_unread_twice(strm);
  }
  strm->stream.last_char = c;
}

ecl_character
ecl_eformat_write_char(cl_object strm, ecl_character c)
{
  unsigned char *buffer = strm->stream.byte_buffer;
  ecl_character nbytes;
  nbytes = strm->stream.encoder(strm, buffer, c);
  strm->stream.ops->write_byte8(strm, buffer, nbytes);
  write_char_increment_column(strm, c);
  return c;
}

static ecl_character
eformat_read_char_cr(cl_object strm)
{
  ecl_character c = ecl_eformat_read_char(strm);
  if (c == ECL_CHAR_CODE_RETURN) {
    c = ECL_CHAR_CODE_NEWLINE;
  }
  return c;
}

static ecl_character
eformat_write_char_cr(cl_object strm, ecl_character c)
{
  if (c == ECL_CHAR_CODE_NEWLINE) {
    ecl_eformat_write_char(strm, ECL_CHAR_CODE_RETURN);
    strm->stream.column = 0;
    return c;
  }
  return ecl_eformat_write_char(strm, c);
}

static ecl_character
eformat_read_char_crlf(cl_object strm)
{
  ecl_character c = ecl_eformat_read_char(strm);
  if (c == ECL_CHAR_CODE_RETURN) {
    c = ecl_eformat_read_char(strm);
    if (c == ECL_CHAR_CODE_LINEFEED) {
      c = ECL_CHAR_CODE_NEWLINE;
    } else {
      ecl_eformat_unread_char(strm, c);
      c = ECL_CHAR_CODE_RETURN;
    }
  }
  return c;
}

static ecl_character
eformat_write_char_crlf(cl_object strm, ecl_character c)
{
  if (c == ECL_CHAR_CODE_NEWLINE) {
    ecl_eformat_write_char(strm, ECL_CHAR_CODE_RETURN);
    ecl_eformat_write_char(strm, ECL_CHAR_CODE_LINEFEED);
    strm->stream.column = 0;
    return c;
  }
  return ecl_eformat_write_char(strm, c);
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

void
ecl_set_stream_elt_type(cl_object stream, cl_fixnum byte_size, int flags,
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
  stream->stream.ops->read_char = ecl_eformat_read_char;
  stream->stream.ops->write_char = ecl_eformat_write_char;
  switch (flags & ECL_STREAM_FORMAT) {
  case ECL_STREAM_BINARY:
    IO_STREAM_ELT_TYPE(stream) = cl_list(2, t, ecl_make_fixnum(byte_size));
    stream->stream.format = t;
    stream->stream.ops->read_char = ecl_not_character_read_char;
    stream->stream.ops->write_char = ecl_not_character_write_char;
    stream->stream.decoder = ecl_not_character_decoder;
    stream->stream.encoder = ecl_not_character_encoder;
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
  if (stream->stream.ops->write_char == ecl_eformat_write_char &&
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
    byte_size = (byte_size+7)&(~(cl_fixnum)7);
    if (byte_size == 8) {
      if (flags & ECL_STREAM_SIGNED_BYTES) {
        stream->stream.byte_decoder = ecl_binary_s8_decoder;
        stream->stream.byte_encoder = ecl_binary_s8_encoder;
      } else {
        stream->stream.byte_decoder = ecl_binary_u8_decoder;
        stream->stream.byte_encoder = ecl_binary_u8_encoder;
      }
    } else if (flags & ECL_STREAM_LITTLE_ENDIAN) {
      stream->stream.byte_decoder = ecl_binary_le_decoder;
      stream->stream.byte_encoder = ecl_binary_le_encoder;
    } else {
      stream->stream.byte_decoder = ecl_binary_be_decoder;
      stream->stream.byte_encoder = ecl_binary_be_encoder;
    }
    if (ecl_input_stream_p(stream)) {
      stream->stream.ops->read_byte = ecl_binary_read_byte;
    }
    if (ecl_output_stream_p(stream)) {
      stream->stream.ops->write_byte = ecl_binary_write_byte;
    }
  }
  stream->stream.flags = flags;
  stream->stream.byte_size = byte_size;
  {
    cl_fixnum buffer_size = byte_size/8;
    if (buffer_size < ENCODING_BUFFER_MAX_SIZE)
      buffer_size = ENCODING_BUFFER_MAX_SIZE;
    stream->stream.byte_buffer = ecl_alloc_atomic(buffer_size);
  }
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
      ecl_set_stream_elt_type(stream, stream->stream.byte_size, stream->stream.flags, format);
    }
    break;
  default:
    FEerror("Cannot change external format of stream ~A", 1, stream);
  }
  @(return);
}

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
ecl_eformat_file_string_length(cl_object stream, cl_object string)
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
