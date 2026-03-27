/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * strm_binary.d - Byte encoding/decoding for streams
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

/* Binary operators */

cl_object
ecl_binary_read_byte(cl_object strm)
{
  cl_index (*read_byte8)(cl_object, unsigned char *, cl_index);
  unsigned char *buf = strm->stream.byte_buffer;
  cl_index nbytes;
  strm->stream.last_char = EOF;
  strm->stream.last_byte = OBJNULL;
  read_byte8 = strm->stream.ops->read_byte8;
  nbytes = strm->stream.byte_size/8;
  if (read_byte8(strm, buf, nbytes) < nbytes)
    return OBJNULL;
  return strm->stream.byte_decoder(strm, buf);
}

void
ecl_binary_write_byte(cl_object strm, cl_object byte)
{
  cl_index (*write_byte8)(cl_object strm, unsigned char *c, cl_index n);
  cl_index nbytes = strm->stream.byte_size/8;
  unsigned char *buf = strm->stream.byte_buffer;
  write_byte8 = strm->stream.ops->write_byte8;
  strm->stream.byte_encoder(strm, buf, byte);
  write_byte8(strm, buf, nbytes);
}

void
ecl_binary_unread_byte(cl_object strm, cl_object byte)
{
  unlikely_if (strm->stream.last_char != EOF
               || strm->stream.last_byte != OBJNULL) {
    ecl_unread_twice(strm);
  }
  strm->stream.last_byte = byte;
}

/*
 * 8-bit unsigned
 */

cl_object
ecl_binary_u8_decoder(cl_object strm, unsigned char *buf)
{
  unsigned char c = buf[0];
  return ecl_make_fixnum(c);
}

void
ecl_binary_u8_encoder(cl_object strm, unsigned char *buf, cl_object byte)
{
  unsigned char c = ecl_to_uint8_t(byte);
  buf[0] = c;
}

/*
 * 8-bit signed
 */
cl_object
ecl_binary_s8_decoder(cl_object strm, unsigned char *buf)
{
  signed char c = (signed char)buf[0];
  return ecl_make_fixnum(c);
}

void
ecl_binary_s8_encoder(cl_object strm, unsigned char *buf, cl_object byte)
{
  signed char c = ecl_to_int8_t(byte);
  buf[0] = (unsigned char)c;
}

/*
 * Big Endian
 */
cl_object
ecl_binary_be_decoder(cl_object strm, unsigned char *buf)
{
  cl_index idx, ndx = strm->stream.byte_size/8;
  cl_object output = OBJNULL;
  cl_object offset = ecl_make_fixnum(8);
  unsigned char c;
  for (idx=0; idx<ndx; idx++) {
    c = buf[idx];
    if (output) {
      output = cl_logior(2, ecl_make_fixnum(c), cl_ash(output, offset));
    } else  {
      output = (strm->stream.flags & ECL_STREAM_SIGNED_BYTES)
        ? ecl_make_fixnum((signed char)c)
        : ecl_make_fixnum((unsigned char)c);
    }
  }
  return output;
}

void
ecl_binary_be_encoder(cl_object strm, unsigned char *buf, cl_object byte)
{
  cl_index idx, ndx = strm->stream.byte_size/8;
  cl_object offset = ecl_make_fixnum(-8);
  cl_object mask = ecl_make_fixnum(0xFF);
  for (idx=0; idx<ndx; idx++) {
    cl_object b = cl_logand(2, byte, mask);
    buf[ndx-idx-1] = (unsigned char)ecl_fixnum(b);
    byte = cl_ash(byte, offset);
  }
}

/*
 * Little Endian
 */
cl_object
ecl_binary_le_decoder(cl_object strm, unsigned char *buf)
{
  cl_index idx, ndx = strm->stream.byte_size/8;
  cl_object output = OBJNULL;
  cl_object offset = ecl_make_fixnum(8);
  unsigned char c;
  for (idx=0; idx<ndx; idx++) {
    c = buf[ndx-idx-1];
    if (output) {
      output = cl_logior(2, ecl_make_fixnum(c), cl_ash(output, offset));
    } else  {
      output = (strm->stream.flags & ECL_STREAM_SIGNED_BYTES)
        ? ecl_make_fixnum((signed char)c)
        : ecl_make_fixnum((unsigned char)c);
    }
  }
  return output;
}

void
ecl_binary_le_encoder(cl_object strm, unsigned char *buf, cl_object byte)
{
  cl_index idx, ndx = strm->stream.byte_size/8;
  cl_object offset = ecl_make_fixnum(-8);
  cl_object mask = ecl_make_fixnum(0xFF);
  for (idx=0; idx<ndx; idx++) {
    cl_object b = cl_logand(2, byte, mask);
    buf[idx] = (unsigned char)ecl_fixnum(b);
    byte = cl_ash(byte, offset);
  }
}
