/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * strm_os.d - Operating System Stream dispatch tables
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 * Copyright (c) 2025 Daniel Kochmanski
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

/* Size of the encoding buffer for vectors */
#define VECTOR_ENCODING_BUFFER_SIZE 2048

static int file_listen(cl_object, FILE *);
static int fd_listen(cl_object, int);

static void cannot_close(cl_object stream) ecl_attr_noreturn;
static void file_libc_error(cl_object error_type, cl_object stream, const char *msg, int narg, ...) ecl_attr_noreturn;
static int restartable_io_error(cl_object strm, const char *s);
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

/* -- Byte stack --------------------------------------------------- */
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

static void
io_file_unread_char(cl_object strm, ecl_character c)
{
  ecl_eformat_unread_char(strm, c);
  if (c == ECL_CHAR_CODE_NEWLINE) {
    unsigned char *buffer = strm->stream.byte_buffer;
    int ndx = 0;
    cl_object l = strm->stream.byte_stack;
    int flags = strm->stream.flags;
    if (flags & ECL_STREAM_CR) {
      if (flags & ECL_STREAM_LF) {
        /* Byte stack lands in a reverse order. */
        ndx = strm->stream.encoder(strm, buffer, ECL_CHAR_CODE_LINEFEED);
        while (ndx != 0) l = CONS(ecl_make_fixnum(buffer[--ndx]), l);
      }
      ndx = strm->stream.encoder(strm, buffer, ECL_CHAR_CODE_RETURN);
      while (ndx != 0) l = CONS(ecl_make_fixnum(buffer[--ndx]), l);
    } else {
      ndx = strm->stream.encoder(strm, buffer, ECL_CHAR_CODE_NEWLINE);
      while (ndx != 0) l = CONS(ecl_make_fixnum(buffer[--ndx]), l);
    }
    strm->stream.byte_stack = l;
  } else {
    unsigned char *buffer = strm->stream.byte_buffer;
    int ndx = 0;
    cl_object l = strm->stream.byte_stack;
    ndx = strm->stream.encoder(strm, buffer, c);
    while (ndx != 0) l = CONS(ecl_make_fixnum(buffer[--ndx]), l);
    strm->stream.byte_stack = l;
  }
}

static void
io_file_unread_byte(cl_object strm, cl_object byte)
{
  int ndx = strm->stream.byte_size/8;
  cl_object l = strm->stream.byte_stack;
  unsigned char *buffer = strm->stream.byte_buffer;
  ecl_binary_unread_byte(strm, byte);
  strm->stream.byte_encoder(strm, buffer, byte);
  while (ndx != 0) l = CONS(ecl_make_fixnum(buffer[--ndx]), l);
  strm->stream.byte_stack = l;
}

/**********************************************************************
 * POSIX FILE STREAM
 */

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
    ecl_character c = ecl_eformat_read_char(strm);
    if (c == EOF) return;
  }
}

#define io_file_clear_output ecl_generic_void
#define io_file_force_output ecl_generic_void
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
  return ecl_generic_close(strm);
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
        c = ECL_CHAR_CODE_NEWLINE;
      }
      else {
        *buffer_pos = previous_buffer_pos;
        c = ECL_CHAR_CODE_RETURN;
      }
    } else if (strm->stream.flags & ECL_STREAM_CR && c == ECL_CHAR_CODE_RETURN) {
      if (strm->stream.flags & ECL_STREAM_LF) {
        crlf = 1;
        goto AGAIN;
      }
      else
        c = ECL_CHAR_CODE_NEWLINE;
    }
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
  return ecl_generic_read_vector(strm, data, start, end);
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
  return ecl_generic_write_vector(strm, data, start, end);
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
  io_file_read_byte8,
  io_file_write_byte8,

  ecl_binary_read_byte,
  ecl_binary_write_byte,
  io_file_unread_byte,
  ecl_generic_peek_byte,

  ecl_eformat_read_char,
  ecl_eformat_write_char,
  io_file_unread_char,
  ecl_generic_peek_char,

  io_file_read_vector,
  io_file_write_vector,

  io_file_listen,
  io_file_clear_input,
  io_file_clear_output,
  io_file_finish_output,
  io_file_force_output,

  ecl_generic_always_true, /* input_p */
  ecl_generic_always_true, /* output_p */
  io_file_interactive_p,
  io_file_element_type,

  io_file_length,
  io_file_get_position,
  io_file_set_position,
  ecl_eformat_file_string_length,
  ecl_generic_column,

  io_file_pathname,
  io_file_truename,

  io_file_close
};

const struct ecl_file_ops output_file_ops = {
  ecl_not_input_read_byte8,
  output_file_write_byte8,

  ecl_not_input_read_byte,
  ecl_binary_write_byte,
  ecl_not_input_unread_byte,
  ecl_not_input_read_byte,

  ecl_not_input_read_char,
  ecl_eformat_write_char,
  ecl_not_input_unread_char,
  ecl_not_input_read_char,

  ecl_generic_read_vector,
  io_file_write_vector,

  ecl_not_input_listen,
  ecl_not_input_clear_input,
  io_file_clear_output,
  io_file_finish_output,
  io_file_force_output,

  ecl_generic_always_false, /* input_p */
  ecl_generic_always_true, /* output_p */
  ecl_generic_always_false,
  io_file_element_type,

  io_file_length,
  io_file_get_position,
  io_file_set_position,
  ecl_eformat_file_string_length,
  ecl_generic_column,

  io_file_pathname,
  io_file_truename,

  io_file_close
};

const struct ecl_file_ops input_file_ops = {
  io_file_read_byte8,
  ecl_not_output_write_byte8,

  ecl_binary_read_byte,
  ecl_not_output_write_byte,
  io_file_unread_byte,
  ecl_generic_peek_byte,

  ecl_eformat_read_char,
  ecl_not_output_write_char,
  io_file_unread_char,
  ecl_generic_peek_char,

  io_file_read_vector,
  ecl_generic_write_vector,

  io_file_listen,
  io_file_clear_input,
  ecl_not_output_clear_output,
  ecl_not_output_finish_output,
  ecl_not_output_force_output,

  ecl_generic_always_true, /* input_p */
  ecl_generic_always_false, /* output_p */
  io_file_interactive_p,
  io_file_element_type,

  io_file_length,
  io_file_get_position,
  io_file_set_position,
  ecl_not_output_string_length,
  ecl_unknown_column,

  io_file_pathname,
  io_file_truename,

  io_file_close
};

cl_object
ecl_make_file_stream_from_fd(cl_object fname, int fd, enum ecl_smmode smm,
                             cl_fixnum byte_size, int flags, cl_object external_format)
{
  cl_object stream = ecl_alloc_stream();
  switch(smm) {
  case ecl_smm_input:
    smm = ecl_smm_input_file;
  case ecl_smm_input_file:
  case ecl_smm_probe:
    stream->stream.ops = ecl_duplicate_dispatch_table(&input_file_ops);
    break;
  case ecl_smm_output:
    smm = ecl_smm_output_file;
  case ecl_smm_output_file:
    stream->stream.ops = ecl_duplicate_dispatch_table(&output_file_ops);
    break;
  case ecl_smm_io:
    smm = ecl_smm_io_file;
  case ecl_smm_io_file:
    stream->stream.ops = ecl_duplicate_dispatch_table(&io_file_ops);
    break;
  default:
    FEerror("make_stream: wrong mode", 0);
  }
  stream->stream.mode = (short)smm;
  stream->stream.closed = 0;
  ecl_set_stream_elt_type(stream, byte_size, flags, external_format);
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

#define io_stream_clear_output ecl_generic_void

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
  return ecl_generic_close(strm);
}

/*
 * Specialized sequence operations
 */

#define io_stream_read_vector io_file_read_vector
#define io_stream_write_vector io_file_write_vector

const struct ecl_file_ops io_stream_ops = {
  io_stream_read_byte8,
  io_stream_write_byte8,

  ecl_binary_read_byte,
  ecl_binary_write_byte,
  io_file_unread_byte,
  ecl_generic_peek_byte,

  ecl_eformat_read_char,
  ecl_eformat_write_char,
  io_file_unread_char,
  ecl_generic_peek_char,

  io_file_read_vector,
  io_file_write_vector,

  io_stream_listen,
  io_stream_clear_input,
  io_stream_clear_output,
  io_stream_finish_output,
  io_stream_force_output,

  ecl_generic_always_true, /* input_p */
  ecl_generic_always_true, /* output_p */
  io_stream_interactive_p,
  io_file_element_type,

  io_stream_length,
  io_stream_get_position,
  io_stream_set_position,
  ecl_eformat_file_string_length,
  ecl_generic_column,

  io_file_pathname,
  io_file_truename,

  io_stream_close
};

const struct ecl_file_ops output_stream_ops = {
  ecl_not_input_read_byte8,
  output_stream_write_byte8,

  ecl_not_input_read_byte,
  ecl_binary_write_byte,
  ecl_not_input_unread_byte,
  ecl_not_input_read_byte,

  ecl_not_input_read_char,
  ecl_eformat_write_char,
  ecl_not_input_unread_char,
  ecl_not_input_read_char,

  ecl_generic_read_vector,
  io_file_write_vector,

  ecl_not_input_listen,
  ecl_generic_void,
  io_stream_clear_output,
  io_stream_finish_output,
  io_stream_force_output,

  ecl_generic_always_false, /* input_p */
  ecl_generic_always_true, /* output_p */
  ecl_generic_always_false,
  io_file_element_type,

  io_stream_length,
  io_stream_get_position,
  io_stream_set_position,
  ecl_eformat_file_string_length,
  ecl_generic_column,

  io_file_pathname,
  io_file_truename,

  io_stream_close
};

const struct ecl_file_ops input_stream_ops = {
  input_stream_read_byte8,
  ecl_not_output_write_byte8,

  ecl_binary_read_byte,
  ecl_not_output_write_byte,
  io_file_unread_byte,
  ecl_generic_peek_byte,

  ecl_eformat_read_char,
  ecl_not_output_write_char,
  io_file_unread_char,
  ecl_generic_peek_char,

  io_file_read_vector,
  ecl_generic_write_vector,

  io_stream_listen,
  io_stream_clear_input,
  ecl_generic_void,
  ecl_generic_void,
  ecl_generic_void,

  ecl_generic_always_true, /* input_p */
  ecl_generic_always_false, /* output_p */
  io_stream_interactive_p,
  io_file_element_type,

  io_stream_length,
  io_stream_get_position,
  io_stream_set_position,
  ecl_not_output_string_length,
  ecl_unknown_column,

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
    ecl_eformat_read_char(strm);
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
  return ecl_generic_close(strm);
}

const struct ecl_file_ops winsock_stream_io_ops = {
  winsock_stream_read_byte8,
  winsock_stream_write_byte8,

  ecl_binary_read_byte,
  ecl_binary_write_byte,
  io_file_unread_byte,
  ecl_generic_peek_byte,

  ecl_eformat_read_char,
  ecl_eformat_write_char,
  io_file_unread_char,
  ecl_generic_peek_char,

  ecl_generic_read_vector,
  ecl_generic_write_vector,

  winsock_stream_listen,
  winsock_stream_clear_input,
  ecl_generic_void,
  ecl_generic_void,
  ecl_generic_void,

  ecl_generic_always_true, /* input_p */
  ecl_generic_always_true, /* output_p */
  ecl_generic_always_false,
  winsock_stream_element_type,

  ecl_not_a_file_stream,
  ecl_generic_always_nil, /* get_position */
  ecl_generic_set_position,
  ecl_eformat_file_string_length,
  ecl_generic_column,

  ecl_not_a_file_stream,
  ecl_not_a_file_stream,

  winsock_stream_close
};

const struct ecl_file_ops winsock_stream_output_ops = {
  ecl_not_input_read_byte8,
  winsock_stream_write_byte8,

  ecl_not_input_read_byte,
  ecl_binary_write_byte,
  ecl_not_input_unread_byte,
  ecl_not_input_read_byte,

  ecl_not_input_read_char,
  ecl_eformat_write_char,
  ecl_not_input_unread_char,
  ecl_not_input_read_char,

  ecl_generic_read_vector,
  ecl_generic_write_vector,

  ecl_not_input_listen,
  ecl_not_input_clear_input,
  ecl_generic_void,
  ecl_generic_void,
  ecl_generic_void,

  ecl_generic_always_false, /* input_p */
  ecl_generic_always_true, /* output_p */
  ecl_generic_always_false,
  winsock_stream_element_type,

  ecl_not_a_file_stream,
  ecl_generic_always_nil, /* get_position */
  ecl_generic_set_position,
  ecl_eformat_file_string_length,
  ecl_generic_column,

  ecl_not_a_file_stream,
  ecl_not_a_file_stream,

  winsock_stream_close
};

const struct ecl_file_ops winsock_stream_input_ops = {
  winsock_stream_read_byte8,
  ecl_not_output_write_byte8,

  ecl_binary_read_byte,
  ecl_not_output_write_byte,
  io_file_unread_byte,
  ecl_generic_peek_byte,

  ecl_eformat_read_char,
  ecl_not_output_write_char,
  io_file_unread_char,
  ecl_generic_peek_char,

  ecl_generic_read_vector,
  ecl_generic_write_vector,

  winsock_stream_listen,
  winsock_stream_clear_input,
  ecl_not_output_clear_output,
  ecl_not_output_finish_output,
  ecl_not_output_force_output,

  ecl_generic_always_true, /* input_p */
  ecl_generic_always_false, /* output_p */
  ecl_generic_always_false,
  winsock_stream_element_type,

  ecl_not_a_file_stream,
  ecl_generic_always_nil, /* get_position */
  ecl_generic_set_position,
  ecl_not_output_string_length,
  ecl_unknown_column,

  ecl_not_a_file_stream,
  ecl_not_a_file_stream,

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
  wcon_stream_read_byte8,
  wcon_stream_write_byte8,

  ecl_binary_read_byte,
  ecl_binary_write_byte,
  io_file_unread_byte,
  ecl_generic_peek_byte,

  ecl_eformat_read_char,
  ecl_eformat_write_char,
  io_file_unread_char,
  ecl_generic_peek_char,

  ecl_generic_read_vector,
  ecl_generic_write_vector,

  wcon_stream_listen,
  wcon_stream_clear_input,
  ecl_generic_void,
  wcon_stream_force_output,
  wcon_stream_force_output,

  ecl_generic_always_true, /* input_p */
  ecl_generic_always_true, /* output_p */
  ecl_generic_always_false,
  wcon_stream_element_type,

  ecl_not_a_file_stream,
  ecl_generic_always_nil, /* get_position */
  ecl_generic_set_position,
  ecl_eformat_file_string_length,
  ecl_generic_column,

  io_file_pathname,
  io_file_truename,

  ecl_generic_close,
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
  stream = ecl_alloc_stream();
  stream->stream.mode = (short)smm;
  stream->stream.closed = 0;
  switch (smm) {
  case ecl_smm_io:
    stream->stream.ops = ecl_duplicate_dispatch_table(&io_stream_ops);
    break;
  case ecl_smm_probe:
  case ecl_smm_input:
    stream->stream.ops = ecl_duplicate_dispatch_table(&input_stream_ops);
    break;
  case ecl_smm_output:
    stream->stream.ops = ecl_duplicate_dispatch_table(&output_stream_ops);
    break;
#if defined(ECL_WSOCK)
  case ecl_smm_input_wsock:
    stream->stream.ops = ecl_duplicate_dispatch_table(&winsock_stream_input_ops);
    break;
  case ecl_smm_output_wsock:
    stream->stream.ops = ecl_duplicate_dispatch_table(&winsock_stream_output_ops);
    break;
  case ecl_smm_io_wsock:
    stream->stream.ops = ecl_duplicate_dispatch_table(&winsock_stream_io_ops);
    break;
  case ecl_smm_io_wcon:
    stream->stream.ops = ecl_duplicate_dispatch_table(&wcon_stream_io_ops);
    break;
#endif
  default:
    FEerror("Not a valid mode ~D for ecl_make_stream_from_FILE", 1, ecl_make_fixnum(smm));
  }
  ecl_set_stream_elt_type(stream, byte_size, flags, external_format);
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

/**********************************************************************
 * FILE OPENING AND CLOSING
 */

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
    ecl_generic_close(output);
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

/**********************************************************************
 * ERROR MESSAGES
 */

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

/* -- This function hooks up OS streams into Lisp standard streams ---------- */

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
  /* We start out with :pass-through external format for standard input/output
   * for bootstrap reasons (some of the external format support is implemented
   * in lisp and not available on start of ECL). The correct format is later on
   * set using the encoding specified by the current codepage. */
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
  ecl_generic_close(null_stream);
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
