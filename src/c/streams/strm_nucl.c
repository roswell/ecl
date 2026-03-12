/* -- imports ------------------------------------------------------- */

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/external.h>
#include <ecl/bytecodes.h>

cl_index
not_implemented_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  ecl_ferror(ECL_EX_NIY, ECL_NIL, ECL_NIL);
  return 0;
}

static cl_index
not_implemented_vector(cl_object strm, cl_object data, cl_index start, cl_index end)
{
  ecl_ferror(ECL_EX_NIY, ECL_NIL, ECL_NIL);
  return 0;
}

static void
not_implemented_writer(cl_object strm, cl_object c)
{
  ecl_ferror(ECL_EX_NIY, ECL_NIL, ECL_NIL);
}

static void
not_implemented_option(cl_object strm)
{
  ecl_ferror(ECL_EX_NIY, ECL_NIL, ECL_NIL);
}

static cl_object
not_implemented_setter(cl_object strm, cl_object val)
{
  ecl_ferror(ECL_EX_NIY, ECL_NIL, ECL_NIL);
}

static cl_object
not_implemented_reader(cl_object strm)
{
  ecl_ferror(ECL_EX_NIY, ECL_NIL, ECL_NIL);
  return ECL_NIL;
}

static int
not_implemented_reader_raw(cl_object strm)
{
  ecl_ferror(ECL_EX_NIY, ECL_NIL, ECL_NIL);
  return 0;
}

static int
not_implemented_writer_raw(cl_object strm, int c)
{
  ecl_ferror(ECL_EX_NIY, ECL_NIL, ECL_NIL);
  return 0;
}

static void
not_implemented_unread_raw(cl_object strm, int c)
{
  ecl_ferror(ECL_EX_NIY, ECL_NIL, ECL_NIL);
}

struct ecl_file_ops stub_io_ops = {
  /* Used to implement encodings */
  .write_byte8   = not_implemented_byte8,
  .read_byte8    = not_implemented_byte8,
  /* Binary I/O */
  .write_byte    = not_implemented_writer,
  .read_byte     = not_implemented_reader,
  /* String I/O */
  .read_char     = not_implemented_reader_raw,
  .write_char    = not_implemented_writer_raw,
  .unread_char   = not_implemented_unread_raw,
  .peek_char     = not_implemented_reader_raw,
  /* Used to implement r/w sequence */
  .read_vector   = not_implemented_vector,
  .write_vector  = not_implemented_vector,
  /* Stream operations */
  .listen        = not_implemented_reader_raw,
  .clear_input   = not_implemented_option,
  .clear_output  = not_implemented_option,
  .finish_output = not_implemented_option,
  .force_output  = not_implemented_option,
  /* Stream appraisal */
  .input_p       = not_implemented_reader_raw,
  .output_p      = not_implemented_reader_raw,
  .interactive_p = not_implemented_reader_raw,
  .element_type  = not_implemented_reader,
  /* Cursor operations */
  .length        = not_implemented_reader,
  .get_position  = not_implemented_reader,
  .set_position  = not_implemented_setter,
  .string_length = not_implemented_setter,
  .column        = not_implemented_reader_raw,
  /* File stream readers */
  .pathname      = not_implemented_reader,
  .truename      = not_implemented_reader,
  /* Closing the stream (generic_close replaces the dispatch table) */
  .close         = not_implemented_reader,
};

cl_object
ecl_make_stub_stream(void)
{
  cl_object strm = ecl_alloc_stream();
  strm->stream.ops = &stub_io_ops;
  strm->stream.mode = ecl_smm_other;
  return strm;
}


/* Nucl stream is an input stream that implements only operations that are
   necessary for I/O (either character or binary). Limitations:

 * char === byte === (unsigned-byte 8)
 * use C99 streams (fopen, fread, fwrite etc)
 * streams are bivalent
 * streams are either input or output (io)
 * not all stream operations are implemented

Other than that we follow the same implementation strategy as other streams.
~{read,write}_byte8~ are used to churn bytes and {read,write}_{byte,char}
composes them bytes (in our case it is simply byte casting to lisp type). */

static int
nucl_io_error(cl_object strm, const char *s)
{
  const cl_env_ptr the_env = ecl_process_env();
  volatile int old_errno = errno;
  FILE *f = IO_STREAM_FILE(strm);
  if (f != NULL) clearerr(f);
  ecl_enable_interrupts_env(the_env);
  if (old_errno == EINTR)
    return 1;
  ecl_internal_error("nucl_io_error: something happened!");
}

static cl_index
nucl_r8(cl_object strm, unsigned char *c, cl_index n)
{
  FILE *f = IO_STREAM_FILE(strm);
  cl_fixnum out = 0;
  ecl_disable_interrupts();
  do {
    out = fread(c, sizeof(char), n, f);
  } while (out < n && ferror(f) && nucl_io_error(strm, "fread"));
  ecl_enable_interrupts();
  return out;
}

static cl_index
nucl_w8(cl_object strm, unsigned char *c, cl_index n)
{
  cl_index out;
  ecl_disable_interrupts();
  do {
    out = fwrite(c, sizeof(char), n, IO_STREAM_FILE(strm));
  } while (out < n && nucl_io_error(strm, "fwrite"));
  ecl_enable_interrupts();
  return out;
}

static ecl_character
nucl_read_char(cl_object strm)
{
  unsigned char c;
  if (!Null(strm->stream.byte_stack)) {
    cl_object value = strm->stream.byte_stack;
    strm->stream.byte_stack = ECL_NIL;
    return ECL_CHAR_CODE(value);
  }
  if (nucl_r8(strm, &c, 1) < 1) {
    return EOF;
  }
  return c;
}

static cl_object
nucl_read_byte(cl_object strm)
{
  unsigned char c;
  if (nucl_r8(strm, &c, 1) < 1) {
    return ECL_NIL;
  }
  return ecl_make_fixnum(c);
}

static void
nucl_unread_char(cl_object strm, int ch)
{
  strm->stream.byte_stack = ECL_CODE_CHAR(ch);
}

static ecl_character
nucl_peek_char(cl_object strm)
{
  ecl_character c = ecl_read_char(strm);
  if (c != EOF) ecl_unread_char(c, strm);
  return c;
}

/* FIXME write_char and write_char have different order of args(!) */
/* FIXME write_char and write_byte have different results(!) */
static void
nucl_write_byte(cl_object strm, cl_object byte)
{
  unsigned char v = (unsigned char)ecl_fixnum(byte);
  nucl_w8(strm, &v, 1);
}

static ecl_character
nucl_write_char(cl_object strm, ecl_character c)
{
  unsigned char v = (unsigned char)c;
  nucl_w8(strm, &v, 1);
  return c;
}

struct ecl_file_ops nucl_io_ops = {
  /* Used to implement encodings */
  .write_byte8   = nucl_r8,
  .read_byte8    = nucl_w8,
  /* Binary I/O */
  .write_byte    = nucl_write_byte,
  .read_byte     = nucl_read_byte,
  /* String I/O */
  .read_char     = nucl_read_char,
  .write_char    = nucl_write_char,
  .unread_char   = nucl_unread_char,
  .peek_char     = nucl_peek_char,
  /* Used to implement r/w sequence */
  .read_vector   = not_implemented_vector,
  .write_vector  = not_implemented_vector,
  /* Stream operations */
  .listen        = not_implemented_reader_raw,
  .clear_input   = not_implemented_option,
  .clear_output  = not_implemented_option,
  .finish_output = not_implemented_option,
  .force_output  = not_implemented_option,
  /* Stream appraisal */
  .input_p       = not_implemented_reader_raw,
  .output_p      = not_implemented_reader_raw,
  .interactive_p = not_implemented_reader_raw,
  .element_type  = not_implemented_reader,
  /* Cursor operations */
  .length        = not_implemented_reader,
  .get_position  = not_implemented_reader,
  .set_position  = not_implemented_setter,
  .string_length = not_implemented_setter,
  .column        = not_implemented_reader_raw,
  /* File stream readers */
  .pathname      = not_implemented_reader,
  .truename      = not_implemented_reader,
  /* Closing the stream (generic_close replaces the dispatch table) */
  .close         = not_implemented_reader,
};

cl_object
ecl_make_nucl_stream(FILE *f)
{
  cl_object strm = ecl_alloc_stream();
  /* ecl_make_stream_from_FILE() */
  strm->stream.mode = ecl_smm_other;
  strm->stream.closed = 0;
  strm->stream.column = 0;
  strm->stream.last_op = 0;
  strm->stream.ops = ecl_duplicate_dispatch_table(&nucl_io_ops);
  strm->stream.byte_stack = ECL_NIL;
  IO_STREAM_FILENAME(strm) = ECL_NIL;
  IO_STREAM_FILE(strm) = f;
#if 0                              /* currently we don't do formatting */
  si_set_finalizer(stream, ECL_T); /* calls cl_close */
  /* ecl_set_stream_elt_type() */
  stream->stream.flags = ECL_STREAM_DEFAULT_FORMAT;
  IO_STREAM_ELT_TYPE(stream) = @'base-char';
  stream->stream.format = @':pass-through';
  stream->stream.encoder = passthrough_encoder;
  stream->stream.decoder = passthrough_decoder;
#endif
  return strm;
}
