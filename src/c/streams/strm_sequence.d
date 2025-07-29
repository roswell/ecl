/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * strm_sequence.d - Sequence Stream dispatch tables
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
#include <ecl/ecl-inl.h>
#define ECL_DEFINE_AET_SIZE
#include <ecl/internal.h>

/**********************************************************************
 * Direct vector operations (no conversion, arbitrary element type).
 */

static cl_object
seq_read_object(cl_object strm)
{
  cl_object vec = SEQ_STREAM_VECTOR(strm);
  cl_fixnum pos = SEQ_STREAM_POSITION(strm);
  if (pos < SEQ_INPUT_LIMIT(strm)) {
    SEQ_STREAM_POSITION(strm)++;
    return ecl_aref_unsafe(vec, pos);
  }
  return OBJNULL;
}

static cl_object
seq_peek_object(cl_object strm)
{
  cl_object vec = SEQ_STREAM_VECTOR(strm);
  cl_fixnum pos = SEQ_STREAM_POSITION(strm);
  if (pos < SEQ_INPUT_LIMIT(strm)) {
    return ecl_aref_unsafe(vec, pos);
  }
  return OBJNULL;
}

static void
seq_write_object(cl_object strm, cl_object object)
{
  cl_object vec = SEQ_STREAM_VECTOR(strm);
  cl_fixnum pos = SEQ_STREAM_POSITION(strm);
  cl_fixnum dim = vec->vector.dim;
 AGAIN:
  if (pos >= dim) {
    cl_object size = ecl_ash(ecl_make_fixnum(dim), 1);
    SEQ_STREAM_VECTOR(strm) = vec = si_adjust_vector(vec, size);
    goto AGAIN;
  }
  ecl_aset(vec, pos++, object);
  if (vec->vector.fillp < pos)
    vec->vector.fillp = pos;
  SEQ_STREAM_POSITION(strm) = pos;
}

static void
seq_unread_object(cl_object strm, cl_object object)
{
  unlikely_if (SEQ_STREAM_POSITION(strm) <= 0) {
    ecl_unread_error(strm);
  }
  SEQ_STREAM_POSITION(strm)--;
}

/* -- Direct byte <- byte ------------------------------------------- */
static cl_object
byte_byte(cl_object byte)
{
  unlikely_if (byte != OBJNULL && Null(cl_integerp(byte))) {
    FEwrong_type_argument(@[byte], byte);
  }
  return byte;
}

static cl_object
seq_byte_read_byte(cl_object strm)
{ return byte_byte(seq_read_object(strm)); }

static cl_object
seq_byte_peek_byte(cl_object strm)
{ return byte_byte(seq_peek_object(strm)); }

static void
seq_byte_write_byte(cl_object strm, cl_object byte)
{ seq_write_object(strm, byte_byte(byte)); }

static void
seq_byte_unread_byte(cl_object strm, cl_object byte)
{ seq_unread_object(strm, byte_byte(byte)); }

/* -- Direct char <- char ------------------------------------------- */
static ecl_character
char_char(cl_object byte)
{
  unlikely_if (byte != OBJNULL && !ECL_CHARACTERP(byte)) {
    FEwrong_type_argument(@[char], byte);
  }
  return byte == OBJNULL ? EOF : ECL_CHAR_CODE(byte);
}

static ecl_character
seq_char_read_char(cl_object strm)
{ return char_char(seq_read_object(strm)); }

static ecl_character
seq_char_peek_char(cl_object strm)
{ return char_char(seq_peek_object(strm)); }

static ecl_character
seq_char_write_char(cl_object strm, ecl_character c)
{ seq_write_object(strm, ECL_CODE_CHAR(c)); return c; }

static void
seq_char_unread_char(cl_object strm, ecl_character c)
{ seq_unread_object(strm, ECL_CODE_CHAR(c)); }

/* -- Direct byte <- char ------------------------------------------- */
static cl_object
byte_char(cl_object byte)
{
  unlikely_if (byte != OBJNULL && !ECL_CHARACTERP(byte)) {
    FEwrong_type_argument(@[char], byte);
  }
  return byte == OBJNULL ? OBJNULL : cl_char_code(byte);
}

static cl_object
seq_char_read_byte(cl_object strm)
{ return byte_char(seq_read_object(strm)); }

static cl_object
seq_char_peek_byte(cl_object strm)
{ return byte_char(seq_peek_object(strm)); }

static void
seq_char_write_byte(cl_object strm, cl_object byte)
{ seq_write_object(strm, cl_code_char(byte)); }

static void
seq_char_unread_byte(cl_object strm, cl_object byte)
{ seq_unread_object(strm, cl_code_char(byte)); }

/* -- Direct char <- byte ------------------------------------------- */
static ecl_character
char_byte(cl_object byte)
{
  unlikely_if (byte != OBJNULL && Null(cl_integerp(byte))) {
    FEwrong_type_argument(@[byte], byte);
  }
  /* INV cl_code_char can return NIL for bytes outside of the char range.
     ecl_char_code will signal a condition then. */
  return byte == OBJNULL ? EOF : ecl_char_code(cl_code_char(byte));
}

static ecl_character
seq_byte_read_char(cl_object strm)
{ return char_byte(seq_read_object(strm)); }

static ecl_character
seq_byte_peek_char(cl_object strm)
{ return char_byte(seq_peek_object(strm)); }

static ecl_character
seq_byte_write_char(cl_object strm, ecl_character c)
{ seq_write_object(strm, cl_char_code(ECL_CODE_CHAR(c))); return c; }

static void
seq_byte_unread_char(cl_object strm, ecl_character c)
{ seq_unread_object(strm, cl_char_code(ECL_CODE_CHAR(c))); }

/**********************************************************************
 * SEQUENCE INPUT STREAMS
 */

/* Keep in mind that this function assumes that the element type is the octet
   (we'd need to factor the byte size to the change position and test limit),
   and that for :ucs-2 and :ucs-4 .byte_buffer is NULL. -- jd 2025-07-29 */
static cl_index
seq_in_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
  cl_fixnum curr_pos = SEQ_STREAM_POSITION(strm);
  cl_fixnum last = SEQ_INPUT_LIMIT(strm);
  cl_fixnum delta = last - curr_pos;
  if (delta > 0) {
    cl_object vector = SEQ_STREAM_VECTOR(strm);
    if (delta > n) delta = n;
    ecl_copy(c, vector->vector.self.bc + curr_pos, delta);
    SEQ_STREAM_POSITION(strm) += delta;
    return delta;
  }
  return 0;
}

static void
seq_in_unread_char(cl_object strm, ecl_character c)
{
  int flags = strm->stream.flags;
  ecl_eformat_unread_char(strm, c);
  if (c == ECL_CHAR_CODE_NEWLINE
      && (flags & ECL_STREAM_CR)
      && (flags & ECL_STREAM_LF))
    SEQ_STREAM_POSITION(strm) -= 2;
  else
    SEQ_STREAM_POSITION(strm) -= 1;
}

static void
seq_in_unread_byte(cl_object strm, cl_object byte)
{
  unlikely_if(SEQ_STREAM_POSITION(strm) <= 0) {
    ecl_unread_error(strm);
  }
  SEQ_STREAM_POSITION(strm) -= 1;
}

#ifdef ecl_uint16_t
static ecl_character
seq_in_ucs2_read_char(cl_object strm)
{
  cl_fixnum curr_pos = SEQ_STREAM_POSITION(strm);
  cl_fixnum last = SEQ_INPUT_LIMIT(strm);
  if (curr_pos >= last) {
    return EOF;
  }
  cl_object vector = SEQ_STREAM_VECTOR(strm);
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
  SEQ_STREAM_POSITION(strm) = curr_pos;
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
    SEQ_STREAM_POSITION(strm) -= 2;
  } else {
    SEQ_STREAM_POSITION(strm) -= 1;
  }
}
#endif

#ifdef ecl_uint32_t
static ecl_character
seq_in_ucs4_read_char(cl_object strm)
{
  cl_fixnum curr_pos = SEQ_STREAM_POSITION(strm);
  if (curr_pos >= SEQ_INPUT_LIMIT(strm)) {
    return EOF;
  }
  cl_object vector = SEQ_STREAM_VECTOR(strm);
  SEQ_STREAM_POSITION(strm) += 1;
  return vector->vector.self.b32[curr_pos];
}

static void
seq_in_ucs4_unread_char(cl_object strm, ecl_character c)
{
  SEQ_STREAM_POSITION(strm) -= 1;
}
#endif

static int
seq_in_listen(cl_object strm)
{
  if (SEQ_STREAM_POSITION(strm) < SEQ_INPUT_LIMIT(strm))
    return ECL_LISTEN_AVAILABLE;
  else
    return ECL_LISTEN_EOF;
}

static cl_object
seq_in_get_position(cl_object strm)
{
  return ecl_make_unsigned_integer(SEQ_STREAM_POSITION(strm));
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
  SEQ_STREAM_POSITION(strm) = disp;
  return ECL_T;
}

static cl_object
seq_file_element_type(cl_object strm)
{
  return IO_FILE_ELT_TYPE(strm);
}

const struct ecl_file_ops seq_in_ops = {
  seq_in_read_byte8,
  ecl_not_output_write_byte8,

  ecl_binary_read_byte,
  ecl_not_output_write_byte,
  seq_in_unread_byte,
  ecl_generic_peek_byte,

  ecl_eformat_read_char,
  ecl_not_output_write_char,
  seq_in_unread_char,
  ecl_generic_peek_char,

  ecl_generic_read_vector,
  ecl_generic_write_vector,

  seq_in_listen,
  ecl_generic_void, /* clear-input */
  ecl_not_output_clear_output,
  ecl_not_output_finish_output,
  ecl_not_output_force_output,

  ecl_generic_always_true, /* input_p */
  ecl_generic_always_false, /* output_p */
  ecl_generic_always_false,
  seq_file_element_type,

  ecl_not_a_file_stream, /* length */
  seq_in_get_position,
  seq_in_set_position,
  ecl_not_output_string_length,
  ecl_unknown_column,

  ecl_not_a_file_stream,
  ecl_not_a_file_stream,

  ecl_generic_close
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
  /* ecl_normalize_stream_element_type errors on illegal element type. */
  byte_size = ecl_normalize_stream_element_type(type_name);
  /* Character streams always get some external format. For binary
   * sequences it has to be explicitly mentioned. */
  strm = ecl_alloc_stream();
  strm->stream.ops = ecl_duplicate_dispatch_table(&seq_in_ops);
  strm->stream.mode = (short)ecl_smm_sequence_input;
  if (!byte_size && Null(external_format)) {
    external_format = @':default';
  }
  if (ecl_aet_size[type] == 1 && !Null(external_format)) {
    ecl_set_stream_elt_type(strm, byte_size, flags, external_format);
    /* Override byte size */
    if (byte_size) strm->stream.byte_size = 8;
  }
#ifdef ecl_uint16_t
  else if (ecl_aet_size[type] == 2 && external_format == @':ucs-2') {
    SEQ_STREAM_ELT_TYPE(strm) = @'character';
    strm->stream.format = @':ucs-2';
    strm->stream.byte_size = 2*8;
    /* decoding */
    strm->stream.ops->read_char = seq_in_ucs2_read_char;
    strm->stream.ops->unread_char = seq_in_ucs2_unread_char;
    /* identity */
    strm->stream.ops->read_byte = seq_byte_read_byte;
    strm->stream.ops->peek_byte = seq_byte_peek_byte;
    strm->stream.ops->unread_byte = seq_byte_unread_byte;
  }
#endif
#ifdef ecl_uint32_t
  else if (ecl_aet_size[type] == 4 && external_format == @':ucs-4') {
    SEQ_STREAM_ELT_TYPE(strm) = @'character';
    strm->stream.format = @':ucs-4';
    strm->stream.byte_size = 4*8;
    /* decoding */
    strm->stream.ops->read_char = seq_in_ucs4_read_char;
    strm->stream.ops->unread_char = seq_in_ucs4_unread_char;
    /* identity */
    strm->stream.ops->read_byte = seq_byte_read_byte;
    strm->stream.ops->peek_byte = seq_byte_peek_byte;
    strm->stream.ops->unread_byte = seq_byte_unread_byte;
  }
#endif
  else if(!byte_size && external_format == @':default') {
    /* char vector -> native bivalent stream */
    SEQ_STREAM_ELT_TYPE(strm) = @'character';
    /* identity */
    strm->stream.ops->read_char = seq_char_read_char;
    strm->stream.ops->peek_char = seq_char_peek_char;
    strm->stream.ops->unread_char = seq_char_unread_char;
    /* char-code */
    strm->stream.ops->read_byte = seq_char_read_byte;
    strm->stream.ops->peek_byte = seq_char_peek_byte;
    strm->stream.ops->unread_byte = seq_char_unread_byte;
  }
  else if(Null(external_format)) {
    /* byte vector -> native bivalent stream */
    ecl_set_stream_elt_type(strm, byte_size, flags, external_format);
    /* code-char */
    strm->stream.ops->read_char = seq_byte_read_char;
    strm->stream.ops->peek_char = seq_byte_peek_char;
    strm->stream.ops->unread_char = seq_byte_unread_char;
    /* identity */
    strm->stream.ops->read_byte = seq_byte_read_byte;
    strm->stream.ops->peek_byte = seq_byte_peek_byte;
    strm->stream.ops->unread_byte = seq_byte_unread_byte;
  }
  else {
    FEerror("Illegal combination of external-format ~A and input vector ~A "
            "for MAKE-SEQUENCE-INPUT-STREAM.~%", 2, external_format, vector);
  }
  SEQ_STREAM_VECTOR(strm) = vector;
  SEQ_STREAM_POSITION(strm) = istart;
  SEQ_INPUT_VECTOR_END(strm) = iend;
  return strm;
}

@(defun ext::make_sequence_input_stream (vector &key
                                         (start ecl_make_fixnum(0))
                                         (end ECL_NIL)
                                         (external_format ECL_NIL))
  cl_index_pair p;
  cl_object strm;
  @
  p = ecl_vector_start_end(@[ext::make-sequence-input-stream],
                           vector, start, end);
  strm = make_sequence_input_stream(vector, p.start, p.end,
                                    external_format);
  if (Null(end))
    strm->stream.flags |= ECL_STREAM_USE_VECTOR_FILLP;
  @(return strm)
  @)

/**********************************************************************
 * SEQUENCE OUTPUT STREAMS
 */

static void
seq_out_enlarge_vector(cl_object strm)
{
  cl_object vector = SEQ_STREAM_VECTOR(strm);
  si_adjust_vector(vector, ecl_ash(ecl_make_fixnum(vector->vector.dim), 1));
  SEQ_STREAM_VECTOR(strm) = vector;
}

/* Keep in mind that this function assumes that the element type is the octet
   (we'd need to factor the byte size to the change position and test limit),
   and that for :ucs-2 and :ucs-4 .byte_buffer is NULL. -- jd 2025-07-29 */
static cl_index
seq_out_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
 AGAIN:
  {
    cl_object vector = SEQ_STREAM_VECTOR(strm);
    cl_fixnum curr_pos = SEQ_STREAM_POSITION(strm);
    cl_fixnum last = vector->vector.dim;
    cl_fixnum delta = last - curr_pos;
    if (delta < n) {
      seq_out_enlarge_vector(strm);
      goto AGAIN;
    }
    ecl_copy(vector->vector.self.bc + curr_pos, c, n);
    SEQ_STREAM_POSITION(strm) = curr_pos += n;
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
    cl_object vector = SEQ_STREAM_VECTOR(strm);
    cl_fixnum curr_pos = SEQ_STREAM_POSITION(strm);
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
    SEQ_STREAM_POSITION(strm) = curr_pos;
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
    cl_object vector = SEQ_STREAM_VECTOR(strm);
    cl_fixnum curr_pos = SEQ_STREAM_POSITION(strm);
    if (vector->vector.dim - curr_pos < 1) {
      seq_out_enlarge_vector(strm);
      goto AGAIN;
    }
    vector->vector.self.b32[curr_pos++] = c;
    SEQ_STREAM_POSITION(strm) = curr_pos;
    if (vector->vector.fillp < curr_pos)
      vector->vector.fillp = curr_pos;
  }
  return c;
}
#endif

static cl_object
seq_out_get_position(cl_object strm)
{
  return ecl_make_unsigned_integer(SEQ_STREAM_POSITION(strm));
}

static cl_object
seq_out_set_position(cl_object strm, cl_object pos)
{
  cl_object vector = SEQ_STREAM_VECTOR(strm);
  cl_fixnum disp;
  if (Null(pos)) {
    disp = vector->vector.fillp;
  } else {
    disp = ecl_to_size(pos);
    if (disp >= vector->vector.dim) {
      disp = vector->vector.fillp;
    }
  }
  SEQ_STREAM_POSITION(strm) = disp;
  return ECL_T;
}

const struct ecl_file_ops seq_out_ops = {
  ecl_not_input_read_byte8,
  seq_out_write_byte8,

  ecl_not_input_read_byte,
  ecl_binary_write_byte,
  ecl_not_input_unread_byte,
  ecl_generic_peek_byte,

  ecl_not_input_read_char,
  ecl_eformat_write_char,
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
  seq_file_element_type,

  ecl_not_a_file_stream, /* length */
  seq_out_get_position,
  seq_out_set_position,
  ecl_not_output_string_length,
  ecl_generic_column,

  ecl_not_a_file_stream,
  ecl_not_a_file_stream,

  ecl_generic_close
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
  /* ecl_normalize_stream_element_type errors on illegal element type. */
  byte_size = ecl_normalize_stream_element_type(type_name);
  /* Character streams always get some external format. For binary
   * sequences it has to be explicitly mentioned. */
  strm = ecl_alloc_stream();
  strm->stream.ops = ecl_duplicate_dispatch_table(&seq_out_ops);
  strm->stream.mode = (short)ecl_smm_sequence_output;
  if (!byte_size && Null(external_format)) {
    external_format = @':default';
  }
  if (ecl_aet_size[type] == 1 && !Null(external_format)) {
    /* If elements of the stream are byte8, then we can convert them on fly. */
    ecl_set_stream_elt_type(strm, byte_size, flags, external_format);
    /* Override byte size */
    if (byte_size) strm->stream.byte_size = 8;
  }
#ifdef ecl_uint16_t
  else if (ecl_aet_size[type] == 2 && external_format == @':ucs-2') {
    SEQ_STREAM_ELT_TYPE(strm) = @'character';
    strm->stream.format = @':ucs-2';
    strm->stream.byte_size = 2*8;
    strm->stream.ops->write_char = seq_out_ucs2_write_char;
    strm->stream.ops->write_byte = seq_byte_write_byte;
  }
#endif
#ifdef ecl_uint32_t
  else if (ecl_aet_size[type] == 4 && external_format == @':ucs-4') {
    SEQ_STREAM_ELT_TYPE(strm) = @'character';
    strm->stream.format = @':ucs-4';
    strm->stream.byte_size = 4*8;
    strm->stream.ops->write_char = seq_out_ucs4_write_char;
    strm->stream.ops->write_byte = seq_byte_write_byte;
  }
#endif
  else if(!byte_size && external_format == @':default') {
    /* char vector -> native bivalent stream */
    SEQ_STREAM_ELT_TYPE(strm) = @'character';
    strm->stream.ops->write_char = seq_char_write_char;
    strm->stream.ops->write_byte = seq_char_write_byte;
  }
  else if(Null(external_format)) {
    /* byte vector -> native bivalent stream */
    ecl_set_stream_elt_type(strm, byte_size, flags, external_format);
    strm->stream.ops->write_char = seq_byte_write_char;
    strm->stream.ops->write_byte = seq_byte_write_byte;
  }
  else {
    FEerror("Illegal combination of external-format ~A and output vector ~A "
            "for MAKE-SEQUENCE-OUTPUT-STREAM.~%", 2, external_format, vector);
  }
  SEQ_STREAM_VECTOR(strm) = vector;
  SEQ_STREAM_POSITION(strm) = vector->vector.fillp;
  return strm;
}

@(defun ext::make_sequence_output_stream (vector &key (external_format ECL_NIL))
@
  @(return make_sequence_output_stream(vector, external_format));
@)

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
  ops = ecl_stream_dispatch_table(stream);
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
          ops->write_byte(stream, elt);
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
  ops = ecl_stream_dispatch_table(stream);
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
          if (c == OBJNULL) goto OUTPUT;
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
