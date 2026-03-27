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

/* -- imports --------------------------------------------------------------- */

#include <ecl/ecl.h>
#include <ecl/internal.h>

/* -- File opening and closing ---------------------------------------------- */

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

@(defun close (strm &key (abort ECL_NIL))
@
  @(return ecl_stream_dispatch_table(strm)->close(strm));
@)
