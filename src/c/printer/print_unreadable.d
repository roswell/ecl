/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * print_unreadable.d - helper for print-unreadable-object macro
 *
 * Copyright (c) 2010 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/internal.h>

void
_ecl_write_addr(void *x, cl_object stream)
{
  cl_fixnum i, j;
  int print_zeros = 0;

  i = (cl_index)x;

  if (i == 0) {
    writestr_stream("0x0", stream);
    return;
  }
  writestr_stream("0x", stream);
  cl_object buffer = si_get_buffer_string();
  cl_index buffer_size = ecl_fixnum(cl_array_total_size(buffer));
  cl_index buffer_ndx = 0;
  for (j = sizeof(i)*8-4;  j >= 0;  j -= 4) {
    int k = (i>>j) & 0xf;
    if (!print_zeros && k == 0) {
      ;
    } else {
      if (k < 10) {
        print_zeros = 1;
        ecl_char_set(buffer, buffer_ndx++, '0' + k);
      } else {
        print_zeros = 1;
        ecl_char_set(buffer, buffer_ndx++, 'a' + k - 10);
      }
      if (buffer_ndx >= buffer_size) {
        si_fill_pointer_set(buffer, ecl_make_fixnum(buffer_size));
        cl_write_string(2, buffer, stream);
        si_fill_pointer_set(buffer, ecl_make_fixnum(0));
      }
    }
  }
  si_fill_pointer_set(buffer, ecl_make_fixnum(buffer_ndx));
  cl_write_string(2, buffer, stream);
  si_put_buffer_string(buffer);
}

void
_ecl_write_unreadable(cl_object x, const char *prefix, cl_object name, cl_object stream)
{
  if (ecl_print_readably()) {
    FEprint_not_readable(x);
  }
  ecl_write_char('#', stream);
  ecl_write_char('<', stream);
  writestr_stream(prefix, stream);
  ecl_write_char(' ', stream);
  if (!Null(name)) {
    si_write_ugly_object(name, stream);
    ecl_write_char(' ', stream);
  }
  _ecl_write_addr((void *)x, stream);
  ecl_write_char('>', stream);
}

cl_object
si_print_unreadable_object_function(cl_object o, cl_object stream, cl_object type, cl_object id, cl_object function)
{
  if (ecl_print_readably())
    FEprint_not_readable(o);
  stream = _ecl_stream_or_default_output(stream);
  if (ecl_print_level() == 0) {
    ecl_write_char('#', stream);
  } else {
    writestr_stream("#<", stream);
    if (!Null(type)) {
      cl_index i, l;
      type = cl_type_of(o);
      if (!ECL_SYMBOLP(type)) {
        type = @'standard-object';
      }
      type = type->symbol.name;
      for (i = 0, l = ecl_length(type); i < l; i++)
        ecl_write_char(ecl_char_downcase(ecl_char(type, i)), stream);
      ecl_write_char(' ', stream);
    }
    if (!Null(function)) {
      _ecl_funcall1(function);
    }
    if (!Null(id)) {
      ecl_write_char(' ', stream);
      _ecl_write_addr((void *)o, stream);
    }
    ecl_write_char('>', stream);
  }
  @(return ECL_NIL);
}
