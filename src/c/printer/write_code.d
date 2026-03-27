/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * write_list.d - ugly printer for bytecodes and functions
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/internal.h>
#include <ecl/bytecodes.h>

void
_ecl_write_bytecodes_readably(cl_object x, cl_object stream, cl_object lex)
{
  cl_index i;
  cl_object code_l = ECL_NIL;
  for ( i=x->bytecodes.code_size-1 ; i<(cl_index)(-1l) ; i-- )
    code_l = ecl_cons(ecl_make_fixnum(((cl_opcode*)(x->bytecodes.code))[i]), code_l);
  writestr_stream("#Y", stream);
  /* We don't write the definition because is not guaranteed to be readable. */
  si_write_ugly_object(cl_list(7, x->bytecodes.name, lex,
                               ECL_NIL /* x->bytecodes.definition */,
                               code_l,
                               x->bytecodes.data,
                               x->bytecodes.flex,
                               x->bytecodes.nlcl,
                               x->bytecodes.file,
                               x->bytecodes.file_position),
                       stream);
}

void
_ecl_write_bytecodes(cl_object x, cl_object stream)
{
  if (ecl_print_readably()) {
    _ecl_write_bytecodes_readably(x, stream, ECL_NIL);
  } else {
    _ecl_write_unreadable(x, "bytecompiled-function", x->bytecodes.name, stream);
  }
}

void
_ecl_write_bclosure(cl_object x, cl_object stream)
{
  if (ecl_print_readably()) {
    _ecl_write_bytecodes_readably(x->bclosure.code, stream, x->bclosure.lex);
  } else {
    cl_object name = x->bclosure.code->bytecodes.name;
    writestr_stream("#<bytecompiled-closure ", stream);
    if (name != ECL_NIL) {
      si_write_ugly_object(name, stream);
      writestr_stream(" ", stream);
    }
    _ecl_write_addr((void *)x, stream);
    ecl_write_char('>', stream);
  }
}
