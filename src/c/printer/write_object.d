/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * write_object.d - basic printer routine
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

bool
_ecl_will_print_as_hash(cl_object x)
{
  cl_object circle_counter = ecl_symbol_value(@'si::*circle-counter*');
  cl_object circle_stack = ecl_symbol_value(@'si::*circle-stack*');
  cl_object code = ecl_gethash_safe(x, circle_stack, OBJNULL);
  if (ECL_FIXNUMP(circle_counter)) {
    return !(code == OBJNULL || code == ECL_NIL);
  } else if (code == OBJNULL) {
    /* Was not found before */
    _ecl_sethash(x, circle_stack, ECL_NIL);
    return 0;
  } else {
    return 1;
  }
}

/* To print circular structures, we traverse the structure by adding
   a pair <element, flag> to the interpreter stack for each element visited.
   flag is initially NIL and becomes T if the element is visited again.
   After the visit we squeeze out all the non circular elements.
   The flags is used during printing to distinguish between the first visit
   to the element.
*/

cl_object
si_search_print_circle(cl_object x)
{
  cl_object circle_counter = ecl_symbol_value(@'si::*circle-counter*');
  cl_object circle_stack = ecl_symbol_value(@'si::*circle-stack*');
  cl_object code;

  code = ecl_gethash_safe(x, circle_stack, OBJNULL);
  if (!ECL_FIXNUMP(circle_counter)) {
    if (code == OBJNULL) {
      /* Was not found before */
      _ecl_sethash(x, circle_stack, ECL_NIL);
      return ecl_make_fixnum(0);
    } else if (code == ECL_NIL) {
      /* This object is referenced twice */
      _ecl_sethash(x, circle_stack, ECL_T);
      return ecl_make_fixnum(1);
    } else {
      return ecl_make_fixnum(2);
    }
  } else {
    if (code == OBJNULL || code == ECL_NIL) {
      /* Is not referenced or was not found before */
      return ecl_make_fixnum(0);
    } else if (code == ECL_T) {
      /* This object is referenced twice, but has no code yet */
      circle_counter = ecl_make_fixnum(ecl_fixnum(circle_counter) + 1);
      _ecl_sethash(x, circle_stack, circle_counter);
      ECL_SETQ(ecl_process_env(), @'si::*circle-counter*',
               circle_counter);
      return ecl_make_fixnum(-ecl_fixnum(circle_counter));
    } else {
      return code;
    }
  }
}

cl_object
si_write_object_with_circle(cl_object x, cl_object stream, cl_object print_function)
{
  bool circle = ecl_print_circle();
  if (circle && !Null(x) && !ECL_FIXNUMP(x) && !ECL_CHARACTERP(x) &&
      (ecl_t_of(x) != t_symbol || (Null(x->symbol.hpack))))
    {
      /* everything except fixnums, characters or interned symbols can
         possibly contain cycles */
      cl_object circle_counter;
      cl_fixnum code;
      circle_counter = ecl_symbol_value(@'si::*circle-counter*');
      if (circle_counter == ECL_NIL) {
        cl_env_ptr env = ecl_process_env();
        cl_object hash =
          cl__make_hash_table(@'eq',
                              ecl_make_fixnum(1024),
                              ecl_ct_default_rehash_size,
                              ecl_ct_default_rehash_threshold);
        ecl_bds_bind(env, @'si::*circle-counter*', ECL_T);
        ecl_bds_bind(env, @'si::*circle-stack*', hash);
        si_write_object_with_circle(x, cl_core.null_stream, print_function);
        ECL_SETQ(env, @'si::*circle-counter*', ecl_make_fixnum(0));
        si_write_object_with_circle(x, stream, print_function);
        cl_clrhash(hash);
        ecl_bds_unwind_n(env, 2);
        goto OUTPUT;
      }
      code = ecl_fixnum(si_search_print_circle(x));
      if (!ECL_FIXNUMP(circle_counter)) {
        /* We are only inspecting the object to be printed. */
        /* Only run X if it was not referenced before */
        if (code != 0)
          goto OUTPUT;
      } else if (code == 0) {
        /* Object is not referenced twice */
      } else if (code < 0) {
        /* Object is referenced twice. We print its definition */
        ecl_write_char('#', stream);
        _ecl_write_fixnum(-code, stream);
        ecl_write_char('=', stream);
      } else {
        /* Second reference to the object */
        ecl_write_char('#', stream);
        _ecl_write_fixnum(code, stream);
        ecl_write_char('#', stream);
        goto OUTPUT;
      }
    }
  return _ecl_funcall3(print_function, x, stream);
 OUTPUT:
  @(return x);

}

cl_object
si_write_object(cl_object x, cl_object stream)
{
#ifdef ECL_CMU_FORMAT
  if (ecl_symbol_value(@'*print-pretty*') != ECL_NIL) {
    cl_object f = _ecl_funcall2(@'pprint-dispatch', x);
    if (VALUES(1) != ECL_NIL) {
      _ecl_funcall3(f, stream, x);
      @(return x);
    }
  }
#endif /* ECL_CMU_FORMAT */
  return si_write_object_with_circle(x, stream, @'si::write-ugly-object');
}
