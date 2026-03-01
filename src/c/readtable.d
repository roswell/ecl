/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * readtable.d - readtable implementation
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#include <ecl/number.h>
#include <assert.h>  /* for assert() */
#include <stdio.h>
#include <limits.h>
#include <float.h>
#include <string.h>
#include <stdlib.h>
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>
#include <ecl/bytecodes.h>

static void ECL_INLINE
assert_type_readtable(cl_object function, cl_narg narg, cl_object p)
{
  unlikely_if (!ECL_READTABLEP(p)) {
    FEwrong_type_nth_arg(function, narg, p, @[readtable]);
  }
}

cl_object
ecl_copy_readtable(cl_object from, cl_object to)
{
  struct ecl_readtable_entry *from_rtab, *to_rtab;
  cl_index i;
  size_t entry_bytes = sizeof(struct ecl_readtable_entry);
  size_t total_bytes = entry_bytes * RTABSIZE;
  cl_object output;

  assert_type_readtable(@[copy-readtable], 1, from);
  /* For the sake of garbage collector and thread safety we
   * create an incomplete object and only copy to the destination
   * at the end in a more or less "atomic" (meaning "fast") way.
   */
  output = ecl_alloc_object(t_readtable);
  output->readtable.locked = 0;
  output->readtable.table = to_rtab = (struct ecl_readtable_entry *)
    ecl_alloc_align(total_bytes, entry_bytes);
  from_rtab = from->readtable.table;
  memcpy(to_rtab, from_rtab, total_bytes);
  for (i = 0;  i < RTABSIZE;  i++) {
    cl_object d = from_rtab[i].dispatch;
    if (ECL_HASH_TABLE_P(d)) {
      d = si_copy_hash_table(d);
    }
    to_rtab[i].dispatch = d;
  }
  output->readtable.read_case = from->readtable.read_case;
#ifdef ECL_UNICODE
  if (!Null(from->readtable.hash)) {
    output->readtable.hash = si_copy_hash_table(from->readtable.hash);
  } else {
    output->readtable.hash = ECL_NIL;
  }
#endif
  if (!Null(to)) {
    assert_type_readtable(@[copy-readtable], 2, to);
    to->readtable = output->readtable;
    output = to;
  }
  return output;
}

cl_object
ecl_current_readtable(void)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object r;

  /* INV: *readtable* always has a value */
  r = ECL_SYM_VAL(the_env, @'*readtable*');
  unlikely_if (!ECL_READTABLEP(r)) {
    ECL_SETQ(the_env, @'*readtable*', cl_core.standard_readtable);
    FEerror("The value of *READTABLE*, ~S, was not a readtable.", 1, r);
  }
  return r;
}

@(defun copy_readtable (&o (from ecl_current_readtable()) to)
  @
  if (Null(from)) {
    to = ecl_copy_readtable(cl_core.standard_readtable, to);
  } else {
    to = ecl_copy_readtable(from, to);
  }
  @(return to);
  @)

cl_object
cl_readtable_case(cl_object r)
{
  assert_type_readtable(@[readtable-case], 1, r);
  switch (r->readtable.read_case) {
  case ecl_case_upcase: r = @':upcase'; break;
  case ecl_case_downcase: r = @':downcase'; break;
  case ecl_case_invert: r = @':invert'; break;
  case ecl_case_preserve: r = @':preserve';
  }
  @(return r);
}

static void
error_locked_readtable(cl_object r)
{
  cl_error(2, @"Cannot modify locked readtable ~A.", r);
}

cl_object
si_readtable_case_set(cl_object r, cl_object mode)
{
  assert_type_readtable(@[readtable-case], 1, r);
  if (r->readtable.locked) {
    error_locked_readtable(r);
  }
  if (mode == @':upcase') {
    r->readtable.read_case = ecl_case_upcase;
  } else if (mode == @':downcase') {
    r->readtable.read_case = ecl_case_downcase;
  } else if (mode == @':preserve') {
    r->readtable.read_case = ecl_case_preserve;
  } else if (mode == @':invert') {
    r->readtable.read_case = ecl_case_invert;
  } else {
    const char *type = "(member :upcase :downcase :preserve :invert)";
    FEwrong_type_nth_arg(@[si::readtable-case-set], 2,
                         mode, ecl_read_from_cstring(type));
  }
  @(return mode);
}

cl_object
cl_readtablep(cl_object readtable)
{
  @(return (ECL_READTABLEP(readtable) ? ECL_T : ECL_NIL));
}

int
ecl_readtable_get(cl_object readtable, int c, cl_object *macro_or_table)
{
  cl_object m;
  enum ecl_chattrib cat;
#ifdef ECL_UNICODE
  if (c >= RTABSIZE) {
    cl_object hash = readtable->readtable.hash;
    cat = cat_constituent;
    m = ECL_NIL;
    if (!Null(hash)) {
      cl_object pair = ecl_gethash_safe(ECL_CODE_CHAR(c), hash, ECL_NIL);
      if (!Null(pair)) {
        cat = ecl_fixnum(ECL_CONS_CAR(pair));
        m = ECL_CONS_CDR(pair);
      }
    }
  } else
#endif
    {
      m = readtable->readtable.table[c].dispatch;
      cat = readtable->readtable.table[c].syntax_type;
    }
  if (macro_or_table) *macro_or_table = m;
  return cat;
}

void
ecl_readtable_set(cl_object readtable, int c, enum ecl_chattrib cat,
                  cl_object macro_or_table)
{
  if (readtable->readtable.locked) {
    error_locked_readtable(readtable);
  }
#ifdef ECL_UNICODE
  if (c >= RTABSIZE) {
    cl_object hash = readtable->readtable.hash;
    if (Null(hash)) {
      hash = cl__make_hash_table(@'eql', ecl_make_fixnum(128),
                                 ecl_ct_default_rehash_size,
                                 ecl_ct_default_rehash_threshold);
      readtable->readtable.hash = hash;
    }
    _ecl_sethash(ECL_CODE_CHAR(c), hash,
                 CONS(ecl_make_fixnum(cat), macro_or_table));
  } else
#endif
    {
      readtable->readtable.table[c].dispatch = macro_or_table;
      readtable->readtable.table[c].syntax_type = cat;
    }
}

/* FIXME unicode defines a range of "safe" characters, so that there are no
   misleading pseudo-spaces in symbols and such. Investigate that. */
bool
ecl_invalid_character_p(int c)
{
  return (c <= 32) || (c == 127);
}

@(defun set_syntax_from_char (tochr fromchr
                              &o (tordtbl ecl_current_readtable())
                              fromrdtbl)
  enum ecl_chattrib cat;
  cl_object dispatch;
  cl_fixnum fc, tc;
  @
  if (tordtbl->readtable.locked) {
    error_locked_readtable(tordtbl);
  }
  if (Null(fromrdtbl))
    fromrdtbl = cl_core.standard_readtable;
  assert_type_readtable(@[readtable-case], 1, tordtbl);
  assert_type_readtable(@[readtable-case], 2, fromrdtbl);
  fc = ecl_char_code(fromchr);
  tc = ecl_char_code(tochr);

  cat = ecl_readtable_get(fromrdtbl, fc, &dispatch);
  if (ECL_HASH_TABLE_P(dispatch)) {
    dispatch = si_copy_hash_table(dispatch);
  }
  ecl_readtable_set(tordtbl, tc, cat, dispatch);
  @(return ECL_T);
  @)

@(defun set_macro_character (c function &optional non_terminating_p
                             (readtable ecl_current_readtable()))
  @
  ecl_readtable_set(readtable, ecl_char_code(c),
                    Null(non_terminating_p)?
                    cat_terminating :
                    cat_non_terminating,
                    function);
  @(return ECL_T);
  @)

@(defun get_macro_character (c &optional (readtable ecl_current_readtable()))
  enum ecl_chattrib cat;
  cl_object dispatch;
  @
  if (Null(readtable))
  readtable = cl_core.standard_readtable;
  cat = ecl_readtable_get(readtable, ecl_char_code(c), &dispatch);
  if (ECL_HASH_TABLE_P(dispatch))
    dispatch = cl_core.dispatch_reader;
  @(return dispatch ((cat == cat_non_terminating)? ECL_T : ECL_NIL));
  @)

@(defun make_dispatch_macro_character (chr
                                       &optional non_terminating_p (readtable ecl_current_readtable()))
  enum ecl_chattrib cat;
  cl_object table;
  int c;
  @
  assert_type_readtable(@[make-dispatch-macro-character], 3, readtable);
  c = ecl_char_code(chr);
  cat = Null(non_terminating_p)? cat_terminating : cat_non_terminating;
  table = cl__make_hash_table(@'eql', ecl_make_fixnum(128),
                              ecl_ct_default_rehash_size,
                              ecl_ct_default_rehash_threshold);
  ecl_readtable_set(readtable, c, cat, table);
  @(return ECL_T);
  @)

@(defun set_dispatch_macro_character (dspchr subchr fnc
                                      &optional (readtable ecl_current_readtable()))
  cl_object table;
  cl_fixnum subcode;
  @
  assert_type_readtable(@[set-dispatch-macro-character], 4, readtable);
  ecl_readtable_get(readtable, ecl_char_code(dspchr), &table);
  unlikely_if (readtable->readtable.locked) {
    error_locked_readtable(readtable);
  }
  unlikely_if (!ECL_HASH_TABLE_P(table)) {
    FEerror("~S is not a dispatch character.", 1, dspchr);
  }
  subcode = ecl_char_code(subchr);
  if (Null(fnc)) {
    ecl_remhash(ECL_CODE_CHAR(subcode), table);
  } else {
    _ecl_sethash(ECL_CODE_CHAR(subcode), table, fnc);
  }
  if (ecl_lower_case_p(subcode)) {
    subcode = ecl_char_upcase(subcode);
  } else if (ecl_upper_case_p(subcode)) {
    subcode = ecl_char_downcase(subcode);
  }
  if (Null(fnc)) {
    ecl_remhash(ECL_CODE_CHAR(subcode), table);
  } else {
    _ecl_sethash(ECL_CODE_CHAR(subcode), table, fnc);
  }
  @(return ECL_T);
  @)

@(defun get_dispatch_macro_character (dspchr subchr
                                      &optional (readtable ecl_current_readtable()))
  cl_object table;
  cl_fixnum c;
  @
  if (Null(readtable)) {
    readtable = cl_core.standard_readtable;
  }
  assert_type_readtable(@[get-dispatch-macro-character], 3, readtable);
  c = ecl_char_code(dspchr);
  ecl_readtable_get(readtable, c, &table);
  unlikely_if (!ECL_HASH_TABLE_P(table)) {
    FEerror("~S is not a dispatch character.", 1, dspchr);
  }
  c = ecl_char_code(subchr);

  /* Since macro characters may take a number as argument, it is
     not allowed to turn digits into dispatch macro characters */
  if (ecl_digitp(c, 10) >= 0)
    @(return ECL_NIL);
  @(return ecl_gethash_safe(subchr, table, ECL_NIL));
  @)

cl_object
si_standard_readtable()
{
  @(return cl_core.standard_readtable);
}

@(defun ext::readtable-lock (r &optional yesno)
  cl_object output;
  @
  assert_type_readtable(@[ext::readtable-lock], 1, r);
  output = (r->readtable.locked)? ECL_T : ECL_NIL;
  if (narg > 1) {
    r->readtable.locked = !Null(yesno);
  }
  @(return output);
  @)
