/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

#include <ecl/ecl.h>
#include <ctype.h>
#include <limits.h>
#include <ecl/internal.h>

#define CL_PACKAGE 0
#define SI_PACKAGE 4
#define EXT_PACKAGE 64
#define GRAY_PACKAGE 32
#define FFI_PACKAGE 128
#define KEYWORD_PACKAGE 8
#define MP_PACKAGE 12
#define CLOS_PACKAGE 16
#define ORDINARY_SYMBOL 0
#define CONSTANT_SYMBOL 1
#define SPECIAL_SYMBOL 2
#define FORM_SYMBOL 3
#define PRIVATE 256

#define CL_ORDINARY     CL_PACKAGE | ORDINARY_SYMBOL
#define CL_SPECIAL      CL_PACKAGE | SPECIAL_SYMBOL
#define CL_CONSTANT     CL_PACKAGE | CONSTANT_SYMBOL
#define CL_FORM         CL_PACKAGE | ORDINARY_SYMBOL | FORM_SYMBOL
#define SI_ORDINARY     SI_PACKAGE | ORDINARY_SYMBOL
#define SI_SPECIAL      SI_PACKAGE | SPECIAL_SYMBOL
#define SI_CONSTANT     SI_PACKAGE | CONSTANT_SYMBOL
#define EXT_ORDINARY    EXT_PACKAGE | ORDINARY_SYMBOL
#define EXT_SPECIAL     EXT_PACKAGE | SPECIAL_SYMBOL
#define EXT_CONSTANT    EXT_PACKAGE | CONSTANT_SYMBOL
#define EXT_FORM        EXT_PACKAGE | ORDINARY_SYMBOL | FORM_SYMBOL
#define MP_ORDINARY     MP_PACKAGE | ORDINARY_SYMBOL
#define MP_SPECIAL      MP_PACKAGE | SPECIAL_SYMBOL
#define MP_CONSTANT     MP_PACKAGE | CONSTANT_SYMBOL
#define CLOS_ORDINARY   CLOS_PACKAGE | ORDINARY_SYMBOL
#define CLOS_SPECIAL    CLOS_PACKAGE | SPECIAL_SYMBOL
#define KEYWORD         KEYWORD_PACKAGE | CONSTANT_SYMBOL
#define GRAY_ORDINARY   GRAY_PACKAGE | ORDINARY_SYMBOL
#define FFI_ORDINARY    FFI_PACKAGE | ORDINARY_SYMBOL
#define FFI_CONSTANT    FFI_PACKAGE | CONSTANT_SYMBOL

#include "symbols_list.h"

cl_index cl_num_symbols_in_core = 0;

static unsigned char *
mangle_name(cl_object output, unsigned char *source, int l)
{
  unsigned char c;

  while (l--) {
    c = *(source++);
    if (ecl_alphanumericp(c)) {
      c = ecl_char_downcase(c);
    } else if (c == '-' || c == '_') {
      c = '_';
    } else if (c == '&') {
      c = 'A';
    } else if (c == '*') {
      c = 'X';
    } else if (c == '+') {
      c = 'P';
    } else if (c == '<') {
      c = 'L';
    } else if (c == '>') {
      c = 'G';
    } else if (c == '=') {
      c = 'E';
    } else if (c == '/') {
      c = 'N';
    } else if (c == ':') {
      c = 'X';
    } else {
      return NULL;
    }
    output->base_string.self[output->base_string.fillp++] = c;
  }
  return &output->base_string.self[output->base_string.fillp];
}

@(defun si::mangle-name (symbol &optional as_function)
  cl_index l;
  unsigned char c, *source, *dest;
  cl_object output;
  cl_object package;
  cl_object found = ECL_NIL;
  cl_object maxarg = ecl_make_fixnum(ECL_CALL_ARGUMENTS_LIMIT);
  cl_object minarg = ecl_make_fixnum(0);
  bool is_symbol;
  cl_object name;
@
  name = ecl_symbol_name(symbol);
  is_symbol = Null(as_function);
  if (is_symbol) {
    cl_fixnum p;
    if (symbol == ECL_NIL) {
      @(return ECL_T @"ECL_NIL" minarg maxarg);
    }
    else if (symbol == ECL_T) {
      @(return ECL_T @"ECL_T" minarg maxarg);
    }

    p  = (cl_symbol_initializer*)symbol - cl_symbols;
    if (p >= 0 && p <= cl_num_symbols_in_core) {
      found = ECL_T;
      output = cl_format(4, ECL_NIL,
                         @"ECL_SYM(~S,~D)",
                         name, ecl_make_fixnum(p));
#ifndef ECL_FINAL
      /* XXX to allow the Lisp compiler to check that the narg
       * declaration in symbols_list.h matches the actual function
       * definition, return the previously saved narg here. -- mg
       * 2019-12-02 */
      cl_object plist = cl_symbol_plist(symbol);
      for ( ; ECL_CONSP(plist); plist = ECL_CONS_CDR(plist)) {
        if (ECL_CONS_CAR(plist) == @'call-arguments-limit') {
          plist = ECL_CONS_CDR(plist);
          if (ECL_CONSP(plist) && ECL_FIXNUMP(ECL_CONS_CAR(plist))) {
            cl_fixnum narg = ecl_fixnum(ECL_CONS_CAR(plist));
            if (narg >= 0) {
              minarg = maxarg = ecl_make_fixnum(narg);
            } else {
              minarg = ecl_make_fixnum(-narg-1);
            }
          }
          break;
        }
      }
#endif
      @(return found output minarg maxarg);
    }
  } else if (!Null(symbol)) {
    cl_object fun = symbol->symbol.gfdef;
    cl_type t = (fun == OBJNULL)? t_other : ecl_t_of(fun);
    if ((t == t_cfun || t == t_cfunfixed) && fun->cfun.block == OBJNULL) {
      for (l = 0; l <= cl_num_symbols_in_core; l++) {
        cl_object s = (cl_object)(cl_symbols + l);
        if (fun == ECL_SYM_FUN(s)) {
          symbol = s;
          found = ECL_T;
          if (fun->cfun.narg >= 0) {
            if (t == t_cfunfixed) {
              minarg =
                maxarg = ecl_make_fixnum(fun->cfunfixed.narg);
            } else {
              minarg = ecl_make_fixnum(fun->cfun.narg);
            }
          }
          break;
        }
      }
    }
  }
  if (!Null(symbol->symbol.cname)) {
    @(return found symbol->symbol.cname minarg maxarg);
  }
  package = ecl_symbol_package(symbol);
  if (Null(package)) {
    ;
  }
  else if (package == cl_core.lisp_package)
    package = @"cl";
  else if (package == cl_core.system_package)
    package = @"si";
  else if (package == cl_core.ext_package)
    package = @"si";
  else if (package == cl_core.keyword_package)
    package = ECL_NIL;
  else
    package = package->pack.name;
  symbol = ecl_symbol_name(symbol);
  l      = symbol->base_string.fillp;
  source = symbol->base_string.self;
  output = ecl_alloc_simple_base_string(ecl_length(package) + l + 1);
  if (is_symbol && source[0] == '*') {
    if (l > 2 && source[l-1] == '*') l--;
    c = 'V';
    l--;
    source++;
  } else if (is_symbol && l > 2 && source[0] == '+' && source[l-1] == '+') {
    c = 'C';
    l-= 2;
    source++;
  } else if (!is_symbol) {
    c = '_';
  } else if (package == cl_core.keyword_package) {
    c = 'K';
  } else {
    c = 'S';
  }
  output->base_string.fillp = 0;
  if (!Null(package)) {
    if (!mangle_name(output, package->base_string.self, package->base_string.fillp)) {
      @(return ECL_NIL ECL_NIL minarg maxarg);
    }
  }
  output->base_string.self[output->base_string.fillp++] = c;
  if (!(dest = mangle_name(output, source, l))) {
    @(return ECL_NIL ECL_NIL minarg maxarg);
  }
  if (dest[-1] == '_')
    dest[-1] = 'M';
  *(dest++) = '\0';
  @(return found output minarg maxarg);
@)

static void
make_this_symbol(int i, cl_object s, int code,
                 const char *name, const char *cname,
                 cl_objectfn fun, int narg, cl_object value)
{
  enum ecl_stype stp;
  cl_object package;
  bool form = 0;

  switch (code & 3) {
  case ORDINARY_SYMBOL: stp = ecl_stp_ordinary; break;
  case SPECIAL_SYMBOL: stp = ecl_stp_special; break;
  case CONSTANT_SYMBOL: stp = ecl_stp_constant; break;
  case FORM_SYMBOL: form = 1; stp = ecl_stp_ordinary;
  }
  switch (code & 0xfc) {
  case CL_PACKAGE: package = cl_core.lisp_package; break;
  case SI_PACKAGE: package = cl_core.system_package; break;
  case EXT_PACKAGE: package = cl_core.ext_package; break;
  case KEYWORD_PACKAGE: package = cl_core.keyword_package; break;
  case MP_PACKAGE: package = cl_core.mp_package; break;
  case CLOS_PACKAGE: package = cl_core.clos_package; break;
#ifdef ECL_CLOS_STREAMS
  case GRAY_PACKAGE: package = cl_core.gray_package; break;
#endif
  case FFI_PACKAGE: package = cl_core.ffi_package; break;
  default: printf("%d\n", code & ~(int)3); ecl_internal_error("Unknown package code in init_all_symbols()");
  }
  s->symbol.t = t_symbol;
  s->symbol.dynamic = 0;
#ifdef ECL_THREADS
  s->symbol.binding = ECL_MISSING_SPECIAL_BINDING;
#endif
  ECL_SET(s, OBJNULL);
  ECL_SYM_FUN(s) = ECL_NIL;
  s->symbol.plist = ECL_NIL;
  s->symbol.hpack = ECL_NIL;
  s->symbol.stype = stp;
  s->symbol.hpack = package;
  s->symbol.name = ecl_make_constant_base_string(name,-1);
  s->symbol.cname = ecl_cstring_to_base_string_or_nil(cname);
  if (package == cl_core.keyword_package) {
    package->pack.external =
      _ecl_sethash(s->symbol.name, package->pack.external, s);
    ECL_SET(s, s);
  } else {
    int intern_flag;
    ECL_SET(s, value);
    if (ecl_find_symbol(s->symbol.name, package, &intern_flag) != ECL_NIL
        && intern_flag == ECL_INHERITED) {
      ecl_shadowing_import(s, package);
    } else {
      cl_import2(s, package);
    }
    if (!(code & PRIVATE)) {
      cl_export2(s, package);
      if (package == cl_core.ext_package)
        cl_export2(s, cl_core.system_package);
    }
  }
  if (form) {
    s->symbol.stype |= ecl_stp_special_form;
  } else if (fun) {
    cl_object f;
    if (narg >= 0) {
      f = ecl_make_cfun((cl_objectfn_fixed)fun, s, NULL, narg);
    } else {
      f = ecl_make_cfun_va(fun, s, NULL, -narg - 1);
    }
    ECL_SYM_FUN(s) = f;
  }
#ifndef ECL_FINAL
  /* XXX to allow the Lisp compiler to check that the narg declaration
   * in symbols_list.h matches the actual function definition, we save
   * narg here. -- mg 2019-12-02 */
  si_set_symbol_plist(s, cl_list(2, @'call-arguments-limit', ecl_make_fixnum(narg)));
#endif
  cl_num_symbols_in_core = i + 1;
}

void
init_all_symbols(void)
{
  int i, code, narg;
  const char *name, *cname;
  cl_object s, value;
  cl_objectfn fun;

  /* We skip NIL and T */
  for (i = 2; cl_symbols[i].init.name != NULL; i++) {
    s = (cl_object)(cl_symbols + i);
    code = cl_symbols[i].init.type;
    name = cl_symbols[i].init.name;
    fun = (cl_objectfn)cl_symbols[i].init.fun;
    narg = cl_symbols[i].init.narg;
    value = cl_symbols[i].init.value;
    cname = cl_symbols[i].init.translation;
    make_this_symbol(i, s, code, name, cname, fun, narg, value);
  }
}
