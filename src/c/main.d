/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * main.d - ecl boot proccess
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/******************************** IMPORTS *****************************/

#include <ecl/ecl.h>
#ifdef ECL_USE_MPROTECT
# include <sys/mman.h>
# ifndef MAP_FAILED
#  define MAP_FAILED -1
# endif
#endif
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ecl/cache.h>
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>


#include "ecl_features.h"
#include "iso_latin_names.h"

/******************************* EXPORTS ******************************/

#if !defined(ECL_THREADS)
cl_env_ptr cl_env_p = NULL;
#endif
const char *ecl_self;

/************************ GLOBAL INITIALIZATION ***********************/

static int ARGC;
static char **ARGV;

static void
init_env_mp(cl_env_ptr env)
{
#if defined(ECL_THREADS)
  env->cleanup = 0;
#else
  env->own_process = ECL_NIL;
#endif
}

static void
init_env_int(cl_env_ptr env)
{
  env->interrupt_struct = ecl_alloc(sizeof(*env->interrupt_struct));
  env->interrupt_struct->pending_interrupt = ECL_NIL;
#ifdef ECL_THREADS
  ecl_mutex_init(&env->interrupt_struct->signal_queue_lock, FALSE);
#endif
  {
    int size = ecl_option_values[ECL_OPT_SIGNAL_QUEUE_SIZE];
    env->interrupt_struct->signal_queue = cl_make_list(1, ecl_make_fixnum(size));
  }
  env->fault_address = env;
  env->trap_fpe_bits = 0;
}

static void
init_env_ffi(cl_env_ptr env)
{
#ifdef HAVE_LIBFFI
  env->ffi_args_limit = 0;
  env->ffi_types = 0;
  env->ffi_values = 0;
  env->ffi_values_ptrs = 0;
#endif
}

static void
init_env_aux(cl_env_ptr env)
{
  /* Reader */
  env->string_pool = ECL_NIL;
  env->packages_to_be_created = ECL_NIL;
  env->packages_to_be_created_p = ECL_NIL;
  /* Format (written in C) */
#if !defined(ECL_CMU_FORMAT)
  env->fmt_aux_stream = ecl_make_string_output_stream(64, 1);
#endif
  /* Bignum arithmetic */
  ecl_init_bignum_registers(env);
  /* Bytecodes compiler environment */
  env->c_env = NULL;
  /* CLOS caches */
  env->method_cache = ecl_make_cache(64, 4096);
  env->slot_cache = ecl_make_cache(3, 4096);
}

void
ecl_init_env(cl_env_ptr env)
{
  init_env_mp(env);
  init_env_int(env);
  init_env_aux(env);
  init_env_ffi(env);
  init_stacks(env);
}

void
_ecl_dealloc_env(cl_env_ptr env)
{
  /*
   * Environment cleanup. This is only required when the environment is
   * allocated using mmap or some other method. We could do more, cleaning
   * up stacks, etc, but we actually do not do it because that would need
   * a lisp environment set up -- the allocator assumes one -- and we
   * may have already cleaned up the value of ecl_process_env()
   */
#ifdef ECL_THREADS
  ecl_mutex_destroy(&env->interrupt_struct->signal_queue_lock);
#endif
#if defined(ECL_USE_MPROTECT)
  if (munmap(env, sizeof(*env)))
    ecl_internal_error("Unable to deallocate environment structure.");
#else
# if defined(ECL_USE_GUARD_PAGE)
  if (!VirtualFree(env, 0, MEM_RELEASE))
    ecl_internal_error("Unable to deallocate environment structure.");
# endif
#endif
}

cl_env_ptr
_ecl_alloc_env(cl_env_ptr parent)
{
  /*
   * Allocates the lisp environment for a thread. Depending on which
   * mechanism we use for detecting delayed signals, we may allocate
   * the environment using mmap or the garbage collector.
   *
   * Note that at this point we are not allocating any other memory
   * which is stored via a pointer in the environment. If we would do
   * that, an unlucky interrupt by the gc before the allocated
   * environment is registered in ecl_core.processes could lead to
   * memory being freed because the gc is not aware of the pointer to
   * the allocated memory in the environment.
   */
  cl_env_ptr output;
#if defined(ECL_USE_MPROTECT)
  output = (cl_env_ptr) mmap(0, sizeof(*output), PROT_READ | PROT_WRITE,
                             MAP_ANON | MAP_PRIVATE, -1, 0);
  if (output == MAP_FAILED)
    ecl_internal_error("Unable to allocate environment structure.");
#else
# if defined(ECL_USE_GUARD_PAGE)
  output = VirtualAlloc(0, sizeof(*output), MEM_COMMIT,
                        PAGE_READWRITE);
  if (output == NULL)
    ecl_internal_error("Unable to allocate environment structure.");
# else
  static struct cl_env_struct first_env;
  if (!ecl_option_values[ECL_OPT_BOOTED]) {
    /* We have not set up any environment. Hence, we cannot call ecl_alloc()
     * because it will need to stop interrupts and currently we rely on
     * the environment for that */
    output = ecl_alloc_unprotected(sizeof(*output));
  } else {
    output = ecl_alloc(sizeof(*output));
  }
# endif
#endif
  {
    size_t bytes = ecl_core.default_sigmask_bytes;
    if (bytes == 0) {
      output->default_sigmask = 0;
    } else if (parent) {
      output->default_sigmask = ecl_alloc_atomic(bytes);
      memcpy(output->default_sigmask,
             parent->default_sigmask,
             bytes);
    } else {
      output->default_sigmask = ecl_core.default_sigmask;
    }
  }
  output->method_cache = output->slot_cache = NULL;
  output->interrupt_struct = NULL;
  /*
   * An uninitialized environment _always_ disables interrupts. They
   * are activated later on by the thread entry point or init_unixint().
   */
  output->disable_interrupts = 1;
  return output;
}

void
cl_shutdown(void)
{
  if (ecl_option_values[ECL_OPT_BOOTED] > 0) {
    cl_object l = ecl_symbol_value(@'si::*exit-hooks*');
    cl_object form = cl_list(2, @'funcall', ECL_NIL);
    while (CONSP(l)) {
      ecl_elt_set(form, 1, ECL_CONS_CAR(l));
      si_safe_eval(3, form, ECL_NIL, OBJNULL);
      l = CDR(l);
      ECL_SET(@'si::*exit-hooks*', l);
    }
#ifdef ENABLE_DLOPEN
    ecl_library_close_all();
#endif
#ifdef ECL_TCP
    ecl_tcp_close_all();
#endif
  }
  ecl_set_option(ECL_OPT_BOOTED, -1);
}

ecl_def_ct_base_string(str_common_lisp,"COMMON-LISP",11,static,const);
ecl_def_ct_base_string(str_common_lisp_user,"COMMON-LISP-USER",16,static,const);
ecl_def_ct_base_string(str_cl,"CL",2,static,const);
ecl_def_ct_base_string(str_cl_user,"CL-USER",7,static,const);
ecl_def_ct_base_string(str_LISP,"LISP",4,static,const);
ecl_def_ct_base_string(str_c,"C",1,static,const);
ecl_def_ct_base_string(str_compiler,"COMPILER",8,static,const);
ecl_def_ct_base_string(str_ffi,"FFI",3,static,const);
ecl_def_ct_base_string(str_keyword,"KEYWORD",7,static,const);
ecl_def_ct_base_string(str_si,"SI",2,static,const);
ecl_def_ct_base_string(str_sys,"SYS",3,static,const);
ecl_def_ct_base_string(str_system,"SYSTEM",6,static,const);
ecl_def_ct_base_string(str_ext,"EXT",3,static,const);
ecl_def_ct_base_string(str_clos,"CLOS",4,static,const);
ecl_def_ct_base_string(str_mop,"MOP",3,static,const);
ecl_def_ct_base_string(str_mp,"MP",2,static,const);
ecl_def_ct_base_string(str_multiprocessing,"MULTIPROCESSING",15,static,const);
#ifdef ECL_CLOS_STREAMS
ecl_def_ct_base_string(str_gray,"GRAY",4,static,const);
#endif
ecl_def_ct_base_string(str_star_dot_star,"*.*",3,static,const);
ecl_def_ct_base_string(str_rel_star_dot_star,"./*.*",5,static,const);
ecl_def_ct_base_string(str_G,"G",1,static,const);
ecl_def_ct_base_string(str_T,"T",1,static,const);
#ifdef ENABLE_DLOPEN
ecl_def_ct_base_string(str_fas,"fas",3,static,const);
ecl_def_ct_base_string(str_fasl,"fasl",4,static,const);
#endif
ecl_def_ct_base_string(str_fasb,"fasb",4,static,const);
ecl_def_ct_base_string(str_fasc,"fasc",4,static,const);
ecl_def_ct_base_string(str_FASB,"FASB",4,static,const);
ecl_def_ct_base_string(str_FASC,"FASC",4,static,const);
ecl_def_ct_base_string(str_lsp,"lsp",3,static,const);
ecl_def_ct_base_string(str_LSP,"LSP",3,static,const);
ecl_def_ct_base_string(str_lisp,"lisp",4,static,const);
ecl_def_ct_base_string(str_NIL,"NIL",3,static,const);

struct cl_core_struct cl_core = {
  .packages = ECL_NIL,
  .lisp_package = ECL_NIL,
  .user_package = ECL_NIL,
  .keyword_package = ECL_NIL,
  .system_package = ECL_NIL,
  .ext_package = ECL_NIL,
  .clos_package = ECL_NIL,
# ifdef ECL_CLOS_STREAMS
  .gray_package = ECL_NIL,
# endif
  .mp_package = ECL_NIL,
  .c_package = ECL_NIL,
  .ffi_package = ECL_NIL,

  .terminal_io = ECL_NIL,
  .null_stream = ECL_NIL,
  .standard_input = ECL_NIL,
  .standard_output = ECL_NIL,
  .error_output = ECL_NIL,
  .standard_readtable = ECL_NIL,
  .dispatch_reader = ECL_NIL,

  .char_names = ECL_NIL,

  .gensym_prefix = ECL_NIL,
  .gentemp_prefix = ECL_NIL,
  .gentemp_counter = ecl_make_fixnum(0),

  .system_properties = ECL_NIL,
  .setf_definitions = ECL_NIL,

  .compiler_dispatch = ECL_NIL,
};

#if !defined(ECL_MS_WINDOWS_HOST)
#define maybe_fix_console_stream(strm) (void)0
#else
static void
maybe_fix_console_stream(cl_object stream)
{
  cl_object external_format;
  if (stream->stream.mode != ecl_smm_io_wcon) {
    si_stream_external_format_set(stream, cl_list(2, ecl_symbol_value(@'ext::*default-external-format*'), @':crlf'));
    return;
  }
  external_format = si_windows_codepage_encoding();
  if (external_format == @':pass-through')
    fprintf(stderr,
            "Unsupported codepage %d, input/output encoding may be wrong.\n"
            "Use the chcp command to change codepages, e.g. 'chcp 65001' to change to utf-8.\n",
            GetConsoleCP());
  si_stream_external_format_set(stream, cl_list(2, external_format, @':crlf'));
  stream->stream.eof_char = 26;
}
#endif

int
cl_boot(int argc, char **argv)
{
  cl_object aux;
  cl_object features;
  int i;
  cl_env_ptr env;

  i = ecl_boot();
  if (i==1) return 1;

  ARGC = argc;
  ARGV = argv;
  ecl_self = argv[0];

  init_unixint(0);
  init_alloc();
  GC_disable();
  env = _ecl_alloc_env(0);
  init_threads(env);

  /*
   * 1) Initialize symbols and packages
   */

  ECL_NIL_SYMBOL->symbol.t = t_symbol;
  ECL_NIL_SYMBOL->symbol.value = ECL_NIL;
  ECL_NIL_SYMBOL->symbol.name = str_NIL;
  ECL_NIL_SYMBOL->symbol.gfdef = ECL_NIL;
  ECL_NIL_SYMBOL->symbol.plist = ECL_NIL;
  ECL_NIL_SYMBOL->symbol.hpack = ECL_NIL;
  ECL_NIL_SYMBOL->symbol.stype = ecl_stp_constant;
#ifdef ECL_THREADS
  ECL_NIL_SYMBOL->symbol.binding = ECL_MISSING_SPECIAL_BINDING;
#endif
  cl_num_symbols_in_core=1;

  ECL_T->symbol.t = (short)t_symbol;
  ECL_T->symbol.value = ECL_T;
  ECL_T->symbol.name = str_T;
  ECL_T->symbol.gfdef = ECL_NIL;
  ECL_T->symbol.plist = ECL_NIL;
  ECL_T->symbol.hpack = ECL_NIL;
  ECL_T->symbol.stype = ecl_stp_constant;
#ifdef ECL_THREADS
  ECL_T->symbol.binding = ECL_MISSING_SPECIAL_BINDING;
#endif
  cl_num_symbols_in_core=2;

  cl_core.gensym_prefix = (cl_object)&str_G_data;
  cl_core.gentemp_prefix = (cl_object)&str_T_data;

#ifdef ECL_THREADS
  env->bindings_array = si_make_vector(ECL_T, ecl_make_fixnum(1024),
                                       ECL_NIL, ECL_NIL, ECL_NIL, ECL_NIL);
  si_fill_array_with_elt(env->bindings_array, ECL_NO_TL_BINDING, ecl_make_fixnum(0), ECL_NIL);
  env->thread_local_bindings_size = env->bindings_array->vector.dim;
  env->thread_local_bindings = env->bindings_array->vector.self.t;
#endif

  /*
   * Initialize the per-thread data.
   * This cannot come later, because we need to be able to bind
   * ext::*interrupts-enabled* while creating packages.
   */
  init_big();
  ecl_init_env(env);
  ecl_cs_set_org(env);

  cl_core.lisp_package =
    ecl_make_package(str_common_lisp,
                     cl_list(1, str_cl),
                     ECL_NIL,
                     ECL_NIL);
  cl_core.user_package =
    ecl_make_package(str_common_lisp_user,
                     cl_list(1, str_cl_user),
                     ecl_list1(cl_core.lisp_package),
                     ECL_NIL);
  cl_core.keyword_package =
    ecl_make_package(str_keyword, ECL_NIL, ECL_NIL, ECL_NIL);
  cl_core.ext_package =
    ecl_make_package(str_ext,
                     ECL_NIL,
                     ecl_list1(cl_core.lisp_package),
                     ECL_NIL);
  cl_core.system_package =
    ecl_make_package(str_si,
                     cl_list(2,str_system,str_sys),
                     cl_list(2,cl_core.ext_package,
                             cl_core.lisp_package),
                     ECL_NIL);
  cl_core.c_package =
    ecl_make_package(str_c,
                     ecl_list1(str_compiler),
                     ecl_list1(cl_core.lisp_package),
                     ECL_NIL);
  cl_core.clos_package =
    ecl_make_package(str_clos,
                     ecl_list1(str_mop),
                     ecl_list1(cl_core.lisp_package),
                     ECL_NIL);
  cl_core.mp_package =
    ecl_make_package(str_mp,
                     ecl_list1(str_multiprocessing),
                     ecl_list1(cl_core.lisp_package),
                     ECL_NIL);
#ifdef ECL_CLOS_STREAMS
  cl_core.gray_package = ecl_make_package(str_gray,
                                          ECL_NIL,
                                          ecl_list1(cl_core.lisp_package),
                                          ECL_NIL);
#endif
  cl_core.ffi_package =
    ecl_make_package(str_ffi,
                     ECL_NIL,
                     cl_list(3,cl_core.lisp_package,
                             cl_core.system_package,
                             cl_core.ext_package),
                     ECL_NIL);

  ECL_NIL_SYMBOL->symbol.hpack = cl_core.lisp_package;
  cl_import2(ECL_NIL, cl_core.lisp_package);
  cl_export2(ECL_NIL, cl_core.lisp_package);

  ECL_T->symbol.hpack = cl_core.lisp_package;
  cl_import2(ECL_T, cl_core.lisp_package);
  cl_export2(ECL_T, cl_core.lisp_package);

  /* At exit, clean up */
  atexit(cl_shutdown);

  /* These must come _after_ the packages and NIL/T have been created */
  init_all_symbols();

  /* We need this because a lot of stuff is to be created */
  GC_enable();

  /*
   * Set *default-pathname-defaults* to a temporary fake value. We
   * will fix this when we have access to the condition system to
   * allow for error recovery when we can't parse the output of
   * getcwd.
   */
  ECL_SET(@'*default-pathname-defaults*',
          ecl_make_pathname(ECL_NIL, ECL_NIL, ECL_NIL, ECL_NIL, ECL_NIL, ECL_NIL, @':local'));

#ifdef ECL_THREADS
  ECL_SET(@'mp::*current-process*', env->own_process);
#endif

  /*
   * Load character names. The following hash table is a map
   * from names to character codes and viceversa. Note that we
   * need EQUALP because it has to be case insensitive.
   */
  cl_core.char_names = aux =
    cl__make_hash_table(@'equalp', ecl_make_fixnum(128), /* size */
                        ecl_ct_default_rehash_size,
                        ecl_ct_default_rehash_threshold);
  for (i = 0; char_names[i].elt.self; i++) {
    cl_object name = (cl_object)(char_names + i);
    cl_object code = ecl_make_fixnum(i);
    ecl_sethash(name, aux, code);
    ecl_sethash(code, aux, name);
  }
  for (i = 0; i < extra_char_names_size; i++) {
    cl_object name = (cl_object)(extra_char_names + i);
    cl_object code = ecl_make_fixnum(extra_char_codes[i]);
    ecl_sethash(name, aux, code);
  }

  /*
   * Initialize logical pathname translations. This must come after
   * the character database has been filled.
   */
  @si::pathname-translations(2,str_sys,
                             ecl_list1(cl_list(2,str_star_dot_star,
                                               str_rel_star_dot_star)));

  /*
   * Initialize constants (strings, numbers and time).
   */
  cl_core.system_properties =
    cl__make_hash_table(@'equal', ecl_make_fixnum(1024), /* size */
                        ecl_ct_default_rehash_size,
                        ecl_ct_default_rehash_threshold);
  cl_core.setf_definitions =
    cl__make_hash_table(@'eq', ecl_make_fixnum(256), /* size */
                        ecl_ct_default_rehash_size,
                        ecl_ct_default_rehash_threshold);

  ECL_SET(@'*random-state*', ecl_make_random_state(ECL_T));

  ECL_SET(@'ffi::c-int-max', ecl_make_integer(INT_MAX));
  ECL_SET(@'ffi::c-int-min', ecl_make_integer(INT_MIN));
  ECL_SET(@'ffi::c-long-max', ecl_make_integer(LONG_MAX));
  ECL_SET(@'ffi::c-long-min', ecl_make_integer(LONG_MIN));
  ECL_SET(@'ffi::c-uint-max', ecl_make_unsigned_integer(UINT_MAX));
  ECL_SET(@'ffi::c-ulong-max', ecl_make_unsigned_integer(ULONG_MAX));
#ifdef ecl_long_long_t
  ECL_SET(@'ffi::c-long-long-max', ecl_make_long_long(LLONG_MAX));
  ECL_SET(@'ffi::c-ulong-long-max', ecl_make_ulong_long(ULLONG_MAX));
#endif

  init_unixtime();

  /*
   * Initialize I/O subsystem.
   */
  init_file();
  init_read();

  ECL_SET(@'*print-case*', @':upcase');

  /*
   * Set up hooks for LOAD, errors and macros.
   */
#ifdef ECL_THREADS
  ECL_SET(@'mp::+load-compile-lock+',
          ecl_make_lock(@'mp::+load-compile-lock+', 1));
#endif
#ifdef ENABLE_DLOPEN
  aux = cl_list(11,
                CONS(ECL_NIL, @'si::load-source'),
                CONS(str_fas, @'si::load-binary'),
                CONS(str_fasl, @'si::load-binary'),
                CONS(str_fasb, @'si::load-binary'),
                CONS(str_FASB, @'si::load-binary'),
                CONS(str_lsp, @'si::load-source'),
                CONS(str_lisp, @'si::load-source'),
                CONS(str_LSP, @'si::load-source'),
                CONS(str_LISP, @'si::load-source'),
                CONS(str_fasc, @'si::load-bytecodes'),
                CONS(str_FASC, @'si::load-bytecodes'));
#else
  aux = cl_list(7,
                CONS(ECL_NIL, @'si::load-source'),
                CONS(str_lsp, @'si::load-source'),
                CONS(str_lisp, @'si::load-source'),
                CONS(str_LSP, @'si::load-source'),
                CONS(str_LISP, @'si::load-source'),
                CONS(str_fasc, @'si::load-bytecodes'),
                CONS(str_FASC, @'si::load-bytecodes'));
#endif
  ECL_SET(@'ext::*load-hooks*', aux);
  init_error();
  init_macros();
  init_compiler();

  /*
   * Set up infrastructure for CLOS.
   */
  ECL_SET(@'si::*class-name-hash-table*',
          cl__make_hash_table(@'eq', ecl_make_fixnum(1024), /* size */
                              ecl_ct_default_rehash_size,
                              ecl_ct_default_rehash_threshold));

  /*
   * Features.
   */

  ECL_SET(@'LAMBDA-LIST-KEYWORDS',
          cl_list(8, @'&optional', @'&rest', @'&key', @'&allow-other-keys',
                  @'&aux', @'&whole', @'&environment', @'&body'));

  for (i = 0, features = ECL_NIL; feature_names[i].elt.self; i++) {
    int flag;
    cl_object name = (cl_object)(feature_names + i);
    cl_object key = ecl_intern(name, cl_core.keyword_package, &flag);
    features = CONS(key, features);
  }

  ECL_SET(@'*features*', features);

  ECL_SET(@'*package*', cl_core.lisp_package);

  /* This has to come before init_LSP/CLOS, because we need
   * ecl_clear_compiler_properties() to work in init_CLOS(). */
  ecl_set_option(ECL_OPT_BOOTED, 1);

  ecl_init_module(OBJNULL,init_lib_LSP);

  ECL_HANDLER_CASE_BEGIN(env, ecl_list1(@'ext::stream-decoding-error')) {
    ECL_SET(@'*default-pathname-defaults*', si_getcwd(0));
  } ECL_HANDLER_CASE(1, c) {
    _ecl_funcall3(@'warn', @"Cannot initialize *DEFAULT-PATHNAME-DEFAULTS* with the current directory:~%~A~%", c);
  } ECL_HANDLER_CASE_END;

  if (cl_fboundp(@'ext::make-encoding') != ECL_NIL) {
    maybe_fix_console_stream(cl_core.standard_input);
    maybe_fix_console_stream(cl_core.standard_output);
    maybe_fix_console_stream(cl_core.error_output);
  }

  /* Jump to top level */
  ECL_SET(@'*package*', cl_core.user_package);
  init_unixint(1);
  return 1;
}

/************************* ENVIRONMENT ROUTINES ***********************/

@(defun ext::quit (&optional (code ecl_make_fixnum(0)) (kill_all_threads ECL_T))
@ {
#ifdef ECL_THREADS
    if (!Null(kill_all_threads)) {
      cl_object this_process = the_env->own_process;
      cl_object p, all_threads = mp_all_processes();
      for (p = all_threads; !Null(p); p = ECL_CONS_CDR(p)) {
        cl_object process = ECL_CONS_CAR(p);
        if (process != this_process)
          mp_process_kill(process);
      }
      for (p = all_threads; !Null(p); p = ECL_CONS_CDR(p)) {
        cl_object process = ECL_CONS_CAR(p);
        if (process != this_process)
          mp_process_join(process);
      }
      /* FIXME! We need to do this because of a problem in GC
       * When the thread exits, sometimes the dyld library gets
       * called, and if we call dlopen() at the same time we
       * cause ECL to hang */
      ecl_musleep(1e-3);
    }
#endif
    ECL_SET(@'ext::*program-exit-code*', code);
    if (the_env->frs_org <= the_env->frs_top)
      ecl_unwind(the_env, the_env->frs_org);
    si_exit(1, code);
  }
@)

@(defun ext::exit (&optional (code ECL_SYM_VAL(ecl_process_env(),@'ext::*program-exit-code*')))
@
  cl_shutdown();
  exit(ECL_FIXNUMP(code)? ecl_fixnum(code) : 0);
@)

cl_object
si_argc()
{
  @(return ecl_make_fixnum(ARGC));
}

cl_object
si_argv(cl_object index)
{
  if (ECL_FIXNUMP(index)) {
    cl_fixnum i = ecl_fixnum(index);
    if (i >= 0 && i < ARGC) {
      @(return ecl_make_simple_base_string(ARGV[i],-1));
    }
  }
  FEerror("Illegal argument index: ~S.", 1, index);
}

cl_object
si_getenv(cl_object var)
{
  const char *value;

  /* Strings have to be null terminated base strings */
  var = si_copy_to_simple_base_string(var);
  value = getenv((char*)var->base_string.self);
  @(return ((value == NULL)? ECL_NIL : ecl_make_simple_base_string(value,-1)));
}

#if defined(HAVE_SETENV) || defined(HAVE_PUTENV)
cl_object
si_setenv(cl_object var, cl_object value)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_fixnum ret_val;

  /* Strings have to be null terminated base strings */
  var = si_copy_to_simple_base_string(var);
  if (value == ECL_NIL) {
#ifdef HAVE_SETENV
    /* Remove the variable when setting to nil, so that
     * (si:setenv "foo" nil), then (si:getenv "foo) returns
     * the right thing. */
    unsetenv((char*)var->base_string.self);
#else
#if defined(ECL_MS_WINDOWS_HOST)
    si_setenv(var, ecl_ct_null_string);
#else
    putenv((char*)var->base_string.self);
#endif
#endif
    ret_val = 0;
  } else {
#ifdef HAVE_SETENV
    value = si_copy_to_simple_base_string(value);
    ret_val = setenv((char*)var->base_string.self,
                     (char*)value->base_string.self, 1);
#else
    value = cl_format(4, ECL_NIL, @"~A=~A", var,
                      value);
    value = si_copy_to_simple_base_string(value);
    putenv((char*)value->base_string.self);
#endif
  }
  if (ret_val == -1)
    CEerror(ECL_T, "SI:SETENV failed: insufficient space in environment.",
            1, ECL_NIL);
  ecl_return1(the_env, value);
}
#endif

cl_object
si_environ(void)
{
  cl_object output = ECL_NIL;
#ifdef HAVE_ENVIRON
  char **p;
  extern char **environ;
  for (p = environ; *p; p++) {
    output = CONS(ecl_make_constant_base_string(*p,-1), output);
  }
  output = cl_nreverse(output);
#else
# if defined(ECL_MS_WINDOWS_HOST)
  LPTCH p;
  for (p = GetEnvironmentStrings(); *p; ) {
    output = CONS(ecl_make_constant_base_string(p,-1), output);
    do { (void)0; } while (*(p++));
  }
  output = cl_nreverse(output);
# endif
#endif /* HAVE_ENVIRON */
  @(return output);
}

cl_object
si_pointer(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_make_unsigned_integer((cl_index)x));
}

#if defined(ECL_MS_WINDOWS_HOST)
void
ecl_get_commandline_args(int* argc, char*** argv) {
  /* the caller should use LocalFree to release the memory of strings in argv and argv itself */
  LPWSTR *wArgs;
  int i;

  if (argc == NULL || argv == NULL)
    return;

  wArgs = CommandLineToArgvW(GetCommandLineW(), argc);
  *argv = (char**)LocalAlloc(0, sizeof(char*)*(*argc));
  for (i=0; i<*argc; i++) {
    int len = wcslen(wArgs[i]);
    (*argv)[i] = (char*)LocalAlloc(0, 2*(len+1));
    wcstombs((*argv)[i], wArgs[i], len+1);
  }
  LocalFree(wArgs);
}
#endif
