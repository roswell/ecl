;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    See file 'LICENSE' for the copyright details.

;;;;
;;;;  CMPGLOBALS -- Global variables and flag definitions
;;;;

(in-package "COMPILER")

;;;
;;; VARIABLES
;;;

(defvar *inline-max-depth* 3
  "Depth at which inlining of functions stops.")

;;; --cmputil.lsp--
;;;
;;; Variables and constants for error handling
;;;
(defvar *current-form* '|compiler preprocess|)
(defvar *current-c1form*)
(defvar *current-toplevel-form* '|compiler preprocess|)
(defvar *compile-file-position* -1)
(defvar *active-protection* nil)
(defvar *pending-actions* nil)
(defvar *empty-loc* (gensym))
(defvar *inline-loc* (gensym))

(defvar *compiler-conditions* '()
  "This variable determines whether conditions are printed or just accumulated.")

(defvar *compile-print* nil
  "This variable controls whether the compiler displays messages about
each form it processes. The default value is NIL.")

(defvar *compile-verbose* nil
  "This variable controls whether the compiler should display messages about its
progress. The default value is T.")

(defvar *compiler-features*
  '#.(if (not (boundp '*compiler-features*)) nil *compiler-features*)
  "This alternative list of features contains keywords that were gathered from
running the compiler. It may be updated by running ")

(defvar *suppress-compiler-messages*
  #+ecl-min 'compiler-debug-note #-ecl-min 'compiler-note
  "A type denoting which compiler messages and conditions are _not_ displayed.")

(defvar *compiler-break-enable* nil)

(defvar *compiler-in-use* nil)
(defvar *compiler-output1*)
(defvar *compiler-output2*)

;;; --cmpcbk.lsp--
;;;
;;; List of callbacks to be generated
;;;
(defvar *callbacks* nil)
(defvar *functions* nil)

;;; --cmpc-machine.lsp, cmpffi.lsp ---
(defvar *machine* nil)

;;; --cmpcall.lsp--
(defvar *compiler-declared-globals*)

;;; --cmpenv.lsp--
;;;
;;; Default optimization settings.
;;;
(defvar *safety* 2)
(defvar *speed* 3)
(defvar *space* 0)
(defvar *debug* 0)
(defvar *compilation-speed* 2)

(defvar *current-function* nil)

(defvar *cmp-env* nil
"The compiler environment consists of a pair or cons of two
lists, one containing variable records, the other one macro and
function recors:

variable-record = (:block block-name) |
                  (:tag ({tag-name}*)) |
                  (:function function-name) |
                  (var-name {:special | nil} bound-p) |
                  (symbol si:symbol-macro macro-function) |
                  (:declare type arguments) |
                  SI:FUNCTION-BOUNDARY |
                  SI:UNWIND-PROTECT-BOUNDARY

macro-record    = (function-name function) |
                  (macro-name si:macro macro-function) |
                  (:declare name declaration) |
                  SI:FUNCTION-BOUNDARY |
                  SI:UNWIND-PROTECT-BOUNDARY

A *-NAME is a symbol. A TAG-ID is either a symbol or a number. A MACRO-FUNCTION
is a function that provides us with the expansion for that local macro or symbol
macro. BOUND-P is true when the variable has been bound by an enclosing form,
while it is NIL if the variable-record corresponds just to a special
declaration.  SI:FUNCTION-BOUNDARY and SI:UNWIND-PROTECT-BOUNDARY are only used
by the C compiler and they denote function and unwind-protect boundaries. Note
that compared with the bytecodes compiler, these records contain an additional
variable, block, tag or function object at the end.")

(defvar *cmp-env-root*
  (cons nil (list (list '#:no-macro 'si:macro (constantly nil))))
"This is the common environment shared by all toplevel forms. It can
only be altered by DECLAIM forms and it is used to initialize the
value of *CMP-ENV*.")

;;; --cmpmain.lsp--
;;;
;;; Do we debug the compiler? Then we need files not to be deleted.

(defvar *debug-compiler* nil)
(defvar *delete-files* t)
(defvar *files-to-be-deleted* '())

(defvar *user-ld-flags* '()
"DEPRECATED Flags and options to be passed to the linker when building FASL,
shared libraries and standalone programs. It is not required to surround values
with quotes or use slashes before special characters.")

(defvar *user-linker-flags* '()
  "Command line flags for additional options (e.g. \"-Wl,foo\" flags) to
be passed to the linker when building FASL, shared libraries and
standalone programs. It is not required to surround values with quotes
or use slashes before special characters.")

(defvar *user-linker-libs* '()
"Command line flags for additional libraries (e.g. \"-lfoo\" flags) to
be passed to the linker when building FASL, shared libraries and
standalone programs. It is not required to surround values with quotes
or use slashes before special characters.")

(defvar *user-cc-flags* '()
"Flags and options to be passed to the C compiler when building FASL, shared libraries
and standalone programs. It is not required to surround values with quotes or use
slashes before special characters.")

(defvar *use-precompiled-headers* nil
"This variable controls whether the C compiler uses precompiled header files.")
(defvar *precompiled-header-flags* nil)
(defvar *precompiled-header-cc-config* nil)

;;;
;;; Compiler program and flags.
;;;

;;; --cmptop.lsp--
;;;
(defvar *do-type-propagation* t
  "Flag for switching on the type propagation phase. Use with care, experimental.")

(defvar *compiler-phase* nil)

(defvar *volatile*)
(defvar *setjmps* 0)

(defvar *compile-toplevel* T
  "Holds NIL or T depending on whether we are compiling a toplevel form.")

(defvar *clines-string-list* '()
  "List of strings containing C/C++ statements which are directly inserted
in the translated C/C++ file. Notice that it is unspecified where these
lines are inserted, but the order is preserved")

(defvar *compile-time-too* nil)
(defvar *not-compile-time* nil)

;;; Determines whether the object may be released after the initialization
(defvar *permanent-data* nil)
(defvar *referenced-objects* nil)       ; holds { vv-record }*

(defvar *load-objects* nil)             ; hash with association object -> vv-location
(defvar *load-time-values* nil)         ; holds { ( vv-index form ) }*,
;;;  where each vv-index should be given an object before
;;;  defining the current function during loading process.

(defvar si:*compiler-constants* nil)    ; a vector with all constants only used
                                        ; in COMPILE

(defvar *global-vars* nil)              ; variables declared special
(defvar *global-funs* nil)              ; holds { fun }*
(defvar *use-c-global* nil)             ; honor si::c-global declaration
(defvar *top-level-forms* nil)          ; holds { top-level-form }*
(defvar *make-forms* nil)               ; holds { top-level-form }*

(defvar *objects-being-created* nil)    ; helps detecting circular references
(defvar *objects-init-deferred* nil)    ; helps avoiding circularity

;;;
;;;     top-level-form:
;;;       ( 'DEFUN'     fun-name cfun lambda-expr doc-vv sp )
;;;     | ( 'DEFMACRO'  macro-name cfun lambda-expr doc-vv sp )
;;;     | ( 'ORDINARY'  expr )
;;;     | ( 'DECLARE'   var-name-vv )
;;;     | ( 'DEFVAR'    var-name-vv expr doc-vv )
;;;     | ( 'CLINES'    string* )
;;;     | ( 'LOAD-TIME-VALUE' vv )

(defvar *self-destructing-fasl* '()
"A value T means that, when a FASL module is being unloaded (for
instance during garbage collection), the associated file will be
deleted. We need this for #'COMPILE because windows DLLs cannot
be deleted if they have been opened with LoadLibrary.")

(defvar *undefined-vars* nil)

;;; Only these flags are set by the user.
;;; If (safe-compile) is ON, some kind of run-time checks are not
;;; included in the compiled code.  The default value is OFF.

(defvar +init-env-form+
  '((*gensym-counter* 0)
    (*compiler-in-use* t)
    (*compiler-phase* 't1)
    (*callbacks* nil)
    (*functions* nil)
    (*cmp-env-root* (copy-tree *cmp-env-root*))
    (*cmp-env* nil)
    (*load-objects* (make-hash-table :size 128 :test #'equal))
    (*make-forms* nil)
    (*referenced-objects* (make-array 256 :adjustable t :fill-pointer 0))
    (*global-vars* nil)
    (*global-funs* nil)
    (*undefined-vars* nil)
    (*top-level-forms* nil)
    (*compile-time-too* nil)
    (*clines-string-list* '())
    (si::*defun-inline-hook* 'maybe-install-inline-function)
    (*machine* (or *machine* *default-machine*))))

