;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;;  CMPPACKAGE -- Package definitions and exported symbols

(defpackage "C"
  (:nicknames "ECL-CMP" "COMPILER")
  (:local-nicknames ("OP" "ECL-CMP/OP"))
  (:use "CL")
  (:import-from "EXT" "INSTALL-C-COMPILER")
  (:export
   ;; Flags controlling the compiler behavior.
   "*COMPILER-BREAK-ENABLE*"
   "*COMPILE-PRINT*"
   "*COMPILE-TO-LINKING-CALL*"
   "*COMPILE-VERBOSE*"
   "*COMPILER-FEATURES*"
   "*CC*"
   "*CC-OPTIMIZE*"
   "*USER-CC-FLAGS*"
   "*USER-LD-FLAGS*"                    ; deprecated
   "*USER-LINKER-FLAGS*"
   "*USER-LINKER-LIBS*"
   "*SUPPRESS-COMPILER-MESSAGES*"
   ;; Build targets. BUILD-ECL is not defined, preasumbly it was meant
   ;; for cross compilation.
   "BUILD-ECL"
   "BUILD-PROGRAM"
   "BUILD-FASL"
   "BUILD-STATIC-LIBRARY"
   "BUILD-SHARED-LIBRARY"
   ;; Conditions (and their accessors).
   "COMPILER-WARNING"
   "COMPILER-NOTE"
   "COMPILER-MESSAGE"
   "COMPILER-ERROR"
   "COMPILER-FATAL-ERROR"
   "COMPILER-INTERNAL-ERROR"
   "COMPILER-UNDEFINED-VARIABLE"
   "COMPILER-MESSAGE-FILE"
   "COMPILER-MESSAGE-FILE-POSITION"
   "COMPILER-MESSAGE-FORM"
   ;; Other operators.
   "INSTALL-C-COMPILER"
   "UPDATE-COMPILER-FEATURES"))



(ext:package-lock '#:cl nil)
