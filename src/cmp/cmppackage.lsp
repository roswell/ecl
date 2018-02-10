;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;;  CMPPACKAGE -- Package definitions and exported symbols
;;;;

(defpackage #:ecl-cmp-internals
  (:export #:unwind-protect
           #:function))

(defpackage "C"
  (:nicknames "COMPILER")
  (:use "FFI" "EXT" #+threads "MP" "CL")
  (:export "*COMPILER-BREAK-ENABLE*"
           "*COMPILE-PRINT*"
           "*COMPILE-TO-LINKING-CALL*"
           "*COMPILE-VERBOSE*"
           "*COMPILER-FEATURES*"
           "*CC*"
           "*CC-OPTIMIZE*"
           "*USER-CC-FLAGS*"
           "*USER-LD-FLAGS*"
           "*SUPPRESS-COMPILER-MESSAGES*"
           "BUILD-ECL"
           "BUILD-PROGRAM"
           "BUILD-FASL"
           "BUILD-STATIC-LIBRARY"
           "BUILD-SHARED-LIBRARY"
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
           "*SUPPRESS-COMPILER-MESSAGES*"
           "INSTALL-C-COMPILER"
           "UPDATE-COMPILER-FEATURES")
  (:import-from "SI" "GET-SYSPROP" "PUT-SYSPROP" "REM-SYSPROP" "MACRO"
                "*COMPILER-CONSTANTS*" "REGISTER-GLOBAL" "CMP-ENV-REGISTER-MACROLET"
                "COMPILER-LET"))

(ext:package-lock "CL" nil)
(ext:add-package-local-nickname "ECI" '#:ecl-cmp-internals '#:compiler)
