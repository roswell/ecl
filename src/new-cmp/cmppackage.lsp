;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
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

(si::package-lock "CL" nil)

(defpackage "C-DATA"
  (:nicknames "COMPILER-DATA")
  (:use "FFI" "CL")
  (:export "*COMPILER-BREAK-ENABLE*"
	   "*COMPILE-PRINT*"
	   "*COMPILE-TO-LINKING-CALL*"
	   "*COMPILE-VERBOSE*"
	   "*CC*"
	   "*CC-OPTIMIZE*"
	   "*SUPPRESS-COMPILER-WARNINGS*"
	   "*SUPPRESS-COMPILER-NOTES*"
	   "*SUPPRESS-COMPILER-MESSAGES*"
           "PROCLAIMED-ARG-TYPES"
           "PROCLAIMED-RETURN-TYPE"
           "NO-SP-CHANGE"
           "PURE"
           "NO-SIDE-EFFECTS"

           "MAKE-C1FORM*"

           "LOCATION-TYPE"
           )
  (:import-from "SI" "*COMPILER-CONSTANTS*"))

(defpackage "C-BACKEND"
  (:use "FFI" "CL" "C-DATA")
  (:export "CTOP-WRITE" "DUMP-ALL" "DATA-DUMP"
           "WT-FILTERED-DATA"))

(defpackage "C"
  (:nicknames "COMPILER")
  (:use "FFI" "CL" #+threads "MP" "C-BACKEND" "C-DATA")
  (:export "*COMPILER-BREAK-ENABLE*"
	   "*COMPILE-PRINT*"
	   "*COMPILE-TO-LINKING-CALL*"
	   "*COMPILE-VERBOSE*"
	   "*CC*"
	   "*CC-OPTIMIZE*"
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
	   "*SUPPRESS-COMPILER-WARNINGS*"
	   "*SUPPRESS-COMPILER-NOTES*"
	   "*SUPPRESS-COMPILER-MESSAGES*")
  (:import-from "SI" "GET-SYSPROP" "PUT-SYSPROP" "REM-SYSPROP" "MACRO"
		"*COMPILER-CONSTANTS*" "REGISTER-GLOBAL" "CMP-ENV-REGISTER-MACROLET"
		"COMPILER-LET"))

