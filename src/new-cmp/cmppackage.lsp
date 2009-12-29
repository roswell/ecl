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

           "LOCATION-TYPE" "LOCATION-PRIMARY-TYPE"

           "PPRINT-C1FORM" "PPRINT-C1FORMS"
           ;;
           ;; Symbols naming possible locations
           ;;
           "TEMP" "LCL" "VV" "VV-TEMP" "TRASH"
           "FIXNUM-VALUE" "CHARACTER-VALUE"
           "LONG-FLOAT-VALUE" "DOUBLE-FLOAT-VALUE" "SINGLE-FLOAT-VALUE"
           "VALUE" "VALUE0" "VALUES+VALUE0" "RETURN" "ACTUAL-RETURN"
           "VA-ARG" "CL-VA-ARG" "KEYVARS"
           "CALL" "CALL-NORMAL" "CALL-INDIRECT"
           "COERCE-LOC"
           "FDEFINITION" "MAKE-CCLOSURE"
           "JMP-TRUE" "JMP-FALSE" "JMP-ZERO" "JMP-NONZERO"
           ;;
           ;; Symbols naming C1FORMS
           ;;
           "SET-MV" "BIND" "BIND-SPECIAL" "UNBIND" "PROGV-EXIT"
           "FRAME-POP" "FRAME-SET" "FRAME-SAVE-NEXT" "FRAME-JMP-NEXT"
           "FRAME-ID"
           "CALL-LOCAL" "CALL-GLOBAL" "JMP"
           "FUNCTION-EPILOGUE" "FUNCTION-PROLOGUE" "BIND-REQUIREDS"
           "VARARGS-BIND" "VARARGS-POP" "VARARGS-REST" "VARARGS-UNBIND"
           "STACK-FRAME-OPEN" "STACK-FRAME-PUSH" "STACK-FRAME-PUSH-VALUES"
           "STACK-FRAME-POP-VALUES" "STACK-FRAME-APPLY" "STACK-FRAME-CLOSE"
           "DEBUG-ENV-OPEN" "DEBUG-ENV-CLOSE" "DEBUG-ENV-PUSH-VARS"
           "DEBUG-ENV-POP-VARS"
           "DO-FLET/LABELS"
           )
  (:import-from "SI" "*COMPILER-CONSTANTS*"))

(defpackage "C-PASSES"
  (:use "FFI" "CL" "C-DATA")
  (:export "EXECUTE-PASS"
           "PASS-CONSISTENCY"
           "PASS-DELETE-NO-SIDE-EFFECTS"
           "PASS-ASSIGN-LABELS"
           "PASS-DELETE-UNUSED-BINDINGS"))

(defpackage "C-BACKEND"
  (:use "FFI" "CL" "C-DATA" "C-PASSES")
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

