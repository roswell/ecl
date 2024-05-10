;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: CLOS -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    See file 'LICENSE' for the copyright details.

(pushnew :cdr-1 *features*)

(defpackage "CLOS"
  (:use "CL" "EXT")
  (:import-from "SI" "UNBOUND" "GET-SYSPROP" "PUT-SYSPROP" "REM-SYSPROP"
                "SIMPLE-PROGRAM-ERROR"))

