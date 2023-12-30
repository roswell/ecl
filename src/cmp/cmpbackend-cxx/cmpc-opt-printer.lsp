;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;; C/C++ specific optimizer for the printer.

(in-package "COMPILER")

(define-c-inliner cl:princ (return-type expression &optional stream)
  (unless stream
    (setf stream (emit-inline-form (c1nil) nil)))
  (multiple-value-bind (foundp value)
      (loc-immediate-value-p (inlined-arg-loc expression))
    (cond
      ((and foundp (characterp value))
       (produce-inline-loc (list expression stream)
                           '(:wchar :object) '(:wchar)
                           "ecl_princ_char(#0,#1)" t t))
      ((and foundp (typep value 'base-string) (< (length value) 80))
       (produce-inline-loc (list expression stream)
                           '(:object :object) '(:object)
                           (concatenate 'string "(ecl_princ_str("
                                        (c-inline-safe-string value)
                                        ",#1),#0)")
                           t t))
      (t
       (produce-inline-loc (list expression stream)
                           '(:object :object) '(:object)
                           "ecl_princ(#0,#1)" t t)))))
