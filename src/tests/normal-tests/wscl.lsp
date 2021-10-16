;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

(in-package :cl-test)

(suite 'wscl)


;;;; Author:   Tarn W. Burton
;;;; Created:  2021-10-02
;;;; Contains: type-errors for condition readers
(test wscl.0001.condition-readers-type-error
  (signals type-error (arithmetic-error-operands 234))
  (signals type-error (arithmetic-error-operation 234))
  (signals type-error (cell-error-name 234))
  (signals type-error (file-error-pathname 234))
  (signals type-error (package-error-package 234))
  (signals type-error (print-not-readable-object 234))
  (signals type-error (simple-condition-format-arguments 234))
  (signals type-error (simple-condition-format-control 234))
  (signals type-error (stream-error-stream 234))
  (signals type-error (type-error-datum 234))
  (signals type-error (type-error-expected-type 234))
  (signals type-error (unbound-slot-instance 234)))

