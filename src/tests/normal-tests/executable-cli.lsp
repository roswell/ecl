;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Daniel Kochmański
;;;; Created:  2016-11-19
;;;; Contains: Calling ECL binary tests

(in-package :cl-test)

(suite 'executable)

(defvar *test-image*
  (or (ext:getenv "TEST_IMAGE")
      "/data/Warsztat/Repozytoria/ecl/otp/bin/ecl"))

(defun simple-run (input-string &optional output error)
  (when (null *test-image*)
    (return-from simple-run))

  (with-temporary-file (input-file input-string)
    (ext:run-program *test-image* '("-norc")
                     :input input-file
                     :output output
                     :error error
                     :if-output-exists :append
                     :if-error-exists :append)))


;;; Bug #43: Infinite loop when writing to stderr fails
;;; Reported by: Dmitri Pasechnik
;;; Date: 2015-05-06
#+unix ; Windows doesn't have /dev/...
(test executable.1.infinite-loop
  (is-eql 0 (nth-value 1 (simple-run "Invalid syntax"
                                     "/dev/null"
                                     "/dev/full"))))
