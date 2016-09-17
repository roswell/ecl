;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Daniel Kochmański
;;;; Created:  2016-09-07
;;;; Contains: External process interaction API
;;;;

(in-package :cl-test)

(suite 'features/eprocess)

(test external-process.0001.run-program/wait/terminate
  (let ((p (nth-value 2 (ext:run-program #-windows "sleep"
                                         #+windows "timeout"
                                         (list "3") :wait nil))))
    (is (eql :running (ext:external-process-wait p nil))
        "process doesn't run")
    (ext:terminate-process p)
    (sleep 1)
    (multiple-value-bind (status code)
        (ext:external-process-wait p nil)
      (is (eql :signaled status)
          "status is ~s, should be ~s" status :signalled)
      (is (eql ext:+sigterm+ code)
          "signal code is ~s, should be ~s" code ext:+sigterm+))
    (finishes (ext:terminate-process p))))
