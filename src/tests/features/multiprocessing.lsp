;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Daniel Kochmański
;;;; Created:  2016-10-05
;;;; Contains: Multithreading API tests

(in-package :cl-test)
(suite 'features/mp)
;;; Date: 2016-10-05
;;; From: Daniel Kochmański
;;; Description:
;;;
;;;     HOLDING-LOCK-P verifies, if the current process holds the
;;;     lock.
;;;
(test mp-holding-lock-p
  (let ((lock (mp:make-lock :name "mp-holding-lock-p" :recursive nil)))
    (is-false (mp:holding-lock-p lock))
    (mp:with-lock (lock)
      (is-true (mp:holding-lock-p lock))
      (mp:process-run-function
       "mp-holding-lock-p"
       #'(lambda () (is-false (mp:holding-lock-p lock)))))
    (is-false (mp:holding-lock-p lock))
    (mp:process-run-function
     "mp-holding-lock-p"
     #'(lambda () (is-false (mp:holding-lock-p lock))))))
