;;;; -*- encoding:utf-8; Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Vladimir Sedach <vas@oneofus.la>
;;;; Created:  2020-05-03
;;;; Contains: Unicode handling in the compiler and runtime

(in-package :cl-test)

(suite 'unicode)

;;; Date: 2020-05-03
;;; From: Vladimir Sedach <vas@oneofus.la>
;;; Fixed: 2020-05-03 (Vladimir Sedach)
;;; Description:
;;;
;;;     Compiler does not handle non-ASCII symbols correctly when
;;;     READTABLE-CASE is :INVERT
;;;
(test unicode.0001.compiler-unicode-inverted-case
  (let ((test-readtable (copy-readtable)))
    (dolist (case '(:invert :upcase :downcase :preserve))
      (setf (readtable-case test-readtable) case)
      (let ((*readtable* test-readtable))
        (is (= 3
               (funcall
                (compile nil
                         (cons 'lambda
                               (read-from-string "((𝛅 𝛜) (+ 𝛅 𝛜))")))
                1 2)))))))

;;; Date: 2020-05-03
;;; From: Vladimir Sedach <vas@oneofus.la>
;;; Fixed: 2020-05-03 (Vladimir Sedach)
;;; Description:
;;;
;;;     Pathname :common case conversion fails on Unicode pathnames
;;;
(test unicode.0002.pathname-common-unicode
  (is (equal
       "ДАННЫЕ"
       (pathname-name (pathname "/tmp/данные.txt") :case :common))))
