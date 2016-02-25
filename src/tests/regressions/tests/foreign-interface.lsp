;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Author:   Daniel Kochmański
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Foreign Function Interface regression tests

(in-package :cl-test)

;;; Date: 23/03/2006
;;; From: Klaus Falb
;;; Fixed: 26/02/2006 (juanjo)
;;; Description:
;;;
;;;     Callback functions have to be declared static so that there
;;;     are no conflicts among callbacks in different files.
;;;
;;; Fixed: 13/04/2006 (juanjo)
;;; Description:
;;;
;;;     Header <internal.h> should be included as <ecl/internal.h>
;;;

(deftest foreign-interface.0001.callback
    (and
      (zerop (si::system "rm -rf tmp; mkdir tmp"))
      (with-open-file (s "tmp/a.lsp" :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
        (print '(ffi:defcallback foo :void () nil) s))
      (with-open-file (s "tmp/b.lsp" :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
        (print '(ffi:defcallback foo :void () nil) s))
      (compile-file "tmp/a.lsp" :system-p t)
      (compile-file "tmp/b.lsp" :system-p t)
      (c:build-program "tmp/foo" :lisp-files
                       (list (compile-file-pathname "tmp/a.lsp" :type :object)
                             (compile-file-pathname "tmp/b.lsp" :type :object)))
      (probe-file (compile-file-pathname "tmp/foo" :type :program))
      (zerop (si::system "rm -rf tmp"))
      t)
  t)

;;; Date: 29/07/2008
;;; From: Juajo
;;; Description:
;;;     Callback examples based on the C compiler
;;;
(deftest foreign-interface.0002.callback
    (and
     (zerop (si::system "rm -rf tmp; mkdir tmp"))
     (with-open-file (s "tmp/c.lsp" :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
       (print
        '(defun callback-user (callback arg)
          (ffi:c-inline (callback arg) (:pointer-void :int) :int "
int (*foo)(int) = #0;
@(return) = foo(#1);
"
           :one-liner nil :side-effects nil))
        s)
       (print
        '(ffi:defcallback ffi-002-foo :int ((a :int))
          (1+ a))
        s))
     (compile-file "tmp/c.lsp" :load t)
     (eql (callback-user (ffi:callback 'ffi-002-foo) 2) 3)
     t)
  t)

;;; Date: 29/07/2008
;;; From: Juajo
;;; Description:
;;;     Callback examples based on the DFFI. Only work if this feature
;;;     has been linked in.
;;;
#+dffi
(deftest foreign-interface.0003.callback
    (and
     (zerop (si::system "rm -rf tmp; mkdir tmp"))
     (with-open-file (s "tmp/c.lsp" :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
       (print
        '(defun callback-user (callback arg)
          (ffi:c-inline (callback arg) (:pointer-void :int) :int "
int (*foo)(int) = #0;
@(return) = foo(#1);
"
           :one-liner nil :side-effects nil))
        s))
     (compile-file "tmp/c.lsp" :load t)
     (eval '(ffi:defcallback foo-002b :int ((a :int))
             (1+ a)))
     (eql (callback-user (ffi:callback 'foo-002b) 2) 3)
     t)
  t)

;;; Date: 25/04/2010 (Juanjo)
;;; Description:
;;;      Regression test to ensure that two foreign data compare
;;;      EQUAL when their addresses are the same.
(deftest foreign-interface.0004.foreign-data-equal
    (equal (ffi:make-pointer 1234 :void)
           (ffi:make-pointer 1234 :int))
  t)

;;; Date: 2016-01-04 (jackdaniel)
;;; Description:
;;;     Regression test to ensure, that the string is properly
;;;     recognized as an array
(deftest foreign-interface.0004
    (progn
      (si::make-foreign-data-from-array "dan")
      t)
  t)
