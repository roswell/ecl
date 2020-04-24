;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Author:   Daniel Kochmański
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Foreign Function Interface regression tests

(in-package :cl-test)
(suite 'ffi)

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
#-ecl-bytecmp
(test ffi.0001.callback
  (is
   (and (ensure-directories-exist "tmp/")
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
        (probe-file (compile-file-pathname "tmp/foo" :type :program)))))

;;; Date: 29/07/2008
;;; From: Juajo
;;; Description:
;;;     Callback examples based on the C compiler
;;;
#-ecl-bytecmp
(test ffi.0002.callback-sffi-example
  (is
   (and (ensure-directories-exist "tmp/")
        (with-open-file (s "tmp/c.lsp" :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
          (print
           '(defun callback-user (callback arg)
             (ffi:c-inline (callback arg) (:pointer-void :int) :int "
int (*foo)(int) = (int (*)(int))#0;
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
        t)))

;;; Date: 29/07/2008
;;; From: Juajo
;;; Description:
;;;     Callback examples based on the DFFI. Only work if this feature
;;;     has been linked in.
;;;
#+(and (or) dffi (not ecl-bytecmp))
(test ffi.0003.callback-dffi-example
  (is
   (and (ensure-directories-exist "tmp/")
        (with-open-file (s "tmp/c.lsp" :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
          (print
           '(defun callback-user (callback arg)
             (ffi:c-inline (callback arg) (:pointer-void :int) :int "
int (*foo)(int) = (int (*)(int))#0;
@(return) = foo(#1);
"
              :one-liner nil :side-effects nil))
           s))
        (compile-file "tmp/c.lsp" :load t)
        (eval '(ffi:defcallback foo-002b :int ((a :int))
                (1+ a)))
        (eql (callback-user (ffi:callback 'foo-002b) 2) 3)
        t)))

;;; Date: 25/04/2010 (Juanjo)
;;; Description:
;;;      Regression test to ensure that two foreign data compare
;;;      EQUAL when their addresses are the same.
(test ffi.0004.foreign-data-equal
  (is
   (equal (ffi:make-pointer 1234 :void)
          (ffi:make-pointer 1234 :int))))

;;; Date: 2016-01-04 (jackdaniel)
;;; Description:
;;;     Regression test to ensure, that the string is properly
;;;     recognized as an array
(test ffi.0005.string-is-array
  (finishes
    (si::make-foreign-data-from-array "dan")))

;;; Date: 2019-12-16
;;; Description:
;;;     Regression test to ensure correct complex float handling by
;;;     the interface. On some platforms libffi is miscompiled to
;;;     mishandle complex float return values. See the commit message
;;;     in a commit ad5fe834.
#+complex-float
(defun ffi.0006.complex-floats ()
  ;; dffi
  (let* ((arg #C(10.0s0 0.5s0))
         (expect (atanh arg)))
    (finishes (ffi:def-function "catanhf" ((x :csfloat))
                :returning :csfloat
                :module :default))
    (is (= expect (catanhf arg))))
  ;; sffi
  #-ecl-bytecmp
  (let* ((arg #C(10.0s0 0.5s0))
         (expect (atanh arg)))
    (finishes (ffi:def-function "catanhf" ((x :csfloat))
                :returning :csfloat))
    (compile 'catanhf)
    (is (= expect (catanhf arg)))))
