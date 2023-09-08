;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Daniel Kochmański
;;;; Created:  2023-01-19
;;;; Contains: CLOS tests

(in-package #:cl-test)

(suite 'clos)


;;; Method combination long form

;;; Check whether different types of wildcard patterns are correctly handled.
;;; Most notably the symbol * in a pattern matches any method qualifier on
;;; that position.
(ext:with-clean-symbols (combin gf)
  (test clos.0001.combin/wildcard
    (define-method-combination combin ()
      ((p1 (:wild :no))
       (p2 (:wild * *))
       (p3 (:wild . *))
       (pr *))
      `(list ,(length p1)
             ,(length p2)
             ,(length p3)
             ,(length pr)))
    (defgeneric gf ()
      (:method-combination combin))
    (finishes (defmethod gf :wild :no ()))   ; p1
    (finishes (defmethod gf :wild :x :y ())) ; p2
    (finishes (defmethod gf :wild ()))       ; p3
    (finishes (defmethod gf :wild :foobar ())) ; p3
    (finishes (defmethod gf :wild :no :a :b ())) ; p3
    (finishes (defmethod gf :xxx :yyy 34 23 ())) ; p4
    (is-equal '(1 1 3 1) (gf))))

;;; This test checks whether define-method-combination handles arguments
;;; :GENERIC-FUNCTION and :ARGUMENTS &WHOLE ARGS.
(ext:with-clean-symbols (combin f1 f2)
  (test clos.0002.combin/arguments
    (define-method-combination combin ()
      ((method-list *))
      (:arguments &whole args)
      (:generic-function gf)
      `(list* ,gf ,args))
    (defgeneric f1 (a &key key-1)
      (:method-combination combin)
      (:method (a &key key-1 key-2)
        (declare (ignore a key-1 key-2))))
    (is-equal (list #'f1 1 :key-1 2) (f1 1 :key-1 2))))
