;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  CMPOPT-CONS  Optimization of CONS functions
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defun expand-simple-optimizer (values arg-types inline-form env)
  (declare (si::c-local))
  `(ffi:c-inline ,(if (policy-assume-right-type env)
                      values
                      (loop for v in values
                         for value-and-type in arg-types
                         collect (if (consp value-and-type)
                                     `(ext:checked-value ,(second value-and-type) ,v)
                                     v)))
                 ,@inline-form))

(defun simple-optimizer-function (name args inline-form)
  (declare (si::c-local))
  (si:put-sysprop
   name 'si::compiler-macro
   (if (every #'symbolp args)
       #'(lambda (whole env)
           (if (policy-inline-accessors env)
               `(ffi:c-inline ,(rest whole) ,@inline-form)
               whole))
       #'(lambda (whole env)
           (if (policy-inline-accessors env)
               (expand-simple-optimizer (rest whole) args inline-form env)
               whole)))))

(defmacro si:cons-car (x)
  `(ffi:c-inline (,x) (:object) :object "ECL_CONS_CAR(#0)"
                 :one-liner t :side-effects nil))

(defmacro si:cons-cdr (x)
  `(ffi:c-inline (,x) (:object) :object "ECL_CONS_CDR(#0)"
                 :one-liner t :side-effects nil))
;;;
;;; CONS
;;; turn repetitious cons's into a list*
;;;

(define-compiler-macro cons (&whole whole &rest args)
  (labels ((cons-to-lista (x)
             (let ((tem (last x)))
               (if (and (consp tem)
                        (consp (car tem))
                        (eq (caar tem) 'CONS)
                        (eql (length (cdar tem)) 2))
                   (cons-to-lista (append (butlast x) (cdar tem)))
                   x))))
    (let (temp)
      (if (and (eql (length args) 2)
               (not (eq args (setq temp (cons-to-lista args)))))
          (if (equal '(nil) (last temp))
              (cons 'LIST (butlast temp))
              (cons 'LIST* temp))
          whole))))

;;;
;;; RPLACA / RPLACD
;;;

(defmacro define-simple-optimizer (name args &rest inline-form)
  `(simple-optimizer-function ',name ',args ',inline-form))

(define-simple-optimizer rplaca ((c cons) value)
  (:object :object) :object
  "@0;(ECL_CONS_CAR(#0)=#1,#0)" :one-liner t)

(define-simple-optimizer rplacd ((c cons) value)
  (:object :object) :object
  "@0;(ECL_CONS_CDR(#0)=#1,#0)" :one-liner t)

;;;
;;; NTH / NTHCDR
;;;

(define-compiler-macro nth (&whole whole n list)
  (case n
    (0 `(car ,list))
    (1 `(cadr ,list))
    (2 `(caddr ,list))
    (3 `(cadddr ,list))
    (4 `(car (cddddr ,list)))
    (5 `(cadr (cddddr ,list)))
    (6 `(caddr (cddddr ,list)))
    (7 `(cadddr (cddddr ,list)))
    (t whole)))

(define-compiler-macro nthcdr (&whole whole n list)
  (case n
    (0 list)
    (1 `(cdr ,list))
    (2 `(cddr ,list))
    (3 `(cdddr ,list))
    (4 `(cddddr ,list))
    (5 `(cdr (cddddr ,list)))
    (6 `(cddr (cddddr ,list)))
    (7 `(cdddr (cddddr ,list)))
    (t whole)))

;;;
;;; FIRST, SECOND, THIRD, ...
;;;

(progn .
  #.(loop for n in '(first second third fourth fifth sixth seventh eighth ninth tenth)
        for i from 0
        collect `(define-compiler-macro ,n (x) (list 'nth ,i x))))

(define-compiler-macro rest (x) `(cdr ,x))

;;;
;;; POP
;;;

(define-compiler-macro pop (&whole whole place &environment env)
  (if (policy-inline-accessors)
      (multiple-value-bind (vars vals stores store-form access-form)
          (get-setf-expansion place env)
        (let* ((store-var (first stores))
               (saved-place (gensym)))
          `(let* ,(mapcar #'list
                          (append vars (list saved-place))
                          (append vals (list access-form)))
             (declare (:read-only ,@vars)) ; Beppe
             (optional-type-check ,saved-place list)
             (when ,saved-place
               (let ((,store-var (si:cons-cdr ,saved-place)))
                 (declare (:read-only ,store-var))
                 ,store-form
                 (setq ,saved-place (si:cons-car ,saved-place))))
             ,saved-place)))
    whole))
