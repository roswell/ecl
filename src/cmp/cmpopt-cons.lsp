;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
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
  `(ffi:c-inline ,(if (policy-assume-right-type env)
                      values
                      (loop for v in values
                         for value-and-type in arg-types
                         collect (if (consp value-and-type)
                                     `(assert-type-if-known ,v ,(second value-and-type))
                                     v)))
                 ,@inline-form))

(defun simple-optimizer-function (name args inline-form)
  (si::put-sysprop
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

(defmacro cons-car (x)
  `(ffi:c-inline (,x) (:object) :object "ECL_CONS_CAR(#0)"
                 :one-liner t :side-effects nil))

(defmacro cons-cdr (x)
  `(ffi:c-inline (,x) (:object) :object "ECL_CONS_CDR(#0)"
                 :one-liner t :side-effects nil))
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
