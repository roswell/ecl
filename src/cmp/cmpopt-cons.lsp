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

(define-compiler-macro nth (&whole whole &rest args)
  (if (and (not (endp args))
	   (not (endp (cdr args)))
	   (endp (cddr args))
	   (numberp (car args))
	   (<= 0 (car args) 7))
      (case (car args)
	(0 (cons 'CAR (cdr args)))
	(1 (cons 'CADR (cdr args)))
	(2 (cons 'CADDR (cdr args)))
	(3 (cons 'CADDDR (cdr args)))
	(4 (list 'CAR (cons 'CDDDDR (cdr args))))
	(5 (list 'CADR (cons 'CDDDDR (cdr args))))
	(6 (list 'CADDR (cons 'CDDDDR (cdr args))))
	(7 (list 'CADDDR (cons 'CDDDDR (cdr args))))
	(t whole))
      whole))

(define-compiler-macro nthcdr (&whole whole &rest args)
  (if (and (not (endp args))
	   (not (endp (cdr args)))
	   (endp (cddr args))
	   (numberp (car args))
	   (<= 0 (car args) 7))
      (case (car args)
	(0 (second args))
	(1 (cons 'CDR (cdr args)))
	(2 (cons 'CDDR (cdr args)))
	(3 (cons 'CDDDR (cdr args)))
	(4 (cons 'CDDDDR (cdr args)))
	(5 (list 'CDR (cons 'CDDDDR (cdr args))))
	(6 (list 'CDDR (cons 'CDDDDR (cdr args))))
	(7 (list 'CDDDR (cons 'CDDDDR (cdr args))))
	(t whole))
      whole))

