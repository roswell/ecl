;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;; CMPNUM -- Optimizer for numerical expressions.

;;;;  Copyright (c) 2005, Juan Jose Garcia Ripoll
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defun simplify-arithmetic (operator args whole)
  (if (every #'numberp args)
      (apply operator args)
      (let ((l (length args)))
        (cond ((> l 2)
               (simplify-arithmetic
                operator
                (list* (simplify-arithmetic operator
                                            (list (first args) (second args))
                                            nil)
                       (cddr args))
                nil))
              ((= l 2)
               (or whole (list* operator args)))
              ((= l 1)
               (if (or (eq operator '*) (eq operator '+))
                   (first args)
                   (or whole (list* operator args))))
              ((eq operator '*)
               1)
              ((eq operator '+)
               0)
              (t
               (error 'simple-program-error
                      :format-error "Wrong number of arguments for operator ~a in ~a"
                      :format-arguments (list operators (or whole
                                                            (list* operator args)))))))))

(define-compiler-macro * (&whole all &rest args)
  (simplify-arithmetic '* args all))

(define-compiler-macro + (&whole all &rest args)
  (simplify-arithmetic '+ args all))

(define-compiler-macro / (&whole all &rest args)
  (simplify-arithmetic '/ args all))

(define-compiler-macro - (&whole all &rest args)
  (simplify-arithmetic '- args all))

;;;
;;; The following are type propagators for arithmetic operations. Note
;;; that some of they have become binary operators.
;;;

(defun maximum-number-type (t1 t2)
  (let ((t1-eq nil)
        (t2-eq nil)
        (output nil))
    (dolist (i '(FIXNUM INTEGER RATIONAL
                 #+short-float SHORT-FLOAT SINGLE-FLOAT
                 DOUBLE-FLOAT #+long-float LONG-FLOAT FLOAT REAL
                 COMPLEX NUMBER)
             (values (if (and t1-eq t2-eq output)
                         output
                         t)
                     t1 t2))
      (when (and (null t1-eq) (type>= i t1))
        (if (equalp t1 t2)
            (setf t2-eq i))
        (setf t1-eq i output i))
      (when (and (null t2-eq) (type>= i t2))
        (setf t2-eq i output i)))))

(defun prod/plus-propagator (op1 op2)
  (multiple-value-bind (result-type op1-type op2-type)
        (maximum-number-type (c1form-primary-type op1)
                             (c1form-primary-type op2))
    (when (and (eq op1-type 'FIXNUM)
               (eq op2-type 'FIXNUM))
      (setf result-type 'INTEGER))
    (values (list op1-type op2-type) result-type)))

(def-type-propagator * (fname op1 op2)
  (prod/plus-propagator op1 op2))

(def-type-propagator + (fname op1 op2)
  (prod/plus-propagator op1 op2))

(def-type-propagator - (fname op1 &optional (op2 nil op2-p))
  (let* ((t1 (c1form-primary-type op1))
         (t2 (if op2-p (c1form-primary-type op2) t1)))
    (values (if op2-p (list t1 t2) (list t1))
            (maximum-number-type t1 t2))))

(def-type-propagator / (fname op1 &optional (op2 nil op2-p))
  (multiple-value-bind (output t1 t2)
        (let ((t1 (c1form-primary-type op1)))
          (if op2-p
              (maximum-number-type t1 (c1form-primary-type op2))
              (maximum-number-type 'FIXNUM t1)))
    (when (and (member t1 '(FIXNUM INTEGER))
               (member t2 '(FIXNUM INTEGER)))
      (setf output 'RATIONAL))
    (values (if op2-p (list t1 t2) (list t1))
            output)))
