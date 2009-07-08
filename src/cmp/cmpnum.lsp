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

(defun arithmetic-propagator (op1 others integer-result)
  (loop with op1-type = (c1form-primary-type op1)
     with result-type = (maximum-number-type op1-type op1-type)
     with arg-types = (list (setf op1-type result-type))
     for op2 in others
     for op2-type = (c1form-primary-type op2)
     do (progn
          (multiple-value-setq (result-type op1-type op2-type)
            (maximum-number-type op1-type op2-type))
          (when (and (or (eq op1-type 'FIXNUM) (eq op1-type 'INTEGER))
                     (or (eq op2-type 'FIXNUM) (eq op2-type 'INTEGER)))
            (setf result-type integer-result))
          (setf arg-types (cons op2-type arg-types)
                op1-type result-type))
     finally (return (values (nreverse arg-types) result-type))))

(def-type-propagator * (fname op1 &rest others)
  (arithmetic-propagator op1 others 'integer))

(def-type-propagator + (fname op1 &rest others)
  (arithmetic-propagator op1 others 'integer))

(def-type-propagator - (fname op1 &rest others)
  (arithmetic-propagator op1 others 'integer))

(def-type-propagator / (fname op1 &rest others)
  (arithmetic-propagator op1 others 'rational))
