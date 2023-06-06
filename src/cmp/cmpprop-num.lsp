;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;  Copyright (c) 2010, Juan Jose Garcia Ripoll
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;; Type propagators for numerical expressions.

(in-package "COMPILER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TYPE PROPAGATION
;;;

(def-type-propagator logand (fname &rest args)
  (values args
          (if args
              (dolist (int-type '((UNSIGNED-BYTE 8) FIXNUM) 'integer)
                (when (loop for value in args
                         always (subtypep value int-type))
                  (return int-type)))
              'fixnum)))

;;;
;;; The following are type propagators for arithmetic operations. Note
;;; that some of they have become binary operators.
;;;

(defun maximum-number-type (type1 type2 &key only-real integer-result)
  ;; Computes the output type of an operation between number types T1
  ;; and T2 using the rules of floating point contagion. It returns
  ;; the type of the result, and the types of T1 and T2, if they
  ;; represent known types, or NUMBER, in other cases.
  (let ((t1-eq nil)
        (t2-eq nil)
        (t1 type1)
        (t2 type2)
        (output nil)
        (complex-t1 nil)
        (complex-t2 nil)
        (default (if only-real 'REAL 'NUMBER))
        (number-types #(FIXNUM INTEGER RATIONAL SINGLE-FLOAT
                        DOUBLE-FLOAT LONG-FLOAT FLOAT REAL)))
    (when (and (consp t1) (eq (first t1) 'COMPLEX))
      (setf t1 (second t1) complex-t1 t))
    (when (and (consp t2) (eq (first t2) 'COMPLEX))
      (setf t2 (second t2) complex-t2 t))
    (when (and only-real (or complex-t1 complex-t2))
      (return-from maximum-number-type (values default default default)))
    (loop for i across number-types
          do (when (and (null t1-eq) (type>= i t1))
               (when (equalp t1 t2)
                 (setf t2-eq i))
               (setf t1-eq i output i))
             (when (and (null t2-eq) (type>= i t2))
               (setf t2-eq i output i)))
    (unless (and t1-eq t2-eq output)
      (setf output default))
    (when (and integer-result (or (eq output 'FIXNUM) (eq output 'INTEGER)))
      (setf output integer-result))
    (when (and (or complex-t1 complex-t2) (not (eq output 'NUMBER)))
      (setf output (if (eq output 'REAL) 'COMPLEX `(COMPLEX ,output))))
    (values output (if t1-eq type1 default) (if t2-eq type2 default))))

(defun ensure-number-type (general-type &key integer-result)
  (maximum-number-type general-type general-type :integer-result integer-result))

(defun ensure-nonrational-type (general-type)
  (maximum-number-type general-type 'single-float))

(defun ensure-real-type (general-type)
  (maximum-number-type general-type 'integer :only-real t))

(defun arithmetic-propagator (op1-type others integer-result)
  ;; Propagates types for an associative operator (we do not care which one).
  ;; We collect either the types of the arguments or 'NUMBER, as a generic
  ;; expected type. The output type is computed using the rules of floating
  ;; point contagion, with the exception that an operation between two
  ;; integers has type INTEGER-RESULT (integer for *,-,+ and rational else)
  (multiple-value-bind (result-type op1-type)
      (ensure-number-type op1-type :integer-result integer-result)
    (loop with arg-types = (list op1-type)
       for x in others
       for op2-type = x
       do (progn
            (multiple-value-setq (result-type op1-type op2-type)
              (maximum-number-type result-type op2-type :integer-result integer-result))
            (setf arg-types (cons op2-type arg-types)))
       finally (return (values (nreverse arg-types) result-type)))))

(def-type-propagator * (fname op1 &rest others)
  (arithmetic-propagator op1 others 'integer))

(copy-type-propagator '* '(+ -))

(def-type-propagator / (fname op1 &rest others)
  (arithmetic-propagator op1 others 'rational))

;;;
;;; SPECIAL FUNCTIONS
;;;

(def-type-propagator cos (fname op1-type)
  (multiple-value-bind (output-type op1-type)
      (ensure-nonrational-type op1-type)
    (values (list op1-type) output-type)))

(copy-type-propagator 'cos '(sin tan cosh sinh tanh exp))

(def-type-propagator acos (fname op1-type)
  (multiple-value-bind (output-type op1-type)
      (ensure-nonrational-type op1-type)
    (declare (ignore output-type))
    (values (list op1-type) 'NUMBER)))

(def-type-propagator atan (fname op1-type &optional (op2-type t op2-p))
  (multiple-value-bind (float-t1 t1)
      (ensure-nonrational-type op1-type)
    (if op2-p
        (multiple-value-bind (result t1 t2)
            (maximum-number-type t1 op2-type :only-real t)
          (values (list t1 t2) result))
        (values (list t1) float-t1))))

(def-type-propagator expt (fname base exponent)
  ;; Rules:
  ;;    (expt fixnum integer) -> integer
  ;;    (expt number-type integer) -> number-type
  ;;    (expt number-type1 number-type2) -> (max-float number-type1 number-type2)
  ;;
  (let ((exponent (ensure-real-type exponent)))
    (values (list base exponent)
            (cond ((eql exponent 'integer)
                   (if (subtypep base 'fixnum)
                       'integer
                       base))
                  ((type>= '(real 0 *) base)
                   (let* ((exponent (ensure-nonrational-type exponent)))
                     (maximum-number-type exponent base)))
                  (t
                   'number)))))

(def-type-propagator abs (fname arg)
  (multiple-value-bind (output arg)
      (ensure-number-type arg)
    (values (list arg)
            (or (cdr (assoc output
                            '((FIXNUM . (INTEGER 0 #.MOST-POSITIVE-FIXNUM))
                              (INTEGER . (INTEGER 0 *))
                              (RATIONAL . (RATIONAL 0 *))
                              (SHORT-FLOAT . (SHORT-FLOAT 0 *))
                              (SINGLE-FLOAT . (SINGLE-FLOAT 0 *))
                              (DOUBLE-FLOAT . (DOUBLE-FLOAT 0 *))
                              (LONG-FLOAT . (LONG-FLOAT 0 *))
                              (REAL . (REAL 0 *))
                              (NUMBER . (REAL 0 *)))))
                output))))

(def-type-propagator sqrt (fname arg)
  (multiple-value-bind (output arg)
      (ensure-nonrational-type arg)
    (values (list arg)
            (if (type>= '(REAL 0 *) arg) output 'NUMBER))))

(def-type-propagator isqrt (fname arg)
  (if (type>= '(integer 0 #.MOST-POSITIVE-FIXNUM) arg)
      (values '((integer 0 #.MOST-POSITIVE-FIXNUM))
              '(integer 0 #.MOST-POSITIVE-FIXNUM))
      (values '((integer 0 *)) '(integer 0 *))))
