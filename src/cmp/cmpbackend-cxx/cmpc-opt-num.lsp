;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;; C/C++ specific optimizer for numerical expressions.

(in-package "COMPILER")

;;;
;;; Bit fiddling. It is a bit tricky because C does not allow
;;; shifts in << or >> which exceed the integer size. In those
;;; cases the compiler may do whatever it wants (and gcc does!)
;;;
(define-c-inliner shift (return-type argument orig-shift)
  (let* ((arg-type (inlined-arg-type argument))
         (arg-c-type (lisp-type->rep-type arg-type))
         (return-c-type (lisp-type->rep-type return-type))
         (shift (loc-immediate-value (inlined-arg-loc orig-shift))))
    (if (or (not (c-integer-rep-type-p arg-c-type))
            (not (c-integer-rep-type-p return-c-type)))
        (produce-inline-loc (list argument orig-shift) '(:object :fixnum) '(:object)
                            "ecl_ash(#0,#1)" nil t)
        (let* ((arg-bits (c-integer-rep-type-bits arg-c-type))
               (return-bits (c-integer-rep-type-bits return-c-type))
               (max-type (if (and (plusp shift)
                                  (< arg-bits return-bits))
                             return-c-type
                             arg-c-type)))
          (produce-inline-loc (list argument) (list max-type) (list return-type)
                              (format nil
                                      (if (minusp shift)
                                          "((#0) >> (~D))"
                                          "((#0) << (~D))")
                                      (abs shift))
                              nil t)))))

;;;
;;; Inliners for arithmetic operations.
;;;

(defun most-generic-number-rep-type (r1 r2)
  (let* ((r1 (rep-type-record r1))
         (r2 (rep-type-record r2)))
    (rep-type-name (if (< (rep-type-index r1) (rep-type-index r2))
                       r2
                       r1))))

(defun inline-binop (expected-type arg1 arg2 consing non-consing)
  (let ((arg1-type (inlined-arg-type arg1))
        (arg2-type (inlined-arg-type arg2)))
    (if (and (policy-assume-right-type)
             (c-number-type-p expected-type)
             (c-number-type-p arg1-type)
             (c-number-type-p arg2-type))
        ;; The input arguments have to be coerced to a C
        ;; type that fits the output, to avoid overflow which
        ;; would happen if we used say, long c = (int)a * (int)b
        ;; as the output would be an integer, not a long.
        (let* ((arg1-rep (lisp-type->rep-type arg1-type))
               (arg2-rep (lisp-type->rep-type arg2-type))
               (out-rep (lisp-type->rep-type expected-type))
               (max-rep (most-generic-number-rep-type
                         (most-generic-number-rep-type
                          arg1-rep arg2-rep) out-rep))
               (max-name (rep-type->c-name max-rep)))
          (produce-inline-loc
           (list arg1 arg2)
           (list arg1-rep arg2-rep)
           (list max-rep)
           (format nil "(~@[(~A)~]#0)~A(~@[(~A)~]#1)"
                   (unless (eq arg1-rep max-rep) max-name)
                   non-consing
                   (unless (eq arg2-rep max-rep) max-name))
           nil t))
        (produce-inline-loc (list arg1 arg2) '(:object :object) '(:object)
                            consing nil t))))

(defun inline-arith-unop (expected-type arg1 consing non-consing)
  (let ((arg1-type (inlined-arg-type arg1)))
    (if (and (policy-assume-right-type)
             (c-number-type-p expected-type)
             (c-number-type-p arg1-type))
        (produce-inline-loc (list arg1)
                            (list (lisp-type->rep-type arg1-type))
                            (list (lisp-type->rep-type expected-type))
                            non-consing nil t)
        (produce-inline-loc (list arg1) '(:object :object) '(:object)
                            consing nil t))))

(define-c-inliner * (return-type &rest arguments &aux arg1 arg2)
  (when (null arguments)
    (return (make-vv :rep-type :fixnum :value 1)))
  (setf arg1 (pop arguments))
  (when (null arguments)
    (return (inlined-arg-loc arg1)))
  (setf arg2 (pop arguments))
  (when (null arguments)
    (return (inline-binop return-type arg1 arg2 "ecl_times(#0,#1)" #\*)))
  (cmperr "The C inliner for (FUNCTION *) expected at most 2 arguments."))

(define-c-inliner + (return-type &rest arguments &aux arg1 arg2)
  (when (null arguments)
    (return (make-vv :rep-type :fixnum :value 0)))
  (setf arg1 (pop arguments))
  (when (null arguments)
    (return (inlined-arg-loc arg1)))
  (setf arg2 (pop arguments))
  (when (null arguments)
    (return (inline-binop return-type arg1 arg2 "ecl_plus(#0,#1)" #\+)))
  (cmperr "The C inliner for (FUNCTION +) expected at most 2 arguments."))

(define-c-inliner - (return-type arg1 &rest arguments &aux arg2)
  (when (null arguments)
    (return (inline-arith-unop return-type arg1 "ecl_negate(#0)" "-(#0)")))
  (setf arg2 (pop arguments))
  (when (null arguments)
    (return (inline-binop return-type arg1 arg2 "ecl_minus(#0,#1)" #\-)))
  (cmperr "The C inliner for (FUNCTION -) expected at most 2 arguments."))

(define-c-inliner / (return-type arg1 &rest arguments &aux arg2)
  (when (null arguments)
    (return (inline-arith-unop return-type arg1
                               "ecl_divide(ecl_make_fixnum(1),(#0))" "1/(#0)")))
  (setf arg2 (pop arguments))
  (when (null arguments)
    (return (inline-binop return-type arg1 arg2 "ecl_divide(#0,#1)" #\/)))
  (cmperr "The C inliner for (FUNCTION /) expected at most 2 arguments."))

(define-c-inliner float (return-type arg &optional float)
  (let ((arg-c-type (lisp-type->rep-type (inlined-arg-type arg)))
        (flt-c-type (and float (lisp-type->rep-type (inlined-arg-type float)))))
    (if (member arg-c-type '(:float :double :long-double))
        (when (or (null float) (eq arg-c-type flt-c-type))
          (inlined-arg-loc arg))
        (when (member flt-c-type '(:float :double :long-double))
          (produce-inline-loc (list arg)
                              (list :object)
                              (list flt-c-type)
                              (ecase flt-c-type
                                (:float       "ecl_to_float(#0)")
                                (:double      "ecl_to_double(#0)")
                                (:long-double "ecl_to_long_double(#0)"))
                              nil t)))))
