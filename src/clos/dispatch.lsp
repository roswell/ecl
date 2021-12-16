;;;;
;;;;  Copyright (c) 2021, Daniel Kochmański
;;;;  See file 'LICENSE' for the copyright details.
;;;;

;;;
;;; This file contains implementations of various attempts at the generic
;;; function dispatch.
;;;
(in-package #:clos)

;;; This discriminator function is included for the reference and benchmarks.
;;; It doesn't perform any optimizations and always recomputes everything.
;;;
;;; This function is similar to `generic_compute_applicable_method' in C.
(defun unoptimized-discriminator (gf)
  (lambda (&rest args)
    (multiple-value-bind (method-list ok)
        (compute-applicable-methods-using-classes gf (mapcar #'class-of args))
      (unless ok
        (setf method-list (compute-applicable-methods gf args))
        (unless method-list
          (apply #'no-applicable-method gf args)))
      (let* ((combin (generic-function-method-combination gf))
             (em-fun (compute-effective-method-function gf combin method-list)))
        (funcall em-fun args nil)))))

;;; This discriminator function is similar to UNOPTIMIZED-DISCRIMINATOR except
;;; that uses the fact that the function class is STANDARD-GENERIC-FUNCTION so
;;; it may use non-generic version of computation functions and it doesn't call
;;; COMPUTE-APPLICABLE-METHODS-USING-CLASSES[1]. Both behaviors are permissible.
;;;
;;; This function is similar to `restricted_compute_applicable_method' in C.
(defun standard-gf-discriminator (gf)
  (lambda (&rest args)
    (let ((method-list (std-compute-applicable-methods gf args)))
      (unless method-list
        (apply #'no-applicable-method gf args))
      (let* ((combin (generic-function-method-combination gf))
             (em-fun (compute-effective-method-function gf combin method-list)))
        (funcall em-fun args nil)))))
