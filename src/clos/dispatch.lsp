;;;;
;;;;  Copyright (c) 2021, Daniel Kochmański
;;;;  See file 'LICENSE' for the copyright details.
;;;;

;;;
;;; This file contains implementations of various attempts at the generic
;;; function dispatch.
;;;
(in-package #:clos)

;;; This function is similar to `generic_compute_applicable_method' in C.
(defun generic-compute-applicable-method (gf args)
  (declare (si::c-local))
  (multiple-value-bind (method-list ok)
      (compute-applicable-methods-using-classes gf (mapcar #'class-of args))
    (unless ok
      (setf method-list (compute-applicable-methods gf args)))
    (and method-list
         (let ((combin (generic-function-method-combination gf)))
           (compute-effective-method-function gf combin method-list)))))

;;; This function is similar to `restricted_compute_applicable_method' in C.
(defun restricted-compute-applicable-method (gf args)
  (declare (si::c-local))
  (with-early-accessors (+standard-generic-function-slots+)
    (let ((method-list (std-compute-applicable-methods gf args)))
      (and method-list
           (let ((combin (generic-function-method-combination gf)))
             (compute-effective-method-function gf combin method-list))))))

;;; This function is similar to `compute_applicable_method' in C.
(defun compute-applicable-method (gf args)
  (declare (si::c-local))
  (if (eq (slot-value (class-of gf) 'name) 'standard-generic-function)
      (restricted-compute-applicable-method gf args)
      (generic-compute-applicable-method gf args)))

;;; This is an unoptimized discriminator function that doesn't cache results.
(defun unoptimized-discriminator (gf)
  (lambda (&rest args)
    (if-let ((fn (compute-applicable-method gf args)))
      (funcall fn args nil)
      (apply #'no-applicable-method gf args))))

;;; This discriminator is similar to the discriminator implemented in C. It is
;;; slower, incomplete and defined only for the purpose of benchmarking against
;;; other dispatch prototypes written in Lisp and to better understand C code.
;;;
;;; Dispatch is around 1x-4x slower than the "native" implementation and this
;;; code is reasonably simplified. FIXME accessor optimization is missing.
#+ (or)
(defun soft-legacy-discriminator (gf)
  (let ((method-cache (make-hash-table :test #'equal :synchronized t)))
    (lambda (&rest args)
      (let ((hash-key
              ;; GENERIC-FUNCTION-SPEC-LIST is maintained by the function
              ;; COMPUTE-G-F-SPEC-LIST called on discriminator invalidation.
              (loop for arg in args
                    for (spec-class . spec-eql) in (generic-function-spec-list gf)
                    if (member arg spec-eql)
                      collect `(eql ,arg) into hash-key
                    else
                      collect (class-of arg) into hash-key
                    finally (return (list* gf hash-key)))))
        (if-let ((fn (gethash hash-key method-cache)))
          (funcall fn args nil)
          (if-let ((fn (compute-applicable-method gf args)))
            (progn
              (setf (gethash hash-key method-cache) fn)
              (funcall fn args nil))
            (apply #'no-applicable-method gf args)))))))
