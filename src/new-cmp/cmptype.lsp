;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPTYPE  Type information.

(in-package "C-TYPES")

;;;
;;; and-form-type
;;;   returns a copy of form whose type is the type-and of type and the form's
;;;   type
;;;
(defun and-form-type (type form original-form &optional (mode :safe)
		      (format-string "") &rest format-args)
  (let* ((type2 (location-primary-type form))
	 (type1 (type-and type type2)))
    ;; We only change the type if it is not NIL. Is this wise?
    (if type1
	(setf (location-type form) type1)
	(funcall (if (eq mode :safe) #'cmperr #'cmpwarn)
		 "~?, the type of the form ~s is ~s, not ~s." format-string
		 format-args original-form type2 type))
    form))

(defun default-init (var)
  (let ((new-value (cdr (assoc (var-type var)
			       '((fixnum . 0) (character . #\space)
                                 #+long-float (long-float 0.0L1)
				 (double-float . 0.0D1) (single-float . 0.0F1))
			       :test #'subtypep))))
    new-value))

(defun default-init-loc (var)
  (c1form-arg 0 (c1expr (default-init var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TYPE CHECKING
;;

(defun remove-function-types (type)
  ;; We replace this type by an approximate one that contains no function
  ;; types. This function may not produce the best approximation. Hence,
  ;; it is only used for optional type checks where we do not want to pass
  ;; TYPEP a complex type.
  (flet ((simplify-type (type)
           (cond ((subtypep type '(NOT FUNCTION))
                  type)
                 ((subtypep type 'FUNCTION)
                  'FUNCTION)
                 (t
                  T))))
    (if (atom type)
        (simplify-type type)
        (case (first type)
          ((OR AND NOT)
           (cons (first type)
                 (loop for i in (rest type) collect (remove-function-types i))))
          (FUNCTION 'FUNCTION)
          (otherwise (simplify-type type))))))

(defmacro optional-check-type (&whole whole var-name type &environment env)
  "Generates a type check that is only activated for the appropriate
safety settings and when the type is not trivial."
  (unless (policy-automatic-check-type-p env)
    (cmpnote "Unable to emit check for variable ~A" whole))
  (when (policy-automatic-check-type-p env)
    (setf type (remove-function-types type))
    (multiple-value-bind (ok valid)
	(subtypep 't type)
      (unless (or ok (not valid))
	`(check-type ,var-name ,type)))))
