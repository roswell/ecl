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

(in-package "COMPILER")

;;;
;;; and-form-type
;;;   returns a copy of form whose type is the type-and of type and the form's
;;;   type
;;;
(defun and-form-type (type form original-form &optional (mode :safe)
		      (format-string "") &rest format-args)
  (let* ((type2 (c1form-primary-type form))
	 (type1 (type-and type type2)))
    ;; We only change the type if it is not NIL. Is this wise?
    (if type1
	(setf (c1form-type form) type1)
	(funcall (if (eq mode :safe) #'cmperr #'cmpwarn)
		 "~?, the type of the form ~s is ~s, not ~s." format-string
		 format-args original-form type2 type))
    form))

(defun default-init (var &optional warn)
  (let ((new-value (cdr (assoc (var-type var)
			       '((fixnum . 0) (character . #\space)
                                 #+long-float (long-float 0.0L1)
				 (double-float . 0.0D1) (single-float . 0.0F1))
			       :test #'subtypep))))
    (if new-value
	(c1constant-value new-value :only-small-values t)
        (c1nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TYPE CHECKING
;;

(defun extract-lambda-type-checks (requireds optionals keywords ts other-decls)
  ;; We generate automatic type checks for function arguments that
  ;; are declared These checks can be deactivated by appropriate
  ;; safety settings which are checked by ASSERT-TYPE. Note
  ;; that not all type declarations can be checked (take for instance
  ;; (type (function (t t) t) foo)) We let the macro do the job.
  (when (policy-check-arguments-type)
    (loop with type-checks = (append (loop for spec on optionals by #'cdddr
                                        collect (first spec))
                                     (loop for spec on keywords by #'cddddr
                                        collect (second spec))
                                     requireds)
       for var in type-checks
       for name = (var-name var)
       for type = (cdr (assoc name ts))
       for checked = (and type
                          (loop for decl in other-decls
                             never (and (consp decl)
                                        (eq (first decl)
                                            'si::no-check-type)
                                        (member name (rest decl)))))
       when checked
       collect `(assert-type-if-known ,name ,type) into checks
       ;; We remove assumption about types, which will be checked later
       when checked
       do (setf (var-type var) T)
       ;; And we add additional variables with proclaimed types
       when (and checked (not (eq (var-kind var) 'SPECIAL)))
       nconc `(,name (the ,type ,name)) into new-auxs
       finally
         (progn
           (when checks
             (cmpnote "In ~:[an anonymous function~;function ~:*~A~], checking types of argument~@[s~]~{ ~A~}."
                      (fun-name *current-function*)
                      (mapcar #'second checks)))
           (return (cons checks new-auxs))))))

(defun type-error-check (value type)
  (case type
    (cons
     `(ffi:c-inline (,value) (:object) :void
        "@0;if (ecl_unlikely(ATOM(#0))) FEtype_error_cons(#0);"
        :one-liner nil))
    (array
     `(ffi:c-inline (,value) (:object) :void
        "if (ecl_unlikely(!ECL_ARRAYP(#0))) FEtype_error_array(#0);"
        :one-liner nil))
    (list
     `(ffi:c-inline (,value) (:object) :void
        "if (ecl_unlikely(!ECL_LISTP(#0))) FEtype_error_list(#0);"
        :one-liner nil))
    (sequence
     `(ffi:c-inline (,value) (:object) :void
        "if (ecl_unlikely(!(ECL_LISTP(#0) || ECL_VECTORP(#0))))
           FEtype_error_sequence(#0);"
        :one-liner nil))
    (otherwise
     `(ffi:c-inline
       ((typep ,value ',type) ',type ,value)
       (:bool :object :object) :void
       "if (ecl_unlikely(!(#0)))
         FEwrong_type_argument(#1,#2);" :one-liner nil))))

(defmacro assert-type-if-known (&whole whole value type &environment env)
  "Generates a type check on an expression, ensuring that it is satisfied."
  (multiple-value-bind (trivial valid)
      (subtypep 't type)
    (if (and trivial valid)
        value
        (with-clean-symbols (%value)
          `(let* ((%value ,value))
             ,(type-error-check '%value type)
             (the ,type %value))))))
