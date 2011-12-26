;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2011, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPTYPE-ASSERT  Type assertions automatically generated

(in-package "COMPILER")

(defun c1compiler-typecase (args)
  (let* ((var-name (pop args))
	 (var (c1vref var-name))
	 (expressions (loop for (type . forms) in args
			 collect (list type (c1progn forms)))))
    (make-c1form* 'EXT:COMPILER-TYPECASE
		  :type 't
		  :args var expressions)))

(defun c2compiler-typecase (var expressions)
  (loop with var-type = (var-type var)
     for (type form) in expressions
     when (or (member type '(t otherwise))
	      (subtypep var-type type))
     return (c2expr form)))

(defun variable-or-constant-p (value env)
  (or (when (symbolp value) (known-variable-p value env))
      (constantp value env)))

(defun simple-type-assertion (value type env)
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

(defun expand-type-assertion (value type env compulsory)
  (cond ((constantp value env)
	 ;; For constant values, we simply verify the assertion directly
	 (unless (typep (cmp-eval value env) type)
	   (cmpwarning "Failed type assertion for value ~A and type ~A"
		       value type))
	 t)
	((or (not (symbolp value))
	     (special-variable-p value)
	     (symbol-macro-p value))
	 ;; If multiple references to the value cost time and space,
	 ;; or may cause side effects, we save it.
	 (with-clean-symbols (%value)
	   `(let* ((%value ,value))
	      (declare (:read-only %value))
	      ,(expand-type-assertion '%value type env compulsory))))
	(compulsory
	 ;; The check has to be produced, independent of the declared
	 ;; value of the variable (for instance, in LAMBDA arguments).
	 (simple-type-assertion value type env))
	(t
	 ;; We may rely on the compiler to choose the appropriate
	 ;; branch once type propagation has happened.
	 `(ext:compiler-typecase ,value
            (,type)
	    (t ,(simple-type-assertion value type env))))))

(defmacro optional-type-assertion (&whole whole value type &environment env)
  "Generates a type check on an expression, ensuring that it is satisfied."
  (when (policy-type-assertions env)
    (cmpnote "Checking type ~A for expression~&~A" type value)
    (expand-type-assertion value type env nil)))

(defmacro type-assertion (&whole whole value type &environment env)
  "Generates a type check on an expression, ensuring that it is satisfied."
  (cmpnote "Checking type ~A for expression~&~A" type value)
  (expand-type-assertion value type env t))

(defmacro checked-value (&whole whole value type &environment env)
  (cond ((not (policy-type-assertions env))
	 `(the ,type ,value))
	((or (constantp value type)
	     (and (symbolp value) (local-variable-p value env)))
	 `(progn
	    ,(expand-type-assertion value type env nil)
	    (the ,type ,value)))
	(t
	 (with-clean-symbols (%checked-value)
	   `(let* ((%checked-value ,value))
	      ,(expand-type-assertion '%checked-value type env nil)
	      (the ,type ,value))))))

