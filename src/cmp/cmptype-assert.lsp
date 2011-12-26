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

(defmacro unlikely (bool)
  `(ffi:c-inline (,bool) (:bool) :bool "ecl_unlikely(#0)"
		 :one-liner t :side-effects nil))

(defun expand-type-assertion (value type env)
  (if (not (symbolp value))
      (with-clean-symbols (%value)
	`(let ((%value ,value))
	   (declare (:read-only %value))
	   ,(expand-type-assertions '%value type env)))
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
         FEwrong_type_argument(#1,#2);" :one-liner nil)
	 #+(or)
	 `(if (unlikely (not (typep ,value ',type)))
	      (compiler-type-error ,value ,type))))))

(defmacro optional-type-assertion (&whole whole value type &environment env)
  "Generates a type check on an expression, ensuring that it is satisfied."
  (when (policy-type-assertions env)
    (cmpnote "Checking type ~A for expression~&~A" type value)
    (expand-type-assertion value type env)))

(defmacro type-assertion (&whole whole value type &environment env)
  "Generates a type check on an expression, ensuring that it is satisfied."
  (cmpnote "Checking type ~A for expression~&~A" type value)
  (expand-type-assertion value type env))

(defmacro checked-value (&whole whole value type &environment env)
  (if (policy-type-assertions env)
      (with-clean-symbols (%value)
	`(let* ((%value ,value))
	   ,(expand-type-assertion '%value type env)
	   (the ,type %value)))
      value))

(defmacro compiler-type-error (value type &environment env)
  (case type
    (cons
     `(ffi:c-inline (,value) (:object) :void
        "FEtype_error_cons(#0);"
        :one-liner t))
    (array
     `(ffi:c-inline (,value) (:object) :void
        "FEtype_error_array(#0);"
        :one-liner t))
    (list
     `(ffi:c-inline (,value) (:object) :void
        "FEtype_error_list(#0);"
        :one-liner t))
    (sequence
     `(ffi:c-inline (,value) (:object) :void
        "FEtype_error_sequence(#0);"
        :one-liner t))
    (otherwise
     `(ffi:c-inline (',type ,value) (:object :object) :void
		    "FEwrong_type_argument(#0,#1);"))))
