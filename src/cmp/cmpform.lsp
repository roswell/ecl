;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;; CMPFORM -- Internal representation of Lisp forms
;;;;

(in-package "COMPILER")

;;;
;;; ALL C1FORMS: Intermediate language used by the compiler
;;;
;;;	body =		(c1form*)
;;;	tag-body =	({c1form | tag}*)
;;;	return-type =	{CLB | CCB | UNWIND-PROTECT}
;;;	*value =	c1form
;;;	lambda-list = 	(requireds optionals rest key-flag keywords allow-other-keys)
;;;

(eval-when (:compile-toplevel :execute)
(defconstant +all-c1-forms+
  '((LOCATION		loc)
    (VAR		var)
    (SETQ		var value-c1form :side-effects)
    (PSETQ		var-list value-c1form-list :side-effects)
    (BLOCK		blk-var progn-c1form)
    (PROGN		body)
    (PROGV		symbols values form :side-effects)
    (TAGBODY		tag-var tag-body)
    (RETURN-FROM	blk-var return-type value :side-effects)
    (FUNCALL		fun-value (arg-value*) :side-effects)
    (CALL-LOCAL		obj-fun (arg-value*) :side-effects)
    (CALL-GLOBAL	fun-name (arg-value*))
    (CATCH		catch-value body :side-effects)
    (UNWIND-PROTECT	protected-c1form body :side-effects)
    (THROW		catch-value output-value :side-effects)
    (GO			tag-var return-type :side-effects)
    (C-INLINE		(arg-c1form*)
			(arg-type-symbol*)
			output-rep-type
			c-expression-string
			side-effects-p
			one-liner-p
			:side-effects)
    (LOCALS		local-fun-list body labels-p)
    (IF			fmla-c1form true-c1form false-c1form)
    (FMLA-NOT		fmla-c1form)
    (FMLA-AND		*)
    (FMLA-OR		*)
    (LAMBDA		lambda-list doc body-c1form)
    (LET		vars-list var-init-c1form-list decl-body-c1form)
    (LET*		vars-list var-init-c1form-list decl-body-c1form)
    (VALUES		values-c1form-list)
    (MULTIPLE-VALUE-SETQ vars-list values-c1form-list :side-effects)
    (MULTIPLE-VALUE-BIND vars-list init-c1form body)
    (COMPILER-LET	symbols values body)
    (FUNCTION		(GLOBAL/CLOSURE) lambda-form fun-object)
    (C2PRINC		object-string-or-char stream-var stream-c1form :side-effects)
    (RPLACA		(dest-c1form value-c1form) :side-effects)
    (RPLACD		(dest-c1form value-c1form) :side-effects)
    (MEMBER!2		fun-symbol args-c1form-list)
    (ASSOC!2		fun-symbol args-c1form-list)

    (SI:STRUCTURE-REF	struct-c1form type-name slot-index (:UNSAFE/NIL))
    (SI:STRUCTURE-SET	struct-c1form type-name slot-index value-c1form :side-effects)

    (WITH-STACK		body :side-effects)
    (STACK-PUSH-VALUES value-c1form push-statement-c1form :side-effects)

    (ORDINARY		c1form)
    (LOAD-TIME-VALUE	dest-loc value-c1form)
    (SI:FSET		function-object vv-loc macro-p pprint-p lambda-form
			:side-effects)
    (MAKE-FORM		vv-loc value-c1form :side-effects)
    (INIT-FORM		vv-loc value-c1form :side-effects))))

(defconstant +c1-form-hash+
  #.(loop with hash = (make-hash-table :size 128 :test #'eq)
       for (name . rest) in +all-c1-forms+
       for length = (if (member '* rest) nil (length rest))
       for side-effects = (if (member :side-effects rest)
                              (progn (decf length) t)
                              nil)
       do (setf (gethash name hash) (list length side-effects))
       finally (return hash)))

(defun print-c1form (form stream)
  (format stream "#<form ~A ~X>" (c1form-name form) (ext::pointer form)))

(defun make-c1form (name subform &rest args)
  (let ((form (do-make-c1form :name name :args args
			      :type (info-type subform)
			      :sp-change (info-sp-change subform)
			      :volatile (info-volatile subform)
                              :form *current-form*
                              :toplevel-form *current-toplevel-form*
                              :file *compile-file-truename*
                              :file-position *compile-file-position*)))
    (c1form-add-info form args)
    form))

(defun make-c1form* (name &rest args)
  (let ((info-args '())
	(form-args '()))
    (do ((l args (cdr l)))
	((endp l))
      (let ((key (first l)))
	(cond ((not (keywordp key))
	       (baboon))
	      ((eq key ':args)
	       (setf form-args (rest l))
	       (return))
	      (t
	       (setf info-args (list* key (second l) info-args)
		     l (cdr l))))))
    (let ((form (apply #'do-make-c1form :name name :args form-args
                       :form *current-form*
                       :toplevel-form *current-toplevel-form*
                       :file *compile-file-truename*
                       :file-position *compile-file-position*
		       info-args)))
      (c1form-add-info form form-args)
      form)))

(defun c1form-add-info (form dependents)
  (labels ((add-info-loop (form dependents)
             (loop for subform in dependents
                when (c1form-p subform)
                do (progn
                     (when (c1form-sp-change subform)
                       (setf (c1form-sp-change form) t
                             (c1form-side-effects form) t))
                     (when (c1form-side-effects subform)
                       (setf (c1form-side-effects form) t))
                     (setf (c1form-parent subform) form))
                when (consp subform)
                do (add-info-loop form subform))))
    (let ((record (gethash (c1form-name form) +c1-form-hash+)))
      (unless record
        (error "Internal error: unknown C1FORM name ~A"
               (c1form-name form)))
      (let ((length (first record))
            (sp-change (c1form-sp-change form))
            (side-effects (second record)))
        (setf (c1form-side-effects form) (or (c1form-side-effects form)
                                             sp-change
                                             side-effects))
        (unless (or (null length) (= length (length (c1form-args form))))
          (error "Internal error: illegal number of arguments in ~A" form))))
    (add-info-loop form dependents)))

(defun copy-c1form (form)
  (copy-structure form))

(defmacro c1form-arg (nth form)
  (case nth
    (0 `(first (c1form-args ,form)))
    (1 `(second (c1form-args ,form)))
    (otherwise `(nth ,nth (c1form-args ,form)))))

(defun c1form-volatile* (form)
  (if (c1form-volatile form) "volatile " ""))

(defun c1form-primary-type (form)
  (values-type-primary-type (c1form-type form)))

#-new-cmp
(defun location-primary-type (form)
  (c1form-primary-type form))

(defun find-node-in-list (home-node list)
  (flet ((parent-node-p (node presumed-child)
	   (loop
	    (cond ((null presumed-child) (return nil))
		  ((eq node presumed-child) (return t))
		  (t (setf presumed-child (c1form-parent presumed-child)))))))
    (member home-node list :test #'parent-node-p)))
