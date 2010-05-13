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
  '((LOCATION		loc :movable)
    (VAR		var)
    (SETQ		var value-c1form :side-effects)
    (PSETQ		var-list value-c1form-list :side-effects)
    (BLOCK		blk-var progn-c1form :movable)
    (PROGN		body :movable)
    (PROGV		symbols values form :side-effects)
    (TAGBODY		tag-var tag-body :movable)
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
    (LOCALS		local-fun-list body labels-p :movable)
    (IF			fmla-c1form true-c1form false-c1form :movable)
    (FMLA-NOT		fmla-c1form :movable)
    (FMLA-AND		* :movable)
    (FMLA-OR		* :movable)
    (LAMBDA		lambda-list doc body-c1form)
    (LET		vars-list var-init-c1form-list decl-body-c1form)
    (LET*		vars-list var-init-c1form-list decl-body-c1form)
    (VALUES		values-c1form-list :movable)
    (MULTIPLE-VALUE-SETQ vars-list values-c1form-list :side-effects)
    (MULTIPLE-VALUE-BIND vars-list init-c1form body :movable)
    (COMPILER-LET	symbols values body)
    (FUNCTION		(GLOBAL/CLOSURE) lambda-form fun-object :movable)
    (C2PRINC		object-string-or-char stream-var stream-c1form :side-effects)
    (RPLACA		(dest-c1form value-c1form) :side-effects)
    (RPLACD		(dest-c1form value-c1form) :side-effects)
    (MEMBER!2		fun-symbol args-c1form-list :movable)
    (ASSOC!2		fun-symbol args-c1form-list :movable)

    (SI:STRUCTURE-REF	struct-c1form type-name slot-index (:UNSAFE/NIL) :movable)
    (SI:STRUCTURE-SET	struct-c1form type-name slot-index value-c1form :side-effects)

    (WITH-STACK		body :side-effects)
    (STACK-PUSH-VALUES value-c1form push-statement-c1form :side-effects :movable)

    (ORDINARY		c1form :movable)
    (LOAD-TIME-VALUE	dest-loc value-c1form :movable)
    (SI:FSET		function-object vv-loc macro-p pprint-p lambda-form
			:side-effects)
    (MAKE-FORM		vv-loc value-c1form :side-effects)
    (INIT-FORM		vv-loc value-c1form :side-effects))))

(defconstant +c1-form-hash+
  #.(loop with hash = (make-hash-table :size 128 :test #'eq)
       for (name . rest) in +all-c1-forms+
       for length = (if (member '* rest) nil (length rest))
       for side-effects = (if (member :side-effects rest)
                              (progn (and length (decf length)) t)
                              nil)
       for movable = (if (member :movable rest)
                         (progn (and length (decf length)) t)
                         nil)
       do (setf (gethash name hash) (list length side-effects movable))
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

(defun c1form-add-info-loop (form dependents)
  (loop for subform in dependents
     when (c1form-p subform)
     do (progn
          (when (c1form-sp-change subform)
            (setf (c1form-sp-change form) t
                  (c1form-side-effects form) t))
          (when (c1form-side-effects subform)
            (setf (c1form-side-effects form) t))
          (unless (eq (c1form-name subform) 'LOCATION)
            (when (rest (c1form-parents subform))
              (error "Running twice through same form"))
            (setf (c1form-parents subform)
                  (nconc (c1form-parents subform)
                         (c1form-parents form)))))
     when (consp subform)
     do (c1form-add-info-loop form subform)))

(defun c1form-add-info (form dependents)
  (let ((record (gethash (c1form-name form) +c1-form-hash+)))
    (unless record
      (error "Internal error: unknown C1FORM name ~A"
             (c1form-name form)))
    (let ((length (first record))
          (sp-change (c1form-sp-change form))
          (side-effects (second record)))
      (setf (c1form-side-effects form)
            (or (c1form-side-effects form) sp-change side-effects)
            (c1form-parents form)
            (list form))
      (unless (or (null length) (= length (length (c1form-args form))))
        (error "Internal error: illegal number of arguments in ~A" form))))
  (c1form-add-info-loop form dependents))

(defun c1form-replace-with (dest new-fields)
  ;; We have to relocate the children nodes of NEW-FIELDS in
  ;; the new branch. This implies rewriting the parents chain,
  ;; but only for non-location nodes (these are reused).
  (unless (eq (c1form-name new-fields) 'LOCATION)
    (rplacd (c1form-parents new-fields)
            (c1form-parents dest)))
  ;; Side effects might have to be propagated to the parents
  ;; but currently we do not allow moving forms with side effects
  (when (c1form-side-effects new-fields)
    (baboon "Attempted to move a form with side-effects"))
  ;; Remaining flags are just copied
  (setf (c1form-type dest) (c1form-type new-fields)
        (c1form-sp-change dest) (c1form-sp-change new-fields)
        (c1form-side-effects dest) (c1form-side-effects new-fields)
        (c1form-volatile dest) (c1form-volatile new-fields)
        (c1form-name dest) 'VALUES
        (c1form-args dest) (list (list new-fields))))

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
           (member node (c1form-parents presumed-child))))
    (member home-node list :test #'parent-node-p)))

(defun traverse-c1form-tree (tree function)
  (cond ((consp tree)
         (loop for f in tree
            do (traverse-c1form-tree f function)))
        ((c1form-p tree)
         (loop for f in (c1form-args tree)
            do (traverse-c1form-tree f function))
         (funcall function tree))))

(defun c1form-movable-p (form)
  (flet ((abort-on-not-pure (form)
           (let ((name (c1form-name form)))
             (cond ((eq name 'VAR)
                    (let ((var (c1form-arg 0 form)))
                      (when (or (global-var-p var)
                                (var-set-nodes var))
                        (return-from c1form-movable-p nil))))
                   ((or (c1form-side-effects form)
                        (not (third (gethash name +c1-form-hash+))))
                    (return-from c1form-movable-p nil))))))
    (abort-on-not-pure form)))
