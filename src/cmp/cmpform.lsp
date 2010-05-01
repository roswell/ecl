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
  (dolist (subform dependents form)
    (cond ((c1form-p subform)
	   (when (info-sp-change subform)
	     (setf (info-sp-change form) t))
	   (setf (c1form-parent subform) form))
	  ((consp subform)
	   (c1form-add-info form subform)))))

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
