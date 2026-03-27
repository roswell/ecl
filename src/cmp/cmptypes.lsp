;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    See file 'LICENSE' for the copyright details.

;;;;
;;;;  CMPTYPES -- Data types for the Lisp core structures
;;;;

(in-package "COMPILER")

(defstruct (info)
  (local-vars nil)      ;;; List of var-objects created directly in the form.
  (type '(VALUES &REST T)) ;;; Type of the form.
  (sp-change nil)       ;;; Whether execution of the form may change
                        ;;; the value of a special variable.
  (volatile nil)        ;;; whether there is a possible setjmp. Beppe
  )

(defstruct (c1form (:include info)
                   (:print-object print-c1form)
                   (:constructor do-make-c1form))
  (name nil)
  (parents nil)
  (env (cmp-env-copy)) ;; Environment in which this form was compiled
  (args '())
  (side-effects nil) ;;; Does it have side effects
  (form nil)
  (toplevel-form nil)
  (file nil)
  (file-position 0))

(defun print-c1form (form stream)
  (format stream "#<form ~A ~X>" (c1form-name form) (si:pointer form)))

(defvar *c1form-level* 0)
(defun print-c1forms (form)
  (cond ((consp form)
         (let ((*c1form-level* (1+ *c1form-level*)))
           (mapc #'print-c1forms form)))
        ((c1form-p form)
         (format t "~% ~D > ~A, parent ~A" *c1form-level* form (c1form-parents form))
         (print-c1forms (c1form-args form))
         form)))
