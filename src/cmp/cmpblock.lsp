;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

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

;;;; CMPBLOCK  Block and Return-from.

(in-package "COMPILER")

;;; A dummy variable is created to hold the block identifier.  When a reference
;;; to the block (via `return-from') is found, the `var-ref' count for that
;;; variable is incremented only if the reference appears across a boundary
;;; (`ECI:FUNCTION' or `ECI:UNWIND-PROTECT'), while the `blk-ref' is always
;;; incremented.  Therefore `blk-ref' represents whether the block is used at
;;; all and `var-ref' for the dummy variable represents whether a block
;;; identifier must be created and stored in such variable.

(defun c1block (args)
  (check-args-number 'BLOCK args 1)
  (let ((block-name (first args)))
    (unless (symbolp block-name)
      (cmperr "The block name ~s is not a symbol." block-name))
    (let* ((blk-var (make-var :name block-name :kind 'LEXICAL))
           (blk (make-blk :var blk-var :name block-name))
           (body (let ((*cmp-env* (cmp-env-copy)))
                   (cmp-env-register-block blk)
                   (c1progn (rest args)))))
      (when (or (var-ref-ccb blk-var) (var-ref-clb blk-var))
        (incf *setjmps*))
      (if (plusp (blk-ref blk))
          (make-c1form* 'BLOCK
                        :local-vars (list blk-var)
                        :type (values-type-or (blk-type blk) (c1form-type body))
                        :args blk body)
          body))))

(defun c2block (c1form blk body)
  (declare (ignore c1form))
  (setf (blk-exit blk) *exit*
        (blk-destination blk) *destination*)
  (if (plusp (var-ref (blk-var blk)))
      (let* ((blk-var (blk-var blk))
             (*env-lvl* *env-lvl*))
        (check-vref blk-var)
        (wt-nl-open-brace)
        (when (eq :object (var-kind blk-var))
          (setf (var-loc blk-var) (next-lcl))
          (wt-nl "cl_object " blk-var ";"))
        (when (env-grows (var-ref-ccb blk-var))
          ;; var is referenced from a closure which may escape.
          (let ((env-lvl *env-lvl*))
            (wt-nl "cl_object " *volatile* "env" (incf *env-lvl*) " = env" env-lvl ";")))
        (bind "ECL_NEW_FRAME_ID(cl_env_copy)" blk-var)
        (wt-nl "if (ecl_frs_push(cl_env_copy," blk-var ")!=0) {")
        (let ((*unwind-exit* (cons 'FRAME *unwind-exit*)))
          (unwind-exit 'VALUES)
          (wt-nl "} else {")
          (c2expr body)
          (wt "}"))
        (when (var-ref-ccb blk-var) (decf *env*))
        (wt-nl-close-brace))
      (c2expr body)))

(defun c1return-from (args)
  (check-args-number 'RETURN-FROM args 1 2)
  (let ((name (first args)))
    (unless (symbolp name)
      (cmperr "The block name ~s is not a symbol." name))
    (multiple-value-bind (blk cfb unw)
        (cmp-env-search-block name)
      (unless blk
        (cmperr "The block ~s is undefined." name))
      (let* ((val (c1expr (second args)))
             (var (blk-var blk))
             (type T))
        (cond (cfb (setf type 'CLB
                         (var-ref-clb var) T))
              (unw (setf type 'UNWIND-PROTECT)))
        (incf (blk-ref blk))
        (setf (blk-type blk) (values-type-or (blk-type blk) (c1form-type val)))
        (let ((output (make-c1form* 'RETURN-FROM :type 'T :args blk type val)))
          (when (or cfb unw)
            (add-to-read-nodes var output))
          output)))))

(defun c2return-from (c1form blk type val)
  (declare (ignore c1form))
  (case type
    (CCB
     (let ((*destination* 'VALUES)) (c2expr* val))
     (wt-nl "cl_return_from(" (blk-var blk) "," (add-symbol (blk-name blk)) ");"))
    ((CLB UNWIND-PROTECT)
     (let ((*destination* 'VALUES)) (c2expr* val))
     (wt-nl "cl_return_from(" (blk-var blk) ",ECL_NIL);"))
    (T (let ((*destination* (blk-destination blk))
             (*exit* (blk-exit blk)))
         (c2expr val)))))
