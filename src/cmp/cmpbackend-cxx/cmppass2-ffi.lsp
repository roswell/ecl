;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  CMPFFI --  Foreign functions interface.

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

;;; ----------------------------------------------------------------------
;;; C/C++ DECLARATIONS AND HEADERS
;;;
;;; All lines from CLINES statements are grouped at the beginning of the header
;;; Notice that it does not make sense to guarantee that c-lines statements
;;; are produced in-between the function definitions, because two functions
;;; might be collapsed into one, or we might not produce that function at all
;;; and rather inline it.
;;;

;;; FIXME pass1 handler defined in the pass2 module.
(defun c1clines (args)
  (unless (every #'stringp args)
    (cmperr "The argument to CLINES, ~s, is not a list of strings." args))
  (setf *clines-string-list* (nconc *clines-string-list* (copy-list args)))
  '(progn))

(defun output-clines (output-stream)
  (loop for s in *clines-string-list*
        do (terpri output-stream)
        do (if (find #\@ s)
               (cmperr "The character #\\@ is not allowed in ~s." 'FFI:CLINES)
               (write-string s output-stream)))
  (terpri output-stream)
  (setf *clines-string-list* nil))

;; ----------------------------------------------------------------------
;; C/C++ INLINE CODE
;;

(defun c2c-progn (c1form variables statements)
  (declare (ignore c1form))
  (loop with *destination* = 'TRASH
        for form in statements
        do (cond ((stringp form)
                  (wt-nl)
                  (wt-c-inline-loc :void form variables
                                   t    ; side effects
                                   nil) ; no output variables
                  )
                 (t
                  (c2expr* form)))
        finally (unwind-exit *vv-nil*)))

(defun c2c-inline (c1form arguments &rest rest)
  (declare (ignore c1form))
  (let ((*inline-blocks* 0)
        (*temp* *temp*))
    (unwind-exit (apply #'produce-inline-loc (inline-args arguments) rest))
    (close-inline-blocks)))
