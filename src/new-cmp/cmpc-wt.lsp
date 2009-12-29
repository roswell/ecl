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

;;;; CMPWT -- Routines for writing code to C files.

(in-package "C-BACKEND")

(defvar *wt-string-size* 0)

(defun wt-label (label)
  (wt-nl1 "L" label ":;"))

(defun wt-filtered-comment (text stream single-line)
  (declare (string text))
  (if single-line
      (progn
	(fresh-line stream)
	(princ "/*	" stream))
      (format stream "~50T/*  "))
  (let* ((l (1- (length text))))
    (declare (fixnum l))
    (dotimes (n l)
      (let ((c (schar text n)))
	(princ c stream)
	(when (and (char= c #\*) (char= (schar text (1+ n)) #\/))
	  (princ #\\ stream))))
    (princ (schar text l) stream))
  (format stream "~70T*/")
  )

(defun do-wt-comment (message-or-format args single-line-p)
  (unless (and (symbolp message-or-format) (not (symbol-package message-or-format)))
    (wt-filtered-comment (if (stringp message-or-format)
                             (if args
                                 (apply #'format nil message-or-format args)
                                 message-or-format)
                             (princ-to-string message-or-format))
                         *compiler-output1*
                         single-line-p)))

(defun wt-comment (message &rest extra)
  (do-wt-comment message extra nil))

(defun wt-comment-nl (message &rest extra)
  (do-wt-comment message extra t))

(defun wt1 (form)
  (typecase form
    ((or STRING INTEGER CHARACTER)
     (princ form *compiler-output1*))
    ((or DOUBLE-FLOAT SINGLE-FLOAT)
     (format *compiler-output1* "~10,,,,,,'eG" form))
    (LONG-FLOAT
     (format *compiler-output1* "~,,,,,,'eEl" form))
    (VAR (wt-var form))
    (t (wt-loc form)))
  nil)

(defun wt-h1 (form)
  (if (consp form)
      (let ((fun (get-sysprop (car form) 'wt-loc)))
	(if fun
	    (let ((*compiler-output1* *compiler-output2*))
	      (apply fun (cdr form)))
	    (cmperr "The location ~s is undefined." form)))
      (princ form *compiler-output2*))
  nil)
