;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2021, Daniel Kochmański
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;

(in-package #:compiler)

(defun c2compiler-let (c1form symbols values body)
  (declare (ignore c1form))
  (progv symbols values (c2expr body)))

(defun c2function (c1form fname)
  (declare (ignore c1form))
  (unwind-exit `(FDEFINITION ,fname)))

;;; Mechanism for sharing code.
(defun new-local (fun)
  ;; returns the previous function or NIL.
  (declare (type fun fun))
  (case (fun-closure fun)
    (CLOSURE
     (setf (fun-level fun) 0 (fun-env fun) *env*))
    (LEXICAL
     ;; Only increase the lexical level if there have been some
     ;; new variables created. This way, the same lexical environment
     ;; can be propagated through nested FLET/LABELS.
     (setf (fun-level fun) (if (plusp *lex*) (1+ *level*) *level*)
           (fun-env fun) 0))
    (otherwise
     (setf (fun-env fun) 0 (fun-level fun) 0)))
  (push fun *local-funs*))
