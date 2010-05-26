;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;;  CMPOPT-BITS  -- Optimize operations acting on bits
;;;;

(in-package "COMPILER")

;;;
;;; LDB
;;; Look for inline expansion of LDB1 in sysfun.lsp
;;;

(define-compiler-macro ldb (&whole whole &rest args)
  (let ((arg1 (first args))
	(len (integer-length most-positive-fixnum))
	size pos)
    (if (and (consp arg1)
	     (eq 'BYTE (car arg1))
	     (integerp (setq size (second arg1)))
	     (integerp (setq pos (third arg1)))
	     (<= (+ size pos) len)
	     (subtypep (result-type (second args)) 'FIXNUM))
	`(the fixnum (ldb1 ,size ,pos ,(second args)))
	whole)))
