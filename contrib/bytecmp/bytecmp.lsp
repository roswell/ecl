;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; BYTECMP Fake compiler which is used as a replacement when we do not
;;;;         want or can have the real native compiler.

(in-package "EXT")
(ext:package-lock (find-package :cl) nil)

(defun cl:compile (name &optional (def nil supplied-p))
 (cond ((and supplied-p def)
        (when (functionp def)
	  (unless (function-lambda-expression def)
          (return-from compile def))
	  (setf def (function-lambda-expression def)))
        (setq form (if name
		     `(setf (symbol-function ',name) #',def)
		     `(set 'GAZONK #',def))))
       ((not (fboundp name))
	(error "Symbol ~s is unbound." name))
       ((typep (setf def (symbol-function name)) 'standard-generic-function)
	(warn "COMPILE can not compile generic functions yet")
	(return-from compile (values def t nil)))
       ((null (setq form (function-lambda-expression def)))
	(warn "We have lost the original function definition for ~s. Compilation failed"
              name)
	(return-from compile (values def t nil)))
       (t
	(setq form `(setf (symbol-function ',name) #',form))))
 (eval form)
 (values name nil nil))

(defun cl:compile-file-pathname (name &key (output-file name) (type :fasl type-supplied-p)
				 verbose print c-file h-file data-file shared-data-file
				 system-p load)
  (let ((extension "fasc"))
    (case type
      ((:fasl :fas) (setf extension "fasc"))
      (t (error "In COMPILE-FILE-PATHNAME, the type ~A is unsupported." type)))
    (make-pathname :type extension :defaults output-file)))

(defun cl:compile-file (input-pathname
			&key
			((:verbose *compile-verbose*) *compile-verbose*)
			((:print *compile-print*) *compile-print*)
			(load nil)
			(output-file 't output-file-p)
			&allow-other-keys)
  (setf output-file (if (or (null output-file-p) (eq output-file 't))
                        (compile-file-pathname input-pathname)
                        (pathname output-file)))
  (when *compile-verbose*
    (format t "~&;;; Compiling ~A" input-pathname))
  (with-open-file (sin input-pathname :direction :input)
    (with-open-file (sout output-file :direction :output :if-exists :supersede
			  :if-does-not-exist :create)
      (handler-case
          (sys:with-ecl-io-syntax
              (write (loop with *package* = *package*
                        and ext:*bytecodes-compiler* = t
                        for form = (read sin nil :EOF)
                        until (eq form :EOF)
                        collect (si:eval-with-env form nil nil nil nil))
                     :stream sout :circle t
                     :escape t :readably t :pretty nil)
            (terpri sout))
        (error (c) (let ((*print-readably* nil) (*print-pretty* nil) (*print-circle* t)) (break))))))
  (when load
    (load output-file :verbose *compile-verbose*))
  (values output-file nil nil))

(ext::package-lock (find-package :cl) t)

(pushnew :ecl-bytecmp *features*)

(provide 'BYTECMP)
