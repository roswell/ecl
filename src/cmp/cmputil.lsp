;;;; CMPUTIL  --  Miscellaneous Functions.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defun cmperr (string &rest args &aux (*print-case* :upcase))
  (print-current-form)
  (format t "~&;;; Error: ")
  (apply #'format t string args)
  (incf *error-count*)
  (throw *cmperr-tag* '*cmperr-tag*))

(defun too-many-args (name upper-bound n &aux (*print-case* :upcase))
  (print-current-form)
  (format t
          "~&;;; ~S requires at most ~R argument~:p, ~
          but ~R ~:*~[were~;was~:;were~] supplied.~%"
          name
          upper-bound
          n)
  (incf *error-count*)
  (throw *cmperr-tag* '*cmperr-tag*))

(defun too-few-args (name lower-bound n &aux (*print-case* :upcase))
  (print-current-form)
  (format t
          "~&;;; ~S requires at least ~R argument~:p, ~
          but only ~R ~:*~[were~;was~:;were~] supplied.~%"
          name
          lower-bound
          n)
  (incf *error-count*)
  (throw *cmperr-tag* '*cmperr-tag*))

(defun cmpwarn (string &rest args &aux (*print-case* :upcase))
  (unless *suppress-compiler-warnings*
    (print-current-form)
    (format t "~&;;; Warning: ")
    (apply #'format t string args)
    (terpri))
  nil)

(defun cmpnote (string &rest args &aux (*print-case* :upcase))
  (unless *suppress-compiler-notes* 
    (format t "~&;;; Note: ")
    (apply #'format t string args)
    (terpri))
  nil)

(defun print-current-form ()
  (when *first-error*
        (setq *first-error* nil)
	(let ((*print-length* 2)
	      (*print-level* 2))
	  (format t "~&;;; Compiling ~s.~%" *current-form*)))
  nil)

(defun print-emitting (name)
  (format t "~&;;; Emitting code for ~s.~%" (or name "lambda")))

(defun undefined-variable (sym &aux (*print-case* :upcase))
  (print-current-form)
  (format t
          "~&;;; The variable ~s is undefined.~
           ~%;;; The compiler will assume this variable is a global.~%"
          sym)
  nil)

(defun baboon (&aux (*print-case* :upcase))
  (print-current-form)
  (format
   t "~&;;; A bug was found in the compiler.  Contact attardi@di.unipi.it.~%")
  (incf *error-count*)
  (break)
;  (throw *cmperr-tag* '*cmperr-tag*) DEBUG
)

(defun cmp-eval (form &aux (throw-flag t))
  (unwind-protect
       (prog1
	   (cmp-toplevel-eval form)
	 (setq throw-flag nil))
    (when throw-flag
      (let ((*print-case* :upcase))
	(print-current-form)
	(format t "~&;;; The form ~s was not evaluated successfully.~
                   ~%;;; You are recommended to compile again.~%"
		form)))))


(defun cmp-macroexpand (form &aux env (throw-flag t))
  ;; Obtain the local macro environment for expansion.
  (dolist (v *funs*)
    (when (consp v)
      (push v env)))
  (when env (setq env (cons nil (nreverse env))))
  (unwind-protect
      (prog1
	  (cmp-toplevel-eval `(macroexpand ',form ',env))
	(setq throw-flag nil))
    (when throw-flag
      (let ((*print-case* :upcase))
	(print-current-form)
	(format t
		"~&;;; The macro form ~s was not expanded successfully.~
                   ~%;;; You are recommended to compile again.~%"
		form)))))


(defun cmp-macroexpand-1 (form &aux env (throw-flag t))
  (dolist (v *funs*)
    (when (consp v)
      (push (list (car v) 'MACRO (cadr v)) env))) 
  (when env (setq env (cons nil (nreverse env))))
  (unwind-protect
      (prog1
	  (cmp-toplevel-eval `(macroexpand-1 ',form ',env))
	(setq throw-flag nil))
    (when throw-flag
      (let ((*print-case* :upcase))
	(print-current-form)
	(format t
		"~&;;; The macro form ~s was not expanded successfully.~
                   ~%;;; You are recommended to compile again.~%"
		form)))))
	  

(defun cmp-expand-macro (fd fname args &aux env (throw-flag t))
  (dolist (v *funs*)
	  (if (consp v) (push (list (car v) 'MACRO (cadr v)) env)))
  (when env (setq env (cons nil (nreverse env))))
  (unwind-protect
      (prog1
	  (cmp-toplevel-eval
	   `(funcall *macroexpand-hook* ',fd ',(cons fname args) ',env))
	(setq throw-flag nil))
    (if throw-flag
        (let ((*print-case* :upcase))
          (print-current-form)
          (format t
		  "~&;;; The macro form (~s ...) was not expanded successfully.~
                   ~%;;; You are recommended to compile again.~%"
		  fname)))))

(defun cmp-toplevel-eval (form)
   (let*
     #-:CCL
     ((sys::*ihs-base* sys::*ihs-top*)
      (sys::*ihs-top* (sys::ihs-top 'cmp-toplevel-eval))
      (*break-enable* *compiler-break-enable*)
      (sys::*break-hidden-packages*
       (cons (find-package 'compiler)
	     sys::*break-hidden-packages*)))
     #+:CCL
     ((*break-on-errors* *compiler-break-enable*))
         (eval form)))

(defun compiler-clear-compiler-properties (symbol)
  #-:CCL
  ;(sys::unlink-symbol symbol)
  (remprop symbol 'package-operation)
  (remprop symbol 't1)
  (remprop symbol 't2)
  (remprop symbol 't3)
  (remprop symbol 'c1)
  (remprop symbol 'c2)
  (remprop symbol 'c1conditional)
  (remprop symbol ':inline-always)
  (remprop symbol ':inline-unsafe)
  (remprop symbol ':inline-safe)
  (remprop symbol 'lfun))
  
