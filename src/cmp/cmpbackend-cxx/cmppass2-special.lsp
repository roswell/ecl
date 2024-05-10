;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2021, Daniel Kochmański
;;;;
;;;;    See file 'LICENSE' for the copyright details.

(in-package #:compiler)

(defun c2compiler-let (c1form symbols values body)
  (declare (ignore c1form))
  (progv symbols values (c2expr body)))

(defun c2function (c1form kind funob fun)
  (declare (ignore c1form funob))
  (case kind
    (GLOBAL
     (unwind-exit `(FDEFINITION ,fun)))
    (CLOSURE
     ;; XXX: we have some code after baboon – is CLOSURE legal or not?
     (baboon :format-control "c2function: c1form is of unexpected kind.")
     (new-local fun)
     (unwind-exit `(MAKE-CCLOSURE ,fun)))))

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
