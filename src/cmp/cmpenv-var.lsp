
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See file 'LICENSE' for the copyright details.

(in-package #:compiler)

(defun declare-special (name &optional (env *cmp-env*))
  (when (cmp-env-search-symbol-macro name env)
    (cmperr "Symbol ~A cannot be declared special and appear in a symbol-macrolet." name))
  (cmp-env-register-var (make-global-var name :warn nil :kind 'SPECIAL) env nil))

;;; A special binding creates a var object with the kind field SPECIAL,
;;; whereas a special declaration without binding creates a var object with
;;; the kind field GLOBAL.  Thus a reference to GLOBAL may need to make sure
;;; that the variable has a value.

;;;  Bootstrap problem: proclaim needs this function:
;;;
;;; Check if a variable has been declared as a special variable with a global
;;; value.

(defun check-global (name)
  (member name *global-vars*))

(defun si::register-global (name)
  (pushnew name *global-vars*)
  (values))

(defun special-variable-p (name)
  "Return true if NAME is associated to a special variable in the lexical environment."
  (or (si:specialp name)
      (check-global name)
      (let ((v (cmp-env-search-var name *cmp-env-root*)))
        ;; Fixme! Revise the declamation code to ensure whether
        ;; we also have to consider 'GLOBAL here.
        (and v (eq (var-kind v) 'SPECIAL)))))

(defun constant-variable-p (name)
  (si:constp name))

(defun local-variable-p (name &optional (env *cmp-env*))
  (let ((record (cmp-env-search-var name env)))
    (and record (var-p record))))

(defun symbol-macro-p (name &optional (env *cmp-env*))
  (let ((record (cmp-env-search-var name env)))
    (and record (not (var-p record)))))

(defun read-only-variable-p (name other-decls)
  (dolist (i other-decls nil)
    (when (and (eq (car i) :READ-ONLY)
               (member name (rest i)))
      (return t))))

(defun variable-type-in-env (name &optional (env *cmp-env*))
  (let ((var (cmp-env-search-var name env)))
    (cond ((var-p var)
           (var-type var))
          ((si:get-sysprop name 'CMP-TYPE))
          (t))))
