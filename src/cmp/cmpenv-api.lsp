;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll.
;;;;
;;;;    See file 'LICENSE' for the copyright details.

;;;;
;;;; CMPENVAPI -- API for creating and manipulating environments
;;;;

(in-package "COMPILER")

(defun cmp-env-root (&optional (env *cmp-env-root*))
  "Provide a root environment for toplevel forms storing all declarations
that are susceptible to be changed by PROCLAIM."
  (let ((env (cmp-env-copy env)))
    (add-default-optimizations env)))

(defun cmp-env-copy (&optional (env *cmp-env*))
  (cons (car env) (cdr env)))

(defmacro cmp-env-variables (&optional (env '*cmp-env*))
  `(car ,env))

(defmacro cmp-env-functions (&optional (env '*cmp-env*))
  `(cdr ,env))

(defun cmp-env-register-var (var &optional (env *cmp-env*) (boundp t))
  (push (list (var-name var)
              (if (member (var-kind var) '(special global))
                  :special
                  t)
              boundp
              var)
        (cmp-env-variables env))
  env)

(defun cmp-env-add-declaration (type arguments &optional (env *cmp-env*))
  (push (list* :declare type arguments) 
        (cmp-env-variables env))
  env)

(defun cmp-env-extend-declaration (type arguments &optional (env *cmp-env*) default)
  (let ((x (cmp-env-search-declaration type env default)))
    (cmp-env-add-declaration type (append arguments x) env)
    env))

(defun cmp-env-register-function (fun &optional (env *cmp-env*))
  (push (list (fun-name fun) 'function fun)
        (cmp-env-functions env))
  env)

(defun cmp-env-register-global-macro (name function)
  (cmp-env-register-macro name function *cmp-env*)
  (cmp-env-register-macro name function *cmp-env-root*)
  (values))

(defun cmp-env-register-macro (name function &optional (env *cmp-env*))
  (push (list name 'si:macro function)
        (cmp-env-functions env))
  env)

(defun cmp-env-register-ftype (name declaration &optional (env *cmp-env*))
  (push (list* :declare name declaration)
        (cmp-env-functions env))
  env)

(defun cmp-env-register-symbol-macro (name form &optional (env *cmp-env*))
  (cmp-env-register-symbol-macro-function name
                                          #'(lambda (whole env) (declare (ignore env whole)) form)
                                          env))

(defun cmp-env-register-symbol-macro-function (name function &optional (env *cmp-env*))
  (when (or (constant-variable-p name) (special-variable-p name))
    (cmperr "Cannot bind the special or constant variable ~A with symbol-macrolet." name))
  (push (list name 'si:symbol-macro function)
        (cmp-env-variables env))
  env)

(defun cmp-env-register-block (blk &optional (env *cmp-env*))
  (push (list :block (blk-name blk) blk)
        (cmp-env-variables env))
  env)

(defun cmp-env-register-tag (name tag &optional (env *cmp-env*))
  (push (list :tag (list name) tag)
        (cmp-env-variables env))
  env)

(defun cmp-env-register-compiler-macro (name macro-function)
  (push (list name 'si::compiler-macro macro-function)
        (cmp-env-functions *cmp-env-root*))
  (values))

(defun cmp-env-search-function (name &optional (env *cmp-env*))
  (let ((cfb nil)
        (unw nil)
        (found nil))
    (dolist (record (cmp-env-functions env))
      (cond ((eq record 'SI:FUNCTION-BOUNDARY)
             (setf cfb t))
            ((eq record 'SI:UNWIND-PROTECT-BOUNDARY)
             (setf unw t))
            ((atom record)
             (baboon :format-control "Unknown record found in environment~%~S"
                     :format-arguments (list record)))
            ;; We have to use EQUAL because the name can be a list (SETF whatever)
            ((and (equal (first record) name) (not (eq (second record) 'si::compiler-macro)))
             (setf found (first (last record)))
             (return))))
    (values found cfb unw)))

(defun cmp-env-search-variables (type name env)
  (let ((cfb nil)
        (unw nil)
        (found nil))
    (dolist (record (cmp-env-variables env))
      (cond ((eq record 'SI:FUNCTION-BOUNDARY)
             (setf cfb t))
            ((eq record 'SI:UNWIND-PROTECT-BOUNDARY)
             (setf unw t))
            ((atom record)
             (baboon :format-control "Unknown record found in environment~%~S"
                     :format-arguments (list record)))
            ((not (eq (first record) type)))
            ((eq type :block)
             (when (eq name (second record))
               (setf found record)
               (return)))
            ((eq type :tag)
             (when (member name (second record) :test #'eql)
               (setf found record)
               (return)))
            ((eq name 'si:symbol-macro)
             (when (eq (second record) 'si:symbol-macro)
               (setf found record))
             (return))
            (t
             (when (not (eq (second record) 'si:symbol-macro))
               (setf found record))
             (return))))
    (values (first (last found)) cfb unw)))

(defun cmp-env-search-block (name &optional (env *cmp-env*))
  (cmp-env-search-variables :block name env))

(defun cmp-env-search-tag (name &optional (env *cmp-env*))
  (cmp-env-search-variables :tag name env))

(defun cmp-env-search-symbol-macro (name &optional (env *cmp-env*))
  (cmp-env-search-variables name 'si:symbol-macro env))

(defun cmp-env-search-var (name &optional (env *cmp-env*))
  (cmp-env-search-variables name t env))

(defun cmp-env-search-macro (name &optional (env *cmp-env*))
  (let ((f (cmp-env-search-function name env)))
    (if (functionp f)
        f
        nil)))

;;; Like macro-function except it searches the lexical environment,
;;; to determine if the macro is shadowed by a function or a macro.
(defun cmp-macro-function (name)
  (or (cmp-env-search-macro name)
      (macro-function name)))

(defun cmp-env-search-compiler-macro (name &optional (env *cmp-env*))
  (dolist (record (cmp-env-functions env))
    (when (and (consp record)
               (equal (first record) name)
               (eq (second record) 'si::compiler-macro))
      (return-from cmp-env-search-compiler-macro (third record)))))

(defun cmp-compiler-macro-function (name)
  (or (cmp-env-search-compiler-macro name)
      (compiler-macro-function name)))

(defun cmp-env-search-ftype (name &optional (env *cmp-env*))
  (dolist (i env nil)
    (when (and (consp i)
               (eq (pop i) :declare)
               (same-fname-p (pop i) name))
      (return i))))

(defun cmp-env-mark (mark &optional (env *cmp-env*))
  (cons (cons mark (car env))
        (cons mark (cdr env))))

(defun cmp-env-new-variables (new-env old-env)
  (loop for i in (ldiff (cmp-env-variables new-env)
                        (cmp-env-variables old-env))
        when (and (consp i) (var-p (fourth i)))
        collect (fourth i)))

(defun cmp-env-search-declaration (kind &optional (env *cmp-env*) default)
  (loop for i in (car env)
     when (and (consp i)
               (eq (first i) :declare)
               (eq (second i) kind))
     return (cddr i)
     finally (return default)))

