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

(defun t1expr (form)
  (let* ((*current-toplevel-form* nil)
         (*cmp-env* (if *cmp-env*
                        (cmp-env-copy *cmp-env*)
                        (cmp-env-root))))
    (push (t1expr* form) *top-level-forms*)))

(defvar *toplevel-forms-to-print*
  '(defun defmacro defvar defparameter defclass defmethod defgeneric))

(defun t1expr* (form &aux
                     (*current-toplevel-form* (list* form *current-toplevel-form*))
                     (*current-form* form)
                     (*setjmps* 0))
  (setq form (chk-symbol-macrolet form))
  (unless (consp form)
    (return-from t1expr* (t1ordinary 'NIL)))
  (let ((fun (car form)) (args (cdr form)) fd)
    (when (member fun *toplevel-forms-to-print*)
      (print-current-form))
    (cond
      ((consp fun) (t1ordinary form))
      ((not (symbolp fun))
       (cmperr "~s is illegal function." fun))
      ((eq fun 'QUOTE)
       (t1ordinary 'NIL))
      ((setq fd (gethash fun *t1-dispatch-table*))
       (funcall fd args))
      ((gethash fun *c1-dispatch-table*)
       (t1ordinary form))
      ((and (setq fd (cmp-compiler-macro-function fun))
            (inline-possible fun)
            (let ((success nil))
              (multiple-value-setq (fd success)
                (cmp-expand-macro fd form))
              success))
       (when *compile-time-too*
         ;; Ignore compiler macros during compile time evaluation
         ;; (they may expand in ffi:c-inline which the bytecodes
         ;; compiler can't execute).
         (cmp-eval form))
       (let ((*compile-time-too* nil))
         (push 'macroexpand *current-toplevel-form*)
         (t1expr* fd)))
      ((setq fd (cmp-macro-function fun))
       (push 'macroexpand *current-toplevel-form*)
       (t1expr* (cmp-expand-macro fd form)))
      (t (t1ordinary form)))))

(defun t1/c1expr (form)
  (cond ((not *compile-toplevel*)
         (c1expr form))
        ((atom form)
         (t1ordinary form))
        (t
         (t1expr* form))))

(defun c1eval-when (args)
  (check-args-number 'EVAL-WHEN args 1)
  (let ((load-flag nil)
        (compile-flag nil)
        (execute-flag nil))
    (dolist (situation (car args))
      (case situation
        ((CL:LOAD :LOAD-TOPLEVEL) (setq load-flag t))
        ((CL:COMPILE :COMPILE-TOPLEVEL) (setq compile-flag t))
        ((CL:EVAL :EXECUTE)
         (if *compile-toplevel*
             (setq compile-flag (or *compile-time-too* compile-flag))
             (setq execute-flag t)))
        (otherwise (cmperr "The EVAL-WHEN situation ~s is illegal."
                           situation))))
    (cond ((not *compile-toplevel*)
           (c1progn (and execute-flag (rest args))))
          (load-flag
           (let ((*compile-time-too* compile-flag))
             (c1progn (rest args))))
          (compile-flag
           (cmp-eval (cons 'PROGN (rest args)))
           (c1progn 'NIL))
          (t
           (c1progn 'NIL)))))

(defun t1ordinary (form)
  (when *compile-time-too* (cmp-eval form))
  (let ((*compile-toplevel* nil)
        (*compile-time-too* nil))
    (add-load-time-values (make-c1form* 'ORDINARY :args (c1expr form)))))

(defun add-load-time-values (form)
  (let ((previous (append (and (consp *load-time-values*)
                               (nreverse *load-time-values*))
                          (nreverse *make-forms*))))
    (when previous
      (setf *load-time-values* nil
            *make-forms* nil)
      (setf form (make-c1form* 'PROGN :args (nconc previous (list form))))))
  form)

(defun t1defmacro (args)
  (check-args-number 'LOAD-TIME-VALUE args 2)
  (destructuring-bind (name lambda-list &rest body)
      args
    (multiple-value-bind (function pprint doc-string)
        (si:expand-defmacro name lambda-list body)
      (declare (ignore pprint doc-string))
      (let ((fn (cmp-eval function *cmp-env*)))
        (cmp-env-register-global-macro name fn))
      (t1expr* (macroexpand `(DEFMACRO ,@args))))))

(defun c1load-time-value (args)
  (check-args-number 'LOAD-TIME-VALUE args 1 2)
  (let ((form (first args))
        loc)
    (cond ((not (listp *load-time-values*))
           ;; When using COMPILE, we set *load-time-values* to 'VALUES and
           ;; thus signal that we do not want to compile these forms, but
           ;; just to retain their value.
           (return-from c1load-time-value
             (c1constant-value (cmp-eval form))))
          ((typep form '(or list symbol))
           (setf loc (data-empty-loc))
           (push (make-c1form* 'LOAD-TIME-VALUE :args loc (c1expr form))
                 *load-time-values*))
          (t
           (setf loc (add-object (cmp-eval form)))))
    (make-c1form* 'LOCATION :type t :args loc)))

;;; ----------------------------------------------------------------------------
;;; Optimizer for FSET. Removes the need for a special handling of DEFUN as a
;;; toplevel form and also allows optimizing calls to DEFUN or DEFMACRO which
;;; are not toplevel, but which create no closures.
;;;
;;; The idea is as follows: when the function or macro to be defined is not a
;;; closure, we can use the auxiliary C functions c_def_c_*() instead of
;;; creating a closure and invoking si_fset(). However until the C2 phase of
;;; the compiler we do not know whether a function is a closure, hence the need
;;; for a c2fset.
;;;
;;; We optimize:
;;;
;;;     (SYS:FSET NAME #'(LAMBDA ...) ...)
;;;
;;; where LAMBDA is expanded by C1FUNCTION to:
;;;
;;;     (SYS:FSET NAME (FLET ((FOO ...)) #'FOO))
;;;
(defun t1fset (args)
  (let ((form `(si::fset ,@args)))
    (when *compile-time-too*
      (cmp-eval form))
    (let ((*compile-toplevel* nil)
          (*compile-time-too* nil))
      (add-load-time-values (c1fset form)))))

(defun c1fset (form)
  (destructuring-bind (fname def &optional (macro nil) (pprint nil))
      (rest form)
    (let* ((*use-c-global* t)
           (fun-form (c1expr def)))
      (when (eq (c1form-name fun-form) 'LOCALS)
        (let* ((function-list (c1form-arg 0 fun-form))
               (fun-object (pop function-list))
               (form (c1form-arg 1 fun-form)))
          (when (and
                 ;; Only 1 function
                 (null function-list)
                 ;; Not closed over anything
                 (every #'global-var-p (fun-referenced-vars fun-object))
                 ;; Referencing the function variable
                 (eq (c1form-name form) 'VARIABLE)
                 (eq (c1form-arg 0 form) (fun-var fun-object)))
            (when (fun-no-entry fun-object)
              (when macro
                (cmperr "Declaration C-LOCAL used in macro ~a." fname))
              (return-from c1fset
                (make-c1form* 'SI:FSET :args fun-object nil nil nil nil)))
            (when (and (typep macro 'boolean)
                       (typep pprint '(or integer null))
                       (consp fname)
                       (eq (first fname) 'quote))
              (return-from c1fset
                (make-c1form* 'SI:FSET :args
                              fun-object ;; Function object
                              (let ((fname (second fname)))
                                (add-object fname :permanent t
                                                  :duplicate t
                                                  :always t
                                                  :used-p t))
                              macro
                              pprint
                              ;; The c1form, when we do not optimize
                              (list (c1expr fname)
                                    fun-form
                                    (c1expr macro)
                                    (c1expr pprint)))))))))
    (t1ordinary form)))
