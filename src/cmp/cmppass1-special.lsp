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

(defun c1quote (args)
  (check-args-number 'QUOTE args 1 1)
  (c1constant-value (car args) :always t))

(defun c1declare (args)
  (cmperr "The declaration ~s was found in a bad place." (cons 'DECLARE args)))

(defun c1the (args)
  (check-args-number 'THE args 2 2)
  ;; FIXME: C1CHECKED-VALUE cannot check multiple values.
  (let ((type (first args)))
    (if (and (policy-the-is-checked)
             (not (and (consp type)
                       (eq (first type) 'values))))
        (c1checked-value args)
        (c1truly-the args))))

(defun c1truly-the (args)
  (check-args-number 'ext:truly-the args 2 2)
  (let* ((form (c1expr (second args)))
         (the-type (first args))
         type)
    (setf type (values-type-and the-type (c1form-type form)))
    (if (values-type-primary-type type)
        (setf (c1form-type form) type)
        (cmpwarn "Type mismatch was found in ~s." (cons 'THE args)))
    form))

(defun c1compiler-let (args &aux (symbols nil) (values nil))
  (when (endp args) (too-few-args 'ext:compiler-let 1 0))
  (dolist (spec (car args))
    (cond ((consp spec)
           (cmpck (not (and (symbolp (car spec))
                            (or (endp (cdr spec))
                                (endp (cddr spec)))))
                  "The variable binding ~s is illegal." spec)
           (push (car spec) symbols)
           (push (if (endp (cdr spec)) nil (eval (second spec))) values))
          ((symbolp spec)
           (push spec symbols)
           (push nil values))
          (t (cmperr "The variable binding ~s is illegal." spec))))
  (setq symbols (nreverse symbols))
  (setq values (nreverse values))
  (setq args (progv symbols values (c1progn (cdr args))))
  (make-c1form 'ext:compiler-let args symbols values args))

(defun c1function (args)
  (check-args-number 'FUNCTION args 1 1)
  (let ((fun (car args)))
    (cond ((si::valid-function-name-p fun)
           (let ((funob (local-function-ref fun t)))
             (if funob
                 (let* ((var (fun-var funob)))
                   (add-to-read-nodes var (make-c1form* 'VAR :args var)))
                 (make-c1form* 'FUNCTION
                               :type 'FUNCTION
                               :sp-change (not (and (symbolp fun)
                                                    (si:get-sysprop fun 'NO-SP-CHANGE)))
                               :args 'GLOBAL nil fun))))
          ((and (consp fun) (member (car fun) '(LAMBDA EXT::LAMBDA-BLOCK)))
           (cmpck (endp (cdr fun))
                  "The lambda expression ~s is illegal." fun)
           (let (name body)
             (if (eq (first fun) 'lambda)
                 (let ((decl (si::process-declarations (cddr fun))))
                   (setf name (or (function-block-name-declaration decl)
                                  (gensym "LAMBDA"))
                         body (rest fun)))
                 (setf name (second fun)
                       body (cddr fun)))
             (c1expr `(flet ((,name ,@body)) #',name))))
          (t (cmperr "The function ~s is illegal." fun)))))
