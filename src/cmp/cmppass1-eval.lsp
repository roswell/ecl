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

(defun c1expr-inner (form)
  (declare (si::c-local))
  (cond ((symbolp form)
         (setq form (chk-symbol-macrolet form))
         (cond ((not (symbolp form))
                form)
               ((eq form nil) (c1nil))
               ((eq form t) (c1t))
               ((keywordp form)
                (make-c1form* 'LOCATION :type (object-type form)
                                        :args (add-symbol form)))
               ((constantp form *cmp-env*)
                (c1variable form (c1constant-symbol-value form (symbol-value form))))
               (t
                (c1variable form nil))))
        ((consp form)
         (cmpck (not (si:proper-list-p form))
                "Improper list found in lisp form~%~A" form)
         (let ((fun (car form)))
           (cond ((let ((fd (gethash fun *c1-dispatch-table*)))
                    (and fd (setf fun fd)))
                  (funcall fun (rest form)))
                 ((symbolp fun)
                  (c1call fun (cdr form) t))
                 ((and (consp fun) (eq (car fun) 'LAMBDA))
                  (c1funcall form))
                 (t (cmperr "~s is not a legal function name." fun)))))
        (t (c1constant-value form))))

(defun c1expr (form)
  (let ((*current-form* form))
    (loop
       (setf form (c1expr-inner form))
       (when (c1form-p form)
         (return form)))))

(defvar *vv-nil*
  (make-vv :value CL:NIL))

(defvar *vv-t*
  (make-vv :value CL:T))

(defun c1nil ()
  (let ((*current-form* nil))
    (make-c1form* 'LOCATION :type 'null :args *vv-nil*)))
(defun c1t ()
  (let ((*current-form* t))
    (make-c1form* 'LOCATION :type '(eql t) :args *vv-t*)))

(defun c1with-backend (forms)
  (c1progn (loop for tag = (pop forms)
              for form = (pop forms)
              while tag
              when (eq tag :c/c++)
              collect form)))

(defun c1progn (forms)
  (cond ((endp forms) (t1/c1expr 'NIL))
        ((endp (cdr forms)) (t1/c1expr (car forms)))
        (t (let* ((fl (mapcar #'t1/c1expr forms))
                  (output-form (first (last fl)))
                  (output-type (and output-form (c1form-type output-form))))
             (make-c1form* 'PROGN :type output-type :args fl)))))

(defun c1args* (forms)
  (mapcar #'c1expr forms))

(defun c1decl-body (decls body)
  (if (null decls)
      (c1progn body)
      (let* ((*cmp-env* (reduce #'add-one-declaration decls
                                :initial-value (cmp-env-copy *cmp-env*))))
        (c1progn body))))

(defun c1locally (args)
  (multiple-value-bind (body ss ts is other-decl)
      (c1body args t)
    (if (or ss ts is other-decl)
        (let ((*cmp-env* (cmp-env-copy)))
          (mapc #'declare-special ss)
          (check-vdecl nil ts is)
          (c1decl-body other-decl body))
        (c1progn body))))

(defun c1macrolet (args)
  (check-args-number 'MACROLET args 1)
  (let ((*cmp-env* (si:cmp-env-register-macrolet (first args) (cmp-env-copy))))
    (c1locally (cdr args))))

(defun c1symbol-macrolet (args)
  (check-args-number 'SYMBOL-MACROLET args 1)
  (let ((*cmp-env* (cmp-env-copy)))
    (dolist (def (car args))
      (let ((name (first def)))     
        (cmpck (or (endp def) (not (symbolp name)) (endp (cdr def)))
             "The symbol-macro definition ~s is illegal." def)
        (cmp-env-register-symbol-macro name (second def))))
    (c1locally (cdr args))))

;;;
;;; Check if the symbol has a symbol macro
;;;
(defun chk-symbol-macrolet (form)
  (loop
   (when (not (symbolp form))
     (return form))
   (let ((new-form (macroexpand-1 form *cmp-env*)))
     (when (eq new-form form)
       (return form))
     (setf form new-form))))

(defun c1constant-value (val)
  (cond
    ((eq val nil) (c1nil))
    ((eq val t) (c1t))
    ((make-c1form* 'LOCATION :type `(eql ,val)
                             :args (add-object val)))))

;;; To inline a constant it must be possible to externalize its value or copies
;;; of the value must be EQL to each other.
(defun c1constant-symbol-value (name val)
  (declare (ignore name))
  (unless (si:need-to-make-load-form-p val)
    (add-object val)))

(defun c1if (args)
  (check-args-number 'IF args 2 3)
  (let ((test (c1expr (car args))))
    ;; Resolve IF expressions with constant arguments
    (multiple-value-bind (constant-p value)
        (c1form-constant-p test)
      (when constant-p
        (return-from c1if
          (if value (second args) (third args)))))
    ;; Otherwise, normal IF form
    (let* ((true-branch (c1expr (second args)))
           (false-branch (c1expr (third args))))
      (make-c1form* 'IF
                    :type (values-type-or (c1form-type true-branch)
                                          (c1form-type false-branch))
                    :args test true-branch false-branch))))

(defun c1not (args)
  (check-args-number 'NOT args 1 1)
  (let* ((value (c1expr (first args))))
    ;; When the argument is constant, we can just return
    ;; a constant as well.
    (multiple-value-bind (constant-p value)
        (c1form-constant-p value)
      (when constant-p
        (return-from c1not (not value))))
    (make-c1form* 'FMLA-NOT
                  :type '(member t nil)
                  :args value)))

(defun c1and (args)
  ;; (AND) => T
  (if (null args)
      (c1t)
      (let* ((values (c1args* args))
             (last (first (last values)))
             (butlast (nbutlast values)))
        ;; (AND x) => x
        (if butlast
            (make-c1form* 'FMLA-AND
                          :type (type-or 'null (c1form-primary-type last))
                          :args butlast last)
            last))))

(defun c1or (args)
  ;; (OR) => NIL
  (if (null args)
      (c1nil)
      (let* ((values (c1args* args))
             (last (first (last values)))
             (butlast (butlast values)))
        ;; (OR x) => x
        (if butlast
            (make-c1form* 'FMLA-OR
                         :type (reduce #'type-or butlast
                                       :key #'c1form-primary-type
                                       :initial-value (c1form-primary-type last))
                         :args butlast last)
            last))))

(defun c1multiple-value-prog1 (args)
  (check-args-number 'MULTIPLE-VALUE-PROG1 args 1)
  (destructuring-bind (form . body) args
    (make-c1form* 'MV-PROG1 :args (c1expr form) (c1args* body))))

;;; Beppe:
;;; this is the WRONG way to handle 1 value problem.
;;; should be done in c2values, so that (values (truncate a b)) can
;;; be used to restrict to one value, so we would not have to warn
;;; if this occurred in a proclaimed fun.

(defun c1values (args)
  (make-c1form* 'CL:VALUES :args (c1args* args)))
