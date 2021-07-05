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

(defun read-only-variable-p (v other-decls)
  (dolist (i other-decls nil)
    (when (and (eq (car i) :READ-ONLY)
               (member v (rest i)))
      (return t))))

(defun env-grows (possibily)
  ;; if additional closure variables are introduced and this is not
  ;; last form, we must use a new env.
  (and possibily
       (plusp *env*)
       (dolist (exit *unwind-exit*)
         (case exit
           (RETURN (return NIL))
           (BDS-BIND)
           (t (return T))))))

;; should check whether a form before var causes a side-effect
;; exactly one occurrence of var is present in forms
(defun replaceable (var form)
  (labels ((abort-on-side-effects (form)
             (if (eq (c1form-name form) 'VAR)
                 (when (eq var (first (c1form-args form)))
                   (return-from replaceable t))
                 (when (c1form-side-effects form)
                   (return-from replaceable nil)))))
    (traverse-c1form-tree form #'abort-on-side-effects)
    (baboon :format-control "In REPLACEABLE, variable ~A not found. Form:~%~A"
            :format-arguments (list (var-name var) *current-form*))))

#+not-used
(defun discarded (var form body &aux last)
  (labels ((last-form (x &aux (args (c1form-args x)))
             (case (c1form-name x)
               (PROGN
                 (last-form (car (last (first args)))))
               ((LET LET* FLET LABELS BLOCK CATCH)
                (last-form (car (last args))))
               (VAR (c1form-arg 0 x))
               (t x))))
    (and (not (form-causes-side-effect form))
         (or (< (var-ref var) 1)
             (and (= (var-ref var) 1)
                  (eq var (last-form body))
                  (eq 'TRASH *destination*))))))

(defun nsubst-var (var form)
  (when (var-set-nodes var)
    (baboon :format-control "Cannot replace a variable that is to be changed"))
  (when (var-functions-reading var)
    (baboon :format-control "Cannot replace a variable that forms part of a closure"))
  (dolist (where (var-read-forms var))
    (unless (and (eql (c1form-name where) 'VAR)
                 (eql (c1form-arg 0 where) var))
      (baboon :format-control "VAR-READ-NODES are only C1FORMS of type VAR"))
    (delete-from-read-nodes var where)
    (c1form-replace-with where form))
  (setf (var-ignorable var) 0))

#+not-used
(defun member-var (var list)
  (let ((kind (var-kind var)))
    (if (member kind '(SPECIAL GLOBAL))
        (member var list :test
                #'(lambda (v1 v2)
                    (and (member (var-kind v2) '(SPECIAL GLOBAL))
                         (eql (var-name v1) (var-name v2)))))
        (member var list))))

;;;

(defun make-var (&rest args)
  (let ((var (apply #'%make-var args)))
    (unless (member (var-kind var) '(SPECIAL GLOBAL))
      (when *current-function*
        (push var (fun-local-vars *current-function*))))
    var))

(defun make-lcl-var (&key rep-type (type 'T))
  (unless rep-type
    (setq rep-type (if type (lisp-type->rep-type type) :object)))
  (unless type
    (setq type 'T))
  (make-var :kind rep-type :type type :loc (next-lcl)))

(defun make-temp-var (&optional (type 'T))
  (make-var :kind :object :type type :loc `(TEMP ,(next-temp))))

(defun var-referenced-in-form-list (var form-list)
  (loop for f in form-list
          thereis (var-referenced-in-form var f)))

(defun var-changed-in-form-list (var form-list)
  (loop for f in form-list
     thereis (var-changed-in-form var f)))

;;; FIXME! VAR-REFERENCED-IN-FORM and VAR-CHANGED-IN-FORM are too
;;; pessimistic. One should check whether the functions reading/setting the
;;; variable are actually called from the given node.  The problem arises when
;;; we create a closure of a function, as in
;;;
;;;     (let* ((a 1) (b #'(lambda () (incf a)))) ...)
;;;
;;; To know whether A is changed or read, we would have to track where B is
;;; actually used.

(defun var-referenced-in-form (var form)
  (declare (type var var))
  (or (find-form-in-node-list form (var-read-nodes var))
      (var-functions-reading var)))

(defun var-changed-in-form (var form)
  (declare (type var var))
  (or (find-form-in-node-list form (var-set-nodes var))
      (let ((kind (var-kind var)))
        (if (or (eq kind 'SPECIAL) (eq kind 'GLOBAL))
            (c1form-sp-change form)
            (var-functions-setting var)))))

(defun update-variable-type (var orig-type)
  ;; FIXME! Refuse to update type of variables that are modified
  (when (var-set-nodes var)
    (return-from update-variable-type))
  (let ((type (type-and (var-type var) orig-type)))
    (if (null type)
        (cmpwarn "Variable assigned a value incompatible with its type declaration.~%Variable: ~A~%Expected type: ~A~%Value type: ~A"
                 (var-name var)
                 (var-type var)
                 orig-type)
        (loop for form in (var-read-forms var)
           when (and (eq (c1form-name form) 'VAR)
                     (eq var (c1form-arg 0 form)))
           do (setf (c1form-type form) (type-and type (c1form-primary-type form)))
           finally (setf (var-type var) type)))))

(defun var-read-forms (var)
  (mapcar #'first (var-read-nodes var)))

(defun assert-var-ref-value (var)
  (when *debug-compiler*
    (unless (let ((ref (var-ref var)))
              (or (> ref (/ most-positive-fixnum 2))
                  (= (var-ref var) (+ (length (var-read-nodes var))
                                      (length (var-set-nodes var))))))
      (baboon :format-control "Number of references in VAR ~A unequal to references list"
              :format-arguments (list var)))))

(defun assert-var-not-ignored (var)
  (when (let ((x (var-ignorable var))) (and x (minusp x)))
    (cmpwarn-style "Variable ~A, declared as IGNORE, found in a lisp form."
                   (var-name var))
    (setf (var-ignorable var) nil)))

(defun delete-from-read-nodes (var form)
  (assert-var-ref-value var)
  (setf (var-ref var) (1- (var-ref var))
        (var-read-nodes var) (delete-form-from-node-list form (var-read-nodes var))))

(defun add-to-read-nodes (var form)
  (assert-var-ref-value var)
  (assert-var-not-ignored var)
  (setf (var-ref var) (1+ (var-ref var))
        (var-read-nodes var) (add-form-to-node-list form (var-read-nodes var)))
  (when *current-function*
    (unless (eq *current-function* (var-function var))
      (pushnew *current-function* (var-functions-reading var))
      (pushnew var (fun-referenced-vars *current-function*))))
  form)

(defun add-to-set-nodes (var form)
  (assert-var-ref-value var)
  (assert-var-not-ignored var)
  (setf (var-ref var) (1+ (var-ref var))
        (var-set-nodes var) (add-form-to-node-list form (var-set-nodes var)))
  ;;(push form (var-read-nodes var))
  (when *current-function*
    (unless (eq *current-function* (var-function var))
      (pushnew *current-function* (var-functions-setting var))
      (pushnew var (fun-referenced-vars *current-function*))))
  form)

(defun add-to-set-nodes-of-var-list (var-list form)
  (dolist (v var-list)
    (add-to-set-nodes v form))
  form)

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

(defun special-variable-p (name)
  "Return true if NAME is associated to a special variable in the lexical environment."
  (or (si::specialp name)
      (check-global name)
      (let ((v (cmp-env-search-var name *cmp-env-root*)))
        ;; Fixme! Revise the declamation code to ensure whether
        ;; we also have to consider 'GLOBAL here.
        (and v (eq (var-kind v) 'SPECIAL)))))

(defun local-variable-p (name &optional (env *cmp-env*))
  (let ((record (cmp-env-search-var name env)))
    (and record (var-p record))))

(defun symbol-macro-p (name &optional (env *cmp-env*))
  (let ((record (cmp-env-search-var name env)))
    (and record (not (var-p record)))))

(defun variable-type-in-env (name &optional (env *cmp-env*))
  (let ((var (cmp-env-search-var name)))
    (cond ((var-p var)
           (var-type var))
          ((si:get-sysprop name 'CMP-TYPE))
          (t))))

(defun var-rep-type (var)
  (case (var-kind var)
    ((LEXICAL CLOSURE SPECIAL GLOBAL) :object)
    (t (var-kind var))))

(defun check-vref (var)
  (when (eq (var-kind var) 'LEXICAL)
    (when (and (zerop (var-ref var)) ;;; This field may be -1 (IGNORE). Beppe
               (not (var-ignorable var)))
      (cmpwarn-style "The variable ~s is not used." (var-name var)))
    (when (not (var-ref-clb var))
      ;; if the variable can be stored locally, set it var-kind to its type
      (setf (var-kind var)
            (if (plusp (var-ref var))
                (lisp-type->rep-type (var-type var))
                :OBJECT)))))

(defun push-vars (v)
  (setf (var-index v) (length (cmp-env-variables)))
  (cmp-env-register-var v))

(defun unboxed (var)
  (not (eq (var-rep-type var) :object)))

(defun local (var)
  (and (not (member (var-kind var) '(LEXICAL CLOSURE SPECIAL GLOBAL)))
       (var-kind var)))

(defun global-var-p (var)
  (let ((kind (var-kind var)))
    (or (eq kind 'global)
        (eq kind 'special))))

(defun useful-var-p (var)
  (or (plusp (var-ref var))
      (global-var-p var)))

(defun si::register-global (name)
  (pushnew name *global-vars*)
  (values))
