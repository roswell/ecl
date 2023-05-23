;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPPROP Type propagation.

(in-package "COMPILER")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defparameter *type-propagation-messages* nil)
  (defmacro prop-message (string &rest args)
    (when *type-propagation-messages*
      `(format *standard-output* ,string ,@args))))

(defun p1ordinary (c1form form)
  (declare (ignore c1form))
  (p1propagate form))

(defun p1fset (c1form fun fname macro pprint c1forms)
  (declare (ignore c1form fname macro pprint c1forms))
  (p1propagate-function fun)
  'function)

(defun p1propagate (form)
  (unless form
    (return-from p1propagate 'null))
  (when (c1form-p form)
    (let ((*cmp-env* (c1form-env form))
          (*compile-file-pathname* (c1form-file form))
          (*compile-file-position* (c1form-file-position form))
          (*current-form* (c1form-form form))
          (*current-toplevel-form* (c1form-toplevel-form form))
          (name (c1form-name form)))
      (ext:when-let ((propagator (gethash name *p1-dispatch-table*)))
        (prop-message "~&;;; Entering type propagation for ~A" name)
        (let ((new-type (apply propagator form (c1form-args form))))
          (prop-message "~&;;; Propagating ~A gives type ~A" name new-type)
          (return-from p1propagate
            (setf (c1form-type form)
                  (values-type-and (c1form-type form) new-type)))))))
  (cmpnote "Refusing to propagate ~A" form)
  (c1form-type form))

(defun p1trivial (form &rest rest)
  (declare (ignore rest))
  (c1form-type form))

(defun p1var (form var)
  (let* (;; Use the type of C1FORM because it might have been
         ;; coerced by a THE form.
         (var-type (var-type var))
         (type (type-and var-type (c1form-primary-type form))))
    (prop-message "~&;;; Querying variable ~A gives ~A" (var-name var) type)
    type))

(defun p1values (form values)
  (declare (ignore form))
  (loop for v in values
        collect (values-type-primary-type (p1propagate v))
          into all-values
        finally (return `(values ,@all-values))))

(defun p1propagate-list (list)
  (loop with final-type = t
        for f in list
        do (setf final-type (p1propagate f))
        finally (return final-type)))

(defun p1block (c1form blk body)
  (declare (ignore c1form))
  (setf (blk-type blk) nil)
  (let ((normal-type (p1propagate body))
        (blk-type (blk-type blk)))
    (if blk-type
        (values-type-or blk-type normal-type)
        normal-type)))

(defun p1return-from (c1form blk return-type value)
  (declare (ignore c1form return-type))
  (let* ((values-type (p1propagate value))
         (blk-type (blk-type blk)))
    (setf (blk-type blk) (if blk-type
                             (values-type-or blk-type values-type)
                             values-type))
    values-type))

(defun p1call-global (c1form fname args)
  (declare (ignore c1form))
  (loop for v in args
        do (p1propagate v)
        finally (let ((type (propagate-types fname args)))
                  (prop-message "~&;;; Computing output of function ~A with args~&;;;  ~{ ~A~}~&;;; gives ~A, while before ~A"
                                fname (mapcar #'c1form-primary-type args)
                                type (c1form-type c1form))
                  (return type))))

(defun p1call-local (c1form fun args)
  (declare (ignore c1form))
  (loop for v in args
        do (p1propagate v)
        finally (return (fun-return-type fun))))

(defun p1catch (c1form tag body)
  (declare (ignore c1form))
  (p1propagate tag)
  (p1propagate body)
  t)

(defun p1throw (c1form catch-value output-value)
  (declare (ignore c1form))
  (p1propagate catch-value)
  (p1propagate output-value)
  t)

(defun p1if (c1form fmla true-branch false-branch)
  (declare (ignore c1form))
  (p1propagate fmla)
  (let ((t1 (p1propagate true-branch))
        (t2 (p1propagate false-branch)))
    (values-type-or t1 t2)))

(defun p1fmla-not (c1form form)
  (declare (ignore c1form))
  (p1propagate form)
  '(member t nil))

(defun p1fmla-and (c1form butlast last)
  (declare (ignore c1form))
  (loop with type = t
        for form in (append butlast (list last))
        do (setf type (p1propagate form))
        finally (return (type-or 'null (values-type-primary-type type)))))

(defun p1fmla-or (c1form butlast last)
  (declare (ignore c1form))
  (loop with type
        with output-type = t
        for form in (append butlast (list last))
        do (setf type (p1propagate form)
                 output-type (type-or (values-type-primary-type type)
                                      output-type))
        finally (return output-type)))

(defun p1lambda (c1form lambda-list doc body &rest not-used)
  (declare (ignore c1form lambda-list doc not-used))
  (prop-message "~&;;;~&;;; Propagating function~&;;;")
  (p1propagate body))

(defun p1propagate-function (fun)
  (setf (fun-return-type fun) (p1propagate (fun-lambda fun))))

(defun p1let* (c1form vars forms body)
  (declare (ignore c1form))
  (loop with type
        for v in vars
        for f in forms
        unless (or (global-var-p v) (var-set-nodes v))
          do (progn
               (setf type (p1propagate f))
               (setf (var-type v)
                     (type-and (values-type-primary-type type) (var-type v)))
               (prop-message "~&;;; Variable ~A assigned type ~A"
                             (var-name v) (var-type v))))
  (p1propagate body))

(defun p1locals (c1form funs body labels)
  (declare (ignore c1form labels))
  (loop for f in funs
        do (p1propagate-function f))
  (p1propagate body))

(defun p1multiple-value-bind (c1form vars-list init-c1form body)
  (declare (ignore c1form))
  (let ((init-form-type (p1propagate init-c1form)))
    (loop for v in vars-list
          for type in (values-type-to-n-types init-form-type (length vars-list))
          unless (or (global-var-p v)
                     (var-set-nodes v))
            do (setf (var-type v) (type-and (var-type v) type)) and
          do (prop-message "~&;;; Variable ~A assigned type ~A"
                           (var-name v) (var-type v)))
    (p1propagate body)))

(defun p1multiple-value-setq (c1form vars-list value-c1form)
  (declare (ignore c1form vars-list))
  (p1propagate value-c1form))

(defun p1progn (c1form forms)
  (declare (ignore c1form))
  (p1propagate-list forms))

(defun p1compiler-typecase (c1form variable expressions)
  (declare (ignore c1form))
  (let ((var-type (var-type variable)))
    (loop with output-type = t
          for (a-type c1form) in expressions
          for c1form-type = (p1propagate c1form)
          when (or (member a-type '(t otherwise))
                   (subtypep var-type a-type))
            do (setf output-type c1form-type)
          finally (return output-type))))

(defun p1checked-value (c1form type value let-form)
  (declare (ignore c1form let-form))
  (let ((value-type (p1propagate value))
         ;;(alt-type (p1propagate let-form))
        )
    (if (subtypep value-type type)
        value-type
        type)))

(defun p1progv (c1form variables values body)
  (declare (ignore c1form))
  (p1propagate variables)
  (p1propagate values)
  (p1propagate body))

(defun p1setq (c1form var value-c1form)
  (declare (ignore c1form))
  (let ((value-type (p1propagate value-c1form)))
    (type-and (var-type var) (values-type-primary-type value-type))))

(defun p1psetq (c1form vars c1forms)
  (declare (ignore c1form vars))
  (loop for form in c1forms
        do (p1propagate form))
  'null)

(defun p1with-stack (c1form body)
  (declare (ignore c1form))
  (p1propagate body))

(defun p1stack-push-values (c1form form inline)
  (declare (ignore c1form inline))
  (p1propagate form)
  nil)

(defvar *tagbody-depth* -1
  "If n > 0, limit the number of passes to converge tagbody forms. If
-1, let the compiler do as many passes as it wishes. Complexity grows
as 2^*tagbody-limit* in the worst cases.")

(defun p1go (c1form tag-var return-type)
  (declare (ignore c1form tag-var return-type))
  t)

(defun p1tagbody (c1form tag-loc body)
  (declare (ignore c1form tag-loc))
  (prop-message "~&;;; P1TAGBODY-SIMPLE pass")
  (loop for f in body do
    (if (tag-p f)
        (prop-message "~&;;; Label ~A found" (tag-name f))
        (p1propagate f)))
  'null)

(defun p1unwind-protect (c1form form body)
  (declare (ignore c1form))
  (let ((output-type (p1propagate form)))
    (p1propagate body)
    output-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun type-from-array-elt (array &aux name)
  "Input is a lisp type representing a valid subtype of ARRAY. Output is
either the array element type or NIL, denoting that we are not able to
compute it. This version only handles the simplest cases."
  (values (cond ((eq array 'string)
                 'character)
                ((eq array 'base-string)
                 'base-char)
                ((member (setf array (si::expand-deftype array))
                         '(array vector simple-array))
                 t)
                ((atom array)
                 (setf array 'array)
                 t)
                ((eq (setf name (first array)) 'OR)
                 `(OR ,@(mapcar #'type-from-array-elt (rest array))))
                ((eq (setf name (first array)) 'AND)
                 `(AND ,@(mapcar #'type-from-array-elt (rest array))))
                ((not (member (first array) 
                              '(array vector simple-array)))
                 (setf array 'array)
                 t)
                ((null (rest array))
                 t)
                (t
                 (let ((x (second array)))
                   (if (eq x '*) t x))))
          array))

(def-type-propagator si::aset (fname array-type &rest indices-and-object)
  (multiple-value-bind (elt-type array-type)
      (type-from-array-elt array-type)
    (values (cons array-type
                  (nconc (make-list (1- (length indices-and-object))
                                    :initial-element 'si::index)
                         (list elt-type)))
            elt-type)))

(def-type-propagator aref (fname array-type &rest indices)
  (multiple-value-bind (elt-type array-type)
      (type-from-array-elt array-type)
    (values (list* array-type (make-list (length indices)
                                         :initial-element 'si::index))
            elt-type)))

(def-type-propagator si::row-major-aset (fname array-type index obj)
  (declare (ignore index obj))
  (multiple-value-bind (elt-type array-type)
      (type-from-array-elt array-type)
    (values (list array-type 'si::index elt-type)
            elt-type)))

(def-type-propagator row-major-aref (fname array-type index)
  (declare (ignore index))
  (multiple-value-bind (elt-type array-type)
      (type-from-array-elt array-type)
    (values (list array-type 'si::index) elt-type)))

