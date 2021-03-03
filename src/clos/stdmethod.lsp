;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: CLOS -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

;;;----------------------------------------------------------------------
;;; Method
;;; ----------------------------------------------------------------------

(defmethod function-keywords ((method standard-method))
  (multiple-value-bind (reqs opts rest-var key-flag keywords)
      (si::process-lambda-list (slot-value method 'lambda-list) 'function)
    (declare (ignore reqs opts rest-var))
    (when key-flag
      (do* ((output '())
            (l (cdr keywords) (cddddr l)))
           ((endp l)
            output)
        (push (first l) output)))))

(defmethod shared-initialize ((method standard-method) slot-names &rest initargs
                              &key (specializers nil spec-supplied-p)
                              (lambda-list nil lambda-supplied-p)
                              generic-function)
  (declare (ignore initargs method slot-names))
  (when slot-names
    (unless spec-supplied-p
      (error "Specializer list not supplied in method initialization"))
    (unless lambda-supplied-p
      (error "Lambda list not supplied in method initialization"))
    (unless (= (first (si::process-lambda-list lambda-list 'method))
               (length specializers))
      (error "The list of specializers does not match the number of required arguments in the lambda list ~A"
             lambda-list)))
  (when spec-supplied-p
    (loop for s in specializers
       unless (typep s 'specializer)
       do (error "Object ~A is not a valid specializer" s)))
  (setf method (call-next-method)
        (method-keywords method) (compute-method-keywords (method-lambda-list method)))
  method)

#+threads
(defparameter *eql-specializer-lock* (mp:make-lock :name 'eql-specializer))
(defparameter *eql-specializer-hash*
  (make-hash-table :size 128 :test #'eql))

(defun intern-eql-specializer (object)
  (let ((table *eql-specializer-hash*))
    (mp:with-lock (*eql-specializer-lock*)
      (or (gethash object table nil)
          (setf (gethash object table)
                (make-instance 'eql-specializer :object object))))))

(defmethod add-direct-method ((spec specializer) (method method))
  (let ((cell (specializer-method-holder spec)))
    (setf (cdr cell) nil
          (car cell) (adjoin method (car cell) :test #'eq)))
  (values))

(defmethod remove-direct-method ((spec specializer) (method method))
  (let ((cell (specializer-method-holder spec)))
    (setf (cdr cell) nil
          (car cell) (delete method (car cell) :test #'eq)))
  (values))

(defmethod remove-direct-method ((spec eql-specializer) (method method))
  (mp:with-lock (*eql-specializer-lock*)
    (call-next-method)
    (unless (specializer-direct-methods spec)
      (remhash spec *eql-specializer-hash*)))
  (values))

(defmethod specializer-direct-methods ((spec specializer))
  (car (specializer-method-holder spec)))

(defmethod specializer-direct-generic-functions ((spec specializer))
  (let ((cell (specializer-method-holder spec)))
    (or (cdr cell)
        (when (car cell)
          (loop with acc = nil
                for method in (car cell)
                do (pushnew (method-generic-function method) acc :test #'eq)
                finally (return (setf (cdr cell) (nreverse acc))))))))
