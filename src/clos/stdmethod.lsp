;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

;;; ----------------------------------------------------------------------
;;; Funcallable object
;;; ----------------------------------------------------------------------

(defclass funcallable-standard-object (standard-object function) ())

;;; ----------------------------------------------------------------------
;;; Generic Functions
;;; ----------------------------------------------------------------------

(defclass generic-function (metaobject funcallable-standard-object) ()
  (:metaclass 'funcallable-standard-class))

(defclass standard-generic-function (generic-function)
  #.(remove-accessors +standard-generic-function-slots+)
  (:metaclass 'funcallable-standard-class))

;;;----------------------------------------------------------------------
;;; Method
;;; ----------------------------------------------------------------------

(defclass method (metaobject) ())

(defclass standard-method (method)
  #.(remove-accessors +standard-method-slots+))


(defun function-keywords (method)
  (multiple-value-bind (reqs opts rest-var key-flag keywords)
      (si::process-lambda-list (slot-value method 'lambda-list) 'function)
    (declare (ignore reqs opts rest-var))
    (when key-flag
      (do* ((output '())
	    (l (cdr keywords) (cddddr l)))
	   ((endp l)
	    output)
	(push (first l) output)))))

(defclass standard-accessor-method (standard-method)
  ((slot-definition :initarg :slot-definition
		    :initform nil 
		    :reader accessor-method-slot-definition)))

(defclass standard-reader-method (standard-accessor-method) ())

(defclass standard-writer-method (standard-accessor-method) ())

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
  (add-method-keywords (call-next-method)))

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
  (pushnew method (specializer-direct-methods spec))
  (let ((gf (method-generic-function method)))
    (pushnew gf (specializer-direct-generic-functions spec)))
  (values))

(defmethod remove-direct-method ((spec specializer) (method method))
  (let* ((gf (method-generic-function method))
	 (methods (delete method (specializer-direct-methods spec))))
    (setf (specializer-direct-methods spec) methods)
    (unless (find gf methods :key #'method-generic-function)
      (setf (specializer-direct-generic-functions spec)
	    (delete gf (specializer-direct-generic-functions spec))))
    (values)))

(defmethod remove-direct-method ((spec eql-specializer) (method method))
  (mp:with-lock (*eql-specializer-lock*)
    (call-next-method)
    (unless (specializer-direct-methods spec)
      (remhash spec *eql-specializer-hash*)))
  (values))
