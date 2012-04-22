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
  (unless spec-supplied-p
    (error "Specializer list not supplied in method initialization"))
  (unless lambda-supplied-p
    (error "Lambda list not supplied in method initialization"))
  (unless (= (first (si::process-lambda-list lambda-list 'method))
	     (length specializers))
    (error "The list of specializers does not match the number of required arguments in the lambda list ~A"
	   lambda-list))
  (loop for s in specializers
     unless (or (typep s 'specializer)
		(consp s))
     do (error "Object ~A is not a valid specializer" s))
  (add-method-keywords (call-next-method)))
