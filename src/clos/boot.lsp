;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.o
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

(defconstant +builtin-classes-pre-array+
  (make-array (1+ #.(length +builtin-classes-list+))))

;;; ----------------------------------------------------------------------
;;; Early accessors and class construction
;;;
;;;
;;; The following macro is also used at bootstap for instantiating
;;; a class based only on the s-form description.
;;;
(eval-when (:compile-toplevel :execute)
  (defmacro with-early-make-instance (slots (object class &rest key-value-pairs)
				      &rest body)
    (when (symbolp slots)
      (setf slots (symbol-value slots)))
    `(let* ((%class ,class)
	    (,object (si::allocate-raw-instance nil %class
						,(length slots))))
       (declare (type standard-object ,object))
       ,@(loop for (name . slotd) in slots
	    for initarg = (getf slotd :initarg)
	    for initform = (getf slotd :initform)
	    for initvalue = (getf key-value-pairs initarg)
	    for index from 0
	    do (cond ((and initarg (member initarg key-value-pairs))
		      (setf initform (getf key-value-pairs initarg)))
		     ((getf key-value-pairs name)
		      (setf initform (getf key-value-pairs name))))
	    collect `(si::instance-set ,object ,index ,initform))
       (when %class
	 (si::instance-sig-set ,object))
       (with-early-accessors (,slots)
	 ,@body))))

;;; ----------------------------------------------------------------------
;;; Building the classes T, CLASS, STANDARD-OBJECT and STANDARD-CLASS.
;;;
;;; We cannot use the functions CREATE-STANDARD-CLASS and others because SLOTS,
;;; DIRECT-SLOTS, etc are empty and therefore SLOT-VALUE does not work.

(defun make-empty-standard-class (name &key (metaclass 'standard-class)
				  direct-superclasses direct-slots index)
  (declare (optimize speed (safety 0)))
  (let* ((the-metaclass (and metaclass (gethash metaclass si::*class-name-hash-table*)))
	 (class (or (gethash name si::*class-name-hash-table*)
		    (si:allocate-raw-instance nil the-metaclass
					      #.(length +standard-class-slots+)))))
    (with-early-accessors (+standard-class-slots+)
      (unless the-metaclass
	(si:instance-class-set class class))
      (setf (class-id                  class) name
	    (class-direct-subclasses   class) nil
	    (class-direct-default-initargs class) nil
	    (class-default-initargs    class) nil
	    (class-finalized-p         class) t
	    (eql-specializer-flag      class) nil
	    (specializer-direct-methods class) nil
	    (specializer-direct-generic-functions class) nil
	    (gethash name si::*class-name-hash-table*) class
	    (class-sealedp             class) nil
	    (class-dependents          class) nil
	    (class-valid-initargs      class) nil
	    )
      (let ((superclasses (loop for name in direct-superclasses
			     for parent = (find-class name)
			     do (push class (class-direct-subclasses parent))
			     collect parent)))
	(setf (class-direct-superclasses class) superclasses
	      (class-precedence-list class)
	      (compute-clos-class-precedence-list class superclasses)))
      (when index
	(setf (aref +builtin-classes-pre-array+ index) class))
      (add-slots class direct-slots)
      class)))

(defun remove-accessors (slotds)
  (declare (optimize speed (safety 0)))
  (loop for i in slotds
     for j = (copy-list i)
     do (remf (cdr j) :accessor)
     collect j))

(defun canonical-slots (slots)
  (declare (optimize speed (safety 0)))
  (loop for s in (parse-slots slots)
     collect (canonical-slot-to-direct-slot nil s)))

(defun add-slots (class slots)
  (declare (si::c-local)
	   (optimize speed (safety 0)))
  ;; It does not matter that we pass NIL instead of a class object,
  ;; because CANONICAL-SLOT-TO-DIRECT-SLOT will make simple slots.
  (loop with all-slots = (canonical-slots slots)
     with table = (make-hash-table :size (if all-slots 24 0))
     for i from 0
     for s in all-slots
     for name = (slot-definition-name s)
     do (setf (slot-definition-location s) i
	      (gethash name table) s)
     finally (setf (class-slots class) all-slots
		   (class-size class) (length all-slots)
		   (slot-table class) table
		   (class-direct-slots class) (copy-list all-slots))))

(defun reader-closure (index)
  (declare (si::c-local))
  (lambda (object) (si::instance-ref object index)))

(defun writer-closure (index)
  (declare (si::c-local))
  (lambda (value object) (si::instance-set object index value)))

(defun generate-accessors (slotd-definitions)
  (declare (si::c-local)
	   (optimize speed (safety 0)))
  (loop for index from 0
     for slotd in slotd-definitions
     do (loop with key-value-pairs = (rest slotd)
	   for key = (pop key-value-pairs)
	   for value = (pop key-value-pairs)
	   while key
	   do (case key
		(:reader
		 (setf (fdefinition value) (reader-closure index)))
		#+(or)
		(:writer ;; not used above
		 (setf (fdefinition value) (writer-closure index)))
		(:accessor
		 (setf (fdefinition value) (reader-closure index)
		       (fdefinition `(setf ,value)) (writer-closure index)))))))

;; 1) Create the classes
;;
;; Notice that, due to circularity in the definition, STANDARD-CLASS has
;; itself as metaclass. MAKE-EMPTY-STANDARD-CLASS takes care of that.
;;
(let* ((class-hierarchy '#.+class-hierarchy+))
  (loop for c in class-hierarchy
     do (generate-accessors (getf (rest c) :direct-slots)))
  (let ((all-classes (loop for c in class-hierarchy
			for class = (apply #'make-empty-standard-class c)
			collect class)))
    ;;
    ;; 2) Class T had its metaclass wrong. Fix it.
    ;;
    (si:instance-class-set (find-class 't) (find-class 'built-in-class))
    ;;
    ;; 3) Finalize
    ;;
    (mapc #'si::instance-sig-set all-classes)
    ;;
    ;; This is needed for further optimization
    ;;
    (setf (class-sealedp (find-class 'method-combination)) t)
    ))

(defconstant +the-t-class+ (find-class 't nil))
(defconstant +the-class+ (find-class 'class nil))
(defconstant +the-std-class+ (find-class 'std-class nil))
(defconstant +the-standard-class+ (find-class 'standard-class nil))
(defconstant +the-funcallable-standard-class+
  (find-class 'funcallable-standard-class nil))
