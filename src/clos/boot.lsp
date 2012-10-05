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

;;; ----------------------------------------------------------------------

;;; FROM AMOP:
;;;
;;;	Metaobject Class		Direct Superclasses
;;; 	standard-object			(t)
;;; 	funcallable-standard-object	(standard-object function)
;;; *	metaobject			(standard-object)
;;; *	generic-function		(metaobject funcallable-standard-object)
;;; 	standard-generic-function	(generic-function)
;;; *	method				(metaobject)
;;; 	standard-method			(method)
;;; *	standard-accessor-method	(standard-method)
;;; 	standard-reader-method		(standard-accessor-method)
;;; 	standard-writer-method		(standard-accessor-method)
;;; *	method-combination		(metaobject)
;;; *	slot-definition			(metaobject)
;;; *	direct-slot-definition		(slot-definition)
;;; *	effective-slot-definition	(slot-definition)
;;; *	standard-slot-definition	(slot-definition)
;;; 	standard-direct-slot-definition	(standard-slot-definition direct-slot-definition)
;;; 	standard-effective-slot-definition	(standard-slot-definition effective-slot-definition)
;;; *	specializer			(metaobject)
;;; 	eql-specializer			(specializer)
;;; *	class				(specializer)
;;; 	built-in-class			(class)
;;; 	forward-referenced-class	(class)
;;; 	standard-class			(class)
;;; 	funcallable-standard-class	(class)
;;;
(eval-when (eval)
  (defun canonical-slots (slots)
    (loop for s in (parse-slots (remove-accessors slots))
       collect (canonical-slot-to-direct-slot nil s))))
(eval-when (eval)
  (defconstant +class-hierarchy+
    '((t)
      (standard-object
       :direct-superclasses (t))
      (metaobject
       :direct-superclasses (standard-object))
      (method-combination
       :direct-superclasses (metaobject)
       :direct-slots #.(canonical-slots +method-combination-slots+))
      (specializer
       :direct-superclasses (metaobject)
       :direct-slots #.(canonical-slots +specializer-slots+))
      (eql-specializer
       :direct-superclasses (specializer)
       :direct-slots #.(canonical-slots +eql-specializer-slots+))
      (class
       :direct-superclasses (specializer)
       :direct-slots #.(canonical-slots +class-slots+))
      (forward-referenced-class
       :direct-superclasses (class)
       :direct-slots #.(canonical-slots +class-slots+))
      (built-in-class
       :direct-superclasses (class)
       :direct-slots #1=#.(canonical-slots +standard-class-slots+))
      (std-class
       :direct-superclasses (class)
       :direct-slots #1#)
      (standard-class
       :direct-superclasses (std-class)
       :direct-slots #1#)
      (funcallable-standard-class
       :direct-superclasses (std-class)
       :direct-slots #1#))))

;;; ----------------------------------------------------------------------
;;; Building the classes T, CLASS, STANDARD-OBJECT and STANDARD-CLASS.
;;;
;;; We cannot use the functions CREATE-STANDARD-CLASS and others because SLOTS,
;;; DIRECT-SLOTS, etc are empty and therefore SLOT-VALUE does not work.

(defun make-empty-standard-class (name metaclass direct-superclasses direct-slots)
  (declare (si::c-local))
  (let ((class (or (gethash name si::*class-name-hash-table*)
		   (si:allocate-raw-instance nil metaclass
					     #.(length +standard-class-slots+)))))
    (unless metaclass
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
    (add-slots class direct-slots)
    class))

(defun add-slots (class slots)
  (declare (si::c-local))
  ;; It does not matter that we pass NIL instead of a class object,
  ;; because CANONICAL-SLOT-TO-DIRECT-SLOT will make simple slots.
  (loop with all-slots = (copy-list slots)
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

;; 1) Create the classes
;;
;; Notice that, due to circularity in the definition, STANDARD-CLASS has
;; itself as metaclass. MAKE-EMPTY-STANDARD-CLASS takes care of that.
;;
(let ((all-classes 
       (loop with standard-class = (make-empty-standard-class 'standard-class nil nil nil)
	  for c in '#.+class-hierarchy+
	  for class = (make-empty-standard-class name standard-class
						 (getf (rest c) :direct-superclasses)
						 (getf (rest c) :direct-slots))
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
  )

(defconstant +the-t-class+ (find-class 't nil))
(defconstant +the-class+ (find-class 'class nil))
(defconstant +the-std-class+ (find-class 'std-class nil))
(defconstant +the-standard-class+ (find-class 'standard-class nil))
(defconstant +the-funcallable-standard-class+
  (find-class 'funcallable-standard-class nil))

(defmethod class-prototype ((class class))
  (unless (slot-boundp class 'prototype)
    (setf (slot-value class 'prototype) (allocate-instance class)))
  (slot-value class 'prototype))

;;; ----------------------------------------------------------------------
;;; SLOTS READING AND WRITING
;;;
;;;
;;; 1) Functional interface
;;;

(defun find-slot-definition (class slot-name)
  (declare (si::c-local))
  (if (or (eq (si:instance-class class) +the-standard-class+)
          (eq (si:instance-class class) +the-funcallable-standard-class+))
      (gethash slot-name (slot-table class) nil)
      (find slot-name (class-slots class) :key #'slot-definition-name)))

(defun slot-value (self slot-name)
  (let* ((class (class-of self))
	 (slotd (find-slot-definition class slot-name)))
    (if slotd
	(slot-value-using-class class self slotd)
	(values (slot-missing class self slot-name 'SLOT-VALUE)))))

(defun slot-boundp (self slot-name)
  (let* ((class (class-of self))
	 (slotd (find-slot-definition class slot-name)))
    (if slotd
	(slot-boundp-using-class class self slotd)
	(values (slot-missing class self slot-name 'SLOT-BOUNDP)))))

(defun (setf slot-value) (value self slot-name)
  (let* ((class (class-of self))
	 (slotd (find-slot-definition class slot-name)))
    (if slotd
	(funcall #'(setf slot-value-using-class) value class self slotd)
	(slot-missing class self slot-name 'SETF value))
    value))

(defun slot-makunbound (self slot-name)
  (let* ((class (class-of self))
	 (slotd (find-slot-definition class slot-name)))
    (if slotd
	(slot-makunbound-using-class class self slotd)
	(slot-missing class self slot-name 'SLOT-MAKUNBOUND))
    self))

(defun slot-exists-p (self slot-name)
  (and (find-slot-definition (class-of self) slot-name)
       t))

;;;
;;; 2) Overloadable methods on which the previous functions are based
;;;

(defun standard-instance-get (instance slotd)
  (ensure-up-to-date-instance instance)
  (let* ((class (si:instance-class instance))
	 (location (slot-definition-location slotd)))
    (cond ((ext:fixnump location)
	   ;; local slot
	   (si:instance-ref instance (truly-the fixnum location)))
	  ((consp location)
	   ;; shared slot
	   (car location))
	  (t
	   (invalid-slot-definition instance slotd)))))

(defun standard-instance-set (val instance slotd)
  (ensure-up-to-date-instance instance)
  (let* ((class (si:instance-class instance))
	 (location (slot-definition-location slotd)))
    (cond ((ext:fixnump location)
	   ;; local slot
	   (si:instance-set instance (truly-the fixnum location) val))
	  ((consp location)
	   ;; shared slot
	   (setf (car location) val))
	  (t
	   (invalid-slot-definition instance slotd))))
  val)

(defun invalid-slot-definition (instance slotd)
  (error "Effective slot definition lacks a valid location.
Class name: ~A
Slot name: ~A"
	 (type-of instance) (slot-definition-name slotd)))

(defmethod slot-value-using-class ((class class) self slotd)
  (let ((value (standard-instance-get self slotd)))
    (if (si:sl-boundp value)
	value
	(values (slot-unbound class self (slot-definition-name slotd))))))

(defmethod slot-boundp-using-class ((class class) self slotd)
  (declare (ignore class))
  (si::sl-boundp (standard-instance-get self slotd)))

(defmethod (setf slot-value-using-class) (val (class class) self slotd)
  (declare (ignore class))
  (standard-instance-set val self slotd))

(defmethod slot-makunbound-using-class ((class class) instance slotd)
  (declare (ignore class))
  (ensure-up-to-date-instance instance)
  (let* ((location (slot-definition-location slotd)))
    (cond ((ext:fixnump location)
	   ;; local slot
	   (si:sl-makunbound instance (truly-the fixnum location)))
	  ((consp location)
	   ;; shared slot
	   (setf (car location) (unbound)))
	  (t
	   (error "Effective slot definition lacks a valid location:~%~A"
		  slotd))))
  instance)

;;;
;;; 3) Error messages related to slot access
;;;

(defmethod slot-missing ((class t) object slot-name operation 
			 &optional new-value)
  (declare (ignore operation new-value class))
  (print slot-name)
  (print (class-id class))
  (error "~A is not a slot of ~A" slot-name object))

(defmethod slot-unbound ((class t) object slot-name)
  (declare (ignore class))
  (error 'unbound-slot :instance object :name slot-name))

;;;
;;; For the next accessor we define a method.
;;;

(defmethod class-name ((class class))
  (class-id class))

(defmethod (setf class-name) (new-value (class class))
  (reinitialize-instance class :name new-value)
  new-value)

