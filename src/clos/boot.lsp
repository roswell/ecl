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
(eval-when (:compile-toplevel :execute)
  ;;
  ;; All changes to this are connected to the changes in 
  ;; the code of cl_class_of() in src/instance.d
  ;;
  (defconstant +builtin-classes-list+
	 '(;(t object)
	    (sequence)
	      (list sequence)
	        (cons list)
	    (array)
	      (vector array sequence)
	        (string vector)
                #+unicode
	        (base-string string vector)
	        (bit-vector vector)
	    (stream)
	      (ext:ansi-stream stream)
		(file-stream ext:ansi-stream)
		(echo-stream ext:ansi-stream)
		(string-stream ext:ansi-stream)
		(two-way-stream ext:ansi-stream)
		(synonym-stream ext:ansi-stream)
		(broadcast-stream ext:ansi-stream)
		(concatenated-stream ext:ansi-stream)
		(ext:sequence-stream ext:ansi-stream)
	    (character)
	    (number)
	      (real number)
	        (rational real)
		  (integer rational)
		  (ratio rational)
	        (float real)
	      (complex number)
	    (symbol)
	      (null symbol list)
	      (keyword symbol)
	    (package)
	    (function)
	    (pathname)
	      (logical-pathname pathname)
	    (hash-table)
	    (random-state)
	    (readtable)
            (si::code-block)
	    (si::foreign-data)
	    (si::frame)
	    (si::weak-pointer)
	    #+threads (mp::process)
	    #+threads (mp::lock)
	    #+threads (mp::rwlock)
	    #+threads (mp::condition-variable)
	    #+threads (mp::semaphore)
	    #+threads (mp::barrier)
	    #+threads (mp::mailbox)
	    #+sse2 (ext::sse-pack))))

(defconstant +builtin-classes-pre-array+ (make-array (1+ #.(length +builtin-classes-list+))))

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
    `((standard-class
       :metaclass nil) ; Special-cased below
      (t
       :index 0)
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
       :direct-slots #1#
       :metaclass standard-class)
      (funcallable-standard-class
       :direct-superclasses (std-class)
       :direct-slots #1#)
      ,@(loop for (name . rest) in +builtin-classes-list+
	   for index from 1
	   collect (list name :metaclass 'built-in-class
			 :index index
			 :direct-superclasses (or rest '(t))))
      (funcallable-standard-object
       :direct-superclasses (standard-object function))
      (generic-function
       :metaclass funcallable-standard-class
       :direct-superclasses (metaobject funcallable-standard-object))
      (standard-generic-function
       :direct-superclasses (generic-function)
       :direct-slots #.(canonical-slots +standard-generic-function-slots+)
       :metaclass funcallable-standard-class)
      (method
       :direct-superclasses (metaobject))
      (standard-method
       :direct-superclasses (method)
       :direct-slots #.(canonical-slots +standard-method-slots+))
      )))

;;; ----------------------------------------------------------------------
;;; Building the classes T, CLASS, STANDARD-OBJECT and STANDARD-CLASS.
;;;
;;; We cannot use the functions CREATE-STANDARD-CLASS and others because SLOTS,
;;; DIRECT-SLOTS, etc are empty and therefore SLOT-VALUE does not work.

(defun make-empty-standard-class (name &key (metaclass 'standard-class)
				  direct-superclasses direct-slots index)
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
       (loop for c in '#.+class-hierarchy+
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
  )

(defconstant +the-t-class+ (find-class 't nil))
(defconstant +the-class+ (find-class 'class nil))
(defconstant +the-std-class+ (find-class 'std-class nil))
(defconstant +the-standard-class+ (find-class 'standard-class nil))
(defconstant +the-funcallable-standard-class+
  (find-class 'funcallable-standard-class nil))
