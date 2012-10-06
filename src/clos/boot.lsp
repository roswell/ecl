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
;;; Class SPECIALIZER

(eval-when (:compile-toplevel :execute)
  (defparameter +specializer-slots+
    '((flag :initform nil :accessor eql-specializer-flag)
      (direct-methods :initform nil :accessor specializer-direct-methods)
      (direct-generic-functions :initform nil :accessor specializer-direct-generic-functions)))
  (defparameter +eql-specializer-slots+
    '((flag :initform t :accessor eql-specializer-flag)
      (direct-methods :initform nil :accessor specializer-direct-methods)
      (direct-generic-functions :initform nil :accessor specializer-direct-generic-functions)
      (object :initarg :object :accessor eql-specializer-object))))

;;; ----------------------------------------------------------------------
;;; Class METHOD-COMBINATION

(eval-when (:compile-toplevel :execute)
  (defparameter +method-combination-slots+
    `((name :initform :name :accessor method-combination-name)
      (compiler :initform :compiler :accessor method-combination-compiler)
      (options :initform :options :accessor method-combination-options))))

;;; ----------------------------------------------------------------------
;;; Class CLASS

(eval-when (:compile-toplevel :execute)
  (defparameter +class-slots+
    `(,@+specializer-slots+
      (name :initarg :name :initform nil :accessor class-id)
      (direct-superclasses :initarg :direct-superclasses
       :accessor class-direct-superclasses)
      (direct-subclasses :initform nil :accessor class-direct-subclasses)
      (slots :accessor class-slots)
      (precedence-list :accessor class-precedence-list)
      (direct-slots :initarg :direct-slots :accessor class-direct-slots)
      (direct-default-initargs :initarg :direct-default-initargs
       :initform nil :accessor class-direct-default-initargs)
      (default-initargs :accessor class-default-initargs)
      (finalized :initform nil :accessor class-finalized-p)
      (docstring :initarg :documentation :initform nil)
      (size :accessor class-size)
      (sealedp :initarg :sealedp :initform nil :accessor class-sealedp)
      (prototype)
      (dependents :initform nil :accessor class-dependents)
      (valid-initargs :accessor class-valid-initargs)))

  (defconstant +class-name-ndx+
    (position 'name +class-slots+ :key #'first))
  (defconstant +class-precedence-list-ndx+
    (position 'precedence-list +class-slots+ :key #'first)))

;;; ----------------------------------------------------------------------
;;; STANDARD-CLASS

(eval-when (:compile-toplevel :execute)
  (defparameter +standard-class-slots+
    (append +class-slots+
	    '((slot-table :accessor slot-table)
	      (optimize-slot-access)
	      (forward)))))

;;; ----------------------------------------------------------------------
;;; STANDARD-GENERIC-FUNCTION

(eval-when (:compile-toplevel :execute)
  (defparameter +standard-generic-function-slots+
    '((name :initarg :name :initform nil
       :accessor generic-function-name)
      (spec-list :initform nil :accessor generic-function-spec-list)
      (method-combination 
       :initarg :method-combination :initform (find-method-combination (class-prototype (find-class 'standard-generic-function)) 'standard nil)
       :accessor generic-function-method-combination)
      (lambda-list :initarg :lambda-list
       :accessor generic-function-lambda-list)
      (argument-precedence-order 
       :initarg :argument-precedence-order
       :initform nil
       :accessor generic-function-argument-precedence-order)
      (method-class
       :initarg :method-class
       :initform (find-class 'standard-method))
      (docstring :initarg :documentation :initform nil)
      (methods :initform nil :accessor generic-function-methods)
      (a-p-o-function :initform nil :accessor generic-function-a-p-o-function)
      (declarations
       :initarg :declarations
       :initform nil
       :accessor generic-function-declarations)
      (dependents :initform nil :accessor generic-function-dependents))))

;;; ----------------------------------------------------------------------
;;; STANDARD-METHOD

(eval-when (:compile-toplevel :execute)
  (defparameter +standard-method-slots+
    '((the-generic-function :initarg :generic-function :initform nil
       :accessor method-generic-function)
      (lambda-list :initarg :lambda-list
       :accessor method-lambda-list)
      (specializers :initarg :specializers :accessor method-specializers)
      (qualifiers :initform nil :initarg :qualifiers :accessor method-qualifiers)
      (the-function :initarg :function :accessor method-function)
      (docstring :initarg :documentation :initform nil)
      (plist :initform nil :initarg :plist :accessor method-plist)
      (keywords :initform nil :accessor method-keywords)))

  (defparameter +standard-accessor-method-slots+
    (append +standard-method-slots+
	    '((slot-definition :initarg :slot-definition
		    :initform nil 
	       ;; FIXME! Should be a :reader
		    :accessor accessor-method-slot-definition)))))

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
       :direct-slots #.+method-combination-slots+)
      (specializer
       :direct-superclasses (metaobject)
       :direct-slots #.+specializer-slots+)
      (eql-specializer
       :direct-superclasses (specializer)
       :direct-slots #.+eql-specializer-slots+)
      (class
       :direct-superclasses (specializer)
       :direct-slots #.+class-slots+)
      (forward-referenced-class
       :direct-superclasses (class)
       :direct-slots #.+class-slots+)
      (built-in-class
       :direct-superclasses (class)
       :direct-slots #1=#.+standard-class-slots+)
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
       :direct-slots #.+standard-generic-function-slots+
       :metaclass funcallable-standard-class)
      (method
       :direct-superclasses (metaobject))
      (standard-method
       :direct-superclasses (method)
       :direct-slots #.+standard-method-slots+)
      (standard-accessor-method
       :direct-superclasses (standard-method)
       :direct-slots #2=#.+standard-accessor-method-slots+)
      (standard-reader-method
       :direct-superclasses (standard-accessor-method)
       :direct-slots #2#)
      (standard-writer-method
       :direct-superclasses (standard-accessor-method)
       :direct-slots #2#)
      )))

;;; ----------------------------------------------------------------------
;;; Early accessors and class construction
;;;

(eval-when (:compile-toplevel :execute)
  (defmacro with-early-accessors ((&rest slot-definitions) &rest body)
    `(macrolet
	 ,(loop for slots in slot-definitions
	     nconc (loop for (name . slotd) in (if (symbolp slots)
						   (symbol-value slots)
						   slots)
		      for index from 0
		      for accessor = (getf slotd :accessor)
		      when accessor
		      collect `(,accessor (object) `(si::instance-ref ,object ,,index))))
       ,@body))
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
  (loop for s in (parse-slots (remove-accessors slots))
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
