;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
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

(defpackage "CLOS"
  (:use "CL" "EXT")
  (:import-from "SI" "UNBOUND" "GET-SYSPROP" "PUT-SYSPROP" "REM-SYSPROP"
		"SIMPLE-PROGRAM-ERROR"))

(in-package "CLOS")

(defparameter *clos-booted* nil)

;;;----------------------------------------------------------------------
;;; BOOTSTRAP FUNCTIONS TO ACCESS SLOTS
;;;
;;; ECL has some restictions regarding the basic classes CLASS,
;;; STANDARD-CLASS and STANDARD-GENERIC-FUNCTION. These are that, certain
;;; slots must have pre-defined positions which cannot change. That means
;;; that a user can extend these classes, but they must be the first ones
;;; in the class hierarchy, and the position of their slots must not change.

(eval-when (compile eval)
(defun create-accessors (slotds type)
  (let* ((names '())
	 (forms (loop for i from 0
		   for s in slotds
		   for accessor = (getf (cdr s) :accessor)
		   for reader = (getf (cdr s) :reader)
		   when reader
		   do (pushnew reader names)
		   and collect `(defun ,reader (obj)
				  (si::instance-ref obj ,i))
		   when accessor
		   do (pushnew accessor names)
		   and collect `(defun ,accessor (obj)
				  (si::instance-ref obj ,i))
		   and collect `(defsetf ,accessor (obj) (x)
				  `(si::instance-set ,obj ,,i ,x)))))
    `(progn
       #+nil
       (eval-when (:compile-toplevel :execute)
         (proclaim '(notinline ,@names)))
       ,@forms)))
(defun remove-accessors (slotds)
  (loop for i in slotds
     for j = (copy-list i)
     do (remf (cdr j) :accessor)
     collect j))
)

;;; ----------------------------------------------------------------------
;;; Class SPECIALIZER

(eval-when (compile eval)
  (defparameter +specializer-slots+
    '((flag :initform nil :accessor eql-specializer-flag)
      (direct-methods :initform nil :accessor specializer-direct-methods)
      (direct-generic-functions :initform nil :accessor specializer-direct-generic-functions)))
  (defparameter +eql-specializer-slots+
    '((flag :initform t :accessor eql-specializer-flag)
      (direct-methods :initform nil :accessor specializer-direct-methods)
      (direct-generic-functions :initform nil :accessor specializer-direct-generic-functions)
      (object :initarg :object :accessor eql-specializer-object))))

#.(create-accessors +eql-specializer-slots+ 'eql-specializer)

;;; ----------------------------------------------------------------------
;;; Class METHOD-COMBINATION

(eval-when (compile eval)
  (defparameter +method-combination-slots+
    `((name :initform :name :accessor method-combination-name)
      (compiler :initform :compiler :accessor method-combination-compiler)
      (options :initform :options :accessor method-combination-options))))

#.(create-accessors +method-combination-slots+ 'method-combination)

;;; ----------------------------------------------------------------------
;;; Class CLASS

(eval-when (compile eval)
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
      (valid-initargs :initform nil :accessor class-valid-initargs)))

  (defconstant +class-name-ndx+
    (position 'name +class-slots+ :key #'first))
  (defconstant +class-precedence-list-ndx+
    (position 'precedence-list +class-slots+ :key #'first)))

;#.(create-accessors +class-slots+ 'class)

;;; ----------------------------------------------------------------------
;;; STANDARD-CLASS

(eval-when (compile eval)
  (defparameter +standard-class-slots+
    (append +class-slots+
	    '((slot-table :accessor slot-table)
	      (optimize-slot-access)
	      (forward)))))

#.(create-accessors +standard-class-slots+ 'standard-class)

;;; ----------------------------------------------------------------------
;;; STANDARD-GENERIC-FUNCTION

(eval-when (compile eval)
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

#.(create-accessors +standard-generic-function-slots+
		    'standard-generic-function)

;;; ----------------------------------------------------------------------
;;; STANDARD-METHOD

(eval-when (compile eval)
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
      (keywords :initform nil :accessor method-keywords))))

#.(create-accessors +standard-method-slots+ 'standard-method)

;;; ----------------------------------------------------------------------
;;;
;;; FIND-CLASS  naming classes.
;;;
;;;
;;; (FIND-CLASS <name>) returns the class named <name>.  setf can be used
;;; with find-class to set the class named <name>.  These are "extrinsic"
;;; names.  Neither find-class nor setf of find-class do anything with the
;;; name slot of the class, they only lookup and change the association from
;;; name to class.
;;; 
;;; This is only used during boot. The real one is in built-in.
(eval-when (compile)
  (defun setf-find-class (new-value class &optional errorp env)
    (warn "Ignoring class definition for ~S" class)))

(defun setf-find-class (new-value name &optional errorp env)
  (declare (ignore errorp env))
  (let ((old-class (find-class name nil)))
    (cond
      ((and old-class
	    (or (typep old-class 'built-in-class)
		(member name '(class built-in-class) :test #'eq)))
       (unless (eq new-value old-class)
	 (error "The class associated to the CL specifier ~S cannot be changed."
		name)))
      ((classp new-value)
       (setf (gethash name si:*class-name-hash-table*) new-value))
      ((null new-value) (remhash name si:*class-name-hash-table*))
      (t (error "~A is not a class." new-value))))
  new-value)

(defsetf find-class (&rest x) (v) `(setf-find-class ,v ,@x))

(defun classp (obj)
  (and (si:instancep obj)
       (let ((topmost (find-class 'CLASS nil)))
	 ;; All instances can be classes until the class CLASS has
	 ;; been installed. Otherwise, we check the parents.
	 ;(print (list (class-id (class-of obj))topmost (and topmost (class-precedence-list topmost))))
	 (or (null topmost)
	     (si::subclassp (si::instance-class obj) topmost)))
       t))

;;; ----------------------------------------------------------------------
;;; Methods

(defun install-method (name qualifiers specializers lambda-list fun wrap &rest options)
  (declare (ignore doc)
	   (notinline ensure-generic-function))
;  (record-definition 'method `(method ,name ,@qualifiers ,specializers))
  (let* ((gf (ensure-generic-function name))
	 (fun (if wrap (wrapped-method-function fun) fun))
	 (specializers (mapcar #'(lambda (x)
				   (cond ((consp x) (intern-eql-specializer (second x)))
					 ((typep x 'specializer) x)
					 ((find-class x nil))
					 (t
					  (error "In method definition for ~A, found an invalid specializer ~A" name specializers))))
			       specializers))
	 (method (make-method (generic-function-method-class gf)
			      qualifiers specializers lambda-list
			      fun options)))
    (add-method gf method)
    method))

(defun wrapped-method-function (method-function)
  #'(lambda (.combined-method-args. *next-methods*)
      (declare (special .combined-method-args. *next-methods*))
      (apply method-function .combined-method-args.)))

;;; ----------------------------------------------------------------------
;;;                                                         early versions

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

;;; early version used during bootstrap
(defun ensure-generic-function (name &key (lambda-list (si::unbound) l-l-p))
  (if (and (fboundp name) (si::instancep (fdefinition name)))
      (fdefinition name)
      ;; create a fake standard-generic-function object:
      (with-early-make-instance +standard-generic-function-slots+
	(gfun (find-class 'standard-generic-function)
	      :name name
	      :spec-list nil
	      :method-combination (find-method-combination nil 'standard nil)
	      :lambda-list lambda-list
	      :argument-precedence-order
	      (and l-l-p (rest (si::process-lambda-list lambda-list t)))
	      :method-class (find-class 'standard-method)
	      :docstring nil
	      :methods nil
	      :a-p-o-function nil
	      :declarations nil
	      :dependents nil)
	;; create a new gfun
	(set-funcallable-instance-function gfun 'standard-generic-function)
	(setf (fdefinition name) gfun)
	gfun)))

(defun default-dispatch (generic-function)
  (cond ((null *clos-booted*)
	 'standard-generic-function)
	((eq (class-id (class-of generic-function))
	     'standard-generic-function)
	 'standard-generic-function)
	(t)))

(defun compute-discriminating-function (generic-function)
  (values #'(lambda (&rest args)
	      (multiple-value-bind (method-list ok)
		  (compute-applicable-methods-using-classes
		   generic-function
		   (mapcar #'class-of args))
		(unless ok
		  (setf method-list
			(compute-applicable-methods generic-function args))
		  (unless method-list
		    (no-applicable-methods generic-function args)))
		(funcall (compute-effective-method-function
			  generic-function
			  (generic-function-method-combination generic-function)
			  method-list)
			 args
			 nil)))
	  t))

(defun set-generic-function-dispatch (gfun)
  ;;
  ;; We have to decide which discriminating function to install:
  ;;	1* One supplied by the user
  ;;	2* One coded in C that follows the MOP
  ;;	3* One in C specialized for slot accessors
  ;;	4* One in C that does not use generic versions of compute-applicable-...
  ;; Respectively
  ;;	1* The user supplies a discriminating function, or the number of arguments
  ;;	   is so large that they cannot be handled by the C dispatchers with
  ;;	   with memoization.
  ;;	2* The generic function is not a s-g-f but takes less than 64 arguments
  ;;	3* The generic function is a standard-generic-function and all its slots
  ;;	   are standard-{reader,writer}-slots
  ;;	4* The generic function is a standard-generic-function with less
  ;;	   than 64 arguments
  ;;
  ;; This chain of reasoning uses the fact that the user cannot override methods
  ;; such as COMPUTE-APPLICABLE-METHODS, or COMPUTE-EFFECTIVE-METHOD, or
  ;; COMPUTE-DISCRIMINATING-FUNCTION acting on STANDARD-GENERIC-FUNCTION.
  ;;
  (declare (notinline compute-discriminating-function))
  (multiple-value-bind (default-function optimizable)
      ;;
      ;; If the class is not a standard-generic-function, we must honor whatever function
      ;; the user provides. However, we still recognize the case without user-computed
      ;; function, where we can replace the output of COMPUTE-DISCRIMINATING-FUNCTION with
      ;; a similar implementation in C
      (compute-discriminating-function gfun)
    (set-funcallable-instance-function
     gfun
     (cond
       ;; Case 1*
       ((or (not optimizable)
	    (> (length (generic-function-spec-list gfun))
	       si::c-arguments-limit))
	default-function)
       ;; Case 2*
       ((and (not (eq (class-id (class-of gfun)) 'standard-generic-function))
	     *clos-booted*)
	t)
       ;; Cases 3*
       ((loop with class = (find-class 'standard-reader-method nil)
	   for m in (generic-function-methods gfun)
	   always (eq class (class-of m)))
	'standard-reader-method)
       ((loop with class = (find-class 'standard-writer-method nil)
	   for m in (generic-function-methods gfun)
	   always (eq class (class-of m)))
	'standard-writer-method)
       ;; Case 4*
       (t
	'standard-generic-function)))))

;;; ----------------------------------------------------------------------
;;; COMPUTE-APPLICABLE-METHODS
;;;
;;; FIXME! This should be split int an internal function, like
;;; raw-compute-... and a higher level interface, because the current
;;; version does not check _any_ of the arguments but it is
;;; nevertheless exported by the ANSI specification!
;;;
(defun std-compute-applicable-methods (gf args)
  (sort-applicable-methods gf (applicable-method-list gf args) args))

(setf (fdefinition 'compute-applicable-methods) #'std-compute-applicable-methods)

(defun applicable-method-list (gf args)
  (declare (optimize (speed 3))
	   (si::c-local))
  (flet ((applicable-method-p (method args)
	   #+(or)
	   (print `(= ,(mapcar #'class-id (mapcar #'class-of args))
		      ,(mapcar #'class-id (method-specializers method))))
	   (loop for spec in (method-specializers method)
	      for arg in args
	      always (if (eql-specializer-flag spec)
			 (eql arg (eql-specializer-object spec))
			 (si::of-class-p arg spec)))))
    (loop for method in (generic-function-methods gf)
       when (applicable-method-p method args)
       collect method)))

(defun std-compute-applicable-methods-using-classes (gf classes)
  (declare (optimize (speed 3)))
  (flet ((applicable-method-p (method classes)
	   (loop for spec in (method-specializers method)
	      for class in classes
	      always (cond ((eql-specializer-flag spec)
			    ;; EQL specializer invalidate computation
			    ;; we return NIL
			    (when (si::of-class-p (eql-specializer-object spec) class)
			      (return-from std-compute-applicable-methods-using-classes
				(values nil nil)))
			    nil)
			   ((si::subclassp class spec))))))
    (values (sort-applicable-methods
	     gf
	     (loop for method in (generic-function-methods gf)
		when (applicable-method-p method classes)
		collect method)
	     classes)
	    t)))

(defun sort-applicable-methods (gf applicable-list args)
  (declare (optimize (safety 0) (speed 3)))
  #+(or)
  (unless applicable-list
    (print (generic-function-name gf))
    (print (mapcar #'type-of args)))
  (let ((f (generic-function-a-p-o-function gf))
	(args-specializers (mapcar #'class-of args)))
    ;; reorder args to match the precedence order
    (when f
      (setf args-specializers
	    (funcall f (subseq args-specializers 0
			       (length (generic-function-argument-precedence-order gf))))))
    ;; then order the list
    (do* ((scan applicable-list)
	  (most-specific (first scan) (first scan))
	  (ordered-list))
	 ((null (cdr scan))
	  (when most-specific
	    ;; at least one method
	    (nreverse
	     (push most-specific ordered-list))))
      (dolist (meth (cdr scan))
	(when (eq (compare-methods most-specific
				   meth args-specializers f) 2)
	  (setq most-specific meth)))
      (setq scan (delete most-specific scan))
      (push most-specific ordered-list))))

(defun compare-methods (method-1 method-2 args-specializers f)
  (declare (si::c-local))
  (let* ((specializers-list-1 (method-specializers method-1))
	 (specializers-list-2 (method-specializers method-2)))
    (compare-specializers-lists (if f (funcall f specializers-list-1) specializers-list-1)
				(if f (funcall f specializers-list-2) specializers-list-2)
				args-specializers)))

(defun compare-specializers-lists (spec-list-1 spec-list-2 args-specializers)
  (declare (si::c-local))
  (when (or spec-list-1 spec-list-2)
    (ecase (compare-specializers (first spec-list-1)
				 (first spec-list-2)
				 (first args-specializers))
      (1 '1)
      (2 '2)
      (= 
       (compare-specializers-lists (cdr spec-list-1)
				   (cdr spec-list-2)
				   (cdr args-specializers)))
      ((nil)
       (error "The type specifiers ~S and ~S can not be disambiguated~
                  with respect to the argument specializer: ~S"
	      (or (car spec-list-1) t)
	      (or (car spec-list-2) t)
	      (car args-specializers)))))
  )

(defun fast-subtypep (spec1 spec2)
  (declare (si::c-local))
  ;; Specialized version of subtypep which uses the fact that spec1
  ;; and spec2 are either classes or of the form (EQL x)
  (if (eql-specializer-flag spec1)
      (if (eql-specializer-flag spec2)
	  (eql (eql-specializer-object spec1)
	       (eql-specializer-object spec2))
	  (si::of-class-p (eql-specializer-object spec1) spec2))
      (if (eql-specializer-flag spec2)
	  ;; There is only one class with a single element, which
	  ;; is NULL = (MEMBER NIL).
	  (and (null (eql-specializer-object spec2))
	       (eq (class-name spec1) 'null))
	  (si::subclassp spec1 spec2)))
  #+(or)
  (if (atom spec1)
      (if (atom spec2)
	  (si::subclassp spec1 spec2)
	  ;; There is only one class with a single element, which
	  ;; is NULL = (MEMBER NIL).
	  (and (null (second spec2))
	       (eq (class-name spec1) 'null)))
      (if (atom spec2)
	  (si::of-class-p (second spec1) spec2)
	  (eql (second spec1) (second spec2)))))

(defun compare-specializers (spec-1 spec-2 arg-class)
  (declare (si::c-local))
  (let* ((cpl (class-precedence-list arg-class)))
    (cond ((eq spec-1 spec-2) '=)
	  ((fast-subtypep spec-1 spec-2) '1)
	  ((fast-subtypep spec-2 spec-1) '2)
	  ((eql-specializer-flag spec-1) '1) ; is this engough?
	  ((eql-specializer-flag spec-2) '2) ; Beppe
	  ((member spec-1 (member spec-2 cpl)) '2)
	  ((member spec-2 (member spec-1 cpl)) '1)
	  ;; This will force an error in the caller
	  (t nil))))

(defun compute-g-f-spec-list (gf)
  (flet ((nupdate-spec-how-list (spec-how-list specializers gf)
	   ;; update the spec-how of the gfun 
	   ;; computing the or of the previous value and the new one
	   (setf spec-how-list (or spec-how-list
				   (copy-list specializers)))
	   (do* ((l specializers (cdr l))
		 (l2 spec-how-list (cdr l2))
		 (spec-how)
		 (spec-how-old))
		((null l))
	     (setq spec-how (first l) spec-how-old (first l2))
	     (setf (first l2)
		   (if (eql-specializer-flag spec-how)
		       (list* (eql-specializer-object spec-how)
			      (and (consp spec-how-old) spec-how-old))
		       (if (consp spec-how-old)
			   spec-how-old
			   spec-how))))
	   spec-how-list))
  (let* ((spec-how-list nil)
	 (function nil)
	 (a-p-o (generic-function-argument-precedence-order gf)))
    (dolist (method (generic-function-methods gf))
      (setf spec-how-list
	    (nupdate-spec-how-list spec-how-list (method-specializers method) gf)))
    (setf (generic-function-spec-list gf)
	  (loop for type in spec-how-list
		for i from 0
		when type collect (cons type i)))
    (let* ((g-f-l-l (generic-function-lambda-list gf)))
      (when (consp g-f-l-l)
	(let ((required-arguments (rest (si::process-lambda-list g-f-l-l t))))
	  (unless (equal a-p-o required-arguments)
	    (setf function
		  (coerce `(lambda (%list)
			    (destructuring-bind ,required-arguments %list
			      (list ,@a-p-o)))
			  'function))))))
    (setf (generic-function-a-p-o-function gf) function)
    (si:clear-gfun-hash gf))))

(defun print-object (object stream)
  (print-unreadable-object (object stream)))
