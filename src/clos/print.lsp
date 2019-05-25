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

;;; ----------------------------------------------------------------------
;;; Load forms
;;;
;;; ECL extends the ANSI specification by allowing to use
;;; MAKE-LOAD-FORM on almost any kind of lisp object.
;;;

(defun make-load-form-saving-slots (object &key slot-names environment)
  (declare (ignore environment))
  (let* ((class (class-of object))
         (class-name (class-name class))
         (initialization-form (list 'progn))
         (slots (class-slots class))
         (decls (when (typep object 'structure-object)
                  (get-sysprop class-name 'si::structure-slot-descriptions))))
    (do ((slot #1=(pop slots) #1#)
         (desc #2=(pop decls) #2#))
        ((null slot) (values `(allocate-instance ,class)
                             (nreverse initialization-form)))
      (let ((slot-name (slot-definition-name slot)))
        (when (or (and (null slot-names)
                       (eq (slot-definition-allocation slot) :instance))
                  (member slot-name slot-names))
          (flet ((primitive-set (val)
                   (if (typep object 'structure-object)
                       `(si::structure-set ,object ',class-name ,(nth 4 desc) ',val)
                       `(setf (slot-value ,object ',slot-name) ',val)))
                 (primitive-nil ()
                   (if (typep object 'structure-object)
                       `(si::structure-set ,object ',class-name ,(nth 4 desc) nil)
                       `(slot-makunbound ,object ',slot-name))))
            (push
             (if (slot-boundp object slot-name)
                 (primitive-set (slot-value object slot-name))
                 (primitive-nil))
             initialization-form)))))))

(defun need-to-make-load-form-p (object env)
  "Return T if the object cannot be externalized using the lisp
printer and we should rather use MAKE-LOAD-FORM."
  (declare (ignore env))
  (let ((*load-form-cache* nil))
    (declare (special *load-form-cache*))
    (labels ((recursive-test (object)
               (loop
                ;; For simple, atomic objects we just return NIL. There is no need to
                ;; call MAKE-LOAD-FORM on them
                (when (typep object '(or character number symbol pathname string bit-vector))
                  (return nil))
                ;; For complex objects we set up a cache and run through the
                ;; objects content looking for data that might require
                ;; MAKE-LOAD-FORM to be externalized.  The cache is used to
                ;; solve the problem of circularity and of EQ references.
                (unless *load-form-cache*
                  (setf *load-form-cache* (make-hash-table :size 128 :test #'eq)))
                (when (gethash object *load-form-cache*)
                  (return nil))
                (setf (gethash object *load-form-cache*) t)
                (cond ((arrayp object)
                       (unless (subtypep (array-element-type object) '(or character number))
                         (dotimes (i (array-total-size object))
                           (recursive-test (row-major-aref object i))))
                       (return nil))
                      ((consp object)
                       (recursive-test (car object))
                       (setf object (rest object)))
                      ((compiled-function-p object)
                       (multiple-value-bind (lex code data name)
                           (si::bc-split object)
                         (when (or (null data)
                                   (null code)
                                   (recursive-test lex)
                                   (recursive-test code)
                                   (recursive-test name))
                           (throw 'need-to-make-load-form t))
                         (setf object data)))
                      (t
                       (throw 'need-to-make-load-form t))))))
      (catch 'need-to-make-load-form
        (recursive-test object)
        nil))))

(defmethod make-load-form ((object t) &optional env)
  (flet ((maybe-quote (object)
           (if (or (consp object) (symbolp object))
               (list 'quote object)
               object)))
    (unless (need-to-make-load-form-p object env)
      (return-from make-load-form (maybe-quote object)))
    (typecase object
      (compiled-function
       (multiple-value-bind (lex code data name)
           (si::bc-split object)
         (unless code
           (error "Cannot externalize object ~a" object))
         (values `(si::bc-join ,(make-load-form lex env)
                               ',code ; An specialized array, no load form
                               ,(make-load-form data env)
                               ,(make-load-form name env)))))
      (array
       (let ((init-forms '()))
         (values `(make-array ',(array-dimensions object)
                   :element-type ',(array-element-type object)
                   :adjustable ',(adjustable-array-p object)
                   :initial-contents
                   ',(loop for i from 0 below (array-total-size object)
                           collect (let ((x (row-major-aref object i)))
                                     (if (need-to-make-load-form-p x env)
                                         (progn (push `(setf (row-major-aref ,object ,i) ',x)
                                                      init-forms)
                                                0)
                                         x))))
                 (and init-forms `(progn ,@init-forms)))))
      (cons
       (values `(cons ,(maybe-quote (car object)) nil)
               (and (rest object) `(rplacd ,(maybe-quote object)
                                           ,(maybe-quote (cdr object))))))
      (hash-table
       (let* ((content (ext:hash-table-content object))
              (make-form `(make-hash-table
                           :size ,(hash-table-size object)
                           :rehash-size ,(hash-table-rehash-size object)
                           :rehash-threshold ,(hash-table-rehash-threshold object)
                           :test ',(hash-table-test object))))
         (if (need-to-make-load-form-p content env)
             (values
              make-form
              `(dolist (i ',(loop for key being each hash-key in object
                               using (hash-value obj)
                               collect (cons key obj)))
                 (setf (gethash (car i) ,object) (cdr i))))
             (values
              `(ext:hash-table-fill ,make-form ',content)
              nil))))
      (random-state
       (let ((state (ext:random-state-array object)))
         (values `(make-random-state ,state) nil)))
      (t
       (no-make-load-form object)))))

(defmethod make-load-form ((object standard-object) &optional environment)
  (no-make-load-form object))

(defmethod make-load-form ((object structure-object) &optional environment)
  (no-make-load-form object))

(defmethod make-load-form ((object condition) &optional environment)
  (no-make-load-form object))

(defun no-make-load-form (object)
  (declare (si::c-local))
  (error "No adequate specialization of MAKE-LOAD-FORM for an object type ~A"
         (type-of object)))

(defmethod make-load-form ((class class) &optional environment)
  (declare (ignore environment))
  (let ((name (class-name class)))
    (if (and name (eq (find-class name) class))
        `(find-class ',name)
        (error "Cannot externalize anonymous class ~A" class))))

(defmethod make-load-form ((package package) &optional environment)
  (declare (ignore environment))
  `(find-package ,(package-name package)))

;;; ----------------------------------------------------------------------
;;; Printing
;;; ----------------------------------------------------------------------

(defmethod print-object ((instance t) stream)
  (let ((*package* (find-package "CL")))
    (print-unreadable-object (instance stream)
      (format stream "~S"
              (class-name (si:instance-class instance)))))
  instance)

(defmethod print-object ((instance standard-object) stream)
  (print-unreadable-object (instance stream)
    (let ((*package* (find-package "CL")))
      (format stream "a ~S"
              (class-name (si:instance-class instance)))))
  instance)

(defmethod print-object ((class class) stream)
  (print-unreadable-object (class stream)
    (let ((*package* (find-package "CL")))
      (format stream "The ~S ~S"
              (class-name (si:instance-class class)) (class-name class))))
  class)

(defmethod print-object ((gf standard-generic-function) stream)
  (print-unreadable-object (gf stream :type t)
    (prin1 (generic-function-name gf) stream))
  gf)

(defmethod print-object ((m standard-method) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~A ~{~S ~}~S"
	    (let ((gf (method-generic-function m)))
	      (if gf
		  (generic-function-name gf)
		  'UNNAMED))
            (method-qualifiers m)
	    (loop for spec in (method-specializers m)
                  collect (cond ((and (classp spec)
                                      (class-name spec)))
                                ((typep spec 'eql-specializer)
                                 `(eql ,(eql-specializer-object spec)))
                                (t spec)))))
  m)

(defun ext::float-nan-string (x)
  (unless (ext:float-nan-p x)
    (signal 'type-error :datum x :expected-type 'float-nan))

  (cond
    ((null *print-readably*)
     (etypecase x
       (single-float "#<single-float quiet NaN>")
       (double-float "#<double-float quiet NaN>")
       (long-float   "#<long-float quiet NaN>")
       (short-float  "#<short-float quiet NaN>")))
    #+ieee-floating-point
    (*read-eval*
     (etypecase x
       (single-float "#.(coerce (si:nan) 'single-float)")
       (double-float "#.(coerce (si:nan) 'double-float)")
       (long-float   "#.(coerce (si:nan) 'long-float)")
       (short-float  "#.(coerce (si:nan) 'short-float)")))
    (t (error 'print-not-readable :object x))))

(defun ext::float-infinity-string (x)
  (unless (ext:float-infinity-p x)
    (signal 'type-error :datum x :expected-type 'float-infinity))

  (cond
    ((null *print-readably*)
     (etypecase x
       (ext:negative-single-float "#<single-float negative infinity>")
       (ext:positive-single-float "#<single-float positive infinity>")
       (ext:negative-double-float "#<double-float negative infinity>")
       (ext:positive-double-float "#<double-float positive infinity>")
       (ext:negative-long-float   "#<long-float negative infinity>")
       (ext:positive-long-float   "#<long-float positive infinity>")
       (ext:negative-short-float  "#<short-float negative infinity>")
       (ext:positive-short-float  "#<short-float positive infinity>")))
    #+ieee-floating-point
    (*read-eval*
     (etypecase x
       (ext:negative-single-float "#.ext::single-float-negative-infinity")
       (ext:positive-single-float "#.ext::single-float-positive-infinity")
       (ext:negative-double-float "#.ext::double-float-negative-infinity")
       (ext:positive-double-float "#.ext::double-float-positive-infinity")
       (ext:negative-long-float   "#.ext::long-float-negative-infinity")
       (ext:positive-long-float   "#.ext::long-float-positive-infinity")
       (ext:negative-short-float  "#.ext::short-float-negative-infinity")
       (ext:positive-short-float  "#.ext::short-float-positive-infinity")))
    (t (error 'print-not-readable :object x))))

;;; ----------------------------------------------------------------------
;;; Describe
;;; ----------------------------------------------------------------------

(defmethod describe-object ((obj t) (stream t))
  (let* ((class (class-of obj))
         (slotds (class-slots class)))
    (format stream "~%~A is an instance of class ~A"
            obj (class-name class))
    (do ((scan slotds (cdr scan))
         (i 0 (1+ i))
         (sv))
        ((null scan))
        (declare (fixnum i))
        (setq sv (si:instance-ref obj i))
        (print (slot-definition-name (car scan)) stream) (princ ":      " stream)
        (if (si:sl-boundp sv)
            (prin1 sv stream)
          (prin1 "Unbound" stream))))
  obj)

(defmethod describe-object ((obj class) (stream t))
  (let* ((class  (si:instance-class obj))
         (slotds (class-slots class)))
    (format stream "~%~A is an instance of class ~A"
            obj (class-name class))
    (do ((scan slotds (cdr scan))
         (i 0 (1+ i))
         (sv))
        ((null scan))
        (declare (fixnum i))
        (print (slot-definition-name (car scan)) stream) (princ ":      " stream)
        (case (slot-definition-name (car scan))
              ((superiors inferiors)
               (princ "(" stream)
               (do* ((scan (si:instance-ref obj i) (cdr scan))
                     (e (car scan) (car scan)))
                    ((null scan))
                    (prin1 (class-name e) stream)
                    (when (cdr scan) (princ " " stream)))
               (princ ")" stream))
              (otherwise 
               (setq sv (si:instance-ref obj i))
               (if (si:sl-boundp sv)
                   (prin1 sv stream)
                 (prin1 "Unbound" stream))))))
  obj)

;; ----------------------------------------------------------------------
