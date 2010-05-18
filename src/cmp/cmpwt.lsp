;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPWT  Output routines.

(in-package "COMPILER")

;;; ======================================================================
;;;
;;; DATA FILES
;;;
;;; Each lisp compiled file consists on code and a data section. Whenever an
;;; #'in-package toplevel form is found, a read-time evaluated expression is
;;; inserted in the data section which changes the current package for the
;;; rest of it. This way it is possible to save some space by writing the
;;; symbol's package only when it does not belong to the current package.


(defun data-permanent-storage-size ()
  (length *permanent-objects*))

(defun data-temporary-storage-size ()
  (length *temporary-objects*))

(defun data-size ()
  (+ (data-permanent-storage-size)
     (data-temporary-storage-size)))

(defun data-init (&optional filename)
  (if (and filename (probe-file filename))
      (with-open-file (s filename :direction :input)
	(setf *permanent-objects* (read s)
	      *temporary-objects* (read s)))
      (setf *permanent-objects* (make-array 128 :adjustable t :fill-pointer 0)
	    *temporary-objects* (make-array 128 :adjustable t :fill-pointer 0))))

(defun data-get-all-objects ()
  ;; We collect all objects that are to be externalized, but filter out
  ;; those which will be created by a lisp form.
  (loop for i in (nconc (map 'list #'first *permanent-objects*)
			(map 'list #'first *temporary-objects*))
	collect (if (gethash i *load-objects*)
		    0
		    i)))

(defun data-dump (stream &key as-lisp-file init-name &aux must-close)
  (etypecase stream
    (null (return-from data-dump))
    ((or pathname string)
     (setf stream (open stream :direction :output :if-does-not-exist :create
			:if-exists :supersede :external-format :default)
	   must-close stream))
    (stream))
  (si::with-ecl-io-syntax
    (let ((output nil))
      (cond (as-lisp-file
             (print *permanent-objects* stream)
             (print *temporary-objects* stream))
            (*compiler-constants*
             (format stream "~%#define compiler_data_text NULL~%#define compiler_data_text_size 0~%")
             (setf output (concatenate 'vector (data-get-all-objects))))
            ((plusp (data-size))
             (wt-data-begin stream)
             (wt-filtered-data
              (subseq (prin1-to-string (data-get-all-objects)) 1)
              stream)
             (wt-data-end stream)))
      (when must-close
        (close must-close))
      (data-init)
      output)))

(defun wt-data-begin (stream)
  (setq *wt-string-size* 0)
  (setq *wt-data-column* 80)
  (princ "static const char compiler_data_text[] = " stream)
  nil)

(defun wt-data-end (stream)
  (princ #\; stream)
  (format stream "~%#define compiler_data_text_size ~D~%" *wt-string-size*)
  (setf *wt-string-size* 0))

(defun data-empty-loc ()
  (add-object 0 :duplicate t :permanent t))

(defun add-load-form (object location)
  (when (clos::need-to-make-load-form-p object)
    (if (not (eq *compiler-phase* 't1))
	(cmperr "Unable to internalize complex object ~A in ~a phase" object *compiler-phase*)
	(multiple-value-bind (make-form init-form) (make-load-form object)
	  (setf (gethash object *load-objects*) location)
	  (when make-form
	    (push (make-c1form* 'MAKE-FORM :args location (c1expr make-form)) *make-forms*))
	  (when init-form
	    (push (make-c1form* 'INIT-FORM :args location (c1expr init-form)) *make-forms*))))))

(defun add-object (object &key (duplicate nil)
		   (permanent (or (symbolp object) *permanent-data*)))
  ;; FIXME! Currently we have two data vectors and, when compiling
  ;; files, it may happen that a constant is duplicated and stored
  ;; both in VV and VVtemp. This would not be a problem if the
  ;; constant were readable, but due to using MAKE-LOAD-FORM we may
  ;; end up having two non-EQ objects created for the same value.
  (let* ((test (if *compiler-constants* 'eq 'equal))
	 (array (if permanent *permanent-objects* *temporary-objects*))
	 (vv (if permanent 'VV 'VV-temp))
	 (x (or (and (not permanent)
		     (find object *permanent-objects* :test test
			   :key #'first))
		(find object array :test test :key #'first)))
	 (next-ndx (length array))
	 found)
    (cond ((add-static-constant object))
          ((and x duplicate)
	   (setq x (list vv next-ndx))
	   (vector-push-extend (list object x next-ndx) array)
	   x)
	  (x
	   (second x))
	  ((and (not duplicate)
		(symbolp object)
		(multiple-value-setq (found x) (si::mangle-name object)))
	   x)
	  (t
	   (setq x (list vv next-ndx))
	   (vector-push-extend (list object x next-ndx) array)
	   (unless *compiler-constants*
	     (add-load-form object x))
	   x))))

(defun add-symbol (symbol)
  (add-object symbol :duplicate nil :permanent t))

(defun add-keywords (keywords)
  ;; We have to build, in the vector VV[], a sequence with all
  ;; the keywords that this function uses. It does not matter
  ;; whether each keyword has appeared separately before, because
  ;; cl_parse_key() needs the whole list. However, we can reuse
  ;; keywords lists from other functions when they coincide with ours.
  ;; We search for keyword lists that are similar. However, the list
  ;; *OBJECTS* contains elements in decreasing order!!!
  (let ((x (search keywords *permanent-objects*
		   :test #'(lambda (k record) (eq k (first record))))))
    (if x
        (second (elt *permanent-objects* x))
	(prog1
	    (add-object (pop keywords) :duplicate t :permanent t)
	  (dolist (k keywords)
	    (add-object k :duplicate t :permanent t))))))

;;; ======================================================================
;;;
;;; STATIC CONSTANTS
;;;

(defun static-base-string-builder (name value stream)
  (format stream "ecl_def_ct_base_string(~A," name)
  (wt-filtered-data value stream t)
  (format stream ",~D,static,const);" (length value)))

(defun static-single-float-builder (name value stream)
  (let* ((*read-default-float-format* 'single-float)
         (*print-readably* t))
    (format stream "ecl_def_ct_single_float(~A,~S,static,const);" name value stream)))

(defun static-double-float-builder (name value stream)
  (let* ((*read-default-float-format* 'double-float)
         (*print-readably* t))
    (format stream "ecl_def_ct_single_float(~A,~S,static,const);" name value stream)))

(defun static-constant-builder (format value)
  (lambda (name stream)
    (format stream format name value)))

(defun static-constant-expression (object)
  (typecase object
    (base-string #'static-base-string-builder)
    ;;(single-float #'static-single-float-builder)
    ;;(double-float #'static-double-float-builder)
    (t nil)))

(defun add-static-constant (object)
  #+msvc
  nil
  #-:msvc
  ;; FIXME! The Microsoft compiler does not allow static initialization of bit fields.
  (unless (or *compiler-constants* (not (listp *static-constants*)))
    (let ((record (find object *static-constants* :key #'first :test #'equal)))
      (if record
          (second record)
          (let ((builder (static-constant-expression object)))
            (when builder
              (let* ((c-name (format nil "_ecl_static_~D" (length *static-constants*))))
                (push (list object c-name builder) *static-constants*)
                `(VV ,c-name))))))))

(defun vt-loc-value (loc)
  (flet ((static-constant-value (index)
	   (first (find loc *static-constants* :key #'second
			:test #'string=))))
    (let ((index (second loc)))
      (cond ((stringp index)
	     (static-constant-value index))
	    ((eq (car loc) 'VV)
	     (aref *permanent-objects* index))
	    (t
	     (VV-TEMP (aref *temporary-objects* index)))))))
