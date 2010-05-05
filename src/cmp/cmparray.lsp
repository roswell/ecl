;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPARRAY. Optimizations related to arrays

;;;;  Copyright (c) 2008. Juan Jose Garcia-Ripol
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

;;;
;;; MAKE-ARRAY
;;;

(define-compiler-macro make-array (&whole form dimensions &key (element-type t)
					  (initial-element nil initial-element-supplied-p)
					  (initial-contents nil initial-contents-supplied-p)
					  adjustable fill-pointer
					  displaced-to (displaced-index-offset 0)
					  &environment env)
  ;; This optimization is always done unless we provide content. There
  ;; is no speed, debug or space reason not to do it, unless the user
  ;; specifies not to inline MAKE-ARRAY, but in that case the compiler
  ;; macro should not be used.
  (unless (or initial-element-supplied-p
	      initial-contents-supplied-p)
    ;; If the type is known and we can assume it will not change, we
    ;; replace it with the upgraded form.
    (when (and (constantp element-type env)
	       (policy-assume-types-dont-change-p env))
      (let ((new-type (cmp-eval element-type)))
	(when (known-type-p new-type)
	  (setf element-type `',(upgraded-array-element-type new-type)))))
    ;; Finally, we choose between making a vector or making a general array.
    ;; It only saves some time, since MAKE-PURE-ARRAY will call MAKE-VECTOR
    ;; if a one-dimensional array is to be created.
    (let ((function 'si::make-pure-array))
      (when (constantp dimensions env)
	(let ((d (cmp-eval dimensions)))
	  (when (or (integerp d) (and (listp d) (= (length d) 1) (setf d (first d))))
	    (setf function 'si::make-vector
		  dimensions `',d)))
	(setf form
	      `(,function ,element-type ,dimensions ,adjustable ,fill-pointer
			  ,displaced-to ,displaced-index-offset)))))
  form)

;;;
;;; VECTOR-PUSH and VECTOR-PUSH-EXTEND
;;;

(defun expand-vector-push (whole env)
  (declare (si::c-local))
  (let* ((extend (eq (first whole) 'vector-push-extend))
	 (args (rest whole)))
    (unless (or ;; Avoid infinite recursion
		(eq (first args) '.val)
		(safe-compile)
		(>= (cmp-env-optimization 'space env) 2))
      (setf whole
	    `(let* ((.val ,(car args))
		    (.vec ,(second args))
		    (.i (fill-pointer .vec))
		    (.dim (array-total-size .vec)))
	       (declare (fixnum .i .dim)
			(:read-only .vec .val .i .dim))
	       (cond ((< .i .dim)
		      (sys::fill-pointer-set .vec (the fixnum (+ 1 .i)))
		      (sys::aset .val .vec .i)
		      .i)
		     (t ,(when extend
			       `(vector-push-extend .val .vec ,@(cddr args)))))))))
  whole)

(define-compiler-macro vector-push (&whole whole &rest args &environment env)
  (expand-vector-push whole env))

(define-compiler-macro vector-push-extend (&whole whole &rest args &environment env)
  (expand-vector-push whole env))

;;;
;;; AREF/ASET
;;;

(define-compiler-macro aref (&whole form array &rest indices &environment env)
  (print form)
  (print (policy-open-code-aref/aset-p env))
  (print (policy-array-bounds-check-p env))
  (print
  (if (policy-open-code-aref/aset-p env)
      (expand-aref array indices (policy-array-bounds-check-p env))
      form)))

(defun expand-aref (array indices check)
  (with-clean-symbols (%array)
    `(let ((%array ,array))
       (declare (:read-only %array)
                (optimize (safety 0)))
       (row-major-aref %array
                       ,(expand-row-major-index '%array indices check)))))

(define-compiler-macro si::aset (&whole form value array &rest indices
                                        &environment env)
  (print form)
  (print (policy-open-code-aref/aset-p env))
  (print (policy-array-bounds-check-p env))
  (print
  (if (policy-open-code-aref/aset-p env)
      (expand-aset array indices value
                   (policy-array-bounds-check-p env))
      form)))

(defun expand-aset (array indices value check)
  (with-clean-symbols (%array %value)
    (let ((indices (expand-row-major-index '%array indices check)))
      `(let ((%value ,value)
             (%array ,array))
       (declare (:read-only %array %value)
                (optimize (safety 0)))
       (si::row-major-aset %array ,indices %value)))))

(defun expand-zero-dim-index-check (a check)
  (if check
      `(progn
         (check-arrayp ,a)
         (check-expected-rank ,a 0)
         0)
      0))

(defun expand-vector-index-check (a index check)
  (flet ((expansion (a index)
           `(progn
              (check-vectorp ,a)
              (check-vector-in-bounds ,a ,index)
              ,index)))
    (if check
        (if (constantp index)
            (expansion a index)
            (with-clean-symbols (%array-index)
              `(let ((%array-index ,index))
                 (declare (:read-only %array-index))
                 ,(expansion a '%array-index))))
        index)))

(defun expand-row-major-index (a indices check)
  (when (null indices)
    (return-from expand-row-major-index
      (expand-zero-dim-index-check a check)))
  (when (null (rest indices))
    (return-from expand-row-major-index
      (expand-vector-index-check a (first indices) check)))
  (let* ((expected-rank (length indices)))
    (with-clean-symbols (%ndx-var %output-var %dim-var)
      `(let* ((%ndx-var ,(pop indices))
              (%output-var %ndx-var)
              (%dim-var 0))
         (declare (type si::index %ndx-var %output-var %dim-var))
         ,@(when check
                 `((check-arrayp ,a)
                   (check-expected-rank ,a ,expected-rank)
                   (check-index-in-bounds ,a %output-var %dim-var)))
         ,@(loop for j from 1
              for index in indices
              collect `(setf %dim-var (array-dimension-fast ,a ,j)
                             %ndx-var ,index)
              collect (when check
                        `(check-index-in-bounds ,a %ndx-var %dim-var))
              collect `(setf %output-var
                             (the si::index
                               (+ (the si::index (* %output-var %dim-var))
                                  %ndx-var))))
         %output-var))))

;(trace c::expand-row-major-index c::expand-aset c::expand-aref)

(defmacro check-arrayp (a)
  `(c-inline
    (,a) (:object) :void
    "if (ecl_unlikely(!ECL_ARRAYP(#0))) FEtype_error_array(#0);"
    :one-liner nil))

(defmacro check-vectorp (v)
  `(c-inline
    (,v) (:object) :void
    "if (ecl_unlikely(!ECL_VECTORP(#0)))
           FEtype_error_vector(#0);"
    :one-liner nil))

(defmacro check-expected-rank (a expected-rank)
  `(c-inline
    (,a ,expected-rank) (:object :fixnum) :void
    "if (ecl_unlikely((#0)->array.rank != (#1)))
            FEwrong_dimensions(#0,#1);"
    :one-liner nil))

(defmacro check-index-in-bounds (array index limit)
  `(c-inline
    (,array ,index ,limit) (:object :fixnum :fixnum) :void
    "if (ecl_unlikely((#1)>=(#2)))
           FEwrong_index(Cnil,#0,-1,#1,#2);"
    :one-liner nil))

(defmacro check-vector-in-bounds (vector index)
  `(c-inline
    (,vector ,index) (:object :fixnum) :void
    "if (ecl_unlikely((#1)>=(#0)->vector.dim))
           FEwrong_index(Cnil,#0,-1,#1,(#0)->vector.dim);"
    :one-liner nil))

(defconstant +array-dimension-accessor+
  '#.(loop for i from 0 below array-rank-limit
       collect (format nil "(#0)->array.dims[~D]" i)))

(defun array-dimension-accessor (array n)
  (let ((tails #.(apply 'vector
                   (loop for i from 0 below array-rank-limit
                      for c-code = (format nil "(#0)->array.dims[~D]" i)
                      collect `((:object) :fixnum ,c-code :one-liner t
                                :side-effects nil)))))
    `(c-inline (,array) ,(aref tails n))))

(defmacro array-dimension-fast (array n)
  (if (typep n '(integer 0 #.(1- array-rank-limit)))
      (array-dimension-accessor array n)
      (error "In macro ARRAY-DIMENSION-FAST, the index is not a constant integer: ~A"
             n)))
