;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;;  SEQMACROS -- Macros that are used to expand sequence routines
;;;;

(in-package "SYSTEM")

(defmacro with-predicate ((predicate) &body body)
  `(let ((,predicate (si::coerce-to-function ,predicate)))
     (declare (function ,predicate))
     (macrolet ((,predicate (&rest args)
		  `(locally (declare (optimize (safety 0) (speed 3)))
		     (funcall ,',predicate ,@args))))
       ,@body)))

(defmacro with-key ((akey) &body body)
  `(let ((,akey (if ,akey (si::coerce-to-function ,akey) #'identity)))
     (declare (function ,akey))
     (macrolet ((,akey (value)
		  `(locally (declare (optimize (safety 0) (speed 3)))
		     (funcall ,',akey ,value))))
       ,@body)))

(defmacro with-tests (&whole whole (test test-not &optional key) &body body)
  (ext::with-unique-names (%test %test-not %test-fn)
    `(let* ((,%test ,test)
	    (,%test-not ,test-not)
	    (,%test-fn (if ,%test
			  (progn (when ,%test-not (test-error))
				 (si::coerce-to-function ,%test))
			  (if ,%test-not
			      (si::coerce-to-function ,%test-not)
			      #'eql))))
       (declare (function ,%test-fn))
       (macrolet ((compare (v1 v2)
		    `(locally (declare (optimize (safety 0) (speed 3)))
		       (if ,',%test-not
			   (not (funcall ,',%test-fn ,v1 ,v2))
			   (funcall ,',%test-fn ,v1 ,v2)))))
	 ,@(if key `((with-key (,key) ,@body)) body)))))

(defmacro with-start-end ((start end seq) &body body)
  `(multiple-value-bind (,start ,end)
       (sequence-start-end 'subseq ,seq ,start ,end) 
     (declare (fixnum ,start ,end))
     ,@body))

(defmacro reckless (&body body)
  `(locally (declare (optimize (safety 0) (speed 3) (debug 0))) ,@body))

(defmacro do-vector ((elt vector start end &key from-end output) &body body)
  (with-unique-names (%vector %i %count)
    (if from-end
	`(do* ((,%vector ,vector)
	       (,%i ,end)
	       (,%count ,start))
	      ((= ,%i ,%count) ,output)
	   (declare (fixnum ,%i ,%count)
		    (vector ,%vector))
	   (let ((,elt (reckless (aref ,%vector (setf ,%i (1- ,%i))))))
	     ,@body))
	`(do* ((,%vector ,vector)
	       (,%i ,start (1+ ,%i))
	       (,%count ,end))
	      ((= ,%i ,%count) ,output)
	   (declare (fixnum ,%i ,%count)
		    (vector ,%vector))
	   (let ((,elt (reckless (aref ,%vector ,%i))))
	     ,@body)))))

(defmacro do-sublist ((elt list start end &key output) &body body)
  (with-unique-names (%sublist %i %count)
    `(do* ((,%i ,start)
	   (,%sublist (nthcdr ,%i ,list) (cdr ,%sublist))
	   (,%count (- ,end ,%i) (1- ,%count)))
	  ((<= ,%count 0) ,output)
       (declare (fixnum ,%i ,%count)
		(cons ,%sublist))
       (let ((,elt (car ,%sublist)))
	 ,@body))))

(defmacro do-sequence ((elt sequence start end &key output specialize) &body body)
  (if specialize
      (with-unique-names (%sequence)
	`(let ((,%sequence ,sequence))
	   (if (listp ,%sequence)
	       (do-sublist (,elt ,%sequence ,start ,end :output ,output)
		 ,@body)
	       (do-vector (,elt ,%sequence ,start ,end :output ,output)
		 ,@body))))
      (with-unique-names (%sequence %start %i %count)
	`(do* ((,%sequence ,sequence)
	       (,%start ,start)
	       (,%i (make-seq-iterator ,%sequence ,%start)
		    (seq-iterator-next ,%sequence ,%i))
	       (,%count (- ,end ,start) (1- ,%count)))
	      ((or (null ,%i) (not (plusp ,%count))) ,output)
	   (let ((,elt (seq-iterator-ref ,%sequence ,%i)))
	     ,@body)))))
