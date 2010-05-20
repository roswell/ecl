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
     (macrolet ((,predicate (&rest args)
		  `(locally (declare (optimize (safety 0) (speed 3)))
		     (funcall (the function ,',predicate) ,@args))))
       ,@body)))

(defmacro with-key ((akey) &body body)
  `(let ((,akey (if ,akey (si::coerce-to-function ,akey) #'identity)))
     (macrolet ((,akey (value)
		  `(locally (declare (optimize (safety 0) (speed 3)))
		     (funcall (the function ,',akey) ,value))))
       ,@body)))

(defmacro with-tests (&whole whole (test test-not &optional key) &body body)
  (when key
    (setf body `((with-key (,key) ,@body))))
  `(let ((,test (if ,test (si::coerce-to-function ,test)))
	 (,test-not (if ,test-not (si::coerce-to-function ,test-not))))
     (and test test-not (test-error))
     (macrolet ((compare (v1 v2)
		  `(locally (declare (optimize (safety 0) (speed 3)))
		     (cond (test (funcall (the function test) ,v1 ,v2))
			   (test-not (not (funcall (the function test-not)
						   ,v1 ,v2)))
			   (t (eql ,v1 ,v2))))
		  ))
       ,@body)))

(defmacro with-start-end (start end seq &body body)
  `(multiple-value-bind (,start ,end)
       (sequence-start-end 'subseq ,seq ,start ,end) 
     (declare (fixnum ,start ,end))
     ,@body))


