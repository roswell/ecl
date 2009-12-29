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

;;;; CMPC-DATA -- Dump data used by code in a textual representation

(in-package "C-BACKEND")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DUMP TEXTUAL DATA
;;;
;;; Dumps data that has to be parsed by read_VV() when initializing
;;; this file.

(defun data-dump (stream &key init-name &aux must-close)
  (etypecase stream
    (null (return-from data-dump))
    ((or pathname string)
     (setf stream (open stream :direction :output :if-does-not-exist :create
			:if-exists :supersede :external-format :default)
	   must-close stream))
    (stream))
  (si::with-ecl-io-syntax
    (extract-static-constants stream)
    (adjust-data-indices *permanent-objects*)
    (adjust-data-indices *temporary-objects*)
    (let ((output nil))
      (cond (*compiler-constants*
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

(defun adjust-data-indices (array)
  (loop for last-index from 0
     for record across array
     for location = (second record)
     do (setf (second location) last-index
              (third record) last-index)))

(defun wt-data-begin (stream)
  (setq *wt-string-size* 0)
  (setq *wt-data-column* 80)
  (princ "static const char compiler_data_text[] = " stream)
  nil)

(defun wt-data-end (stream)
  (princ #\; stream)
  (format stream "~%#define compiler_data_text_size ~D~%" *wt-string-size*)
  (setf *wt-string-size* 0))

;;; This routine converts lisp data into C-strings. We have to take
;;; care of escaping special characteres with backslashes. We also have
;;; to split long lines using  the fact that multiple strings are joined
;;; together by the compiler.
;;;
(defun wt-filtered-data (string stream &optional one-liner)
  (let ((N (length string))
	(wt-data-column 80))
    (incf *wt-string-size* (1+ N)) ; 1+ accounts for a blank space
    (format stream (if one-liner "\"" "~%\""))
    (dotimes (i N)
      (decf wt-data-column)
      (when (< wt-data-column 0)
	(format stream "\"~% \"")
	(setq wt-data-column 79))
      (let ((x (aref string i)))
	(cond
	  ((or (< (char-code x) 32)
	       (> (char-code x) 127))
	   (case x
	     ; We avoid a trailing backslash+newline because some preprocessors
	     ; remove them.
	     (#\Newline (princ "\\n" stream))
	     (#\Tab (princ "\\t" stream))
	     (t (format stream "\\~3,'0o" (char-code x)))))
	  ((char= x #\\)
	   (princ "\\\\" stream))
	  ((char= x #\")
	   (princ "\\\"" stream))
	  (t (princ x stream)))))
    (princ (if one-liner "\""  " \"") stream)
    string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DUMP STATIC DATA
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

(defun static-data-dump (stream)
  (loop for (object c-name) in *static-constants*
     for function = (static-constant-expression object)
     do (funcall function c-name object stream)))

(defun extract-static-constants (stream)
  (unless (or *compiler-constants* (not *use-static-constants-p*))
    (let ((static-constants 0))
      (flet ((turned-static-p (record)
               (destructuring-bind (object (&whole location vv-tag index object-copy))
                   (let ((builder (static-constant-expression object)))
                     (when builder
                       (let* ((next-index (incf static-constants))
                              (name (format nil "_ecl_static_~D" next-index)))
                         (setf (second location) name)
                         (funcall name object sream)
                         t))))))
        (setf *permanent-objects*
              (delete-if #'turned-static-p *permanent-objects*)
              *temporary-objects*
              (delete-if #'turned-static-p *temporary-objects*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; KNOWN OPTIMIZABLE CONSTANTS
;;;

(defparameter +optimizable-constants+ '())

(defun replace-optimizable-constants ()
  (let ((found nil))
    (flet ((turned-inline-p (record)
             (destructuring-bind (object (&whole location vv-tag index object-copy))
                 (let ((x (assoc object +optimizable-constants+)))
                   (when x
                     (setf found t)
                     (format *dump-output* "~&;;; Replacing constant ~A with ~A"
                             object (second x))
                     (setf (second location) (second x)
                           (first location) (first x))
                     t)))))
      (setf *permanent-objects*
            (delete-if #'turned-inline-p *permanent-objects*)
            *temporary-objects*
            (delete-if #'turned-inline-p *temporary-objects*))
      (when found
        (push-new "#include <float.h>" *clines-string-list*)))))

(mapc
 #'(lambda (record)
     (let* ((name (first record))
	    (c-value (second record)))
       (push 
        (cond ((symbolp name)
               (let* ((value (symbol-value name))
                      (type (lisp-type->rep-type (type-of value))))
                 (cons value `(VV ,c-value value))))
              ((floatp name)
               (let* ((value name)
                      (type (type-of value))
                      (loc-type (case type
                                  (single-float 'single-float-value)
                                  (double-float 'double-float-value)
                                  (long-float 'long-float-value)))
                      (location `(VV ,c-value)))
                (cons value (list loc-type value location))))
              (t
               (cons name`(VV ,c-value))))
        +optimizable-constants+)))
 (reverse
 `((MOST-POSITIVE-SHORT-FLOAT "FLT_MAX")
   (MOST-POSITIVE-SINGLE-FLOAT "FLT_MAX")

   (MOST-NEGATIVE-SHORT-FLOAT "-FLT_MAX")
   (MOST-NEGATIVE-SINGLE-FLOAT "-FLT_MAX")

   (LEAST-POSITIVE-SHORT-FLOAT "FLT_MIN")
   (LEAST-POSITIVE-SINGLE-FLOAT "FLT_MIN")
   (LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT "FLT_MIN")
   (LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT" FLT_MIN")

   (LEAST-NEGATIVE-SHORT-FLOAT "-FLT_MIN")
   (LEAST-NEGATIVE-SINGLE-FLOAT "-FLT_MIN")
   (LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT "-FLT_MIN")
   (LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT "-FLT_MIN")

   (MOST-POSITIVE-DOUBLE-FLOAT "DBL_MAX")
   (MOST-NEGATIVE-DOUBLE-FLOAT "-DBL_MAX")
   (LEAST-POSITIVE-DOUBLE-FLOAT "DBL_MIN")
   (LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT "DBL_MIN")
   (LEAST-NEGATIVE-DOUBLE-FLOAT "-DBL_MIN")
   (LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT "-DBL_MIN")

   ;; Order is important: on platforms where 0.0 and -0.0 are the same
   ;; the last one is prioritized.
   (#.(coerce 0 'single-float) "cl_core.singlefloat_zero")
   (#.(coerce 0 'double-float) "cl_core.doublefloat_zero")
   (#.(coerce -0.0 'single-float) "cl_core.singlefloat_minus_zero")
   (#.(coerce -0.0 'double-float) "cl_core.doublefloat_minus_zero")

   (#.(si::standard-readtable) "cl_core.standard_readtable")

   (#.(find-package :cl) "cl_core.lisp_package")
   (#.(find-package :cl-user) "cl_core.user_package")
   (#.(find-package :keyword) "cl_core.keyword_package")
   (#.(find-package :clos) "cl_core.clos_package")
   #+threads
   (#.(find-package :mp) "cl_core.mp_package")

   #+long-float
   ,@'(
    (MOST-POSITIVE-LONG-FLOAT "LDBL_MAX")
    (MOST-NEGATIVE-LONG-FLOAT "-LDBL_MAX")
    (LEAST-POSITIVE-LONG-FLOAT "LDBL_MIN")
    (LEAST-POSITIVE-NORMALIZED-LONG-FLOAT" LDBL_MIN")
    (LEAST-NEGATIVE-LONG-FLOAT "-LDBL_MIN")
    (LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT "-LDBL_MIN")
    (#.(coerce -0.0 'long-float) "cl_core.longfloat_minus_zero")
    (#.(coerce 0 'long-float) "cl_core.longfloat_zero")
    )
   )))
