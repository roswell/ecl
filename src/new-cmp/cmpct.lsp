;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;; CMPCT --  Optimizer for several constant values

;;;;  Copyright (c) 2003, Juan Jose Garcia Ripoll.
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defparameter +optimizable-constants+ '())

(defun build-constant-value-loc (val &key always only-small-values)
  (cond
    ((let ((x (assoc val +optimizable-constants+)))
       (when x
         (pushnew "#include <float.h>" *clines-string-list*)
         (values (cdr x) t))))
   ((eq val nil) (values nil t))
   ((eq val t) (values t t))
   ((sys::fixnump val) (values (list 'FIXNUM-VALUE val) t))
   ((characterp val) (values (list 'CHARACTER-VALUE (char-code val)) t))
   ((typep val 'DOUBLE-FLOAT)
    (values (list 'DOUBLE-FLOAT-VALUE val (add-object val)) t))
   ((typep val 'SINGLE-FLOAT)
    (values (list 'SINGLE-FLOAT-VALUE val (add-object val)) t))
   ((typep val 'LONG-FLOAT)
    (values (list 'LONG-FLOAT-VALUE val (add-object val)) t))
   (always
    (values (list 'VV (add-object val) val) t))
   (t (values nil nil))))

(defun c1constant-value (destination val &key always only-small-values)
  (when (eq destination 'TRASH)
    (return-from c1constant-value (c1nil destination)))
  (multiple-value-bind (loc found-p)
      (build-constant-value-loc val :always always :only-small-values only-small-values)
    (when found-p
      (c1set-loc destination loc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; KNOWN OPTIMIZABLE CONSTANTS
;;;

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
