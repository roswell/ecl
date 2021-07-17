;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; KNOWN OPTIMIZABLE CONSTANTS
;;;

(defun make-single-constant-optimizer (name c-value)
  (cond ((symbolp name)
         (let* ((value (symbol-value name))
                (type (lisp-type->rep-type (type-of value))))
           (cons value `(c-inline () () ,type ,c-value
                                  :one-liner t :side-effects nil))))
        ((floatp name)
         (let* ((value name)
                      (type (type-of value))
                (loc-type (case type
                            (single-float 'single-float-value)
                            (double-float 'double-float-value)
                            (long-float 'long-float-value)
                            (si:complex-single-float 'csfloat-value)
                            (si:complex-double-float 'cdfloat-value)
                            (si:complex-long-float 'clfloat-value)))
                (location (make-vv :location c-value :value value)))
           (cons value (make-c1form* 'LOCATION :type type
                                     :args (list loc-type value location)))))
        (t
         (cons name (make-c1form* 'LOCATION :type (type-of name)
                                  :args (make-vv :location c-value
                                                 :value name))))))

(defun make-optimizable-constants (machine)
  (loop for (value name) in (optimizable-constants-list machine)
     collect (make-single-constant-optimizer value name)))

(defun optimizable-constants-list (machine)
  (append
   ;; Constants that appear everywhere
   '(
     ;; Order is important: on platforms where 0.0 and -0.0 are the same
     ;; the last one is prioritized.
     (#.(coerce 0 'single-float) "cl_core.singlefloat_zero")
     (#.(coerce 0 'double-float) "cl_core.doublefloat_zero")
     (#.(coerce -0.0 'single-float) "cl_core.singlefloat_minus_zero")
     (#.(coerce -0.0 'double-float) "cl_core.doublefloat_minus_zero")
     (#.(coerce 0 'long-float) "cl_core.longfloat_zero")
     (#.(coerce -0.0 'long-float) "cl_core.longfloat_minus_zero")

     ;; We temporarily remove this constant, because the bytecodes compiler
     ;; does not know how to externalize it.
     ;;(#.(si::standard-readtable) "cl_core.standard_readtable")

     (#.(find-package :cl) "cl_core.lisp_package")
     (#.(find-package :cl-user) "cl_core.user_package")
     (#.(find-package :keyword) "cl_core.keyword_package")
     (#.(find-package :clos) "cl_core.clos_package")
     #+threads
     (#.(find-package :mp) "cl_core.mp_package")
     )
   (when (eq machine *default-machine*)
     ;; Constants which are not portable
     `((MOST-POSITIVE-SHORT-FLOAT "FLT_MAX")
       (MOST-POSITIVE-SINGLE-FLOAT "FLT_MAX")
       
       (MOST-NEGATIVE-SHORT-FLOAT "-FLT_MAX")
       (MOST-NEGATIVE-SINGLE-FLOAT "-FLT_MAX")

       (LEAST-POSITIVE-SHORT-FLOAT "FLT_TRUE_MIN")
       (LEAST-POSITIVE-SINGLE-FLOAT "FLT_TRUE_MIN")
       (LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT "FLT_MIN")
       (LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT" FLT_MIN")

       (LEAST-NEGATIVE-SHORT-FLOAT "-FLT_TRUE_MIN")
       (LEAST-NEGATIVE-SINGLE-FLOAT "-FLT_TRUE_MIN")
       (LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT "-FLT_MIN")
       (LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT "-FLT_MIN")

       (MOST-POSITIVE-DOUBLE-FLOAT "DBL_MAX")
       (MOST-NEGATIVE-DOUBLE-FLOAT "-DBL_MAX")
       (LEAST-POSITIVE-DOUBLE-FLOAT "DBL_TRUE_MIN")
       (LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT "DBL_MIN")
       (LEAST-NEGATIVE-DOUBLE-FLOAT "-DBL_TRUE_MIN")
       (LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT "-DBL_MIN")

       #+ieee-floating-point
       ,@'((SHORT-FLOAT-POSITIVE-INFINITY  "INFINITY")
           (SINGLE-FLOAT-POSITIVE-INFINITY "INFINITY")
           (DOUBLE-FLOAT-POSITIVE-INFINITY "INFINITY")

           (SHORT-FLOAT-NEGATIVE-INFINITY  "-INFINITY")
           (SINGLE-FLOAT-NEGATIVE-INFINITY "-INFINITY")
           (DOUBLE-FLOAT-NEGATIVE-INFINITY "-INFINITY"))

       ,@'((MOST-POSITIVE-LONG-FLOAT "LDBL_MAX")
           (MOST-NEGATIVE-LONG-FLOAT "-LDBL_MAX")
           (LEAST-POSITIVE-LONG-FLOAT "LDBL_TRUE_MIN")
           (LEAST-POSITIVE-NORMALIZED-LONG-FLOAT" LDBL_MIN")
           (LEAST-NEGATIVE-LONG-FLOAT "-LDBL_TRUE_MIN")
           (LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT "-LDBL_MIN")
           #+ieee-floating-point
           (LONG-FLOAT-POSITIVE-INFINITY   "INFINITY")
           #+ieee-floating-point
           (LONG-FLOAT-NEGATIVE-INFINITY   "-INFINITY"))))))
