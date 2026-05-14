;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2003, Juan Jose Garcia Ripoll.
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;;  Optimizer for several constant values

(in-package "COMPILER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; KNOWN OPTIMIZABLE CONSTANTS
;;;

(defun make-single-constant-optimizer (name c-value)
  (cond ((symbolp name)
         (let* ((value (symbol-value name))
                (host-type (lisp-type->host-type (type-of value)))
                (location (make-vv :location c-value :value value :host-type host-type)))
           (cons value location)))
        ((floatp name)
         (let* ((value name)
                (location (make-vv :location c-value :value value :host-type :object)))
           (cons value location)))
        (t
         (cons name (make-vv :location c-value :value name)))))

(defun make-optimizable-constants (machine)
  (loop for (value name) in (optimizable-constants-list machine)
     collect (make-single-constant-optimizer value name)))

;;; FIXME this is messy - numbers are lisp objects and symbols are native
;;; types. We should provide both alternatives for different expected types.
;;; See WT-TO-OBJECT-CONVERSION
(defun optimizable-constants-list (machine)
  (append
   ;; Constants that appear everywhere
   '(
     ;; Order is important: on platforms where 0.0 and -0.0 are the same
     ;; the last one is prioritized.
     (#.(coerce 0 'cl:single-float) "ecl_ct_singlefloat_zero")
     (#.(coerce 0 'cl:double-float) "ecl_ct_doublefloat_zero")
     (#.(coerce -0.0 'cl:single-float) "ecl_ct_singlefloat_minus_zero")
     (#.(coerce -0.0 'cl:double-float) "ecl_ct_doublefloat_minus_zero")
     (#.(coerce 0 'cl:long-float) "ecl_ct_longfloat_zero")
     (#.(coerce -0.0 'cl:long-float) "ecl_ct_longfloat_minus_zero")

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
     `((cl:most-positive-short-float "FLT_MAX")
       (cl:most-positive-single-float "FLT_MAX")

       (cl:most-negative-short-float "-FLT_MAX")
       (cl:most-negative-single-float "-FLT_MAX")

       (cl:least-positive-short-float "FLT_TRUE_MIN")
       (cl:least-positive-single-float "FLT_TRUE_MIN")
       (cl:least-positive-normalized-short-float "FLT_MIN")
       (cl:least-positive-normalized-single-float" FLT_MIN")

       (cl:least-negative-short-float "-FLT_TRUE_MIN")
       (cl:least-negative-single-float "-FLT_TRUE_MIN")
       (cl:least-negative-normalized-short-float "-FLT_MIN")
       (cl:least-negative-normalized-single-float "-FLT_MIN")

       (cl:most-positive-double-float "DBL_MAX")
       (cl:most-negative-double-float "-DBL_MAX")
       (cl:least-positive-double-float "DBL_TRUE_MIN")
       (cl:least-positive-normalized-double-float "DBL_MIN")
       (cl:least-negative-double-float "-DBL_TRUE_MIN")
       (cl:least-negative-normalized-double-float "-DBL_MIN")

       #+ieee-floating-point
       ,@'((ext:short-float-positive-infinity  "INFINITY")
           (ext:single-float-positive-infinity "INFINITY")
           (ext:double-float-positive-infinity "INFINITY")

           (ext:short-float-negative-infinity  "-INFINITY")
           (ext:single-float-negative-infinity "-INFINITY")
           (ext:double-float-negative-infinity "-INFINITY"))

       ,@'((cl:most-positive-long-float "LDBL_MAX")
           (cl:most-negative-long-float "-LDBL_MAX")
           (cl:least-positive-long-float "LDBL_TRUE_MIN")
           (cl:least-positive-normalized-long-float" LDBL_MIN")
           (cl:least-negative-long-float "-LDBL_TRUE_MIN")
           (cl:least-negative-normalized-long-float "-LDBL_MIN")
           #+ieee-floating-point
           (ext:long-float-positive-infinity   "INFINITY")
           #+ieee-floating-point
           (ext:long-float-negative-infinity   "-INFINITY"))))))
