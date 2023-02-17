;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

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

;;;; CMPLOC  Set-loc and Wt-loc.

(in-package "COMPILER")

(defun wt-loc (loc)
  (cond ((consp loc)
         (let ((fd (gethash (car loc) *wt-loc-dispatch-table*)))
           (if fd
               (apply fd (cdr loc))
               (unknown-location 'wt-loc loc))))
        ((symbolp loc)
         (let ((txt (gethash loc *wt-loc-dispatch-table* :not-found)))
           (when (eq txt :not-found)
             (unknown-location 'wt-loc loc))
           (wt txt)))
        ((stringp loc)
         (wt loc))
        ((var-p loc)
         (wt-var loc))
        ((vv-p loc)
         (wt-vv loc))
        (t
         (unknown-location 'wt-loc loc))))

(defun wt-lcl (lcl)
  (unless (numberp lcl) (baboon :format-control "wt-lcl: ~s NaN"
                                :format-arguments (list lcl)))
  (wt "v" lcl))

(defun wt-lcl-loc (lcl &optional type name)
  (declare (ignore type))
  (unless (numberp lcl)
    (baboon :format-control "wt-lcl-loc: ~s NaN"
            :format-arguments (list lcl)))
  (wt "v" lcl name))

(defun wt-temp (temp)
  (wt "T" temp))

(defun wt-fixnum (value &optional vv)
  (declare (ignore vv))
  (princ value *compiler-output1*)
  ;; Specify explicit type suffix as a workaround for MSVC. C99
  ;; standard compliant compilers don't need type suffixes and choose
  ;; the correct type themselves. Note that we cannot savely use
  ;; anything smaller than a long long here, because we might perform
  ;; some other computation on the integer constant which could
  ;; overflow if we use a smaller integer type (overflows in long long
  ;; computations are taken care of by the compiler before we get to
  ;; this point).
  #+msvc (princ (cond ((typep value (rep-type->lisp-type :long-long)) "LL")
                      ((typep value (rep-type->lisp-type :unsigned-long-long)) "ULL")
                      (t (baboon :format-control
                                 "wt-fixnum: The number ~A doesn't fit any integer type."
                                 value)))
                *compiler-output1*))

(defun wt-number (value &optional vv)
  (declare (ignore vv))
  (wt value))

(defun wt-character (value &optional vv)
  (declare (ignore vv))
  ;; We do not use the '...' format because this creates objects of type
  ;; 'char' which have sign problems
  (wt value))

(defun wt-value (i) (wt "cl_env_copy->values[" i "]"))

(defun wt-keyvars (i) (wt "keyvars[" i "]"))

(defun wt-the (type loc)
  (declare (ignore type))
  (wt-loc loc))

;;;
;;; SET-LOC
;;;

(defun set-unknown-loc (loc)
  (declare (ignore loc))
  (unknown-location 'set-loc *destination*))

(defun set-loc (loc &aux fd)
  (let ((destination *destination*))
    (cond ((eq destination loc))
          ((symbolp destination)
           (funcall (gethash destination *set-loc-dispatch-table*
                             'set-unknown-loc)
                    loc))
          ((var-p destination)
           (set-var loc destination))
          ((vv-p destination)
           (set-vv loc destination))
          ((atom destination)
           (unknown-location 'set-loc destination))
          (t
           (let ((fd (gethash (first destination) *set-loc-dispatch-table*)))
             (if fd
                 (apply fd loc (rest destination))
                 (progn
                   (wt-nl) (wt-loc destination) (wt " = ")
                   (wt-coerce-loc (loc-representation-type *destination*) loc)
                   (wt ";"))))))))

(defun set-the-loc (loc type orig-loc)
  (declare (ignore type))
  (let ((*destination* orig-loc))
    (set-loc loc)))
                 
(defun set-values-loc (loc)
  (cond ((eq loc 'VALUES))
        ((uses-values loc)
         (wt-nl "cl_env_copy->values[0] = ") (wt-coerce-loc :object loc) (wt ";"))
        (t
         (wt-nl "cl_env_copy->values[0] = ") (wt-coerce-loc :object loc)
         (wt ";")
         (wt-nl "cl_env_copy->nvalues = 1;"))))

(defun set-value0-loc (loc)
  (wt-nl "value0 = ") (wt-coerce-loc :object loc) (wt ";"))

(defun set-return-loc (loc)
  (cond ((or (eq loc 'VALUES) (uses-values loc))
         (wt-nl "value0 = ") (wt-coerce-loc :object loc) (wt ";"))
        ((eq loc 'VALUE0)
         (wt-nl "cl_env_copy->nvalues = 1;"))
        ((eq loc 'RETURN))
        (t
         (wt-nl "value0 = ") (wt-coerce-loc :object loc) (wt ";")
         (wt-nl "cl_env_copy->nvalues = 1;"))))

(defun set-trash-loc (loc)
  (when (loc-with-side-effects-p loc)
    (wt-nl loc ";")
    t))
