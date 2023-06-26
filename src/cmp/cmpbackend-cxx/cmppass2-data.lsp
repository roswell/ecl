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

;;;; CMPWT  Output routines.

(in-package "COMPILER")

(defun data-dump-array ()
  (cond (si:*compiler-constants*
         (setf si:*compiler-constants* (concatenate 'vector (data-get-all-objects)))
         "")
        #+externalizable
        ((plusp (data-size))
         (let* ((data-vector (concatenate 'vector (data-get-all-objects))))
           (si::serialize data-vector)))
        #-externalizable
        ((plusp (data-size))
         (let* ((*wt-string-size* 0)
                (*wt-data-column* 80)
                (data (data-get-all-objects))
                (data-string (si:with-ecl-io-syntax
                               (prin1-to-string data)))
                (l (length data-string)))
           (subseq data-string 1 (1- l))))
        (t
         "")))

#-externalizable
(defun data-c-dump (filename)
  (labels ((produce-strings ()
             ;; Only Windows has a size limit in the strings it creates.
             #-windows
             (let ((s (data-dump-array)))
               (when (plusp (length s))
                 (list s)))
             #+windows
             (loop with string = (data-dump-array)
                   with max-string-size = 65530
                   with l = (length string)
                   for i from 0 below l by max-string-size
                   for this-l = (min (- l i) max-string-size)
                   collect (make-array this-l :displaced-to string
                                              :element-type (array-element-type string)
                                              :displaced-index-offset i)))
           (output-one-c-string (name string stream)
             (let* ((*wt-string-size* 0)
                    (*wt-data-column* 80)
                    (s (with-output-to-string (stream)
                         (wt-filtered-data string stream))))
               (format stream "static const struct ecl_base_string ~A[] = {
        (int8_t)t_base_string, 0, ecl_aet_bc, 0,
        ECL_NIL, (cl_index)~D, (cl_index)~D,
        (ecl_base_char*)~A };~%"
                       name *wt-string-size* *wt-string-size* s)
               name))
           (output-c-strings (strings stream)
             (format stream
                     "~%static const cl_object compiler_data_text[] = {~{~%(cl_object)~A,~}~%NULL};"
                     (loop for s in strings
                           for i from 1
                           for name = (format nil "compiler_data_text~D" i)
                           collect (output-one-c-string name s stream)))))
    (with-open-file (stream filename :direction :output :if-does-not-exist :create
                                     :if-exists :supersede :external-format :default)
      (let ((strings (produce-strings)))
        (if strings
            (output-c-strings strings stream)
            (princ "#define compiler_data_text NULL" stream))
        ;; Ensure a final newline or some compilers complain
        (terpri stream)))))

#+externalizable
(defun data-c-dump (filename)
  (with-open-file (stream filename :direction :output :if-does-not-exist :create
                                   :if-exists :supersede :external-format :default)
    (let ((data (data-dump-array)))
      (if (plusp (length data))
          (let ((s (with-output-to-string (stream)
                     (loop for i below (length data) do
                       (princ (elt data i) stream)
                       (if (< i (1- (length data)))
                           (princ "," stream))))))
            (format stream "static uint8_t serialization_data[] = {~A};~%" s)
            (format stream "static const struct ecl_vector compiler_data_text1[] = {{
        (int8_t)t_vector, 0, ecl_aet_b8, 0,
        ECL_NIL, (cl_index)~D, (cl_index)~D,
        { .b8=serialization_data } }};~%"
                    (length data) (length data))
            (format stream "static const cl_object compiler_data_text[] = {
(cl_object)compiler_data_text1}; "))
          (princ "#define compiler_data_text NULL" stream))
      ;; Ensure a final newline or some compilers complain
      (terpri stream))))



;;; ======================================================================
;;;
;;; STATIC CONSTANTS
;;;

(defun static-base-string-builder (name value stream)
  (format stream "ecl_def_ct_base_string(~A," name)
  (wt-filtered-data value stream :one-liner t)
  (format stream ",~D,static,const);" (length value)))

(defun static-single-float-builder (name value stream)
  (let* ((*read-default-float-format* 'single-float)
         (*print-readably* t))
    (format stream "ecl_def_ct_single_float(~A,~S,static,const);"
            name value)))

(defun static-double-float-builder (name value stream)
  (let* ((*read-default-float-format* 'double-float)
         (*print-readably* t))
    (format stream "ecl_def_ct_double_float(~A,~S,static,const);"
            name value)))

(defun static-long-float-builder (name value stream)
  (let* ((*read-default-float-format* 'long-float)
         (*print-readably* t))
    (format stream "ecl_def_ct_long_float(~A,~SL,static,const);"
            name value)))

(defun static-rational-builder (name value stream)
  (let* ((*read-default-float-format* 'double-float)
         (*print-readably* t))
    (format stream
            "ecl_def_ct_ratio(~A,ecl_make_fixnum(~D),ecl_make_fixnum(~D),static,const);"
            name (numerator value) (denominator value))))

(defun static-constant-delegate (name value stream)
  (funcall (static-constant-expression value)
           name value stream))

(defun static-complex-builder (name value stream)
  (let* ((*read-default-float-format* 'double-float)
         (*print-readably* t)
         (name-real (concatenate 'string name "_real"))
         (name-imag (concatenate 'string name "_imag")))
    (static-constant-delegate name-real (realpart value) stream)
    (terpri stream)
    (static-constant-delegate name-imag (imagpart value) stream)
    (terpri stream)
    (format stream
            "ecl_def_ct_complex(~A,&~A_data,&~A_data,static,const);"
            name name-real name-imag)))

#+complex-float
(defun static-csfloat-builder (name value stream)
  (let* ((*read-default-float-format* 'single-float)
         (*print-readably* t))
    (format stream "ecl_def_ct_csfloat(~A,(~S + I*~S),static,const);"
            name (realpart value) (imagpart value) stream)))

#+complex-float
(defun static-cdfloat-builder (name value stream)
  (let* ((*read-default-float-format* 'double-float)
         (*print-readably* t))
    (format stream "ecl_def_ct_cdfloat(~A,(~S + I*~S),static,const);"
            name (realpart value) (imagpart value) stream)))

#+complex-float
(defun static-clfloat-builder (name value stream)
  (let* ((*read-default-float-format* 'long-float)
         (*print-readably* t))
    (format stream "ecl_def_ct_clfloat(~A,(~SL + I*~SL),static,const);"
            name (realpart value) (imagpart value) stream)))

#+sse2
(defun static-sse-pack-builder (name value stream)
  (let* ((bytes (ext:sse-pack-to-vector value '(unsigned-byte 8)))
         (type-code (nth-value 1 (ext:sse-pack-element-type value))))
    (format stream
            "ecl_def_ct_sse_pack(~A,~A~{,~A~});"
            name type-code (coerce bytes 'list))))

(defun static-constant-expression (object)
  (typecase object
    (base-string #'static-base-string-builder)
    (ratio (and (static-constant-expression (numerator object))
                (static-constant-expression (denominator object))
                #'static-rational-builder))
    (single-float (and (not (ext:float-nan-p object))
                       (not (ext:float-infinity-p object))
                       #'static-single-float-builder))
    (double-float (and (not (ext:float-nan-p object))
                       (not (ext:float-infinity-p object))
                       #'static-double-float-builder))
    (long-float (and (not (ext:float-nan-p object))
                     (not (ext:float-infinity-p object))
                     #'static-long-float-builder))
    #+complex-float
    (si:complex-single-float #'static-csfloat-builder)
    #+complex-float
    (si:complex-double-float #'static-cdfloat-builder)
    #+complex-float
    (si:complex-long-float #'static-clfloat-builder)
    (complex (and (static-constant-expression (realpart object))
                  (static-constant-expression (imagpart object))
                  #'static-complex-builder))
    #+sse2
    (ext:sse-pack #'static-sse-pack-builder)
    (t nil)))

(defun add-static-constant (object)
  ;; FIXME! The MSVC compiler does not allow static initialization of bit
  ;; fields. SSE uses always unboxed static constants. No reference is kept to
  ;; them -- it is thus safe to use them even on code that might be unloaded.
  (unless (or #+msvc t
              si:*compiler-constants*
              (and (not *use-static-constants-p*)
                   #+sse2
                   (not (typep object 'ext:sse-pack)))
              (not (listp *static-constants*)))
    (ext:if-let ((record (find object *static-constants* :key #'first :test #'equal)))
      (second record)
      (ext:when-let ((builder (static-constant-expression object)))
        (let ((c-name (format nil "_ecl_static_~D" (length *static-constants*))))
          (push (list object c-name builder) *static-constants*)
          (make-vv :location c-name :value object))))))

(defun wt-vv-index (index permanent-p)
  (cond ((not (numberp index))
         (wt index))
        (permanent-p
         (wt "VV[" index "]"))
        (t
         (wt "VVtemp[" index "]"))))

(defun wt-vv-value (vv value)
  (etypecase value
    (fixnum          (wt-fixnum value vv))
    (character       (wt-character value vv))
    (float           (wt-number value vv))
    ((complex float) (wt-number value vv))))

(defun wt-vv (vv-loc)
  (setf (vv-used-p vv-loc) t)
  (let ((index (vv-location vv-loc)))
    (if (null index)
        (wt-vv-value vv-loc (vv-value vv-loc))
        (wt-vv-index index (vv-permanent-p vv-loc)))))

(defun set-vv-index (loc index permanent-p)
  (wt-nl) (wt-vv-index index permanent-p) (wt "= ")
  (wt-coerce-loc :object loc)
  (wt ";"))

(defun set-vv (loc vv-loc)
  (setf (vv-used-p vv-loc) t)
  (set-vv-index loc (vv-location vv-loc) (vv-permanent-p vv-loc)))

