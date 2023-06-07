;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;; Copyright (c) 1991, Giuseppe Attardi. All rights reserved.
;;;; Copyright (c) 2003, Juan Jose Garcia Ripoll
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;;
;;;; Database for system functions.
;;;;

(in-package "COMPILER")

;;;
;;; DATABASE OF INLINE EXPANSIONS
;;;
;;;     (DEF-INLINE function-name kind ([arg-type]*) return-rep-type
;;;             expansion-string)
;;;
;;; Here, ARG-TYPE is the list of argument types belonging to the lisp family,
;;; while RETURN-REP-TYPE is a representation type, i.e. the C type of the
;;; output expression. EXPANSION-STRING is a C/C++ expression template, like the
;;; ones used by C-INLINE. Finally, KIND can be :ALWAYS, :SAFE or :UNSAFE,
;;; depending on whether the inline expression should be applied always, in safe
;;; or in unsafe compilation mode, respectively.
;;;

(defun inline-information (name safety)
  (gethash (list name safety) *inline-information*))

(defun (setf inline-information) (value name safety)
  (setf (gethash (list name safety) *inline-information*) value))

(defun %def-inline (name safety arg-types return-rep-type expansion
                    &key (one-liner t) (exact-return-type nil) (inline-or-warn nil)
                      (multiple-values t)
                    &aux arg-rep-types)
  (setf safety
        (case safety
          (:unsafe :inline-unsafe)
          (:safe :inline-safe)
          (:always :inline-always)
          (t (error "In DEF-INLINE, wrong value of SAFETY"))))
  ;; Ensure we can inline this form. We only inline when the features are
  ;; there (checked above) and when the C types are part of this machine
  ;; (checked here).
  (loop for type in (list* return-rep-type arg-types)
        unless (or (eq type 'fixnum-float)
                   (and (consp type) (eq (car type) 'values))
                   (lisp-type-p type)
                   (machine-c-type-p type))
          do (warn "Dropping inline form for ~A because of missing type ~A" name type)
             (return-from %def-inline))
  (setf arg-rep-types
        (mapcar #'(lambda (x) (if (eq x '*) x (lisp-type->rep-type x)))
                arg-types))
  (when (eq return-rep-type t)
    (setf return-rep-type :object))
  (when inline-or-warn
    (setf (inline-information name 'should-be-inlined) t))
  (let* ((return-type (if (and (consp return-rep-type)
                               (eq (first return-rep-type) 'values))
                          t
                          (rep-type->lisp-type return-rep-type)))
         (inline-info
           (make-inline-info :name name
                             :arg-rep-types arg-rep-types
                             :return-rep-type return-rep-type
                             :return-type return-type
                             :arg-types arg-types
                             :exact-return-type exact-return-type
                             :multiple-values multiple-values
                             ;; :side-effects (not (si:get-sysprop name 'no-side-effects))
                             :one-liner one-liner
                             :expansion expansion)))
    #+(or)
    (loop for i in (inline-information name safety)
          when (and (equalp (inline-info-arg-types i) arg-types)
                    (not (equalp return-type (inline-info-return-type i))))
            do (format t "~&;;; Redundand inline definition for ~A~&;;; ~<~A~>~&;;; ~<~A~>"
                       name i inline-info))
    (push inline-info (gethash (list name safety) *inline-information*))))

(defmacro def-inline (&rest args)
  `(apply #'%def-inline ',args))

(defun make-inline-information (*machine*)
  (let ((*inline-information* (make-hash-table :size 768 :test 'equal)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; ALL FUNCTION DECLARATIONS AND INLINE FORMS
    ;;
    (def-inline cl:aref :unsafe (t t t) t "@0;ecl_aref_unsafe(#0,ecl_fixnum(#1)*(#0)->array.dims[1]+ecl_fixnum(#2))")
    (def-inline cl:aref :unsafe ((array t) t t) t "@0;(#0)->array.self.t[ecl_fixnum(#1)*(#0)->array.dims[1]+ecl_fixnum(#2)]")
    (def-inline cl:aref :unsafe ((array bit) t t) :fixnum "@0;ecl_aref_bv(#0,ecl_fixnum(#1)*(#0)->array.dims[1]+ecl_fixnum(#2))")
    (def-inline cl:aref :unsafe ((array t) fixnum fixnum) t "@0;(#0)->array.self.t[#1*(#0)->array.dims[1]+#2]")
    (def-inline cl:aref :unsafe ((array bit) fixnum fixnum) :fixnum "@0;ecl_aref_bv(#0,(#1)*(#0)->array.dims[1]+#2)")
    (def-inline cl:aref :unsafe ((array base-char) fixnum fixnum) :unsigned-char "@0;(#0)->base_string.self[#1*(#0)->array.dims[1]+#2]")
    (def-inline cl:aref :unsafe ((array double-float) fixnum fixnum) :double "@0;(#0)->array.self.df[#1*(#0)->array.dims[1]+#2]")
    (def-inline cl:aref :unsafe ((array single-float) fixnum fixnum) :float "@0;(#0)->array.self.sf[#1*(#0)->array.dims[1]+#2]")
    (def-inline cl:aref :unsafe ((array long-float) fixnum fixnum) :long-double "@0;(#0)->array.self.lf[#1*(#0)->array.dims[1]+#2]")
    (when (member :complex-float *features*)
      (def-inline cl:aref :unsafe ((array si:complex-single-float) fixnum fixnum) :csfloat "@0;(#0)->array.self.csf[#1*(#0)->array.dims[1]+#2]")
      (def-inline cl:aref :unsafe ((array si:complex-double-float) fixnum fixnum) :cdfloat "@0;(#0)->array.self.cdf[#1*(#0)->array.dims[1]+#2]")
      (def-inline cl:aref :unsafe ((array si:complex-long-float) fixnum fixnum) :clfloat "@0;(#0)->array.self.clf[#1*(#0)->array.dims[1]+#2]"))

    (def-inline cl:aref :unsafe ((array fixnum) fixnum fixnum) :fixnum "@0;(#0)->array.self.fix[#1*(#0)->array.dims[1]+#2]")

    (def-inline cl:aref :always (t t) t "ecl_aref1(#0,ecl_to_size(#1))")
    (def-inline cl:aref :always (t fixnum) t "ecl_aref1(#0,#1)")
    (def-inline cl:aref :unsafe (t t) t "ecl_aref1(#0,ecl_fixnum(#1))")
    (def-inline cl:aref :unsafe ((array bit) t) :fixnum "ecl_aref_bv(#0,ecl_fixnum(#1))")
    (def-inline cl:aref :unsafe ((array bit) fixnum) :fixnum "ecl_aref_bv(#0,#1)")
    (when (member :unicode *features*)
      (def-inline cl:aref :unsafe ((array character) fixnum) :wchar "(#0)->string.self[#1]"))
    (def-inline cl:aref :unsafe ((array base-char) fixnum) :unsigned-char "(#0)->base_string.self[#1]")
    (def-inline cl:aref :unsafe ((array double-float) fixnum) :double "(#0)->array.self.df[#1]")
    (def-inline cl:aref :unsafe ((array single-float) fixnum) :float "(#0)->array.self.sf[#1]")
    (def-inline cl:aref :unsafe ((array long-float) fixnum) :long-double "(#0)->array.self.lf[#1]")
    (when (member :complex-float *features*)
      (def-inline cl:aref :unsafe ((array si:complex-single-float) fixnum) :csfloat "(#0)->array.self.csf[#1]")
      (def-inline cl:aref :unsafe ((array si:complex-double-float) fixnum) :cdfloat "(#0)->array.self.cdf[#1]")
      (def-inline cl:aref :unsafe ((array si:complex-long-float) fixnum) :clfloat "(#0)->array.self.clf[#1]"))
    (def-inline cl:aref :unsafe ((array fixnum) fixnum) :fixnum "(#0)->array.self.fix[#1]")

    (def-inline cl:row-major-aref :always (t t) t "ecl_aref(#0,ecl_to_size(#1))")
    (def-inline cl:row-major-aref :always (t fixnum) t "ecl_aref(#0,#1)")
    (def-inline cl:row-major-aref :unsafe (t t) t "ecl_aref_unsafe(#0,ecl_fixnum(#1))")
    (def-inline cl:row-major-aref :unsafe (t fixnum) t "ecl_aref_unsafe(#0,#1)")
    (def-inline cl:row-major-aref :unsafe ((array bit) t) :fixnum "ecl_aref_bv(#0,ecl_fixnum(#1))")
    (def-inline cl:row-major-aref :unsafe ((array bit) fixnum) :fixnum "ecl_aref_bv(#0,#1)")
    (when (member :unicode *features*)
      (def-inline cl:row-major-aref :unsafe ((array character) fixnum) :wchar "(#0)->string.self[#1]"))
    (def-inline cl:row-major-aref :unsafe ((array base-char) fixnum) :unsigned-char "(#0)->base_string.self[#1]")
    (def-inline cl:row-major-aref :unsafe ((array ext:byte8) fixnum) :uint8-t "(#0)->vector.self.b8[#1]")
    (def-inline cl:row-major-aref :unsafe ((array ext:integer8) fixnum) :int8-t "(#0)->vector.self.i8[#1]")
    (def-inline cl:row-major-aref :unsafe ((array ext:byte16) fixnum) :uint16-t "(#0)->vector.self.b16[#1]")
    (def-inline cl:row-major-aref :unsafe ((array ext:integer16) fixnum) :int16-t "(#0)->vector.self.i16[#1]")
    (def-inline cl:row-major-aref :unsafe ((array ext:byte32) fixnum) :uint32-t "(#0)->vector.self.b32[#1]")
    (def-inline cl:row-major-aref :unsafe ((array ext:integer32) fixnum) :int32-t "(#0)->vector.self.i32[#1]")
    (def-inline cl:row-major-aref :unsafe ((array ext:byte64) fixnum) :uint64-t "(#0)->vector.self.b64[#1]")
    (def-inline cl:row-major-aref :unsafe ((array ext:integer64) fixnum) :int64-t "(#0)->vector.self.i64[#1]")
    (def-inline cl:row-major-aref :unsafe ((array long-float) fixnum) :long-double "(#0)->array.self.lf[#1]")
    (def-inline cl:row-major-aref :unsafe ((array double-float) fixnum) :double "(#0)->array.self.df[#1]")
    (def-inline cl:row-major-aref :unsafe ((array single-float) fixnum) :float "(#0)->array.self.sf[#1]")
    (when (member :complex-float *features*)
      (def-inline cl:row-major-aref :unsafe ((array si:complex-single-float) fixnum) :csfloat "(#0)->array.self.csf[#1]")
      (def-inline cl:row-major-aref :unsafe ((array si:complex-double-float) fixnum) :cdfloat "(#0)->array.self.cdf[#1]")
      (def-inline cl:row-major-aref :unsafe ((array si:complex-long-float) fixnum) :clfloat "(#0)->array.self.clf[#1]"))
    (def-inline cl:row-major-aref :unsafe ((array fixnum) fixnum) :fixnum "(#0)->array.self.fix[#1]")

    (def-inline si:row-major-aset :always (t t t) t "ecl_aset(#0,ecl_to_size(#1),#2)")
    (def-inline si:row-major-aset :always (t fixnum t) t "ecl_aset(#0,#1,#2)")
    (def-inline si:row-major-aset :unsafe (t t t) t "ecl_aset_unsafe(#0,ecl_fixnum(#1),#2)")
    (def-inline si:row-major-aset :unsafe (t fixnum t) t "ecl_aset_unsafe(#0,#1,#2)")
    (def-inline si:row-major-aset :unsafe ((array t) fixnum t) t "(#0)->vector.self.t[#1]= #2")
    (def-inline si:row-major-aset :unsafe ((array bit) fixnum t) :fixnum "ecl_aset_bv(#0,#1,ecl_fixnum(#2))")
    (def-inline si:row-major-aset :unsafe ((array bit) fixnum fixnum) :fixnum "ecl_aset_bv(#0,#1,#2)")
    (def-inline si:row-major-aset :unsafe ((array base-char) fixnum base-char) :unsigned-char "(#0)->base_string.self[#1]= #2")
    (when (member :unicode *features*)
      (def-inline si:row-major-aset :unsafe ((array character) fixnum character) :wchar "(#0)->string.self[#1]= #2"))
    (def-inline si:row-major-aset :unsafe ((array ext:byte8) fixnum ext:byte8) :uint8-t "(#0)->vector.self.b8[#1]= #2")
    (def-inline si:row-major-aset :unsafe ((array ext:integer8) fixnum ext:integer8) :int8-t "(#0)->vector.self.i8[#1]= #2")
    (def-inline si:row-major-aset :unsafe ((array ext:byte16) fixnum ext:byte16) :uint16-t "(#0)->vector.self.b16[#1]= #2")
    (def-inline si:row-major-aset :unsafe ((array ext:integer16) fixnum ext:integer16) :int16-t "(#0)->vector.self.i16[#1]= #2")
    (def-inline si:row-major-aset :unsafe ((array ext:byte32) fixnum ext:byte32) :uint32-t "(#0)->vector.self.b32[#1]= #2")
    (def-inline si:row-major-aset :unsafe ((array ext:integer32) fixnum ext:integer32) :int32-t "(#0)->vector.self.i32[#1]= #2")
    (def-inline si:row-major-aset :unsafe ((array ext:byte64) fixnum ext:byte64) :uint64-t "(#0)->vector.self.b64[#1]= #2")
    (def-inline si:row-major-aset :unsafe ((array ext:integer64) fixnum ext:integer64) :int64-t "(#0)->vector.self.i64[#1]= #2")
    (def-inline si:row-major-aset :unsafe ((array long-float) fixnum long-float) :long-double "(#0)->array.self.lf[#1]= #2")
    (def-inline si:row-major-aset :unsafe ((array double-float) fixnum double-float) :double "(#0)->array.self.df[#1]= #2")
    (def-inline si:row-major-aset :unsafe ((array single-float) fixnum single-float) :float "(#0)->array.self.sf[#1]= #2")
    (when (member :complex-float *features*)
      (def-inline si:row-major-aset :unsafe ((array si:complex-single-float) fixnum si:complex-single-float) :csfloat "(#0)->array.self.csf[#1]= #2")
      (def-inline si:row-major-aset :unsafe ((array si:complex-double-float) fixnum si:complex-double-float) :cdfloat "(#0)->array.self.cdf[#1]= #2")
      (def-inline si:row-major-aset :unsafe ((array si:complex-long-float) fixnum si:complex-long-float) :clfloat "(#0)->array.self.clf[#1]= #2"))
    (def-inline si:row-major-aset :unsafe ((array fixnum) fixnum fixnum) :fixnum "(#0)->array.self.fix[#1]= #2")

    (def-inline si:copy-subarray :always (array ext:array-index array ext:array-index ext:array-index) array "@0;(ecl_copy_subarray(#0,#1,#2,#3,#4),#0)")

    (def-inline cl:array-rank :unsafe (array) :fixnum "@0;(((#0)->d.t == t_array)?(#0)->array.rank:1)")
    (def-inline cl:array-rank :always (array) :fixnum "ecl_array_rank(#0)")

    (def-inline cl:array-dimension :always (t t) fixnum "ecl_array_dimension(#0,ecl_to_size(#1))")
    (def-inline cl:array-dimension :always (t fixnum) fixnum "ecl_array_dimension(#0,#1)")

    (def-inline cl:array-total-size :unsafe (t) :fixnum "((#0)->array.dim)")

    (def-inline cl:adjustable-array-p :always (t) :bool "@0;(ECL_ARRAYP(#0)? (void)0: FEtype_error_array(#0),ECL_ADJUSTABLE_ARRAY_P(#0))")
    (def-inline cl:adjustable-array-p :unsafe (array) :bool "ECL_ADJUSTABLE_ARRAY_P(#0)")

    (def-inline cl:svref :always (t t) t "ecl_aref1(#0,ecl_to_size(#1))")
    (def-inline cl:svref :always (t fixnum) t "ecl_aref1(#0,#1)")
    (def-inline cl:svref :unsafe (t t) t "(#0)->vector.self.t[ecl_fixnum(#1)]")
    (def-inline cl:svref :unsafe (t fixnum) t "(#0)->vector.self.t[#1]")

    (def-inline si:svset :always (t t t) t "ecl_aset1(#0,ecl_to_size(#1),#2)")
    (def-inline si:svset :always (t fixnum t) t "ecl_aset1(#0,#1,#2)")
    (def-inline si:svset :unsafe (t t t) t "((#0)->vector.self.t[ecl_fixnum(#1)]=(#2))")
    (def-inline si:svset :unsafe (t fixnum t) t "(#0)->vector.self.t[#1]= #2")

    (def-inline cl:array-has-fill-pointer-p :always (t) :bool "@0;(ECL_ARRAYP(#0)?(void)0:FEtype_error_array(#0),ECL_ARRAY_HAS_FILL_POINTER_P(#0))")
    (def-inline cl:array-has-fill-pointer-p :unsafe (array) :bool "ECL_ARRAY_HAS_FILL_POINTER_P(#0)")

    (def-inline cl:fill-pointer :unsafe (t) :fixnum "((#0)->vector.fillp)")
    (def-inline si:fill-pointer-set :unsafe (t fixnum) :fixnum "((#0)->vector.fillp)=(#1)")

    ;; file character.d

    (def-inline cl:standard-char-p :always (character) :bool "ecl_standard_char_p(#0)")
    (def-inline cl:graphic-char-p :always (character) :bool "ecl_graphic_char_p(#0)")
    (def-inline cl:alpha-char-p :always (character) :bool "ecl_alpha_char_p(#0)")
    (def-inline cl:upper-case-p :always (character) :bool "ecl_upper_case_p(#0)")
    (def-inline cl:lower-case-p :always (character) :bool "ecl_lower_case_p(#0)")
    (def-inline cl:both-case-p :always (character) :bool "ecl_both_case_p(#0)")
    (def-inline cl:alphanumericp :always (character) :bool "ecl_alphanumericp(#0)")

    (def-inline cl:char= :always (t t) :bool "ecl_char_code(#0)==ecl_char_code(#1)")
    (def-inline cl:char= :always (character character) :bool "(#0)==(#1)")

    (def-inline cl:char/= :always (t t) :bool "ecl_char_code(#0)!=ecl_char_code(#1)")
    (def-inline cl:char/= :always (character character) :bool "(#0)!=(#1)")

    (def-inline cl:char< :always (character character) :bool "(#0)<(#1)")
    (def-inline cl:char> :always (character character) :bool "(#0)>(#1)")
    (def-inline cl:char<= :always (character character) :bool "(#0)<=(#1)")
    (def-inline cl:char>= :always (character character) :bool "(#0)>=(#1)")

    (def-inline cl:char-code :always (character) :fixnum "#0")
    (def-inline cl:code-char :always (fixnum) :wchar "#0")

    (def-inline cl:char-upcase :always (base-char) :unsigned-char "ecl_char_upcase(#0)")
    (def-inline cl:char-upcase :always (character) :wchar "ecl_char_upcase(#0)")

    (def-inline cl:char-downcase :always (base-char) :unsigned-char "ecl_char_downcase(#0)")
    (def-inline cl:char-downcase :always (character) :wchar "ecl_char_downcase(#0)")

    (def-inline cl:char-int :always (character) :fixnum "#0")

    ;; file ffi.d

    (def-inline si:foreign-data-p :always (t) :bool "@0;ECL_FOREIGN_DATA_P(#0)")

    ;; file file.d

    (def-inline cl:input-stream-p :always (stream) :bool "ecl_input_stream_p(#0)")
    (def-inline cl:output-stream-p :always (stream) :bool "ecl_output_stream_p(#0)")

    ;; file hash.d

    (def-inline cl:gethash :always (t t t) t "ecl_gethash_safe(#0,#1,#2)" :multiple-values nil)
    (def-inline cl:gethash :always (t t) t "ecl_gethash_safe(#0,#1,ECL_NIL)" :multiple-values nil)
    (def-inline cl:hash-table-count :unsafe (hash-table) ext:array-index "ecl_hash_table_count(#0)")

    ;; file list.d

    (def-inline cl:car :unsafe (cons) t "ECL_CONS_CAR(#0)")
    (def-inline cl:car :unsafe (t) t "_ecl_car(#0)")

    (def-inline si:cons-car :always (t) t "_ecl_car(#0)")
    (def-inline si:cons-car :unsafe (t) t "ECL_CONS_CAR(#0)")

    (def-inline cl:cdr :unsafe (cons) t "ECL_CONS_CDR(#0)")
    (def-inline cl:cdr :unsafe (t) t "_ecl_cdr(#0)")

    (def-inline si:cons-cdr :always (t) t "_ecl_cdr(#0)")
    (def-inline si:cons-cdr :unsafe (t) t "ECL_CONS_CDR(#0)")

    ;; BEGIN-GENERATED (gen-cons-sysfun)

    (def-inline cl:car :always (t) t "ecl_car(#0)")
    (def-inline cl:car :unsafe (t) t "_ecl_car(#0)")
    (def-inline cl:cdr :always (t) t "ecl_cdr(#0)")
    (def-inline cl:cdr :unsafe (t) t "_ecl_cdr(#0)")
    (def-inline cl:caar :always (t) t "ecl_caar(#0)")
    (def-inline cl:caar :unsafe (t) t "_ecl_caar(#0)")
    (def-inline cl:cdar :always (t) t "ecl_cdar(#0)")
    (def-inline cl:cdar :unsafe (t) t "_ecl_cdar(#0)")
    (def-inline cl:cadr :always (t) t "ecl_cadr(#0)")
    (def-inline cl:cadr :unsafe (t) t "_ecl_cadr(#0)")
    (def-inline cl:cddr :always (t) t "ecl_cddr(#0)")
    (def-inline cl:cddr :unsafe (t) t "_ecl_cddr(#0)")
    (def-inline cl:caaar :always (t) t "ecl_caaar(#0)")
    (def-inline cl:caaar :unsafe (t) t "_ecl_caaar(#0)")
    (def-inline cl:cdaar :always (t) t "ecl_cdaar(#0)")
    (def-inline cl:cdaar :unsafe (t) t "_ecl_cdaar(#0)")
    (def-inline cl:cadar :always (t) t "ecl_cadar(#0)")
    (def-inline cl:cadar :unsafe (t) t "_ecl_cadar(#0)")
    (def-inline cl:cddar :always (t) t "ecl_cddar(#0)")
    (def-inline cl:cddar :unsafe (t) t "_ecl_cddar(#0)")
    (def-inline cl:caadr :always (t) t "ecl_caadr(#0)")
    (def-inline cl:caadr :unsafe (t) t "_ecl_caadr(#0)")
    (def-inline cl:cdadr :always (t) t "ecl_cdadr(#0)")
    (def-inline cl:cdadr :unsafe (t) t "_ecl_cdadr(#0)")
    (def-inline cl:caddr :always (t) t "ecl_caddr(#0)")
    (def-inline cl:caddr :unsafe (t) t "_ecl_caddr(#0)")
    (def-inline cl:cdddr :always (t) t "ecl_cdddr(#0)")
    (def-inline cl:cdddr :unsafe (t) t "_ecl_cdddr(#0)")
    (def-inline cl:caaaar :always (t) t "ecl_caaaar(#0)")
    (def-inline cl:caaaar :unsafe (t) t "_ecl_caaaar(#0)")
    (def-inline cl:cdaaar :always (t) t "ecl_cdaaar(#0)")
    (def-inline cl:cdaaar :unsafe (t) t "_ecl_cdaaar(#0)")
    (def-inline cl:cadaar :always (t) t "ecl_cadaar(#0)")
    (def-inline cl:cadaar :unsafe (t) t "_ecl_cadaar(#0)")
    (def-inline cl:cddaar :always (t) t "ecl_cddaar(#0)")
    (def-inline cl:cddaar :unsafe (t) t "_ecl_cddaar(#0)")
    (def-inline cl:caadar :always (t) t "ecl_caadar(#0)")
    (def-inline cl:caadar :unsafe (t) t "_ecl_caadar(#0)")
    (def-inline cl:cdadar :always (t) t "ecl_cdadar(#0)")
    (def-inline cl:cdadar :unsafe (t) t "_ecl_cdadar(#0)")
    (def-inline cl:caddar :always (t) t "ecl_caddar(#0)")
    (def-inline cl:caddar :unsafe (t) t "_ecl_caddar(#0)")
    (def-inline cl:cdddar :always (t) t "ecl_cdddar(#0)")
    (def-inline cl:cdddar :unsafe (t) t "_ecl_cdddar(#0)")
    (def-inline cl:caaadr :always (t) t "ecl_caaadr(#0)")
    (def-inline cl:caaadr :unsafe (t) t "_ecl_caaadr(#0)")
    (def-inline cl:cdaadr :always (t) t "ecl_cdaadr(#0)")
    (def-inline cl:cdaadr :unsafe (t) t "_ecl_cdaadr(#0)")
    (def-inline cl:cadadr :always (t) t "ecl_cadadr(#0)")
    (def-inline cl:cadadr :unsafe (t) t "_ecl_cadadr(#0)")
    (def-inline cl:cddadr :always (t) t "ecl_cddadr(#0)")
    (def-inline cl:cddadr :unsafe (t) t "_ecl_cddadr(#0)")
    (def-inline cl:caaddr :always (t) t "ecl_caaddr(#0)")
    (def-inline cl:caaddr :unsafe (t) t "_ecl_caaddr(#0)")
    (def-inline cl:cdaddr :always (t) t "ecl_cdaddr(#0)")
    (def-inline cl:cdaddr :unsafe (t) t "_ecl_cdaddr(#0)")
    (def-inline cl:cadddr :always (t) t "ecl_cadddr(#0)")
    (def-inline cl:cadddr :unsafe (t) t "_ecl_cadddr(#0)")
    (def-inline cl:cddddr :always (t) t "ecl_cddddr(#0)")
    (def-inline cl:cddddr :unsafe (t) t "_ecl_cddddr(#0)")
    ;; END-GENERATED

    (def-inline cl:cons :always (t t) t "CONS(#0,#1)")

    (def-inline cl:endp :safe (t) :bool "ecl_endp(#0)")
    (def-inline cl:endp :unsafe (t) :bool "#0==ECL_NIL")

    (def-inline cl:nth :always (t t) t "ecl_nth(ecl_to_size(#0),#1)")
    (def-inline cl:nth :always (fixnum t) t "ecl_nth(#0,#1)")
    (def-inline cl:nth :unsafe (t t) t "ecl_nth(ecl_fixnum(#0),#1)")
    (def-inline cl:nth :unsafe (fixnum t) t "ecl_nth(#0,#1)")

    (def-inline cl:nthcdr :always (t t) t "ecl_nthcdr(ecl_to_size(#0),#1)")
    (def-inline cl:nthcdr :always (fixnum t) t "ecl_nthcdr(#0,#1)")
    (def-inline cl:nthcdr :unsafe (t t) t "ecl_nthcdr(ecl_fixnum(#0),#1)")
    (def-inline cl:nthcdr :unsafe (fixnum t) t "ecl_nthcdr(#0,#1)")

    (def-inline cl:last :always (t) t "ecl_last(#0,1)")

    (def-inline cl:list :always nil t "ECL_NIL")
    (def-inline cl:list :always (t) t "ecl_list1(#0)")

    (def-inline cl:list* :always (t) t "#0")
    (def-inline cl:list* :always (t t) t "CONS(#0,#1)")

    (def-inline cl:append :always (t t) t "ecl_append(#0,#1)")
    (def-inline cl:nconc :always (t t) t "ecl_nconc(#0,#1)")
    (def-inline cl:butlast :always (t) t "ecl_butlast(#0,1)")
    (def-inline cl:nbutlast :always (t) t "ecl_nbutlast(#0,1)")

    ;; file num_arith.d

    (def-inline cl:1+ :always (t) t "ecl_one_plus(#0)")
    (def-inline cl:1+ :always (fixnum) t "ecl_make_integer((#0)+1)")
    (def-inline cl:1+ :always (long-float) :long-double "(long double)(#0)+1")
    (def-inline cl:1+ :always (double-float) :double "(double)(#0)+1")
    (def-inline cl:1+ :always (single-float) :float "(float)(#0)+1")
    (when (member :complex-float *features*)
      (def-inline cl:1+ :always (si:complex-single-float) :csfloat "(_Complex float)(#0)+1")
      (def-inline cl:1+ :always (si:complex-double-float) :cdfloat "(_Complex double)(#0)+1")
      (def-inline cl:1+ :always (si:complex-long-float) :clfloat "(_Complex long double)(#0)+1"))
    (def-inline cl:1+ :always (fixnum) :fixnum "(#0)+1" :exact-return-type t)

    (def-inline cl:1- :always (t) t "ecl_one_minus(#0)")
    (def-inline cl:1- :always (fixnum) t "ecl_make_integer((#0)-1)")
    (def-inline cl:1- :always (long-float) :long-double "(long double)(#0)-1")
    (def-inline cl:1- :always (double-float) :double "(double)(#0)-1")
    (def-inline cl:1- :always (single-float) :float "(float)(#0)-1")
    (when (member :complex-float *features*)
      (def-inline cl:1- :always (si:complex-single-float) :csfloat "(_Complex float)(#0)-1")
      (def-inline cl:1- :always (si:complex-double-float) :cdfloat "(_Complex double)(#0)-1")
      (def-inline cl:1- :always (si:complex-long-float) :clfloat "(_Complex long double)(#0)-1"))
    (def-inline cl:1- :always (fixnum) :fixnum "(#0)-1" :exact-return-type t)

    ;; file num_co.d

    (def-inline cl:float :always (t single-float) :float "ecl_to_float(#0)")
    (def-inline cl:float :always (t double-float) :double "ecl_to_double(#0)")
    (def-inline cl:float :always (t long-float) :long-double "ecl_to_long_double(#0)")
    (def-inline cl:float :always (fixnum-float) :long-double "((long double)(#0))" :exact-return-type t)
    (def-inline cl:float :always (fixnum-float) :double "((double)(#0))" :exact-return-type t)
    (def-inline cl:float :always (fixnum-float) :float "((float)(#0))" :exact-return-type t)

    (def-inline cl:numerator :unsafe (integer) integer "(#0)")
    (def-inline cl:numerator :unsafe (ratio) integer "(#0)->ratio.num")

    (def-inline cl:denominator :unsafe (integer) integer "ecl_make_fixnum(1)")
    (def-inline cl:denominator :unsafe (ratio) integer "(#0)->ratio.den")

    (def-inline cl:floor :always (t) (values &rest t) "ecl_floor1(#0)")
    (def-inline cl:floor :always (t t) (values &rest t) "ecl_floor2(#0,#1)")
    #+(or) ; does not work well, no multiple values
    (def-inline cl:floor :always (fixnum fixnum) :fixnum "@01;(#0>=0&&#1>0?(#0)/(#1):ecl_ifloor(#0,#1))")

    (def-inline cl:ceiling :always (t) (values &rest t) "ecl_ceiling1(#0)")
    (def-inline cl:ceiling :always (t t) (values &rest t) "ecl_ceiling2(#0,#1)")

    (def-inline cl:truncate :always (t) (values &rest t) "ecl_truncate1(#0)")
    (def-inline cl:truncate :always (t t) (values &rest t) "ecl_truncate2(#0,#1)")
    #+(or) ; does not work well, no multiple values
    (def-inline cl:truncate :always (fixnum-float) :fixnum "(cl_fixnum)(#0)")

    (def-inline cl:round :always (t) (values &rest t) "ecl_round1(#0)")
    (def-inline cl:round :always (t t) (values &rest t) "ecl_round2(#0,#1)")

    (def-inline cl:mod :always (t t) t "(ecl_floor2(#0,#1),cl_env_copy->values[1])")
    (def-inline cl:mod :always (fixnum fixnum) :fixnum "@01;(#0>=0&&#1>0?(#0)%(#1):ecl_imod(#0,#1))")

    (def-inline cl:rem :always (t t) t "(ecl_truncate2(#0,#1),cl_env_copy->values[1])")
    (def-inline cl:rem :always (fixnum fixnum) :fixnum "(#0)%(#1)")

    (def-inline cl:= :always (t t) :bool "ecl_number_equalp(#0,#1)")
    (def-inline cl:= :always (fixnum-float fixnum-float) :bool "(#0)==(#1)")

    (def-inline cl:/= :always (t t) :bool "!ecl_number_equalp(#0,#1)")
    (def-inline cl:/= :always (fixnum-float fixnum-float) :bool "(#0)!=(#1)")

    (def-inline cl:< :always (t t) :bool "ecl_lower(#0,#1)")
    (def-inline cl:< :always (fixnum-float fixnum-float) :bool "(#0)<(#1)")
    (def-inline cl:< :always (fixnum-float fixnum-float fixnum-float) :bool "@012;((#0)<(#1) && (#1)<(#2))")

    (def-inline cl:> :always (t t) :bool "ecl_greater(#0,#1)")
    (def-inline cl:> :always (fixnum-float fixnum-float) :bool "(#0)>(#1)")
    (def-inline cl:> :always (fixnum-float fixnum-float fixnum-float) :bool "@012;((#0)>(#1) && (#1)>(#2))")

    (def-inline cl:<= :always (t t) :bool "ecl_lowereq(#0,#1)")
    (def-inline cl:<= :always (fixnum-float fixnum-float) :bool "(#0)<=(#1)")
    (def-inline cl:<= :always (fixnum-float fixnum-float fixnum-float) :bool "@012;((#0)<=(#1) && (#1)<=(#2))")

    (def-inline cl:>= :always (t t) :bool "ecl_greatereq(#0,#1)")
    (def-inline cl:>= :always (fixnum-float fixnum-float) :bool "(#0)>=(#1)")
    (def-inline cl:>= :always (fixnum-float fixnum-float fixnum-float) :bool "@012;((#0)>=(#1) && (#1)>=(#2))")

    (def-inline cl:max :always (fixnum fixnum) :fixnum "@01;(#0)>=(#1)?#0:#1")
    (def-inline cl:min :always (fixnum fixnum) :fixnum "@01;(#0)<=(#1)?#0:#1")
    (if (member :ieee-floating-point *features*)
        (progn
          (def-inline cl:max :always (t t) t "@01;((ecl_float_nan_p(#1) || ecl_greatereq(#0,#1))?#0:#1)")
          (def-inline cl:min :always (t t) t "@01;((ecl_float_nan_p(#1) || ecl_lowereq(#0,#1))?#0:#1)"))
        (progn
          (def-inline cl:max :always (t t) t "@01;(ecl_greatereq(#0,#1)?#0:#1)")
          (def-inline cl:min :always (t t) t "@01;(ecl_lowereq(#0,#1)?#0:#1)")))

    ;; file num_log.d

    (def-inline cl:logand :always nil t "ecl_make_fixnum(-1)")
    (def-inline cl:logand :always nil :fixnum "-1")
    (def-inline cl:logand :always (t t) t "ecl_boole(ECL_BOOLAND,(#0),(#1))")
    (def-inline cl:logand :always (fixnum fixnum) :fixnum "((#0) & (#1))")

    (def-inline cl:logandc1 :always (t t) t "ecl_boole(ECL_BOOLANDC1,(#0),(#1))")
    (def-inline cl:logandc1 :always (fixnum fixnum) :fixnum "(~(#0) & (#1))")

    (def-inline cl:logandc2 :always (t t) t "ecl_boole(ECL_BOOLANDC2,(#0),(#1))")
    (def-inline cl:logandc2 :always (fixnum fixnum) :fixnum "((#0) & ~(#1))")

    (def-inline cl:logeqv :always nil t "ecl_make_fixnum(-1)")
    (def-inline cl:logeqv :always nil :fixnum "-1")
    (def-inline cl:logeqv :always (t t) t "ecl_boole(ECL_BOOLEQV,(#0),(#1))")
    (def-inline cl:logeqv :always (fixnum fixnum) :fixnum "(~( (#0) ^ (#1) ))")

    (def-inline cl:logior :always nil t "ecl_make_fixnum(0)")
    (def-inline cl:logior :always nil :fixnum "0")
    (def-inline cl:logior :always (t t) t "ecl_boole(ECL_BOOLIOR,(#0),(#1))")
    (def-inline cl:logior :always (fixnum fixnum) :fixnum "((#0) | (#1))")

    (def-inline cl:lognand :always (t t) t "ecl_boole(ECL_BOOLNAND,(#0),(#1))")
    (def-inline cl:lognand :always (fixnum fixnum) :fixnum "(~( (#0) & (#1) ))")

    (def-inline cl:lognor :always (t t) t "ecl_boole(ECL_BOOLNOR,(#0),(#1))")
    (def-inline cl:lognor :always (fixnum fixnum) :fixnum "(~( (#0) | (#1) ))")

    (def-inline cl:lognot :always (t) t "ecl_boole(ECL_BOOLXOR,(#0),ecl_make_fixnum(-1))")
    (def-inline cl:lognot :always (fixnum) :fixnum "(~(#0))")

    (def-inline cl:logorc1 :always (t t) t "ecl_boole(ECL_BOOLORC1,(#0),(#1))")
    (def-inline cl:logorc1 :always (fixnum fixnum) :fixnum "(~(#0) | (#1))")

    (def-inline cl:logorc2 :always (t t) t "ecl_boole(ECL_BOOLORC2,(#0),(#1))")
    (def-inline cl:logorc2 :always (fixnum fixnum) :fixnum "((#0) | ~(#1))")

    (def-inline cl:logxor :always nil t "ecl_make_fixnum(0)")
    (def-inline cl:logxor :always nil :fixnum "0")
    (def-inline cl:logxor :always (t t) t "ecl_boole(ECL_BOOLXOR,(#0),(#1))")
    (def-inline cl:logxor :always (fixnum fixnum) :fixnum "((#0) ^ (#1))")

    (def-inline cl:boole :always (fixnum t t) t "ecl_boole((#0),(#1),(#2))")

    (def-inline cl:logbitp :always ((integer -29 29) fixnum) :bool "(#1 >> #0) & 1")

    (def-inline cl:integer-length :always (t) :cl-index "ecl_integer_length(#0)")

    (def-inline cl:zerop :always (t) :bool "ecl_zerop(#0)")
    (def-inline cl:zerop :always (fixnum-float) :bool "(#0)==0")

    (def-inline cl:plusp :always (t) :bool "ecl_plusp(#0)")
    (def-inline cl:plusp :always (fixnum-float) :bool "(#0)>0")

    (def-inline cl:minusp :always (t) :bool "ecl_minusp(#0)")
    (def-inline cl:minusp :always (fixnum-float) :bool "(#0)<0")

    (def-inline cl:oddp :always (t) :bool "ecl_oddp(#0)")
    (def-inline cl:oddp :always (fixnum fixnum) :bool "(#0) & 1")

    (def-inline cl:evenp :always (t) :bool "ecl_evenp(#0)")
    (def-inline cl:evenp :always (fixnum fixnum) :bool "~(#0) & 1")

    (def-inline cl:abs :always (t t) t "ecl_abs(#0,#1)")
    (def-inline cl:exp :always (t) t "ecl_exp(#0)")

    (def-inline cl:expt :always (t t) t "ecl_expt(#0,#1)")
    (def-inline cl:expt :always ((integer 2 2) (integer 0 29)) :fixnum "(1<<(#1))")
    (def-inline cl:expt :always ((integer 0 0) t) :fixnum "0")
    (def-inline cl:expt :always ((integer 1 1) t) :fixnum "1")
    (def-inline cl:expt :always ((long-float 0.0l0 *) long-float) :long-double "powl((long double)#0,(long double)#1)")
    (def-inline cl:expt :always ((double-float 0.0d0 *) double-float) :double "pow((double)#0,(double)#1)")
    (def-inline cl:expt :always ((single-float 0.0f0 *) single-float) :float "powf((float)#0,(float)#1)")
    (when (member :complex-float *features*)
      (def-inline cl:expt :always (si:complex-single-float si:complex-single-float) :csfloat "cpowf(#0,#1)")
      (def-inline cl:expt :always (si:complex-double-float si:complex-double-float) :cdfloat "cpow(#0,#1)")
      (def-inline cl:expt :always (si:complex-long-float si:complex-long-float) :clfloat "cpowl(#0,#1)"))

    (def-inline cl:log :always (fixnum-float) :long-double "logl((long double)(#0))" :exact-return-type t)
    (def-inline cl:log :always (fixnum-float) :double "log((double)(#0))" :exact-return-type t)
    (def-inline cl:log :always (fixnum-float) :float "logf((float)(#0))" :exact-return-type t)
    (when (member :complex-float *features*)
      (def-inline cl:log :always (si:complex-single-float) :csfloat "clogf(#0)")
      (def-inline cl:log :always (si:complex-double-float) :cdfloat "clog(#0)")
      (def-inline cl:log :always (si:complex-long-float) :clfloat "clogl(#0)"))

    (def-inline cl:sqrt :always (number) number "ecl_sqrt(#0)")
    (def-inline cl:sqrt :always ((long-float 0.0l0 *)) :long-double "sqrtl((long double)(#0))")
    (def-inline cl:sqrt :always ((double-float 0.0d0 *)) :double "sqrt((double)(#0))")
    (def-inline cl:sqrt :always ((single-float 0.0f0 *)) :float "sqrtf((float)(#0))")
    (when (member :complex-float *features*)
      (def-inline cl:sqrt :always (si:complex-single-float) :csfloat "csqrtf(#0)")
      (def-inline cl:sqrt :always (si:complex-double-float) :cdfloat "csqrt(#0)")
      (def-inline cl:sqrt :always (si:complex-long-float) :clfloat "csqrtl(#0)"))

    (def-inline cl:sin :always (number) number "ecl_sin(#0)")
    (def-inline cl:sin :always (fixnum-float) :long-double "sinl((long double)(#0))" :exact-return-type t)
    (def-inline cl:sin :always (fixnum-float) :double "sin((double)(#0))" :exact-return-type t)
    (def-inline cl:sin :always (fixnum-float) :float "sinf((float)(#0))" :exact-return-type t)
    (when (member :complex-float *features*)
      (def-inline cl:sin :always (si:complex-single-float) :csfloat "csinf(#0)")
      (def-inline cl:sin :always (si:complex-double-float) :cdfloat "csin(#0)")
      (def-inline cl:sin :always (si:complex-long-float) :clfloat "csinl(#0)"))

    (def-inline cl:cos :always (t) number "ecl_cos(#0)")
    (def-inline cl:cos :always (fixnum-float) :long-double "cosl((long double)(#0))" :exact-return-type t)
    (def-inline cl:cos :always (fixnum-float) :double "cos((double)(#0))" :exact-return-type t)
    (def-inline cl:cos :always (fixnum-float) :float "cosf((float)(#0))" :exact-return-type t)
    (when (member :complex-float *features*)
      (def-inline cl:cos :always (si:complex-single-float) :csfloat "ccosf(#0)")
      (def-inline cl:cos :always (si:complex-double-float) :cdfloat "ccos(#0)")
      (def-inline cl:cos :always (si:complex-long-float) :clfloat "ccosl(#0)"))

    (def-inline cl:tan :always (t) number "ecl_tan(#0)")
    (def-inline cl:tan :always (fixnum-float) :long-double "tanl((long double)(#0))" :exact-return-type t)
    (def-inline cl:tan :always (fixnum-float) :double "tan((double)(#0))" :exact-return-type t)
    (def-inline cl:tan :always (fixnum-float) :float "tanf((float)(#0))" :exact-return-type t)
    (when (member :complex-float *features*)
      (def-inline cl:tan :always (si:complex-single-float) :csfloat "ctanf(#0)")
      (def-inline cl:tan :always (si:complex-double-float) :cdfloat "ctan(#0)")
      (def-inline cl:tan :always (si:complex-long-float) :clfloat "ctanl(#0)"))

    (def-inline cl:sinh :always (t) number "ecl_sinh(#0)")
    (def-inline cl:sinh :always (fixnum-float) :long-double "sinhl((long double)(#0))" :exact-return-type t)
    (def-inline cl:sinh :always (fixnum-float) :double "sinh((double)(#0))" :exact-return-type t)
    (def-inline cl:sinh :always (fixnum-float) :float "sinhf((float)(#0))" :exact-return-type t)
    (when (member :complex-float *features*)
      (def-inline cl:sinh :always (si:complex-single-float) :csfloat "csinhf(#0)")
      (def-inline cl:sinh :always (si:complex-double-float) :cdfloat "csinh(#0)")
      (def-inline cl:sinh :always (si:complex-long-float) :clfloat "csinhl(#0)"))

    (def-inline cl:cosh :always (t) number "ecl_cosh(#0)")
    (def-inline cl:cosh :always (fixnum-float) :long-double "coshl((long double)(#0))" :exact-return-type t)
    (def-inline cl:cosh :always (fixnum-float) :double "cosh((double)(#0))" :exact-return-type t)
    (def-inline cl:cosh :always (fixnum-float) :float "coshf((float)(#0))" :exact-return-type t)
    (when (member :complex-float *features*)
      (def-inline cl:cosh :always (si:complex-single-float) :csfloat "ccoshf(#0)")
      (def-inline cl:cosh :always (si:complex-double-float) :cdfloat "ccosh(#0)")
      (def-inline cl:cosh :always (si:complex-long-float) :clfloat "ccoshl(#0)"))

    (def-inline cl:tanh :always (t) number "ecl_tanh(#0)")
    (def-inline cl:tanh :always (fixnum-float) :long-double "tanhl((long double)(#0))" :exact-return-type t)
    (def-inline cl:tanh :always (fixnum-float) :double "tanh((double)(#0))" :exact-return-type t)
    (def-inline cl:tanh :always (fixnum-float) :float "tanhf((float)(#0))" :exact-return-type t)
    (when (member :complex-float *features*)
      (def-inline cl:tanh :always (si:complex-single-float) :csfloat "ctanhf(#0)")
      (def-inline cl:tanh :always (si:complex-double-float) :cdfloat "ctanh(#0)")
      (def-inline cl:tanh :always (si:complex-long-float) :clfloat "ctanhl(#0)"))

    ;; file package.d

    ;; file pathname.d

    (def-inline cl:null :always (t) :bool "#0==ECL_NIL")
    (def-inline cl:symbolp :always (t) :bool "@0;ECL_SYMBOLP(#0)")
    (def-inline cl:atom :always (t) :bool "@0;ECL_ATOM(#0)")
    (def-inline cl:consp :always (t) :bool "@0;ECL_CONSP(#0)")
    (def-inline cl:listp :always (t) :bool "@0;ECL_LISTP(#0)")
    (def-inline cl:numberp :always (t) :bool "ecl_numberp(#0)")
    (def-inline cl:integerp :always (t) :bool "@0;ECL_FIXNUMP(#0)||ECL_BIGNUMP(#0)")
    (def-inline cl:floatp :always (t) :bool "floatp(#0)")
    (def-inline cl:characterp :always (t) :bool "ECL_CHARACTERP(#0)")
    (def-inline si:base-char-p :always (character) :bool "ECL_BASE_CHAR_P(#0)")
    (def-inline cl:stringp :always (t) :bool "@0;ECL_STRINGP(#0)")
    (def-inline si:base-string-p :always (t) :bool "@0;ECL_BASE_STRING_P(#0)")
    (def-inline cl:bit-vector-p :always (t) :bool "@0;ECL_BIT_VECTOR_P(#0)")
    (def-inline cl:vectorp :always (t) :bool "@0;ECL_VECTORP(#0)")
    (def-inline cl:arrayp :always (t) :bool "@0;ECL_ARRAYP(#0)")

    (def-inline cl:eq :always (t t) :bool "(#0)==(#1)")
    (def-inline cl:eq :always (fixnum fixnum) :bool "(#0)==(#1)")

    (def-inline cl:eql :always (t t) :bool "ecl_eql(#0,#1)")
    (def-inline cl:eql :always (character t) :bool "(ECL_CODE_CHAR(#0)==(#1))")
    (def-inline cl:eql :always (t character) :bool "((#0)==ECL_CODE_CHAR(#1))")
    (def-inline cl:eql :always (character character) :bool "(#0)==(#1)")
    (def-inline cl:eql :always ((not (or complex bignum ratio float)) t) :bool "(#0)==(#1)")
    (def-inline cl:eql :always (t (not (or complex bignum ratio float))) :bool "(#0)==(#1)")
    (def-inline cl:eql :always (fixnum fixnum) :bool "(#0)==(#1)")

    (def-inline cl:equal :always (t t) :bool "ecl_equal(#0,#1)")
    (def-inline cl:equal :always (fixnum fixnum) :bool "(#0)==(#1)")

    (def-inline cl:equalp :always (t t) :bool "ecl_equalp(#0,#1)")
    (def-inline cl:equalp :always (fixnum fixnum) :bool "(#0)==(#1)")

    (def-inline cl:not :always (t) :bool "(#0)==ECL_NIL")

    ;; file print.d, read.d

    (def-inline cl:clear-output :always (stream) NULL "(ecl_clear_output(#0),ECL_NIL)")
    (def-inline cl:finish-output :always (stream) NULL "(ecl_finish_output(#0),ECL_NIL)")
    (def-inline cl:finish-output :always (stream) NULL "(ecl_force_output(#0),ECL_NIL)")
    (def-inline cl:write-char :always (t) t "@0;(ecl_princ_char(ecl_char_code(#0),ECL_NIL),(#0))")
    (def-inline cl:clear-input :always (stream) NULL "(ecl_clear_input(#0),ECL_NIL)")
    (def-inline cl:copy-readtable :always (null null) t "standard_readtable")

    (def-inline cl:boundp :always (t) :bool "ecl_boundp(cl_env_copy,#0)")
    (def-inline cl:boundp :unsafe ((and symbol (not null))) :bool "ECL_SYM_VAL(cl_env_copy,#0)!=OBJNULL")

    ;; file unixsys.d

    ;; file sequence.d

    (def-inline cl:elt :always (t t) t "ecl_elt(#0,ecl_to_size(#1))")
    (def-inline cl:elt :always (t fixnum) t "ecl_elt(#0,#1)")
    (def-inline cl:elt :unsafe (t t) t "ecl_elt(#0,ecl_fixnum(#1))")
    (def-inline cl:elt :unsafe (t fixnum) t "ecl_elt(#0,#1)")
    (def-inline cl:elt :unsafe (vector t) t "ecl_aref_unsafe(#0,ecl_fixnum(#1))")
    (def-inline cl:elt :unsafe (vector fixnum) t "ecl_aref_unsafe(#0,#1)")
    (def-inline cl:aref :unsafe ((array bit) t) :fixnum "ecl_aref_bv(#0,ecl_fixnum(#1))")
    (def-inline cl:aref :unsafe ((array bit) fixnum) :fixnum "ecl_aref_bv(#0,#1)")
    (when (member :unicode *features*)
      (def-inline cl:aref :unsafe ((array character) fixnum) :wchar "(#0)->string.self[#1]"))
    (def-inline cl:aref :unsafe ((array base-char) fixnum) :unsigned-char "(#0)->base_string.self[#1]")
    (def-inline cl:aref :unsafe ((array double-float) fixnum) :double "(#0)->array.self.df[#1]")
    (def-inline cl:aref :unsafe ((array single-float) fixnum) :float "(#0)->array.self.sf[#1]")
    (def-inline cl:aref :unsafe ((array fixnum) fixnum) :fixnum "(#0)->array.self.fix[#1]")

    (def-inline si:elt-set :always (t t t) t "ecl_elt_set(#0,ecl_to_size(#1),#2)")
    (def-inline si:elt-set :always (t fixnum t) t "ecl_elt_set(#0,#1,#2)")
    (def-inline si:elt-set :unsafe (t t t) t "ecl_elt_set(#0,ecl_fixnum(#1),#2)")
    (def-inline si:elt-set :unsafe (vector t t) t "ecl_aset_unsafe(#0,ecl_to_size(#1),#2)")
    (def-inline si:elt-set :unsafe (vector fixnum t) t "ecl_aset_unsafe(#0,#1,#2)")

    (def-inline cl:length :always (t) :fixnum "ecl_length(#0)")
    (def-inline cl:length :unsafe (vector) :fixnum "(#0)->vector.fillp")

    (def-inline cl:copy-seq :always (t) t "ecl_copy_seq(#0)")

    ;; file character.d

    (def-inline cl:char :always (t fixnum) t "ecl_aref1(#0,#1)")
    (def-inline cl:char :always (t fixnum) :wchar "ecl_char(#0,#1)")
    (if (member :unicode *features*)
        (def-inline cl:char :unsafe (ext:extended-string fixnum) :wchar "(#0)->string.self[#1]")
        (progn
          (def-inline cl:char :unsafe (t t) t "ECL_CODE_CHAR((#0)->base_string.self[ecl_fixnum(#1)])")
          (def-inline cl:char :unsafe (t fixnum) :unsigned-char "(#0)->base_string.self[#1]")))
    (def-inline cl:char :unsafe (base-string fixnum) :unsigned-char "(#0)->base_string.self[#1]")

    (def-inline si:char-set :always (t t t) t "si_char_set(#0,#1,#2)")
    (def-inline si:char-set :always (t fixnum t) t "ecl_aset1(#0,#1,#2)")
    (def-inline si:char-set :always (t fixnum character) :wchar "ecl_char_set(#0,#1,#2)")

    (unless (member :unicode *features*)
      (def-inline si:char-set :unsafe (t t t) t "@2;((#0)->base_string.self[ecl_fixnum(#1)]=ecl_char_code(#2),(#2))")
      (def-inline si:char-set :unsafe (t fixnum character) :unsigned-char "(#0)->base_string.self[#1]= #2"))
    (def-inline si:char-set :unsafe (base-string t t) t "@2;((#0)->base_string.self[ecl_fixnum(#1)]=ecl_char_code(#2),(#2))")
    (def-inline si:char-set :unsafe (base-string fixnum base-char) :unsigned-char "(#0)->base_string.self[#1]= #2")
    (def-inline si:char-set :unsafe (ext:extended-string t t) t "@2;((#0)->string.self[ecl_fixnum(#1)]=ecl_char_code(#2),(#2))")
    (def-inline si:char-set :unsafe (ext:extended-string fixnum character) :unsigned-char "(#0)->string.self[#1]= #2")

    (def-inline cl:schar :always (t t) t "ecl_elt(#0,ecl_to_size(#1))")
    (def-inline cl:schar :always (t fixnum) t "ecl_elt(#0,#1)")
    (def-inline cl:schar :always (t fixnum) :wchar "ecl_char(#0,#1)")
    (def-inline cl:schar :unsafe (base-string t) t "ECL_CODE_CHAR((#0)->base_string.self[ecl_fixnum(#1)])")
    (def-inline cl:schar :unsafe (base-string fixnum) :unsigned-char "(#0)->base_string.self[#1]")
    (if (member :unicode *features*)
        (def-inline cl:schar :unsafe (ext:extended-string fixnum) :wchar "(#0)->string.self[#1]")
        (def-inline cl:schar :unsafe (t fixnum) :unsigned-char "(#0)->base_string.self[#1]"))
    (def-inline si:schar-set :always (t t t) t "ecl_elt_set(#0,ecl_to_size(#1),#2)")
    (def-inline si:schar-set :always (t fixnum t) t "ecl_elt_set(#0,#1,#2)")
    (def-inline si:schar-set :always (t fixnum character) :wchar "ecl_char_set(#0,#1,#2)")
    (if (member :unicode *features*)
        (progn
          (def-inline si:schar-set :unsafe (ext:extended-string fixnum t) :wchar "@2;((#0)->string.self[#1]= ecl_char_code(#2),(#2))")
          (def-inline si:schar-set :unsafe (ext:extended-string fixnum character) :wchar "(#0)->string.self[#1]= #2"))
        (progn
          (def-inline si:schar-set :unsafe (t t t) t "@2;((#0)->base_string.self[ecl_fixnum(#1)]=ecl_char_code(#2),(#2))")
          (def-inline si:schar-set :unsafe (t fixnum base-char) :unsigned-char "(#0)->base_string.self[#1]= #2")))
    (def-inline si:schar-set :unsafe (base-string t t) t "@2;((#0)->base_string.self[ecl_fixnum(#1)]=ecl_char_code(#2),(#2))")
    (def-inline si:schar-set :unsafe (base-string fixnum base-char) :unsigned-char "(#0)->base_string.self[#1]= #2")

    (def-inline cl:string= :always (string string) :bool "ecl_string_eq(#0,#1)")

    ;; file structure.d

    (def-inline si:structure-name :always (structure-object) symbol "ECL_STRUCT_NAME(#0)")
    (def-inline si:structure-ref :always (t t fixnum) t "ecl_structure_ref(#0,#1,#2)")
    (def-inline si:structure-set :always (t t fixnum t) t "ecl_structure_set(#0,#1,#2,#3)")

    ;; file symbol.d

    (def-inline cl:get :always (t t t) t "ecl_get(#0,#1,#2)")
    (def-inline cl:get :always (t t) t "ecl_get(#0,#1,ECL_NIL)")

    (def-inline cl:symbol-name :always (t) string "ecl_symbol_name(#0)")

    ;; Additions used by the compiler.
    ;; The following functions do not exist. They are always expanded into the
    ;; given C code. References to these functions are generated in the C1 phase.

    (def-inline shift>> :always (fixnum fixnum) :fixnum "((#0) >> (- (#1)))")
    (def-inline shift<< :always (fixnum fixnum) :fixnum "((#0) << (#1))")

    (def-inline si:short-float-p :always (t) :bool "@0;ECL_SINGLE_FLOAT_P(#0)")
    (def-inline si:single-float-p :always (t) :bool "@0;ECL_SINGLE_FLOAT_P(#0)")
    (def-inline si:double-float-p :always (t) :bool "@0;ECL_DOUBLE_FLOAT_P(#0)")
    (def-inline si:long-float-p :always (t) :bool "@0;ECL_LONG_FLOAT_P(#0)")

    (when (member :complex-float *features*)
      (def-inline si::complex-single-float-p :always (t) :bool "@0;ECL_COMPLEX_SINGLE_FLOAT_P(#0)")
      (def-inline si::complex-double-float-p :always (t) :bool "@0;ECL_COMPLEX_DOUBLE_FLOAT_P(#0)")
      (def-inline si::complex-long-float-p :always (t) :bool "@0;ECL_COMPLEX_LONG_FLOAT_P(#0)"))

    (def-inline ext:fixnump :always (t) :bool "ECL_FIXNUMP(#0)")
    (def-inline ext:fixnump :always (fixnum) :bool "1")

    ;; Functions only available with threads
    (when (member :threads *features*)
      (def-inline mp:lock-count :unsafe (mp:lock) fixnum "((#0)->lock.counter)")
      (def-inline mp:compare-and-swap-car :always (cons t t) t "ecl_compare_and_swap(&ECL_CONS_CAR(#0),(#1),(#2))")
      (def-inline mp:atomic-incf-car :always (cons t) t "ecl_atomic_incf(&ECL_CONS_CAR(#0),(#1))")
      (def-inline mp:atomic-incf-car :always (cons fixnum) t "ecl_atomic_incf_by_fixnum(&ECL_CONS_CAR(#0),(#1))")

      (def-inline mp:compare-and-swap-cdr :always (cons t t) t "ecl_compare_and_swap(&ECL_CONS_CDR(#0),(#1),(#2))")
      (def-inline mp:atomic-incf-cdr :always (cons t) t "ecl_atomic_incf(&ECL_CONS_CDR(#0),(#1))")
      (def-inline mp:atomic-incf-cdr :always (cons fixnum) t "ecl_atomic_incf_by_fixnum(&ECL_CONS_CDR(#0),(#1))")

      (def-inline mp:compare-and-swap-symbol-value :unsafe (symbol t t) t "ecl_compare_and_swap(ecl_bds_ref(ecl_process_env(),(#0)),(#1),(#2))")
      (def-inline mp:atomic-incf-symbol-value :always (t fixnum) t "ecl_atomic_incf_by_fixnum(ecl_bds_ref(ecl_process_env(),(#0)),(#1))")
      (def-inline mp:atomic-incf-symbol-value :unsafe (symbol t) t "ecl_atomic_incf(ecl_bds_ref(ecl_process_env(),(#0)),(#1))")
      (def-inline mp:atomic-incf-symbol-value :unsafe (symbol fixnum) t "ecl_atomic_incf_by_fixnum(ecl_bds_ref(ecl_process_env(),(#0)),(#1))")

      (def-inline mp:compare-and-swap-svref :unsafe (t t t t) t "ecl_compare_and_swap((#0)->vector.self.t + ecl_fixnum(#1),(#2),(#3))")
      (def-inline mp:compare-and-swap-svref :unsafe (t fixnum t t) t "ecl_compare_and_swap((#0)->vector.self.t + (#1),(#2),(#3))")

      ;; :threads are implicit
      (when (member :clos *features*)
        (def-inline mp:compare-and-swap-instance :always (t fixnum t t) t "ecl_compare_and_swap_instance((#0),(#1),(#2),(#3))")
        (def-inline mp:compare-and-swap-instance :unsafe (standard-object fixnum t t) t "ecl_compare_and_swap((#0)->instance.slots+(#1),(#2),(#3))")
        (def-inline mp:atomic-incf-instance :always (t fixnum t) t "ecl_atomic_incf_instance((#0),(#1),(#2))")
        (def-inline mp:atomic-incf-instance :unsafe (standard-object fixnum t) t "ecl_atomic_incf((#0)->instance.slots+(#1),(#2))")
        (def-inline mp:atomic-incf-instance :unsafe (standard-object fixnum fixnum) t "ecl_atomic_incf_by_fixnum((#0)->instance.slots+(#1),(#2))"))

      (def-inline mp:compare-and-swap-structure :unsafe (structure-object t fixnum t t) t "ecl_compare_and_swap(&(ECL_STRUCT_SLOT((#0),(#2))),(#3),(#4))"))

    ;; Functions only available with CLOS
    (when (member :clos *features*)
      (def-inline si:instance-ref :always (t fixnum) t "ecl_instance_ref((#0),(#1))")
      (def-inline si:instance-ref :unsafe (standard-object fixnum) t "(#0)->instance.slots[#1]")
      (def-inline si::instance-slotds :unsafe (standard-object) list "(#0)->instance.slotds")

      (def-inline si:instance-set :unsafe (t fixnum t) t "ecl_instance_set((#0),(#1),(#2))")
      (def-inline si:instance-set :unsafe (standard-object fixnum t) t "(#0)->instance.slots[#1]=(#2)")

      (def-inline si:instance-class :always (standard-object) t "ECL_CLASS_OF(#0)")
      (def-inline cl:class-of :unsafe (standard-object) t "ECL_CLASS_OF(#0)")

      (def-inline si:instancep :always (t) :bool "@0;ECL_INSTANCEP(#0)")
      (def-inline si:unbound :always nil t "ECL_UNBOUND")

      (def-inline si:sl-boundp :always (t) :bool "(#0)!=ECL_UNBOUND")

      (def-inline clos:standard-instance-access :always (t fixnum) t "ecl_instance_ref((#0),(#1))")
      (def-inline clos:standard-instance-access :unsafe (standard-object fixnum) t "(#0)->instance.slots[#1]")

      (def-inline clos:funcallable-standard-instance-access :always (t fixnum) t "ecl_instance_ref((#0),(#1))")
      (def-inline clos:funcallable-standard-instance-access :unsafe (clos:funcallable-standard-object fixnum) t "(#0)->instance.slots[#1]"))

    *inline-information*))
