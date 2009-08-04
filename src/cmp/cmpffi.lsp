;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPFFI --  Foreign functions interface.

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

;; ----------------------------------------------------------------------
;; REPRESENTATION TYPES
;;

(defconstant +all-integer-rep-types+
  '(:byte :unsigned-byte :short :unsigned-short :int :unsigned-int
	  :long :unsigned-long :fixnum :cl-index
	  :long-long :unsigned-long-long
	  :int8-t :uint8-t :int16-t :uint16-t :int32-t :uint32-t
	  :int64-t :uint64-t))

(defconstant +all-number-rep-types+
  (append +all-integer-rep-types+ '(:float :double :long-double)))

(defconstant +representation-types+
  '(;; These types can be used by ECL to unbox data
    ;; They are sorted from the most specific, to the least specific one.
    :byte
    #1=((signed-byte 8) "int8_t" "ecl_make_int8_t" "ecl_to_int8_t" "fix")
    :unsigned-byte
    #2=((unsigned-byte 8) "uint8_t" "ecl_make_uint8_t" "ecl_to_uint8_t" "fix")
    :fixnum
    (fixnum "cl_fixnum" "MAKE_FIXNUM" "ecl_to_fixnum" "fix")
    :int
    ((integer #.si:c-int-min #.si:c-int-max) "int"
     "ecl_make_int" "ecl_to_int" "ecl_to_int")
    :unsigned-int
    ((integer 0 #.si:c-uint-max) "unsigned int"
     "ecl_make_uint" "ecl_to_uint" "ecl_to_uint")
    :long
    ((integer #.si:c-long-min #.si:c-long-max) "long"
     "ecl_make_long" "ecl_to_long" "ecl_to_long")
    :unsigned-long
    ((integer 0 #.si:c-ulong-max) "unsigned long"
     "ecl_make_ulong" "ecl_to_ulong" "ecl_to_ulong")
    :cl-index
    ((integer 0 #.most-positive-fixnum) "cl_index"
     "ecl_make_unsigned_integer" "fixnnint" "fix")
    :float
    (single-float "float" "ecl_make_singlefloat" "ecl_to_float" "ecl_to_float")
    :double
    (double-float "double" "ecl_make_doublefloat" "ecl_to_double" "ecl_to_double")
    #+:long-float
    :long-double
    #+:long-float
    (long-float "long double" "ecl_make_longfloat" "ecl_to_long_double"
		"ecl_to_long_double")
    :unsigned-char
    (base-char "char" "CODE_CHAR" "ecl_base_char_code" "CHAR_CODE")
    :char
    (base-char "char" "CODE_CHAR" "ecl_base_char_code" "CHAR_CODE")
    :wchar
    (character "ecl_character" "CODE_CHAR" "ecl_char_code" "CHAR_CODE")
    :object
    (t "cl_object")
    :bool
    (t "bool" "ecl_make_bool" "ecl_to_bool" "ecl_to_bool")
    ;; These types are never selected to unbox data.
    ;; They are here, because we need to know how to print them.
    :void
    (nil "void")
    :pointer-void
    (si::foreign-data "void*" "ecl_make_pointer" "ecl_to_pointer" "ecl_to_pointer")
    :cstring
    (string "char*" "ecl_cstring_to_base_string_or_nil")
    :char*
    (string "char*")
    :int8-t
    #1#
    :uint8-t
    #2#
    #+:uint16-t
    :int16-t
    #+:uint16-t
    ((signed-byte 16) "ecl_int16_t" "ecl_make_int16_t" "ecl_to_int16_t" "fix")
    #+:uint16-t
    :uint16-t
    #+:uint16-t
    ((signed-byte 16) "ecl_uint16_t" "ecl_make_uint16_t" "ecl_to_uint16_t" "fix")
    #+:uint32-t
    :int32-t
    #+:uint32-t
    ((signed-byte 32) "ecl_int32_t" "ecl_make_int32_t" "ecl_to_int32_t" "fix")
    #+:uint32-t
    :uint32-t
    #+:uint32-t
    ((signed-byte 32) "ecl_uint32_t" "ecl_make_uint32_t" "ecl_to_uint32_t" "fix")
    #+:uint64-t
    :int64-t
    #+:uint64-t
    ((signed-byte 64) "ecl_int64_t" "ecl_make_int64_t" "ecl_to_int64_t" "fix")
    #+:uint64-t
    :uint64-t
    #+:uint64-t
    ((signed-byte 64) "ecl_uint64_t" "ecl_make_uint64_t" "ecl_to_uint64_t" "fix")
    :short
    ((integer #.si:c-short-min #.si:c-short-max) "short"
     "ecl_make_short" "ecl_to_short" "fix")
    :unsigned-short
    ((integer 0 #.si:c-ushort-max) "unsigned short"
     "ecl_make_ushort" "ecl_to_ushort" "fix")
    ))


(defun rep-type->lisp-type (rep-type)
  (let ((output (getf +representation-types+ rep-type)))
    (cond (output
           (if (eq rep-type :void) nil
	     (or (first output)
	         (cmperr "Representation type ~S cannot be coerced to lisp"
                         rep-type))))
	  ((lisp-type-p rep-type) rep-type)
	  (t (cmperr "Unknown representation type ~S" rep-type)))))

(defun lisp-type->rep-type (type)
  (cond
    ;; We expect type = NIL when we have no information. Should be fixed. FIXME!
    ((null type)
     :object)
    ((getf +representation-types+ type)
     type)
    (t
     (do ((l +representation-types+ (cddr l)))
	 ((endp l) :object)
       (when (subtypep type (first (second l)))
	 (return-from lisp-type->rep-type (first l)))))))

(defun rep-type-name (type)
  (or (second (getf +representation-types+ type))
      (cmperr "Not a valid type name ~S" type)))

(defun lisp-type-p (type)
  (subtypep type 'T))

(defun wt-to-object-conversion (loc-rep-type loc)
  (when (and (consp loc) (member (first loc) '(single-float-value
					       double-float-value
					       long-float-value)))
    (wt (third loc)) ;; VV index
    (return-from wt-to-object-conversion))
  (let ((x (caddr (getf +representation-types+ loc-rep-type))))
    (unless x
      (cmperr "Cannot coerce C variable of type ~A to lisp object" loc-rep-type))
    (wt x "(" loc ")")))

(defun wt-from-object-conversion (dest-type loc-type rep-type loc)
  (let ((x (cdddr (getf +representation-types+ rep-type))))
    (unless x
      (cmperr "Cannot coerce lisp object to C type ~A" rep-type))
    (wt (if (and (not (policy-check-all-arguments-p))
		 (subtypep loc-type dest-type))
	    (second x)
	  (first x))
	"(" loc ")")))

;; ----------------------------------------------------------------------
;; LOCATIONS and representation types
;;
;; Locations are lisp expressions which represent actual C data. To each
;; location we can associate a representation type, which is the type of
;; the C data. The following routines help in determining these types,
;; and also in moving data from one location to another.

(defun loc-movable-p (loc)
  (if (atom loc)
      t
      (case (first loc)
	((CALL CALL-LOCAL) NIL)
	((C-INLINE) (not (fifth loc))) ; side effects?
	(otherwise t))))

(defun loc-type (loc)
  (cond ((eq loc NIL) 'NULL)
	((var-p loc) (var-type loc))
	((si::fixnump loc) 'fixnum)
	((atom loc) 'T)
	(t
	 (case (first loc)
	   (FIXNUM-VALUE 'FIXNUM)
	   (CHARACTER-VALUE (type-of (code-char (second loc))))
	   (DOUBLE-FLOAT-VALUE 'DOUBLE-FLOAT)
	   (SINGLE-FLOAT-VALUE 'SINGLE-FLOAT)
	   (LONG-FLOAT-VALUE 'LONG-FLOAT)
	   (C-INLINE (let ((type (first (second loc))))
                       (cond ((and (consp type) (eq (first type) 'VALUES)) T)
                             ((lisp-type-p type) type)
                             (t (rep-type->lisp-type type)))))
	   (BIND (var-type (second loc)))
	   (LCL (or (third loc) T))
	   (otherwise T)))))

(defun loc-representation-type (loc)
  (cond ((member loc '(NIL T)) :object)
	((var-p loc) (var-rep-type loc))
	((si::fixnump loc) :fixnum)
        ((eq loc 'TRASH) :void)
	((atom loc) :object)
	(t
	 (case (first loc)
	   (FIXNUM-VALUE :fixnum)
	   (CHARACTER-VALUE (if (<= (second loc) 255) :unsigned-char :wchar))
	   (DOUBLE-FLOAT-VALUE :double)
	   (SINGLE-FLOAT-VALUE :float)
	   (LONG-FLOAT-VALUE :long-double)
	   (C-INLINE (let ((type (first (second loc))))
                       (cond ((and (consp type) (eq (first type) 'VALUES)) :object)
                             ((lisp-type-p type) (lisp-type->rep-type type))
                             (t type))))
	   (BIND (var-rep-type (second loc)))
	   (LCL (lisp-type->rep-type (or (third loc) T)))
	   (otherwise :object)))))

(defun wt-coerce-loc (dest-rep-type loc)
  (setq dest-rep-type (lisp-type->rep-type dest-rep-type))
  ;(print dest-rep-type)
  ;(print loc)
  (let* ((dest-type (rep-type->lisp-type dest-rep-type))
	 (loc-type (loc-type loc))
	 (loc-rep-type (loc-representation-type loc)))
    (labels ((coercion-error ()
	       (cmpwarn "Unable to coerce lisp object from type (~S,~S)~%~
			to C/C++ type (~S,~S)"
                        loc-type loc-rep-type dest-type dest-rep-type))
	     (ensure-valid-object-type (a-lisp-type)
	       (when (subtypep `(AND ,loc-type ,a-lisp-type) NIL)
		 (coercion-error))))
      (when (eq dest-rep-type loc-rep-type)
	(wt loc)
	(return-from wt-coerce-loc))
      (case dest-rep-type
	(#.+all-integer-rep-types+
	 (case loc-rep-type
	   (#.+all-number-rep-types+
	    (wt "((" (rep-type-name dest-rep-type) ")" loc ")"))
	   ((:object)
	    (ensure-valid-object-type dest-type)
	    (wt-from-object-conversion dest-type loc-type dest-rep-type loc))
	   (otherwise
	    (coercion-error))))
	((:char :unsigned-char :wchar)
	 (case loc-rep-type
	   ((:char :unsigned-char :wchar)
	    (wt "((" (rep-type-name dest-rep-type) ")" loc ")"))
	   ((:object)
	    (ensure-valid-object-type dest-type)
	    (wt-from-object-conversion dest-type loc-type dest-rep-type loc))
	   (otherwise
	    (coercion-error))))
	((:float :double :long-double)
	 (case loc-rep-type
	   (#.+all-number-rep-types+
	    (wt "((" (rep-type-name dest-rep-type) ")" loc ")"))
	   ((:object)
	    ;; We relax the check a bit, because it is valid in C to coerce
	    ;; between floats of different types.
	    (ensure-valid-object-type 'FLOAT)
	    (wt-from-object-conversion dest-type loc-type dest-rep-type loc))
	   (otherwise
	    (coercion-error))))
	((:bool)
	 (case loc-rep-type
	   (#.+all-number-rep-types+ ; number type
	    (wt "1"))
	   ((:object)
	    (wt "(" loc ")!=Cnil"))
	   (otherwise
	    (coercion-error))))
	((:object)
	 (wt-to-object-conversion loc-rep-type loc))
	((:pointer-void)
	 (case loc-rep-type
	   ((:object)
	    (wt-from-object-conversion dest-type loc-type dest-rep-type loc))
	   ((:cstring)
	    (wt "(char *)(" loc ")"))
	   (otherwise
	    (coercion-error))))
	((:cstring)
	 (coercion-error))
	((:char*)
	 (case loc-rep-type
	   ((:object)
	    (wt "ecl_base_string_pointer_safe(" loc ")"))
	   ((:pointer-void)
	    (wt "(char *)(" loc ")"))
	   (otherwise
	    (coercion-error))))
	(t
	 (coercion-error))))))

;; ----------------------------------------------------------------------
;; C/C++ DECLARATIONS AND HEADERS
;;

(defun c1clines (args)
  (unless (every #'stringp args)
    (cmperr "The argument to CLINES, ~s, is not a list of strings." args))
  (setf *clines-string-list* (nconc *clines-string-list* (copy-list args)))
  (c1expr '(progn)))

;; ----------------------------------------------------------------------
;; C/C++ INLINE CODE
;;

(defun c1c-inline (args)
  ;; We are on the safe side by assuming that the form has side effects
  (destructuring-bind (arguments arg-types output-type c-expression
				 &rest rest
				 &key (side-effects t) one-liner
				 &aux output-rep-type)
      args
    (unless (= (length arguments) (length arg-types))
      (cmperr "In a C-INLINE form the number of declare arguments and the number of supplied ones do not match:~%~S"
	      `(C-INLINE ,@args)))
    ;; We cannot handle :cstrings as input arguments. :cstrings are
    ;; null-terminated strings, but not all of our lisp strings will
    ;; be null terminated. In particular, those with a fill pointer
    ;; will not.
    (let ((ndx (position :cstring arg-types)))
      (when ndx
	(let* ((var (gensym))
	       (value (elt arguments ndx)))
	  (setf (elt arguments ndx) var
		(elt arg-types ndx) :char*)
	  (return-from c1c-inline
	    (c1expr
	     `(ffi::with-cstring (,var ,value)
	       (c-inline ,arguments ,arg-types ,output-type ,c-expression
		,@rest)))))))
    ;; Find out the output types of the inline form. The syntax is rather relaxed
    ;; 	output-type = lisp-type | c-type | (values {lisp-type | c-type}*)
    (flet ((produce-type-pair (type)
	     (if (lisp-type-p type)
		 (cons type (lisp-type->rep-type type))
		 (cons (rep-type->lisp-type type) type))))
      (cond ((eq output-type ':void)
	     (setf output-rep-type '()
		   output-type 'NIL))
	    ((equal output-type '(VALUES &REST t))
	     (setf output-rep-type '((VALUES &REST t))))
	    ((and (consp output-type) (eql (first output-type) 'VALUES))
	     (setf output-rep-type (mapcar #'cdr (mapcar #'produce-type-pair (rest output-type)))
		   output-type 'T))
	    (t
	     (let ((x (produce-type-pair output-type)))
	       (setf output-type (car x)
		     output-rep-type (list (cdr x)))))))
    (let* ((processed-arguments '()))
      (unless (and (listp arguments)
		   (listp arg-types)
		   (stringp c-expression))
	(cmperr "C-INLINE: wrong type of arguments ~S"
		arguments arg-types c-expression))
      (do ((processed-arguments '())
	   (processed-arg-types '()))
	  ((and (endp arguments) (endp arg-types))
	   (make-c1form* 'C-INLINE :type output-type :args
			 (nreverse processed-arguments)
			 (nreverse processed-arg-types)
			 output-rep-type
			 c-expression
			 side-effects
			 one-liner))
	(push (or (pop arg-types) 'T) processed-arg-types)
	(push (c1expr (pop arguments)) processed-arguments)))))

(defun produce-inline-loc (inlined-arguments arg-types output-rep-type
			   c-expression side-effects one-liner)
  (let* (args-to-be-saved
	 coerced-arguments)
    ;; If the expression begins with @[0-9a-z]*, this means we are
    ;; saving some variables.
    (when (and (> (length c-expression) 1)
	       (eq (char c-expression 0) #\@))
      (do ((ndx 1 (1+ ndx)))
	  ((>= ndx (length c-expression)))
	(let ((c (char c-expression ndx)))
	  (when (eq c #\;)
	    (setf c-expression (subseq c-expression (1+ ndx)))
	    (return))
	  (unless (alphanumericp c)
	    (setf args-to-be-saved nil)
	    (return))
	  (push (- (char-code c) (char-code #\0))
		args-to-be-saved))))

    (setf coerced-arguments (coerce-locs inlined-arguments arg-types args-to-be-saved))
    ;;(setf output-rep-type (lisp-type->rep-type output-rep-type))

    ;; If the form does not output any data, and there are no side
    ;; effects, try to omit it.
    (when (null output-rep-type)
      (if side-effects
	  (progn
	    (wt-nl)
	    (wt-c-inline-loc output-rep-type c-expression coerced-arguments t nil)
	    (when one-liner (wt ";")))
	  (cmpwarn "Ignoring form ~S" c-expression))
      (return-from produce-inline-loc NIL))

    ;; If the form is a one-liner, we can simply propagate this expression until the
    ;; place where the value is used.
    (when one-liner
      (return-from produce-inline-loc
	`(C-INLINE ,output-rep-type ,c-expression ,coerced-arguments ,side-effects
                   ,(if (equalp output-rep-type '((VALUES &REST T)))
                        'VALUES NIL))))

    ;; If the output is a in the VALUES vector, just write down the form and output
    ;; the location of the data.
    (when (equalp output-rep-type '((VALUES &REST T)))
      (wt-c-inline-loc output-rep-type c-expression coerced-arguments side-effects
                       'VALUES)
      (return-from produce-inline-loc 'VALUES))

    ;; Otherwise we have to set up variables for holding the output.
    (flet ((make-output-var (type)
	     (let ((var (make-lcl-var :rep-type type)))
	       (wt-nl (rep-type-name type) " " var ";")
	       var)))
      (incf *inline-blocks*)
      (wt-nl "{")
      (let ((output-vars (mapcar #'make-output-var output-rep-type)))
	(wt-c-inline-loc output-rep-type c-expression coerced-arguments side-effects output-vars)
	(cond ((= (length output-vars) 1)
	       (first output-vars))
	      (t
	       (loop for v in output-vars
		     for i from 0
		     do (let ((*destination* `(VALUE ,i))) (set-loc v)))
	       (wt "cl_env_copy->nvalues=" (length output-vars) ";")
	       'VALUES))))))

(defun c2c-inline (arguments &rest rest)
  (let ((*inline-blocks* 0)
        (*temp* *temp*))
    (unwind-exit (apply #'produce-inline-loc (inline-args arguments) rest))
    (close-inline-blocks)))

(defun coerce-locs (inlined-args &optional types args-to-be-saved)
  (do* ((l inlined-args (cdr l))
	(item (first l) (first l))
	(i 0 (1+ i))
	(block-opened nil))
       ((endp l)
	inlined-args)
    (let* ((type (if types (pop types) :object))
	   (rep-type (lisp-type->rep-type type))
	   (lisp-type (first item))
	   (loc (second item)))
      (cond ((and (not (loc-movable-p loc)) (member i args-to-be-saved))
	     (let ((lcl (make-lcl-var :rep-type rep-type)))
	       (wt-nl)
	       (unless block-opened
		 (incf *inline-blocks*)
		 (wt-nl "{"))
	       (wt (rep-type-name rep-type) " " lcl "= ")
	       (wt-coerce-loc rep-type loc)
	       (wt ";")
	       (setq loc lcl)))
	    ((and (not (equal rep-type (loc-representation-type loc))))
	     (setq loc `(COERCE-LOC ,rep-type ,loc))))
      (setf (first l) loc))))

(defun wt-c-inline-loc (output-rep-type c-expression coerced-arguments side-effects output-vars)
  (with-input-from-string (s c-expression)
    (when (and output-vars (not (eq output-vars 'VALUES)))
      (wt-nl))
    (do ((c (read-char s nil nil)
	    (read-char s nil nil)))
	((null c))
      (case c
	(#\@
	 (let ((object (read s)))
	   (cond ((and (consp object) (equal (first object) 'RETURN))
		  (if (eq output-vars 'VALUES)
		      (cmperr "User @(RETURN ...) in a C-INLINE form with no output values")
		      (let ((ndx (or (second object) 0))
			    (l (length output-vars)))
			(if (< ndx l)
			    (wt (nth ndx output-vars))
			  (cmperr "Used @(RETURN ~D) in a C-INLINE form with ~D output values"
				  ndx l)))))
		 (t
		  (when (and (consp object) (eq (first object) 'QUOTE))
		    (setq object (second object)))
		  (wt (add-object object :permanent t))))))
	(#\#
	 (let* ((k (read-char s))
		(next-char (peek-char nil s nil nil))
		(index (digit-char-p k 36)))
	   (cond ((or (null index) (and next-char (alphanumericp next-char)))
		  (wt #\# k))
		 ((< index (length coerced-arguments))
		  (wt (nth index coerced-arguments)))
		 (t
		  (cmperr "C-INLINE: Variable code exceeds number of arguments")))))
	(otherwise
	 (write-char c *compiler-output1*))))))

(put-sysprop 'FFI:CLINES 'C1SPECIAL #'c1clines)
(put-sysprop 'FFI:C-INLINE 'C1SPECIAL #'c1c-inline)
(put-sysprop 'FFI:C-INLINE 'C2 #'c2c-inline)
(put-sysprop 'FFI:C-INLINE 'WT-LOC #'wt-c-inline-loc)
(put-sysprop 'COERCE-LOC 'WT-LOC #'wt-coerce-loc)
