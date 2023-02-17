
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See file 'LICENSE' for the copyright details.

(in-package #:compiler)

;;; ----------------------------------------------------------------------------
;;; LOCATIONS and representation types
;;;
;;; Locations are lisp expressions which represent actual target (i.e C) data.
;;; To each location we can associate a representation type, which is the type
;;; of the target data (i.e uint32_t).

;;; The following routines help in determining these types, and also in moving
;;; data from one location to another.

(defstruct vv
  (location nil)
  (used-p nil)
  (permanent-p t)
  (value nil))

(defun vv-type (loc)
  (let ((value (vv-value loc)))
    (if (and value (not (ext:fixnump value)))
        (type-of value)
        t)))

(defun loc-movable-p (loc)
  (if (atom loc)
      t
      (case (first loc)
        ((CALL CALL-LOCAL) NIL)
        ((ffi:c-inline) (not (fifth loc))) ; side effects?
        (otherwise t))))

(defun loc-type (loc)
  (cond ((eq loc NIL) 'NULL)
        ((var-p loc) (var-type loc))
        ((vv-p loc) (vv-type loc))
        ((numberp loc) (lisp-type->rep-type (type-of loc)))
        ((atom loc) 'T)
        (t
         (case (first loc)
           (FIXNUM-VALUE 'FIXNUM)
           (CHARACTER-VALUE (type-of (code-char (second loc))))
           (DOUBLE-FLOAT-VALUE 'DOUBLE-FLOAT)
           (SINGLE-FLOAT-VALUE 'SINGLE-FLOAT)
           (LONG-FLOAT-VALUE 'LONG-FLOAT)
           (CSFLOAT-VALUE 'SI:COMPLEX-SINGLE-FLOAT)
           (CDFLOAT-VALUE 'SI:COMPLEX-DOUBLE-FLOAT)
           (CLFLOAT-VALUE 'SI:COMPLEX-LONG-FLOAT)
           (FFI:C-INLINE (let ((type (first (second loc))))
                           (cond ((and (consp type) (eq (first type) 'VALUES)) T)
                                 ((lisp-type-p type) type)
                                 (t (rep-type->lisp-type type)))))
           (BIND (var-type (second loc)))
           (LCL (or (third loc) T))
           (THE (second loc))
           (CALL-NORMAL (fourth loc))
           (otherwise T)))))

(defun loc-representation-type (loc)
  (cond ((member loc '(NIL T)) :object)
        ((var-p loc) (var-rep-type loc))
        ((vv-p loc) :object)
        ((numberp loc) (lisp-type->rep-type (type-of loc)))
        ((eq loc 'TRASH) :void)
        ((atom loc) :object)
        (t
         (case (first loc)
           (FIXNUM-VALUE :fixnum)
           (CHARACTER-VALUE (if (<= (second loc) 255) :unsigned-char :wchar))
           (DOUBLE-FLOAT-VALUE :double)
           (SINGLE-FLOAT-VALUE :float)
           (LONG-FLOAT-VALUE :long-double)
           (CSFLOAT-VALUE :csfloat)
           (CDFLOAT-VALUE :cdfloat)
           (CLFLOAT-VALUE :clfloat)
           (FFI:C-INLINE (let ((type (first (second loc))))
                           (cond ((and (consp type) (eq (first type) 'VALUES)) :object)
                                 ((lisp-type-p type) (lisp-type->rep-type type))
                                 (t type))))
           (BIND (var-rep-type (second loc)))
           (LCL (lisp-type->rep-type (or (third loc) T)))
           ((JUMP-TRUE JUMP-FALSE) :bool)
           (THE (loc-representation-type (third loc)))
           (otherwise :object)))))

(defun loc-with-side-effects-p (loc &aux name)
  (cond ((var-p loc)
         (and (global-var-p loc)
              (policy-global-var-checking)))
        ((atom loc)
         nil)
        ((member (setf name (first loc)) '(CALL CALL-NORMAL CALL-INDIRECT)
                 :test #'eq)
         t)
        ((eq name 'cl:THE)
         (loc-with-side-effects-p (third loc)))
        ((eq name 'cl:FDEFINITION)
         (policy-global-function-checking))
        ((eq name 'ffi:C-INLINE)
         (or (eq (sixth loc) 'cl:VALUES) ;; Uses VALUES
             (fifth loc)))))             ;; or side effects

(defun loc-refers-to-special-p (loc)
  (cond ((var-p loc)
         (member (var-kind loc) '(SPECIAL GLOBAL)))
        ((atom loc)
         nil)
        ((eq (first loc) 'THE)
         (loc-refers-to-special-p (third loc)))
        ((eq (setf loc (first loc)) 'BIND)
         t)
        ((eq loc 'ffi:C-INLINE)
         t)                             ; We do not know, so guess yes
        (t nil)))

;;; Valid locations are:
;;;     NIL
;;;     T
;;;     fixnum
;;;     VALUE0
;;;     VALUES
;;;     var-object
;;;     a string                        designating a C expression
;;;     ( VALUE i )                     VALUES(i)
;;;     ( VV vv-index )
;;;     ( VV-temp vv-index )
;;;     ( LCL lcl [representation-type]) local variable, type unboxed
;;;     ( TEMP temp )                   local variable, type object
;;;     ( FRAME ndx )                   variable in local frame stack
;;;     ( CALL-NORMAL fun locs 1st-type ) similar as CALL, but number of arguments is fixed
;;;     ( CALL-INDIRECT fun narg args)  similar as CALL, but unknown function
;;;     ( FFI:C-INLINE output-type fun/string locs side-effects output-var )
;;;     ( COERCE-LOC representation-type location)
;;;     ( FDEFINITION vv-index )
;;;     ( MAKE-CCLOSURE cfun )
;;;     ( FIXNUM-VALUE fixnum-value )
;;;     ( CHARACTER-VALUE character-code )
;;;     ( LONG-FLOAT-VALUE long-float-value vv )
;;;     ( DOUBLE-FLOAT-VALUE double-float-value vv )
;;;     ( SINGLE-FLOAT-VALUE single-float-value vv )
;;;     ( CSFLOAT-VALUE csfloat-value vv )
;;;     ( CDFLOAT-VALUE cdfloat-value vv )
;;;     ( CLFLOAT-VALUE clfloat-value vv )
;;;     ( STACK-POINTER index ) retrieve a value from the stack
;;;     ( SYS:STRUCTURE-REF loc slot-name-vv slot-index )
;;;     ( THE type location )
;;;     ( KEYVARS n )
;;;     VA-ARG
;;;     CL-VA-ARG

;;; Valid *DESTINATION* locations are:
;;;
;;;     VALUE0
;;;     RETURN                          Object returned from current function.
;;;     TRASH                           Value may be thrown away.
;;;     VALUES                          Values vector.
;;;     var-object
;;;     ( LCL lcl )
;;;     ( LEX lex-address )
;;;     ( BIND var alternative )        Alternative is optional
;;;     ( JUMP-TRUE label )
;;;     ( JUMP-FALSE label )

(defun tmp-destination (loc)
  (case loc
    (VALUES 'VALUES)
    (TRASH 'TRASH)
    (T 'RETURN)))

(defun precise-loc-type (loc new-type)
  (if (subtypep (loc-type loc) new-type)
      loc
      `(the ,new-type ,loc)))

(defun loc-in-c1form-movable-p (loc)
  "A location that is in a C1FORM and can be moved"
  (cond ((member loc '(t nil))
         t)
        ((numberp loc)
         t)
        ((stringp loc)
         t)
        ((vv-p loc)
         t)
        ((member loc '(value0 values va-arg cl-va-arg))
         nil)
        ((atom loc)
         (baboon :format-control "Unknown location ~A found in C1FORM"
                 :format-arguments (list loc)))
        ((eq (first loc) 'THE)
         (loc-in-c1form-movable-p (third loc)))
        ((member (setf loc (car loc))
                 '(VV VV-TEMP FIXNUM-VALUE CHARACTER-VALUE
                   DOUBLE-FLOAT-VALUE SINGLE-FLOAT-VALUE LONG-FLOAT-VALUE
                   #+complex-float CSFLOAT-VALUE
                   #+complex-float CDFLOAT-VALUE
                   #+complex-float CLFLOAT-VALUE
                   KEYVARS))
         t)
        (t
         (baboon :format-control "Unknown location ~A found in C1FORM"
                 :format-arguments (list loc)))))

(defun uses-values (loc)
  (and (consp loc)
       (or (member (car loc) '(CALL CALL-NORMAL CALL-INDIRECT) :test #'eq)
           (and (eq (car loc) 'ffi:C-INLINE)
                (eq (sixth loc) 'cl:VALUES)))))

(defun loc-immediate-value-p (loc)
  (cond ((eq loc t)
         (values t t))
        ((eq loc nil)
         (values t nil))
        ((numberp loc)
         (values t loc))
        ((vv-p loc)
         (let ((value (vv-value loc)))
           (if (or (null value) (ext:fixnump value))
               (values nil nil)
               (values t value))))
        ((atom loc)
         (values nil nil))
        ((eq (first loc) 'THE)
         (loc-immediate-value-p (third loc)))
        ((member (first loc)
                 '(fixnum-value long-float-value
                   double-float-value single-float-value
                   csfloat-value cdfloat-value clfloat-value))
         (values t (second loc)))
        ((eq (first loc) 'character-value)
         (values t (code-char (second loc))))
        (t
         (values nil nil))))

(defun loc-immediate-value (loc)
  (nth-value 1 (loc-immediate-value-p loc)))

(defun unknown-location (where loc)
  (baboon :format-control "Unknown location found in ~A~%~S"
          :format-arguments (list where loc)))
