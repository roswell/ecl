
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

;;; VV represents an object referenced in the compiled program. In the first
;;; pass VV instances are added to the vector *REFERENCED-OBJECT*, and in the
;;; second pass the backend decides if the referenced object can be inlined, or
;;; if it needs to be put in the data segment and initialized at load-time.

(defstruct vv
  (location nil)
  (used-p nil)
  (always nil)       ; when true then vv is never optimized
  (permanent-p t)
  (value nil)
  (rep-type :object))

;;; When the value is the "empty location" then it was created to be filled
;;; later and the real type of the object is not known. See DATA-EMPTY-LOC.
(defun vv-type (loc)
  (let ((value (vv-value loc)))
    (if (eq value *empty-loc*)
        t
        (type-of value))))

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
        ((vv-p loc) (vv-rep-type loc))
        ((numberp loc) (lisp-type->rep-type (type-of loc)))
        ((eq loc 'TRASH) :void)
        ((atom loc) :object)
        (t
         (case (first loc)
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
  (when (atom loc)
    (return-from loc-with-side-effects-p
      (and (var-p loc)
           (global-var-p loc)
           (policy-global-var-checking))))
  (case (first loc)
    ((CALL CALL-NORMAL CALL-INDIRECT CALL-STACK) T)
    (CL:THE (loc-with-side-effects-p (third loc)))
    (CL:FDEFINITION (policy-global-function-checking))
    ;; Uses VALUES or has side effects.
    (FFI:C-INLINE (or (eq (sixth loc) 'CL:VALUES) (fifth loc)))
    (otherwise NIL)))

(defun loc-refers-to-special-p (loc)
  (when (atom loc)
    (return-from loc-refers-to-special-p
      (and (var-p loc)
           (member (var-kind loc) '(SPECIAL GLOBAL)))))
  (case (first loc)
    (CL:THE (loc-refers-to-special-p (third loc)))
    (BIND T)
     ;; We do not know, so guess yes.
    (FFI:C-INLINE T)
    (otherwise NIL)))

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
;;;     ( CALL-STACK fun)  similar as CALL-INDIRECT, but args are on the stack
;;;     ( FFI:C-INLINE output-type fun/string locs side-effects output-var )
;;;     ( COERCE-LOC representation-type location)
;;;     ( FDEFINITION vv-index )
;;;     ( MAKE-CCLOSURE cfun )
;;;     ( STACK-POINTER index ) retrieve a value from the stack
;;;     ( SYS:STRUCTURE-REF loc slot-name-vv slot-index )
;;;     ( THE type location )
;;;     ( KEYVARS n )
;;;     VA-ARG
;;;     CL-VA-ARG

;;; Valid *DESTINATION* locations are:
;;;
;;;     var-object                      Variable
;;;     loc-object                      VV Location
;;;     TRASH                           Value may be thrown away.
;;;     RETURN                          Object returned from current function.
;;;     VALUES                          Values vector.
;;;     VALUE0
;;;     ( VALUE i )                     Nth value
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
  (cond ((numberp loc)
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
                 '(VV VV-TEMP KEYVARS))
         t)
        (t
         (baboon :format-control "Unknown location ~A found in C1FORM"
                 :format-arguments (list loc)))))

(defun uses-values (loc)
  (and (consp loc)
       (or (member (car loc) '(CALL CALL-NORMAL CALL-INDIRECT CALL-STACK) :test #'eq)
           (and (eq (car loc) 'ffi:C-INLINE)
                (eq (sixth loc) 'cl:VALUES)))))

(defun loc-immediate-value-p (loc)
  (cond ((numberp loc)
         (values t loc))
        ((vv-p loc)
         (let ((value (vv-value loc)))
           (if (eq value *empty-loc*)
               (values nil nil)
               (values t value))))
        ((atom loc)
         (values nil nil))
        ((eq (first loc) 'THE)
         (loc-immediate-value-p (third loc)))
        (t
         (values nil nil))))

(defun loc-immediate-value (loc)
  (nth-value 1 (loc-immediate-value-p loc)))

(defun unknown-location (where loc)
  (baboon :format-control "Unknown location found in ~A~%~S"
          :format-arguments (list where loc)))
