
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

(defstruct (vv (:constructor %make-vv))
  (location nil)
  (used-p nil)
  (always nil)       ; when true then vv is never optimized
  (permanent-p t)
  (value nil)
  (type nil)
  (host-type :object))

(defun make-vv (&rest args &key location used-p always permanent-p value type host-type)
  (declare (ignore location used-p always permanent-p host-type))
  (unless type
    ;; When the value is the "empty location" then it was created to be filled
    ;; later and the real type of the object is not known. See DATA-EMPTY-LOC.
    (setf type (if (eq value *empty-loc*) t (type-of value))))
  (apply #'%make-vv :type type args))

;;; TODO investigate where raw numbers are stemming from and change them to VV
;;; TODO tighter checks for compound locations (cons)
;;; TODO baboon on locations that are unknown (for now we signal a warning)
;;;
;;; -- jd 2023-12-07

(defun loc-lisp-type (loc)
  (typecase loc
    (var    (var-type loc))
    (vv     (vv-type loc))
    (number (type-of loc))
    (atom
     (case loc
       (FRAME++ 'FIXNUM)
       ((TRASH LEAVE VALUE0 VA-ARG VALUEZ CL-VA-ARG) T)
       (otherwise
        (cmpwarn "LOC-LISP-TYPE: unknown location ~s." loc)
        T)))
    (otherwise
     (case (first loc)
       (FFI:C-INLINE (let ((type (first (second loc))))
                       (cond ((and (consp type) (eq (first type) 'VALUES)) T)
                             ((lisp-type-p type) type)
                             (t (host-type->lisp-type type)))))
       (BIND (var-type (second loc)))
       (LCL (or (third loc) T))
       (THE (second loc))
       (CALL-NORMAL (fourth loc))
       (otherwise T)))))

(defun loc-host-type (loc)
  (typecase loc
    (var    (var-host-type loc))
    (vv     (vv-host-type loc))
    (number (lisp-type->host-type (type-of loc)))
    (atom
     (case loc
       (TRASH :void)
       ((FRAME++ LEAVE VALUE0 VA-ARG VALUEZ CL-VA-ARG) :object)
       (otherwise
        (cmpwarn "LOC-LISP-TYPE: unknown location ~s." loc)
        :object)))
    (otherwise
     (case (first loc)
       (FFI:C-INLINE (let ((type (first (second loc))))
                       (cond ((and (consp type) (eq (first type) 'VALUES)) :object)
                             ((lisp-type-p type) (lisp-type->host-type type))
                             (t type))))
       (BIND (var-host-type (second loc)))
       (LCL (lisp-type->host-type (or (third loc) T)))
       ((JUMP-TRUE JUMP-FALSE) :bool)
       (THE (loc-host-type (third loc)))
       (otherwise :object)))))

(defun loc-movable-p (loc)
  (if (atom loc)
      t
      (case (first loc)
        ((CALL CALL-LOCAL) NIL)
        ((ffi:c-inline) (not (fifth loc))) ; side effects?
        (otherwise t))))

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
  (flet ((special-var-p (loc)
           (and (var-p loc)
                (member (var-kind loc) '(SPECIAL GLOBAL)))))
    (if (atom loc)
        (special-var-p loc)
        (case (first loc)
          (CL:THE (loc-refers-to-special-p (third loc)))
          (BIND (special-var-p (second loc)))
          ;; We do not know, so guess yes.
          (FFI:C-INLINE T)
          (otherwise NIL)))))

;;; Valid locations are:
;;;     VALUE0
;;;     VALUEZ
;;;     var-object
;;;     a string                        designating a C expression
;;;     ( VALUE i )                     VALUES(i)
;;;     ( VV vv-index )
;;;     ( VV-temp vv-index )
;;;     ( LCL lcl [host-type]) local variable, type unboxed
;;;     ( TEMP temp )                   local variable, type object
;;;     ( FRAME ndx )                   variable in local frame stack
;;;     ( CALL-NORMAL fun locs 1st-type ) similar as CALL, but number of arguments is fixed
;;;     ( CALL-INDIRECT fun narg args)  similar as CALL, but unknown function
;;;     ( CALL-STACK fun)  similar as CALL-INDIRECT, but args are on the stack
;;;     ( FFI:C-INLINE output-type fun/string locs side-effects output-var )
;;;     ( COERCE-LOC host-type location)
;;;     ( FDEFINITION vv-index )
;;;     ( MAKE-CCLOSURE cfun )
;;;     ( STACK-POINTER index ) retrieve a value from the stack
;;;     ( SYS:STRUCTURE-REF loc slot-name-vv slot-index )
;;;     ( THE type location )
;;;     ( KEYVARS n )
;;;     VA-ARG
;;;     CL-VA-ARG

(defun precise-loc-lisp-type (loc new-type)
  (let ((loc-type (loc-lisp-type loc)))
    (if (subtypep loc-type new-type)
        loc
        `(the ,(type-and loc-type new-type) ,loc))))

(defun loc-in-c1form-movable-p (loc)
  "A location that is in a C1FORM and can be moved"
  (cond ((numberp loc)
         t)
        ((stringp loc)
         t)
        ((vv-p loc)
         t)
        ((member loc '(VALUE0 VALUEZ VA-ARG CL-VA-ARG))
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
       (or (member (car loc) '(CALL-NORMAL CALL-INDIRECT CALL-STACK) :test #'eq)
           (and (eq (car loc) 'ffi:C-INLINE)
                (eq (sixth loc) 'cl:VALUES)))))

(defun loc-immediate-value-p (loc)
  (cond ((numberp loc)
         (values t loc))
        ((vv-p loc)
         (let ((value (vv-value loc)))
           (cond
             ((eq value *empty-loc*)
              (values nil nil))
             ((eq value *inline-loc*)
              (loc-immediate-value-p (vv-location loc)))
             (t
              (values t value)))))
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
