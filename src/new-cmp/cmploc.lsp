;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
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

;;; Valid locations are:
;;;	NIL
;;;	T
;;;	fixnum
;;;	VALUE0
;;;	VALUES
;;;	var-object
;;;	( VALUE i )			VALUES(i)
;;;	( VV vv-index )
;;;	( VV-temp vv-index )
;;;	( LCL lcl [representation-type]) local variable, type unboxed
;;;	( TEMP temp )			local variable, type object
;;;	( CALL c-fun-name args fname )	locs are locations containing the arguments
;;;	( CALL-NORMAL fun locs)		similar as CALL, but number of arguments is fixed
;;;	( CALL-INDIRECT fun narg args)	similar as CALL, but unknown function
;;;	( C-INLINE output-type fun/string locs side-effects output-var )
;;;	( COERCE-LOC representation-type location)
;;;	( CAR lcl )
;;;	( CDR lcl )
;;;	( CADR lcl )
;;;	( FDEFINITION vv-index )
;;;	( MAKE-CCLOSURE cfun )
;;;	( FIXNUM-VALUE fixnum-value )
;;;	( CHARACTER-VALUE character-code )
;;;	( LONG-FLOAT-VALUE long-float-value vv )
;;;	( DOUBLE-FLOAT-VALUE double-float-value vv )
;;;	( SINGLE-FLOAT-VALUE single-float-value vv )
;;;	( STACK-POINTER index )	retrieve a value from the stack
;;;	( SYS:STRUCTURE-REF loc slot-name-vv slot-index )
;;;	( KEYVARS n )
;;;	( THE type loc )
;;;	VA-ARG
;;;	CL-VA-ARG

;;; Valid *DESTINATION* locations are:
;;;
;;;	VALUE0
;;;	RETURN				Object returned from current function.
;;;	TRASH				Value may be thrown away.
;;;	VALUES				Values vector.
;;;	var-object
;;;	( LCL lcl )
;;;	( LEX lex-address )
;;;	( BIND var alternative )	Alternative is optional
;;;	( JUMP-TRUE label )
;;;	( JUMP-FALSE label )

(defun tmp-destination (loc)
  (case loc
    (VALUES 'VALUES)
    (TRASH 'TRASH)
    (T 'RETURN)))

(defun loc-has-side-effects (loc)
  (if (atom loc)
      ;; Needed to produce calls to ecl_symbol_value() for global
      ;; variables. These calls can detect whether a variable is
      ;; boudp and since the error is a side effect, they can not
      ;; be suppressed.
      (and (global-var-p loc)
           (policy-global-var-checking))
      (case (first loc)
        ((CALL CALL-NORMAL CALL-INDIRECT) T)
        (FFI:C-INLINE (fifth loc))
        (ACTUAL-RETURN T)
        (FDEFINITION (policy-global-function-checking))
        (otherwise nil))))

;;; ------------------------------------------------------------------
;;; WRITING C/C++ REPRESENTATIONS LOCATIONS
;;;

(defun wt-loc (loc &aux fd)
  (cond ((consp loc)
         (unless (setq fd (gethash (first loc) +c2-wt-loc-table+))
           (error "Unknown or invalid location ~A" loc))
         (apply fd (rest loc)))
        ((var-p loc)
         (wt-var loc))
        ((setq fd (gethash loc +c2-wt-loc-table+))
         (funcall fd))
        (t
         (error "Unknown or invalid location ~A" loc))))

(defun wt-nil-loc () (wt "Cnil"))

(defun wt-t-loc () (wt "Ct"))

(defun wt-value0-loc () (wt "value0"))

(defun wt-values-loc () (wt "cl_env_copy->values[0]"))

(defun wt-va-arg-loc () (wt "(narg--,va_arg(args,cl_object))"))

(defun wt-cl-va-arg-loc () (wt "(narg--,cl_va_arg(cl_args))"))

(defun last-call-p ()
  (member *exit*
          '(RETURN RETURN-FIXNUM RETURN-CHARACTER RETURN-SINGLE-FLOAT
            RETURN-DOUBLE-FLOAT RETURN-LONG-FLOAT RETURN-OBJECT)))

(defun wt-car (loc) (wt "CAR(" loc ")"))

(defun wt-cdr (loc) (wt "CDR(" loc ")"))

(defun wt-cadr (loc) (wt "CADR(" loc ")"))

(defun lcl-name (lcl)
  (if (minusp lcl)
      (format nil "X~D" lcl)
      (format nil "V~D" lcl)))

(defun wt-lcl (lcl)
  (cond ((not (numberp lcl)) (baboon))
        ((minusp lcl) (wt "X" (- lcl)))
        (t (wt "V" lcl))))

(defun wt-vv (vv &optional value)
  (if (numberp vv)
    (wt "VV[" vv "]")
    (wt vv)))

(defun wt-vv-temp (vv &optional value)
  (if (numberp vv)
    (wt "VVtemp[" vv "]")
    (wt vv)))

(defun wt-lcl-loc (lcl &optional type)
  (wt-lcl lcl))

(defun wt-temp (temp)
  (wt "T" temp))

(defun wt-number (value &optional vv)
  (wt value))

(defun wt-character (value &optional vv)
  (wt (format nil "'\\~O'" value)))

(defun wt-value (i) (wt "cl_env_copy->values[" i "]"))

(defun wt-keyvars (i) (wt "keyvars[" i "]"))

(defun loc-refers-to-special (loc)
  (cond ((var-p loc)
	 (member (var-kind loc) '(SPECIAL GLOBAL)))
	((atom loc)
	 nil)
	((eq (setf loc (first loc)) 'BIND)
	 t)
	((eq loc 'C-INLINE)
	 t) ; We do not know, so guess yes
	(t nil)))

(defun values-loc (n)
  (list 'VALUE n))

(defun wt-the-loc (type loc)
  (wt-loc loc))

;;; ------------------------------------------------------------------
;;; ASSIGNING TO LOCATIONS
;;;

(defun uses-values (loc)
  (and (consp loc)
       (or (member (car loc) '(CALL CALL-NORMAL CALL-INDIRECT) :test #'eq)
           (and (eq (car loc) 'C-INLINE)
                (eq (sixth loc) 'VALUES)))))

(defun set-loc (loc destination)
  (unless (eql destination loc)
    (cond ((var-p destination)
           (set-var loc destination))
          ((atom destination)
           (let ((fd (gethash destination +c2-set-loc-table+)))
             (cond (fd
                    (funcall fd loc))
                   ((setq fd (gethash destination +c2-wt-loc-table+))
                    (wt-nl) (funcall fd) (wt "= ")
                    (wt-coerce-loc (loc-representation-type destination) loc)
                    (wt ";"))
                   (t
                    (error "No known way to assign to location ~A"
                           destination)))))
          (t
           (let* ((name (first destination))
                  (fd (gethash name +c2-set-loc-table+)))
             (cond (fd
                    (apply fd loc (rest destination)))
                   ((setq fd (gethash name +c2-wt-loc-table+))
                    (wt-nl) (apply fd (rest destination)) (wt "= ")
                    (wt-coerce-loc (loc-representation-type destination) loc)
                    (wt ";"))
                   (t
                    (error "No known way to assign to location ~A"
                           destination))))))))

(defun set-values-loc (loc)
  (cond ((eq loc 'VALUES))
        ((uses-values loc)
         (wt-nl "cl_env_copy->values[0]=") (wt-coerce-loc :object loc) (wt ";"))
        (t
         (wt-nl "cl_env_copy->values[0]=") (wt-coerce-loc :object loc)
         (wt ";")
         (wt-nl "cl_env_copy->nvalues=1;"))))

(defun set-values+value0-loc (loc)
  (cond ((eq loc 'VALUES)
         (wt-nl "value0=cl_env_copy->values[0];"))
        ((uses-values loc)
         (wt-nl "value0=")(wt-coerce-loc :object loc) (wt ";"))
        (t
         (wt-nl "value0=") (wt-coerce-loc :object loc) (wt ";")
         (wt-nl "cl_env_copy->nvalues=1;"))))

(defun set-value0-loc (loc)
  (wt-nl "value0=") (wt-coerce-loc :object loc) (wt ";"))

(defun set-return-loc (loc)
  (set-values+value0-loc loc))

(defun set-actual-return-loc (loc)
  (set-loc loc 'VALUES+VALUE0)
  (wt-nl "return value0;"))

(defun set-trash-loc (loc)
  (when (loc-has-side-effects loc)
    (wt-nl "(void)(" loc ");")))

(defun set-the-loc (value type loc)
  (set-loc value loc))
