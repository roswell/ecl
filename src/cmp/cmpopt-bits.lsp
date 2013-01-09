;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;;  CMPOPT-BITS  -- Optimize operations acting on bits
;;;;

(in-package "COMPILER")

;;;
;;; LDB
;;; Look for inline expansion of LDB1 in sysfun.lsp
;;;

(defun inline-bytespec (bytespec)
  (declare (si::c-local))
  (and (consp bytespec)
       (eq 'BYTE (car bytespec))
       (= (length bytespec) 3)
       (policy-inline-bit-operations)))

(define-compiler-macro ldb (&whole whole bytespec integer)
  (if (inline-bytespec bytespec)
      (let ((size (second bytespec))
	    (pos (third bytespec)))
	(cond ((and (integerp size)
		    (integerp pos)
		    (<= (+ size pos) #.(integer-length most-positive-fixnum))
		    (policy-assume-right-type)
		    (subtypep (result-type integer) 'FIXNUM))
	       `(truly-the fixnum (ldb1 ,size ,pos ,integer)))
	      ((or (policy-assume-right-type)
		   (typep pos 'unsigned-byte))
	       `(logand (lognot (ash -1 ,size)) (ash ,integer (- ,pos))))
	      (t
	       (with-clean-symbols (%pos)
		 `(let ((%pos (optional-type-assertion ,pos unsigned-byte)))	     
		    (logand (lognot (ash -1 ,size))
			    (ash ,integer (- %pos))))))))
      whole))

(define-compiler-macro ldb-test (&whole whole bytespec integer)
  (if (inline-bytespec bytespec)
      `(not (zerop (mask-field ,bytespec ,integer)))
      whole))

#+(or)
(define-compiler-macro mask-field (&whole bytespec integer)
  (if (inline-bytespec bytespec)
      (let ((size (second bytespec))
	    (pos (third bytespec)))
	`(logand (ash (lognot (ash -1 ,size))
		      (optional-type-assertion ,pos unsigned-byte))
		 ,integer))
      whole))

#+(or)
(define-compiler-macro dpb (&whole bytespec integer newbyte)
  (if (inline-bytespec bytespec)
      (let ((size (second bytespec))
	    (pos (third bytespec)))
	`(logand (ash (lognot (ash -1 ,size))
		      (optional-type-assertion ,pos unsigned-byte))
		 ,integer))
      whole))

;;;
;;; ASH
;;; Bit fiddling. It is a bit tricky because C does not allow
;;; shifts in << or >> which exceed the integer size. In those
;;; cases the compiler may do whatever it wants (and gcc does!)
;;;

(define-compiler-macro ash (&whole whole argument shift)
  (cond ((and (integerp argument)
	      (integerp shift))
	 (ash argument shift))
	((and (policy-assume-right-type)
	      (integerp shift))
	 (if (zerop shift)
	     argument
	     `(shift ,argument ,shift)))
	(t
	 whole)))

(define-c-inliner shift (return-type argument orig-shift)
  (let* ((arg-type (inlined-arg-type argument))
         (arg-c-type (lisp-type->rep-type arg-type))
	 (return-c-type (lisp-type->rep-type return-type))
         (shift (loc-immediate-value (inlined-arg-loc orig-shift))))
    (if (or (not (c-integer-rep-type-p arg-c-type))
            (not (c-integer-rep-type-p return-c-type)))
        (produce-inline-loc (list argument orig-shift) '(:object :fixnum) '(:object)
                            "ecl_ash(#0,#1)" nil t)
        (let* ((arg-bits (c-integer-rep-type-bits arg-c-type))
	       (return-bits (c-integer-rep-type-bits return-c-type))
	       (max-type (if (and (plusp shift)
				  (< arg-bits return-bits))
			   return-c-type
			   arg-c-type)))
	  (produce-inline-loc (list argument) (list max-type) (list return-type)
			      (format nil
				      (if (minusp shift)
					  "((#0) >> (~D))"
					  "((#0) << (~D))")
				      (abs shift))
			      nil t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TYPE PROPAGATION
;;;

(def-type-propagator logand (fname &rest args)
  (values args
	  (if args
	      (dolist (int-type '((UNSIGNED-BYTE 8) FIXNUM) 'integer)
		(when (loop for value in args
			 always (subtypep value int-type))
		  (return int-type)))
	      'fixnum)))
