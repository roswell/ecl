;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;  Copyright (c) 2010, Juan Jose Garcia Ripoll
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;; Optimizer for numerical expressions.

(in-package "COMPILER")

;;;
;;; We transform BOOLE into the individual operations, which have inliners
;;;

(define-compiler-macro boole (&whole form op-code op1 op2)
  (or (and (constantp op-code *cmp-env*)
           (case (ext:constant-form-value op-code *cmp-env*)
             (#. boole-clr `(progn (ext:checked-value integer ,op1) (ext:checked-value integer ,op2) 0))
             (#. boole-set `(progn (ext:checked-value integer ,op1) (ext:checked-value integer ,op2) -1))
             (#. boole-1 `(prog1 (ext:checked-value integer ,op1) (ext:checked-value integer ,op2)))
             (#. boole-2 `(progn (ext:checked-value integer ,op1) (ext:checked-value integer ,op2)))
             (#. boole-c1 `(prog1 (lognot ,op1) (ext:checked-value integer ,op2)))
             (#. boole-c2 `(progn (ext:checked-value integer ,op1) (lognot ,op2)))
             (#. boole-and `(logand ,op1 ,op2))
             (#. boole-ior `(logior ,op1 ,op2))
             (#. boole-xor `(logxor ,op1 ,op2))
             (#. boole-eqv `(logeqv ,op1 ,op2))
             (#. boole-nand `(lognand ,op1 ,op2))
             (#. boole-nor `(lognor ,op1 ,op2))
             (#. boole-andc1 `(logandc1 ,op1 ,op2))
             (#. boole-andc2 `(logandc2 ,op1 ,op2))
             (#. boole-orc1 `(logorc1 ,op1 ,op2))
             (#. boole-orc2 `(logorc2 ,op1 ,op2))))
      form))

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
      (ext:with-clean-symbols (%pos %size)
        `(with-let*-type-check ((%size ,(second bytespec) unsigned-byte)
                                (%pos ,(third bytespec) unsigned-byte))
           (logand (lognot (ash -1 %size)) (ash ,integer (- %pos)))))
      whole))

(define-compiler-macro ldb-test (&whole whole bytespec integer)
  (if (inline-bytespec bytespec)
      `(not (zerop (mask-field ,bytespec ,integer)))
      whole))

(define-compiler-macro mask-field (&whole whole bytespec integer)
  (if (inline-bytespec bytespec)
      (ext:with-clean-symbols (%pos %size)
        `(with-let*-type-check ((%size ,(second bytespec) unsigned-byte)
                                (%pos ,(third bytespec) unsigned-byte))
           (logand (ash (lognot (ash -1 %size)) %pos)
                   ,integer)))
      whole))

(define-compiler-macro dpb (&whole whole newbyte bytespec integer)
  (if (inline-bytespec bytespec)
      (ext:with-clean-symbols (%pos %size %mask)
        `(with-let*-type-check ((%size ,(second bytespec) unsigned-byte)
                                (%pos ,(third bytespec) unsigned-byte)
                                (%mask (ash (lognot (ash -1 %size)) %pos) t))
             (logior (logand (ash ,newbyte %pos) %mask)
                     (logandc2 ,integer %mask))))
      whole))

(define-compiler-macro deposit-field (&whole whole newbyte bytespec integer)
  (if (inline-bytespec bytespec)
      (ext:with-clean-symbols (%pos %size %mask)
        `(with-let*-type-check ((%size ,(second bytespec) unsigned-byte)
                                (%pos ,(third bytespec) unsigned-byte)
                                (%mask (ash (lognot (ash -1 %size)) %pos) t))
             (logior (logand ,newbyte %mask)
                     (logandc2 ,integer %mask)
                     )))
      whole))

(define-setf-expander ldb (&environment env bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion int env)
    (if (inline-bytespec bytespec)
        (let* ((bpos (gensym))
               (bsize (gensym))
               (store (gensym))
               (btemp `(byte ,bpos ,bsize))
               (stemp (first stores)))
          (values `(,bpos ,bsize ,@temps)
                  `(,(second bytespec) ,(third bytespec) ,@vals)
                  `(,store)
                  `(let ((,stemp (dpb ,store ,btemp ,access-form)))
                     ,store-form ,store)
                  `(ldb ,btemp ,access-form)))
        (let* ((btemp (gensym))
               (store (gensym))
               (stemp (first stores)))
          (values `(,btemp ,@temps)
                  `(,bytespec ,@vals)
                  `(,store)
                  `(let ((,stemp (dpb ,store ,btemp ,access-form)))
                     ,store-form ,store)
                  `(ldb ,btemp ,access-form))))))

(define-setf-expander mask-field (&environment env bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion int env)
    (if (inline-bytespec bytespec)
        (let* ((bpos (gensym))
               (bsize (gensym))
               (store (gensym))
               (btemp `(byte ,bpos ,bsize))
               (stemp (first stores)))
          (values `(,bpos ,bsize ,@temps)
                  `(,(second bytespec) ,(third bytespec) ,@vals)
                  `(,store)
                  `(let ((,stemp (deposit-field ,store ,btemp ,access-form)))
                     ,store-form ,store)
                  `(mask-field ,btemp ,access-form)))
        (let* ((btemp (gensym))
               (store (gensym))
               (stemp (first stores)))
          (values `(,btemp ,@temps)
                  `(,bytespec ,@vals)
                  `(,store)
                  `(let ((,stemp (deposit-field ,store ,btemp ,access-form)))
                     ,store-form ,store)
                  `(mask-field ,btemp ,access-form))))))

;;;
;;; ASH
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

(defun simplify-arithmetic (operator args whole)
  (if (every #'numberp args)
      (apply operator args)
      (let ((l (length args)))
        (cond ((> l 2)
               (simplify-arithmetic
                operator
                (list* (simplify-arithmetic operator
                                            (list (first args) (second args))
                                            nil)
                       (cddr args))
                nil))
              ((= l 2)
               (or whole (list* operator args)))
              ((= l 1)
               (if (or (eq operator '*) (eq operator '+))
                   (first args)
                   (or whole (list* operator args))))
              ((eq operator '*)
               1)
              ((eq operator '+)
               0)
              (t
               (error 'simple-program-error
                      :format-error "Wrong number of arguments for operator ~a in ~a"
                      :format-arguments (list operator (or whole
                                                           (list* operator args)))))))))

(define-compiler-macro * (&whole all &rest args)
  (simplify-arithmetic '* args all))

(define-compiler-macro + (&whole all &rest args)
  (simplify-arithmetic '+ args all))

(define-compiler-macro / (&whole all &rest args)
  (simplify-arithmetic '/ args all))

(define-compiler-macro - (&whole all &rest args)
  (simplify-arithmetic '- args all))
