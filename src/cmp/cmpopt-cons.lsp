;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  CMPOPT-CONS  Optimization of CONS functions
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

(in-package "COMPILER")

;;;
;;; CONS
;;; turn repetitious cons's into a list*
;;;

(define-compiler-macro cons (&whole whole &rest args)
  (labels ((cons-to-lista (x)
             (let ((tem (last x)))
               (if (and (consp tem)
                        (consp (car tem))
                        (eq (caar tem) 'CONS)
                        (eql (length (cdar tem)) 2))
                   (cons-to-lista (append (butlast x) (cdar tem)))
                   x))))
    (let (temp)
      (if (and (eql (length args) 2)
               (not (eq args (setq temp (cons-to-lista args)))))
          (if (equal '(nil) (last temp))
              (cons 'LIST (butlast temp))
              (cons 'LIST* temp))
          whole))))

;;;
;;; RPLACA / RPLACD
;;;

(define-compiler-macro rplaca (&whole whole place value)
  (if (policy-inline-accessors)
      `(ffi:c-inline (,(if (policy-assume-right-type)
                           place
                           `(ext:checked-value cons ,place))
                      ,value)
                     (:object :object) :object
                     "(ECL_CONS_CAR(#0)=#1,#0)" :one-liner t)
      whole))

(define-compiler-macro rplacd (&whole whole place value)
  (if (policy-inline-accessors)
      `(ffi:c-inline (,(if (policy-assume-right-type)
                           place
                           `(ext:checked-value cons ,place))
                      ,value)
                     (:object :object) :object
                     "(ECL_CONS_CDR(#0)=#1,#0)" :one-liner t)
      whole))

;;;
;;; NTH / NTHCDR
;;;

(define-compiler-macro nth (&whole whole n list)
  (case n
    (0 `(car ,list))
    (1 `(cadr ,list))
    (2 `(caddr ,list))
    (3 `(cadddr ,list))
    (4 `(car (cddddr ,list)))
    (5 `(cadr (cddddr ,list)))
    (6 `(caddr (cddddr ,list)))
    (7 `(cadddr (cddddr ,list)))
    (t whole)))

(define-compiler-macro nthcdr (&whole whole n list)
  (case n
    (0 list)
    (1 `(cdr ,list))
    (2 `(cddr ,list))
    (3 `(cdddr ,list))
    (4 `(cddddr ,list))
    (5 `(cdr (cddddr ,list)))
    (6 `(cddr (cddddr ,list)))
    (7 `(cdddr (cddddr ,list)))
    (t whole)))

;;;
;;; FIRST, SECOND, THIRD, ...
;;;

(progn .
  #.(loop for n in '(first second third fourth fifth sixth seventh eighth ninth tenth)
        for i from 0
        collect `(define-compiler-macro ,n (x) (list 'nth ,i x))))

(define-compiler-macro rest (x) `(cdr ,x))

;;;
;;; POP
;;;

(define-compiler-macro pop (&whole whole place &environment env)
  (if (policy-inline-accessors)
      (multiple-value-bind (vars vals stores store-form access-form)
          (get-setf-expansion place env)
        (let* ((store-var (first stores))
               (saved-place (gensym)))
          `(let* ,(mapcar #'list
                          (append vars (list saved-place))
                          (append vals (list access-form)))
             (declare (:read-only ,@vars)) ; Beppe
             (optional-type-check ,saved-place list)
             (when ,saved-place
               (let ((,store-var (si:cons-cdr ,saved-place)))
                 (declare (:read-only ,store-var))
                 ,store-form
                 (setq ,saved-place (si:cons-car ,saved-place))))
             ,saved-place)))
    whole))
