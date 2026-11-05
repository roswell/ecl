;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author: Daniel Kochmański
;;;; Created: 2026-10-07
;;;; Contains: tests for hashing functions

(in-package :cl-test)

(suite 'sxhash)

(ext:with-clean-symbols (my-struct make-my-struct my-class)
  (defstruct (my-struct (:constructor make-my-struct))
    a (b 0 :type integer))
  (defclass my-class () ())
  (defparameter *sxhash.universe*
    (list
     ;; numbers
     42 (1+ most-positive-fixnum) 1/4 #c(1 4)
     0.25s0 3.12f0 4.12d0 5.13l0 #c(1.0 2.0)
     ;; sequences
     '(1 2 "d") "foo" #(5 2 1) #*1010 (coerce "foo" 'base-string)
     ;; various
     :some-symbol #P"/a/pathname" (make-random-state) (make-hash-table)
     (make-my-struct) (make-instance 'my-class) (find-package :cl-test))))

;;; SXHASH is specified to return a fixnum.
;;; Internal interfaces: SI:HASH-EQL, SI:HASH-EQUAL and SI:HASH-EQUALP
;;; Tangential: hash tables are not required to use any of these functions
(deftest sxhash.smoke ()
  (mapc (lambda (o)
          (is (ext:fixnump (sxhash o)))
          (is (ext:fixnump (si:hash-eql o)))
          (is (ext:fixnump (si:hash-equal o)))
          (is (ext:fixnump (si:hash-equalp o))))
        *sxhash.universe*))

;;; In the next test we will be checking whether float that have an exact
;;; rational representation produce the same hash under EQUALP. Notably:
;;; - dyadic ratio within long float but not within double flaot precision
;;; - dyadic bignum representable as two different float types
;;; - dyadic bignum representable as one float type and not as another float type
;;; Assumption: FLOAT-RADIX is 2.
(defun make-dyadic-ratio (significant-bits &optional (shift 1))
  "Construct a dyadic ratio that requires exactly SIGNIFICANT-BITS as a float."
  (assert (>= significant-bits 1))
  (assert (>= shift 1))
  (/ (1- (expt 2 significant-bits)) (expt 2 shift)))

(defun make-dyadic-integer (significant-bits &optional (shift 0))
  "Construct a dyadic bignum that requires exactly SIGNIFICANT-BITS as a float."
  (assert (>= significant-bits 1))
  (assert (>= shift 0))
  (* (1- (expt 2 significant-bits)) (expt 2 shift)))

;;; Ensure that numbers under #'= have the same hash under EQUALP (not trivial).
;;; While SI:HASH-EQUALP truncates the computed hash to produce a fixnum, in
;;; practice it is enough for us to determine whether ECL screws up numbers.
;;; When comparing with floats we go through ->float->rational to avoid slips.
;;;
;;; Some tests distinguish LONG-FLOAT from DOUBLE-FLOAT and SHORT-FLOAT from
;;; SINGLE-FLOAT. In case these are the same these tests just check identity so
;;; there is no need to create separate paths depending on features.
(defparameter *sxhash.=numbers*
  (let ((mpf most-positive-fixnum)
        (mnf most-negative-fixnum))
    (flet ((f (v) (float v 0.0d0))
           (r (v) (rational (float v 0.0d0))))
      (append
       (list (list 1   1.0s0 1.0f0 1.0d0 1.0l0)
             (list 1/2 0.5s0 0.5f0 0.5d0 0.5l0)
             (list (r mpf)      (f mpf))
             (list (r mnf)      (f mnf))
             (list (r (1+ mpf)) (f (1+ mpf)))
             (list (r (1- mnf)) (f (1- mnf)))
             (list (complex (r (+ mpf 32)) (r (- mpf 32)))
                   (complex (f (+ mpf 32)) (f (- mpf 32))))
             ;; Cherry picked problematic examples (all are cherry picked!)
             (list 42 #c(42.0 0.0))     ; canonicalize cfloat to real under #'=
             (list #c(1 3) #c(1.0 3.0)) ; canonicalize different complex ctypes
             ;; Katzero janai, Zero da!
             (list 0 -0.0s0 +0.0s0 -0.0f0 +0.0f0 -0.0d0 +0.0d0 -0.0l0 +0.0l0)
             ;; 1/4 and 0.25d0 are exact and should conform.
             (list 1/4 0.25d0))
       ;; RATIO is dyadic, does not fit in a double precision but does fit
       ;; in a long double precision (on supported platforms). There are
       ;; two possibilities now:
       ;;
       ;; 1. LONG-FLOAT is hashed after casting to double -- in that case
       ;; both equal ratio and long-float should be cast to double
       ;; 2. LONG-FLOAT is hashed as itself
       ;;
       ;; In either case ratio and long-float should produce the same hash.
       ;;
       ;; NB When RATIO is dyadic but does not correspond to any FLOAT, we
       ;; can hash it as C(h(num),h(den)) or cast to float. Otherwise
       ;; (RATIO not dyadic), we can hash it as C(h(num),h(den)).
       (let ((s-ratio (make-dyadic-ratio (float-digits 1.0s0)))
             (f-ratio (make-dyadic-ratio (float-digits 1.0f0)))
             (d-ratio (make-dyadic-ratio (float-digits 1.0d0)))
             (l-ratio (make-dyadic-ratio (float-digits 1.0l0))))
         (flet ((list-ratio (ratio &rest floats)
                  (list* ratio (mapcar (lambda (x) (float ratio x)) floats))))
           (list (list-ratio s-ratio 1.0s0 1.0f0 1.0d0 1.0l0)
                 (list-ratio f-ratio       1.0f0 1.0d0 1.0l0)
                 (list-ratio d-ratio             1.0d0 1.0l0)
                 (list-ratio l-ratio                   1.0l0))))
       ;; Regarding integers vs floats we can take one of three paths:
       ;;
       ;; 1. worst: floats are cast to integers truncating fractional part
       ;; 2. better: integers (fixnums and bignums) are cast to floats
       ;; 3. best: only integer floats are cast to integers (no truncation)
       ;;
       ;; In either case integer and float should produce the same hash.
       (let ((s-bignum (make-dyadic-integer (float-digits 1.0s0) si:cl-fixnum-bits))
             (f-bignum (make-dyadic-integer (float-digits 1.0f0) si:cl-fixnum-bits))
             (d-bignum (make-dyadic-integer (float-digits 1.0d0) si:cl-fixnum-bits))
             (l-bignum (make-dyadic-integer (float-digits 1.0l0) si:cl-fixnum-bits)))
         (flet ((list-bignum (bignum &rest floats)
                  (list* bignum (mapcar (lambda (x) (float bignum x)) floats))))
           (list (list-bignum s-bignum 1.0s0 1.0f0 1.0d0 1.0l0)
                 (list-bignum f-bignum       1.0f0 1.0d0 1.0l0)
                 (list-bignum d-bignum             1.0d0 1.0l0)
                 (list-bignum l-bignum                   1.0l0))))))))

(deftest sxhash.equalp-numbers ()
  (dolist (numbers *sxhash.=numbers*)
    ;; This is an assertion.
    (is (apply #'= numbers) "Assertion: ~s should be #'=." numbers)
    (let ((results (mapcar #'si:hash-equalp numbers)))
      ;; This is the actual test.
      (is (apply #'= results)
          "Hashing failure: should produce the same hash under EQUALP.
numbers: ~a
types:   ~a
hashes:  ~a"
          numbers
          (mapcar (lambda (n)
                    (typecase n
                      (fixnum 'fixnum)
                      (bignum 'bignum)
                      (otherwise (type-of n))))
                  numbers)
          results))))

(deftest sxhash.equal-strings ()
  (is (= (si:hash-equal "foobar")
         (si:hash-equal (coerce "foobar" 'base-string)))))

(deftest sxhash.equalp-strings ()
  (is (= (si:hash-equalp "foobar")
         (si:hash-equalp "FOOBAR")
         (si:hash-equalp (coerce "foobar" 'base-string))
         (si:hash-equalp (coerce "FOOBAR" 'base-string)))))
