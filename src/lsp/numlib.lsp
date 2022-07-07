;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                           number routines

(in-package "SYSTEM")

#-ecl-min
(ffi:clines "#include <math.h>")
#+(and (not ecl-min) complex-float)
(ffi:clines "#include <complex.h>")

#.
(flet ((binary-search (f min max)
         (do ((new (/ (+ min max) 2) (/ (+ min max) 2)))
             ((>= min max)
              max)
           (if (funcall f new)
               (if (= new max)
                   (return max)
                   (setq max new))
               (if (= new min)
                   (return max)
                   (setq min new)))))
       (epsilon+ (x)
         (/= (float 1 x) (+ (float 1 x) x)))
       (epsilon- (x)
         (/= (float 1 x) (- (float 1 x) x))))
  #+ecl-min
  (si::trap-fpe 'last nil)
  `(eval-when (compile load eval)
    (defconstant short-float-epsilon
      ,(binary-search #'epsilon+ (coerce 0 'short-float) (coerce 1 'short-float))
      "The smallest postive short-float E that satisfies
        (not (= (float 1 E) (+ (float 1 E) E)))")
    (defconstant single-float-epsilon
      ,(binary-search #'epsilon+ (coerce 0 'single-float) (coerce 1 'single-float))
      "The smallest postive single-float E that satisfies
        (not (= (float 1 E) (+ (float 1 E) E)))")
    (defconstant double-float-epsilon
      ,(binary-search #'epsilon+ (coerce 0 'double-float) (coerce 1 'double-float))
      "The smallest postive double-float E that satisfies
        (not (= (float 1 E) (+ (float 1 E) E)))")
    (defconstant long-float-epsilon
      ,(binary-search #'epsilon+ (coerce 0 'long-float) (coerce 1 'long-float))
      "The smallest postive long-float E that satisfies
        (not (= (float 1 E) (+ (float 1 E) E)))")
    (defconstant short-float-negative-epsilon
      ,(binary-search #'epsilon- (coerce 0 'short-float) (coerce 1 'short-float))
      "The smallest positive short-float E that satisfies
        (not (= (float 1 E) (- (float 1 E) E)))")
    (defconstant single-float-negative-epsilon
      ,(binary-search #'epsilon- (coerce 0 'single-float) (coerce 1 'single-float))
      "The smallest positive single-float E that satisfies
        (not (= (float 1 E) (- (float 1 E) E)))")
    (defconstant double-float-negative-epsilon
      ,(binary-search #'epsilon- (coerce 0 'double-float) (coerce 1 'double-float))
      "The smallest positive double-float E that satisfies
        (not (= (float 1 E) (- (float 1 E) E)))")
    (defconstant long-float-negative-epsilon
      ,(binary-search #'epsilon- (coerce 0 'long-float) (coerce 1 'long-float))
      "The smallest positive long-float E that satisfies
        (not (= (float 1 E) (- (float 1 E) E)))")
    ))

#+ieee-floating-point
(locally (declare (notinline -))
  (let ((bits (si::trap-fpe 'last nil)))
    (unwind-protect
         (progn
           (let ((a (/ (coerce 1 'short-float) (coerce 0.0 'short-float))))
             (defconstant short-float-positive-infinity a)
             (defconstant short-float-negative-infinity (- a)))
           (let ((a (/ (coerce 1 'single-float) (coerce 0.0 'single-float))))
             (defconstant single-float-positive-infinity a)
             (defconstant single-float-negative-infinity (- a)))
           (let ((a (/ (coerce 1 'double-float) (coerce 0.0 'double-float))))
             (defconstant double-float-positive-infinity a)
             (defconstant double-float-negative-infinity (- a)))
           (let ((a (/ (coerce 1 'long-float) (coerce 0.0 'long-float))))
             (defconstant long-float-positive-infinity a)
             (defconstant long-float-negative-infinity (- a))))
      (si::trap-fpe bits t))))

(defconstant imag-one #C(0.0 1.0))

(defun isqrt (i)
  "Args: (integer)
Returns the integer square root of INTEGER."
       (unless (and (integerp i) (>= i 0))
               (error 'type-error :datum i :expected-type 'unsigned-byte))
       (if (zerop i)
           0
           (let ((n (integer-length i)))
                (do ((x (ash 1 (ceiling n 2)))
                     (y))
                    (nil)
                    (setq y (floor i x))
                    (when (<= x y)
                          (return x))
                    (setq x (floor (+ x y) 2))))))

(defun phase (x)
  "Args: (number)
Returns the angle part (in radians) of the polar representation of NUMBER.
Returns zero for non-complex numbers."
  (if (zerop x)
      (if (eq x 0)
          0.0
          (float 0 (realpart x)))
      (atan (imagpart x) (realpart x))))

(defun signum (x)
  "Args: (number)
Returns a number that represents the sign of NUMBER.  Returns NUMBER If it is
zero.  Otherwise, returns the value of (/ NUMBER (ABS NUMBER))"
  (if (complexp x)
      (if (zerop x)
          x
          (cis (atan (imagpart x) (realpart x))))
      (let ((result (cond ((> x 0) 1)
                          ((< x 0) -1)
                          (t ; x is 0 or NaN
                           x))))
        (if (floatp x)
            (float result x)
            result))))

(defun cis (x)
  "Args: (radians)
Returns a complex number whose realpart and imagpart are the values of (COS
RADIANS) and (SIN RADIANS) respectively."
  (declare (ext:check-arguments-type))
  (exp (* imag-one x)))

#-ecl-min
(eval-when (:compile-toplevel)
  (defmacro c-num-op (name arg restriction &body gencomplex)
    `(progn
       (when (rationalp ,arg)
         (setf ,arg (float ,arg)))
       (typecase ,arg
         (single-float
          (if (or ,restriction #+ieee-floating-point (ext:float-nan-p ,arg))
              (ffi::c-inline (,arg) (:float) :float
                             ,(format nil "~af(#0)" name)
                             :one-liner t)
              #+complex-float
              (ffi::c-inline (,arg) (:float) :csfloat
                             ,(format nil "c~af(#0 + I*0.0f)" name)
                             :one-liner t)
              #-complex-float
              (progn ,@gencomplex)))
         (double-float
          (if (or ,restriction #+ieee-floating-point (ext:float-nan-p ,arg))
              (ffi::c-inline (,arg) (:double) :double
                             ,(format nil "~a(#0)" name)
                             :one-liner t)
              #+complex-float
              (ffi::c-inline (,arg) (:double) :cdfloat
                             ,(format nil "c~a(#0 + I*0.0)" name)
                             :one-liner t)
              #-complex-float
              (progn ,@gencomplex)))
         (long-float
          (if (or ,restriction #+ieee-floating-point (ext:float-nan-p ,arg))
              (ffi::c-inline (,arg) (:long-double) :long-double
                             ,(format nil "~al(#0)" name)
                             :one-liner t)
              #+complex-float
              (ffi::c-inline (,arg) (:long-double) :clfloat
                             ,(format nil "c~al(#0 + I*0.0l)" name)
                             :one-liner t)
              #-complex-float
              (progn ,@gencomplex)))
        #+complex-float
        ((complex single-float)
         (ffi::c-inline (,arg) (:csfloat) :csfloat
                        ,(format nil "c~af(#0)" name)
                        :one-liner t))
        #+complex-float
        ((complex double-float)
         (ffi::c-inline (,arg) (:cdfloat) :cdfloat
                        ,(format nil "c~a(#0)" name)
                        :one-liner t))
        #+complex-float
        ((complex long-float)
         (ffi::c-inline (,arg) (:clfloat) :clfloat
                        ,(format nil "c~al(#0)" name)
                        :one-liner t))
        (complex
         #+complex-float
         (ffi::c-inline ((float (realpart ,arg))
                         (float (imagpart ,arg)))
                        (:float :float) :csfloat
                        ,(format nil "c~af(#0 + I*(#1))" name)
                        :one-liner t)
         #-complex-float
         ,@gencomplex)
        (otherwise
         (error 'type-error :datum ,arg :expected-type 'number))))))

;;; Branch cuts and signed zeros
;;;
;;; The value of the following multi-valued complex functions along
;;; their branch cuts is chosen depending on the sign of zero when
;;; applicable. For example, the imaginary part of (asin (complex x y))
;;; for x real and x>1 is positive for y=+0.0 and negative for
;;; y=-0.0, consistent with approaching the branch cut at y=0 from
;;; above and respectively below.
;;; Note that this differs from the specification in the ANSI
;;; standard, which gives only one value (the ANSI standard is silent
;;; about signed zeros with regards to branch cuts). We take this
;;; approach because it is mathematically more sensible and consistent
;;; with the specification for complex numbers in the C programming
;;; language. -- mg 2022-01-06

(defun asin (x)
  "Args: (number)
Returns the arc sine of NUMBER."
  #+ecl-min
  (complex-asin x)
  #-ecl-min
  (c-num-op "asin" x
      (<= -1.0 x 1.0)
    (complex-asin x)))

;; Ported from CMUCL
#+(or ecl-min (not complex-float))
(defun complex-asin (z)
  (declare (number z)
           (si::c-local))
  (let* ((re-z (realpart z))
         (im-z (imagpart z))
         ;; add real and imaginary parts separately for correct signed
         ;; zero handling around the branch cuts
         (sqrt-1+z (sqrt (complex (+ 1 re-z) im-z)))
         (sqrt-1-z (sqrt (complex (- 1 re-z) (- im-z)))))
    (complex (atan (realpart z) (realpart (* sqrt-1-z sqrt-1+z)))
             (asinh (imagpart (* (conjugate sqrt-1-z) sqrt-1+z))))))

(defun acos (x)
  "Args: (number)
Returns the arc cosine of NUMBER."
  #+ecl-min
  (complex-acos x)
  #-ecl-min
  (c-num-op "acos" x
      (<= -1.0 x 1.0)
    (complex-acos x)))

;; Ported from CMUCL
#+(or ecl-min (not complex-float))
(defun complex-acos (z)
  (declare (number z)
           (si::c-local))
  (let* ((re-z (realpart z))
         (im-z (imagpart z))
         ;; add real and imaginary parts separately for correct signed
         ;; zero handling around the branch cuts
         (sqrt-1+z (sqrt (complex (+ 1 re-z) im-z)))
         (sqrt-1-z (sqrt (complex (- 1 re-z) (- im-z)))))
    (complex (* 2 (atan (realpart sqrt-1-z) (realpart sqrt-1+z)))
             (asinh (imagpart (* (conjugate sqrt-1+z) sqrt-1-z))))))

(defun asinh (x)
  "Args: (number)
Returns the hyperbolic arc sine of NUMBER."
  ;; (log (+ x (sqrt (+ 1.0 (* x x)))))
  #+ecl-min
  (complex-asinh x)
  #-ecl-min
  (c-num-op "asinh" x
      t
    (complex-asinh x)))

;; Ported from CMUCL
#+(or ecl-min (not complex-float))
(defun complex-asinh (z)
  (declare (number z) (si::c-local))
  (let* ((iz (complex (- (imagpart z)) (realpart z)))
         (result (complex-asin iz)))
    (complex (imagpart result)
             (- (realpart result)))))

(defun acosh (x)
  "Args: (number)
Returns the hyperbolic arc cosine of NUMBER."
  ;; (log (+ x (sqrt (* (1- x) (1+ x)))))
  #+ecl-min
  (complex-acosh x)
  #-ecl-min
  (c-num-op "acosh" x
      (<= 1.0 x)
    (complex-acosh x)))

;; Ported from CMUCL
#+(or ecl-min (not complex-float))
(defun complex-acosh (z)
  (declare (number z) (si::c-local))
  (let* ((re-z (realpart z))
         (im-z (imagpart z))
         ;; add real and imaginary parts separately for correct signed
         ;; zero handling around the branch cuts
         (sqrt-z+1 (sqrt (complex (+ re-z 1) im-z)))
         (sqrt-z-1 (sqrt (complex (- re-z 1) im-z))))
    (complex (asinh (realpart (* (conjugate sqrt-z-1)
                                 sqrt-z+1)))
             (* 2 (atan (imagpart sqrt-z-1) (realpart sqrt-z+1))))))

(defun atanh (x)
  "Args: (number)
Returns the hyperbolic arc tangent of NUMBER."
  #+ecl-min
  (complex-atanh x)
  #-ecl-min
  (c-num-op "atanh" x
      (<= -1.0 x 1.0)
    (complex-atanh x)))

;; Ported from CMUCL
#+(or ecl-min (not complex-float))
(defun complex-atanh (z)
  (declare (number z) (si::c-local))
  (let* ((re-z (realpart z))
         (im-z (imagpart z))
         ;; add real and imaginary parts separately for correct signed
         ;; zero handling around the branch cuts
         (log-1+z (log (complex (+ 1 re-z) im-z)))
         (log-1-z (log (complex (- 1 re-z) (- im-z)))))
    (/ (- log-1+z log-1-z) 2)))

(defun ffloor (x &optional (y 1.0f0))
  "Args: (number &optional (divisor 1))
Same as FLOOR, but returns a float as the first value."
  (multiple-value-bind (i r) (floor x y)
    (values (if (floatp r) (float i r) (float i)) r)))

(defun fceiling (x &optional (y 1.0f0))
  "Args: (number &optional (divisor 1))
Same as CEILING, but returns a float as the first value."
  (multiple-value-bind (i r) (ceiling x y)
    (values (if (floatp r) (float i r) (float i)) r)))

(defun ftruncate (x &optional (y 1.0f0))
  "Args: (number &optional (divisor 1))
Same as TRUNCATE, but returns a float as the first value."
  (multiple-value-bind (i r) (truncate x y)
    (values (if (floatp r) (float i r) (float i)) r)))

(defun fround (x &optional (y 1.0f0))
  "Args: (number &optional (divisor 1))
Same as ROUND, but returns a float as the first value."
  (multiple-value-bind (i r) (round x y)
    (values (if (floatp r) (float i r) (float i)) r)))

(defun logtest (x y)
  "Args: (integer1 integer2)
Equivalent to (NOT (ZEROP (LOGAND INTEGER1 INTEGER2)))."
  (not (zerop (logand x y))))


(defun byte (size position)
  "Args: (size position)
Returns a byte specifier of integers.  The value specifies the SIZE-bits byte
starting the least-significant-bit but POSITION bits of integers.  In ECL, a
byte specifier is represented by a dotted pair (SIZE . POSITION)."
  (cons size position))

(defun byte-size (bytespec)
  "Args: (byte)
Returns the size part (in ECL, the car part) of the byte specifier BYTE."
  (car bytespec))

(defun byte-position (bytespec)
  "Args: (byte)
Returns the position part (in ECL, the cdr part) of the byte specifier BYTE."
  (cdr bytespec))

(defun ldb (bytespec integer)
  "Args: (bytespec integer)
Extracts a byte from INTEGER at the specified byte position, right-justifies
the byte, and returns the result as an integer."
  (logand (ash integer (- (byte-position bytespec)))
          (lognot (ash -1 (byte-size bytespec)))))

(defun ldb-test (bytespec integer)
  "Args: (bytespec integer)
Returns T if at least one bit of the specified byte is 1; NIL otherwise."
  (not (zerop (mask-field bytespec integer))))

(defun mask-field (bytespec integer)
  "Args: (bytespec integer)
Extracts the specified byte from INTEGER and returns the result as an integer."
  (logand (ash (lognot (ash -1 (byte-size bytespec)))
               (byte-position bytespec))
          integer))

(defun dpb (newbyte bytespec integer)
  "Args: (newbyte bytespec integer)
Replaces the specified byte of INTEGER with NEWBYTE (an integer) and returns
the result."
  (let* ((pos (byte-position bytespec))
         (size (byte-size bytespec))
         (mask (ash (lognot (ash -1 size)) pos)))
    (logior (logandc2 integer mask)
            (logand (ash newbyte pos) mask))))

(defun deposit-field (newbyte bytespec integer)
  "Args: (integer1 bytespec integer2)
Returns an integer represented by the bit sequence obtained by replacing the
specified bits of INTEGER2 with the specified bits of INTEGER1."
  (let* ((pos (byte-position bytespec))
         (size (byte-size bytespec))
         (mask (ash (lognot (ash -1 size)) pos)))
    (logior (logandc2 integer mask)
            (logand newbyte mask))))

(defun single-float-bits (num)
  (ffi:c-inline (num) (:float) :uint32-t "ecl_float_bits(#0)" :one-liner t))

(defun bits-single-float (num)
  (ffi:c-inline (num) (:uint32-t) :float "ecl_bits_float(#0)" :one-liner t))

(defun double-float-bits (num)
  (ffi:c-inline (num) (:double) :uint64-t "ecl_double_bits(#0)" :one-liner t))

(defun bits-double-float (num)
  (ffi:c-inline (num) (:uint64-t) :double "ecl_bits_double(#0)" :one-liner t))

;;; XXX long double may have 64, 80, 96 or 128 bits (possibly more). The layout
;;; in the memory is also an unknown, so we punt here. -- jd 2022-07-07

(defun long-float-bits (num)
  #+long-float (error "Operation not supported.")
  #-long-float (double-float-bits num))

(defun bits-long-float (num)
  #+long-float (error "Operation not supported.")
  #-long-float (bits-double-float num))
