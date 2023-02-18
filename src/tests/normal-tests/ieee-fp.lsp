;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Daniel Kochmański
;;;; Created:  2016-08-12
;;;; Contains: IEEE floating point tests
;;;;

(in-package :cl-test)

(suite 'ieee-fp)

(defmacro without-fpe-traps (&body body)
  `(let ((bits (si:trap-fpe 'cl:last t)))
     (unwind-protect
          (progn
            (si:trap-fpe t nil)
            ,@body)
       (si:trap-fpe bits t))))

(defconstant +number-subtypes-and-values+
  '((fixnum 3)
    (bignum #.(- most-negative-fixnum 13))
    (ratio 3/5)
    (single-float 1.23f0)
    (double-float 4.56d0)
    (long-float 7.89l0)
    ((complex fixnum) #C(2 3))
    ((complex integer) #.(complex 2 (+ most-positive-fixnum 3)))
    ((complex rational) #C(5/7 -4))
    ((complex single-float) #C(0.98f0 77.7f0))
    ((complex double-float) #C(0.87d0 88.8d0))
    ((complex long-float) #C(0.76l0 99.9l0))))

(defmacro for-all-number-subtypes ((variable &optional (type number) default-value) &body body)
  (when (not (subtypep type 'number))
    (error "~a must be a subtype of number" type))
  `(progn ,@(loop :for (subtype value) :in +number-subtypes-and-values+
               :if (subtypep subtype type)
               :collect `(let ((,variable
                                ,(if default-value
                                     `(coerce ,default-value ',subtype)
                                     value)))
                           ,@body))))

(defmacro for-all-NaNs (variable &body body)
  `(loop :for ,variable :in *ieee-fp.nan* :do
      ,@body))

(defmacro for-all-infinities (minus-infinity-var plus-infinity-var &body body)
  `(loop :for ,minus-infinity-var :in (remove-if-not #'minusp *ieee-fp.inf*) :do
      (loop :for ,plus-infinity-var :in (remove-if-not #'plusp *ieee-fp.inf*) :do
         ,@body)))

(defun type-contagion (&rest numbers)
  (labels ((type-of-proper (number)
             (etypecase number
               (single-float 'single-float)
               (double-float 'double-float)
               (long-float 'long-float)
               (ratio 'ratio)
               (integer 'integer)
               (complex `(complex ,(type-of-proper (realpart number))))))
           (type-contagion1 (t1 &optional t2)
             (cond ((not t2) t1)
                   ((subtypep t1 'float)
                    (cond ((subtypep t2 'float)
                           (cond ((or (eql t1 'long-float)
                                      (eql t2 'long-float))
                                  'long-float)
                                 ((or (eql t1 'double-float)
                                      (eql t2 'double-float))
                                  'double-float)
                                 ((or (eql t1 'single-float)
                                      (eql t2 'single-float))
                                  'single-float)))
                          ((subtypep t2 'rational)
                           t1)
                          ((subtypep t2 'complex)
                           `(complex ,(type-contagion1 t1 (second t2))))))
                   ((subtypep t1 'ratio)
                    (cond ((subtypep t2 'float) t2)
                          ((subtypep t2 'rational) 'ratio)
                          ((subtypep t2 'complex)
                           `(complex ,(type-contagion1 t1 (second t2))))))
                   ((subtypep t1 'integer)
                    (cond ((or (subtypep t2 'float) (subtypep t2 'ratio)) t2)
                          ((subtypep t2 'integer) 'integer)
                          ((subtypep t2 'complex)
                           `(complex ,(type-contagion1 t1 (second t2))))))
                   ((subtypep t1 'complex)
                    (cond ((or (subtypep t2 'float) (subtypep t2 'rational))
                           `(complex ,(type-contagion (second t1) t2)))
                          ((subtypep t2 'complex)
                           `(complex ,(type-contagion1 (second t1) (second t2)))))))))
    (reduce #'type-contagion1 (mapcar #'type-of-proper numbers))))

(defmacro type-and-value-check ((function &rest args) &rest values)
  "Arguments ((function &rest args) &rest values)
Check that function applied to args returns numbers that are
mathematically equal to values and have the correct type according to
Common Lisp type contagion rules."
  (if (eql function 'funcall)
      (setf function (first args)
            args (rest args))
      (setf function `(quote ,function)))
  `(is (every #'eql
              (multiple-value-list (funcall ,function ,@args))
              (mapcar #'(lambda (v)
                          (coerce v (type-contagion ,@args)))
                      (list ,@values)))))

;;; Constructing complex floats with infinities is difficult, because
;;; any finite real number x plus infinite imaginary number leads to
;;; an complex number with NaN real part due to a very literal
;;; application of calculation rules:
;;; x + I*∞ = x + (0 + I*1)*∞ = x + NaN + I*∞ = NaN + I*∞ 
;;; Therefore, when comparing the output of numeric functions
;;; returning complex infinities to expected results, we use the
;;; following function
(defmacro complex-equality (equality-comparison form realpart imagpart)
  `(progn
     (is (funcall ,equality-comparison (realpart ,form) ,realpart))
     (is (funcall ,equality-comparison (imagpart ,form) ,imagpart))))



(test ieee-fp.0001.infinity-eql
  (without-fpe-traps
    (let ((sfni ext:single-float-negative-infinity)
          (sfpi ext:single-float-positive-infinity)
          (dfni ext:double-float-negative-infinity)
          (dfpi ext:double-float-positive-infinity))
      (is (eql sfni (- sfpi)))
      (is (eql dfni (- dfpi)))
      (is (not (eql sfni (- dfpi))))
      (is (= sfni (- dfpi))))))

(test ieee-fp.0002.printing
  (let ((nums (list ext:single-float-negative-infinity
                    ext:single-float-positive-infinity
                    ext:double-float-negative-infinity
                    ext:double-float-positive-infinity
                    (si:nan)))
        (*standard-output* (make-string-output-stream)))
    (dolist (i nums)
      (finishes
        (let ((*print-readably* t)
              (*read-eval* t))
          (print nums)))
      (finishes
        (let ((*print-readably* nil)
              (*read-eval* nil))
          (print nums)))
      (signals print-not-readable
        (let ((*print-readably* t)
              (*read-eval* nil))
          (print nums))))))

(test ieee-fp.0003.predicates
  (is (ext:float-infinity-p ext:single-float-negative-infinity))
  (is (ext:float-infinity-p ext:single-float-positive-infinity))
  (is (ext:float-nan-p (si:nan))))

(test ieee-fp.0004.nan-comparison
      (for-all-number-subtypes (x real)
        (for-all-NaNs NaN
          (is (not (< NaN x)))
          (is (not (< x NaN)))
          (is (not (<= NaN x)))
          (is (not (<= x NaN)))
          (is (not (> NaN x)))
          (is (not (> x NaN)))
          (is (not (>= NaN x)))
          (is (not (>= x NaN))))))

(test ieee-fp.0005.infinity-comparison
      (for-all-number-subtypes (x real)
        (for-all-infinities -infinity +infinity
          (progn (is (< -infinity x))
                 (is (not (< x -infinity)))
                 (is (< x +infinity))
                 (is (not (< +infinity x))))
          (progn (is (<= -infinity x))
                 (is (not (<= x -infinity)))
                 (is (<= x +infinity))
                 (is (not (<= +infinity x))))
          (progn (is (not (> -infinity x)))
                 (is (> x -infinity))
                 (is (not (> x +infinity)))
                 (is (> +infinity x)))
          (progn (is (not (>= -infinity x)))
                 (is (>= x -infinity))
                 (is (not (>= x +infinity)))
                 (is (>= +infinity x))))))



;;; We treat NaNs as missing data (like fmin/fmax C functions)
(test ieee-fp.0006.NaN-min/max
      (for-all-NaNs NaN
        (is (eql (min NaN) NaN))
        (is (eql (max NaN) NaN))
        (is (eql (min NaN NaN) NaN))
        (is (eql (max NaN NaN) NaN))
        (for-all-number-subtypes (x real)
          (let* ((y (* 2 x))
                 (min (min x y))
                 (max (max x y)))
            (is (= (min x NaN) x))
            (is (= (max x NaN) x))
            (is (= (min x y NaN) min))
            (is (= (max x y NaN) max))))))

(test ieee-fp.0007.infinity-min/max
      (for-all-number-subtypes (x real)
        (let* ((y (* 2 x))
               (max (max x y))
               (min (min x y)))
          (for-all-infinities -infinity +infinity
            (is (= (min -infinity x y) -infinity))
            (is (= (min +infinity x y) min))
            (is (= (max -infinity x y) max))
            (is (= (max +infinity x y) +infinity))))))

(test ieee-fp.0008.NaN-minusp/zerop/plusp
      (without-fpe-traps
        (for-all-NaNs NaN
          (is (not (minusp NaN)))
          (is (not (zerop NaN)))
          (is (not (plusp NaN))))))

(test ieee-fp.0009.infinity-minusp/zerop/plusp
      (for-all-infinities -infinity +infinity
        (is (minusp -infinity))
        (is (not (minusp +infinity)))
        (is (not (zerop -infinity)))
        (is (not (zerop +infinity)))
        (is (not (plusp -infinity)))
        (is (plusp +infinity))))



;;; If we supported NaN/infinity for rounding functions operating
;;; on floats, sensible behaviour would be as follows ...

#|
(test ieee-fp.0010.NaN-ffloor/fceiling/ftruncate/fround
      (without-fpe-traps
          (loop :for function :in '(ffloor fceiling ftruncate fround)
             :do
             (for-all-number-subtypes (x real)
               (for-all-NaNs NaN
                 (type-and-value-check (funcall function NaN) NaN NaN)
                 (type-and-value-check (funcall function x NaN) NaN NaN)
                 (type-and-value-check (funcall function NaN x) NaN NaN)
                 (type-and-value-check (funcall function NaN NaN) NaN NaN)))
             (for-all-number-subtypes (x float 0)
               (type-and-value-check (funcall function x x) (ext:nan) (ext:nan))))))

(test ieee-fp.0011.infinity-ffloor/fceiling/ftruncate/fround
      (without-fpe-traps
        (loop :for function :in '(ffloor fceiling ftruncate fround)
           :do
           (for-all-infinities -infinity +infinity
             ;; +-infinity and 1
             (type-and-value-check (funcall function -infinity)
                                   -infinity (ext:nan))
             (type-and-value-check (funcall function +infinity)
                                   +infinity (ext:nan))
             (for-all-number-subtypes (x real)
               ;; +-infinity and finite real
               (type-and-value-check (funcall function -infinity (abs x))
                                     -infinity (ext:nan))
               (type-and-value-check (funcall function -infinity (- (abs x)))
                                     +infinity (ext:nan))
               (type-and-value-check (funcall function +infinity (abs x))
                                     +infinity (ext:nan))
               (type-and-value-check (funcall function +infinity (- (abs x)))
                                     -infinity (ext:nan))
               ;; finite real and +-infinity
               (type-and-value-check (funcall function (abs x) -infinity)
                                     (case function
                                       (ffloor -1)
                                       (t 0))
                                     (case function
                                       (ffloor -infinity)
                                       (t x)))
               (type-and-value-check (funcall function (- (abs x)) -infinity)
                                     (case function
                                       (fceiling 1)
                                       (t 0))
                                     (case function
                                       (fceiling +infinity)
                                       (t x)))
               (type-and-value-check (funcall function (abs x) +infinity)
                                     (case function
                                       (fceiling 1)
                                       (t 0))
                                     (case function
                                       (fceiling -infinity)
                                       (t x)))
               (type-and-value-check (funcall function (- (abs x)) +infinity)
                                     (case function
                                       (ffloor -1)
                                       (t 0))
                                     (case function
                                       (ffloor +infinity)
                                       (t x))))
             ;; +-infinity and +-infinity
             (type-and-value-check (funcall function -infinity -infinity)
                                   (ext:nan) (ext:nan))
             (type-and-value-check (funcall function -infinity +infinity)
                                   (ext:nan) (ext:nan))
             (type-and-value-check (funcall function +infinity -infinity)
                                   (ext:nan) (ext:nan))
             (type-and-value-check (funcall function +infinity +infinity)
                                   (ext:nan) (ext:nan))
             (for-all-number-subtypes (x float 0)
               ;; finite real and 0
               (for-all-number-subtypes (y real)
                 (type-and-value-check (funcall function (abs y) x)
                                       +infinity (ext:nan))
                 (type-and-value-check (funcall function (- (abs y)) x)
                                       -infinity (ext:nan)))
               ;; +-infinity and 0
               (type-and-value-check (funcall function -infinity 0)
                                     -infinity (ext:nan))
               (type-and-value-check (funcall function +infinity 0)
                                     +infinity (ext:nan)))))))
|#

;;; ... but we don't, therefore everything throws arithmetic errors.

#+floating-point-exceptions
(test ieee-fp.0010.NaN-floor/ceiling/truncate/round/mod/rem
      (loop :for function :in '(floor ceiling truncate round
                                ffloor fceiling ftruncate fround
                                mod rem)
         :do
         (for-all-NaNs NaN
           (unless (member function '(mod rem))
             (signals arithmetic-error (funcall function NaN)))
           (signals arithmetic-error (funcall function NaN NaN))
           (for-all-number-subtypes (x real)
             (signals arithmetic-error (funcall function x NaN))
             (signals arithmetic-error (funcall function NaN x))))
         (for-all-number-subtypes (x float 0)
           (signals arithmetic-error (funcall function x x)))))

#+floating-point-exceptions
(test ieee-fp.0011.infinity-floor/ceiling/truncate/round
      (loop :for function :in '(floor ceiling truncate round
                                ffloor fceiling ftruncate fround
                                mod rem)
         :do
         (for-all-infinities -infinity +infinity
           ;; +-infinity and 1
           (unless (member function '(mod rem))
             (signals arithmetic-error (funcall function -infinity))
             (signals arithmetic-error (funcall function +infinity)))
           (for-all-number-subtypes (x real)
             ;; +-infinity and finite real
             (signals arithmetic-error (funcall function -infinity x))
             (signals arithmetic-error (funcall function +infinity x))
             ;; finite real and +-infinity
             (signals arithmetic-error (funcall function x -infinity))
             (signals arithmetic-error (funcall function x +infinity)))
           ;; +-infinity and +-infinity
           (signals arithmetic-error (funcall function -infinity -infinity))
           (signals arithmetic-error (funcall function -infinity +infinity))
           (signals arithmetic-error (funcall function +infinity -infinity))
           (signals arithmetic-error (funcall function +infinity +infinity))
           (for-all-number-subtypes (x float 0)
             ;; finite real and 0
             (for-all-number-subtypes (y real)
               (signals arithmetic-error (funcall function y x)))
             ;; +-infinity and 0
             (signals arithmetic-error (funcall function -infinity x))
             (signals arithmetic-error (funcall function +infinity x))))))



;;; All numeric functions should return a NaN if given one
(test ieee-fp.0012.sticky-NaN
      (without-fpe-traps
        (for-all-NaNs NaN
          (macrolet ((f-NaN (&rest functions)
                       `(progn ,@(loop :for f :in functions :collect
                                      `(is (eql (,f NaN) NaN))))))
            (f-NaN sin cos tan
                   asin acos atan
                   sinh cosh tanh
                   asinh acosh atanh
                   * + - / 1+ 1-
                   abs signum exp log sqrt
                   conjugate phase))
          (for-all-number-subtypes (x real)
            (macrolet ((f-NaN (&rest functions)
                         `(progn ,@(loop :for f :in functions :append
                                        `((type-and-value-check (,f x NaN) NaN)
                                          (type-and-value-check (,f NaN x) NaN)
                                          (type-and-value-check (,f NaN NaN) NaN))))))
              (f-NaN atan * + - /))
            (macrolet ((f-NaN (&rest functions)
                         `(progn ,@(loop :for f :in functions :append
                                        `((type-and-value-check (,f (abs x) NaN) NaN)
                                          (type-and-value-check (,f NaN (abs x)) NaN)
                                          (type-and-value-check (,f NaN NaN) NaN))))))
              (f-NaN expt log)))
          (is (eql (cis NaN)
                   (complex (coerce (ext:nan) (type-of NaN))
                            (coerce (ext:nan) (type-of NaN))))))))



(test ieee-fp.0013.infinity-sin/cos/tan
      (without-fpe-traps
        (for-all-infinities -infinity +infinity
          (type-and-value-check (sin -infinity) (ext:nan))
          (type-and-value-check (sin +infinity) (ext:nan))
          #|(type-and-value-check (sin (complex 0 -infinity)) (complex 0 -infinity))
          (type-and-value-check (sin (complex 0 +infinity)) (complex 0 +infinity))|#
          (type-and-value-check (cos -infinity) (ext:nan))
          (type-and-value-check (cos +infinity) (ext:nan))
          #|(type-and-value-check (cos (complex 0 -infinity)) +infinity)
          (type-and-value-check (cos (complex 0 +infinity)) +infinity)|#
          (type-and-value-check (tan -infinity) (ext:nan))
          (type-and-value-check (tan +infinity) (ext:nan))
          #|(type-and-value-check (tan (complex 0 -infinity)) (complex 0 -1))
          (type-and-value-check (tan (complex 0 +infinity)) (complex 0 +1))|#)))

#|(test ieee-fp.0014.infinity-asin/acos/atan
      (for-all-infinities -infinity +infinity
        (complex-equality #'approx= (asin -infinity) (- (/ pi 2)) +infinity)
        (complex-equality #'approx= (asin +infinity) (/ pi 2) +infinity)
        (complex-equality #'approx= (acos -infinity) pi -infinity)
        (complex-equality #'approx= (acos +infinity) 0 -infinity)
        (is (approx= (atan -infinity) (- (/ pi 2))))
        (is (approx= (atan +infinity) (/ pi 2)))))|#


;;; Reported by: Raymond Toy
;;; Test corner-cases of calculating atan2
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/issues/332

(flet((+pi-p (elt) (= (coerce pi (type-of elt)) elt))
      (-pi-p (elt) (= (coerce pi (type-of elt)) (- elt)))
      (+pi/2-p (elt) (approx= (coerce (/ pi +2) (type-of elt)) elt))
      (-pi/2-p (elt) (approx= (coerce (/ pi -2) (type-of elt)) elt))
      (+zerop (elt) (and (zerop elt) (plusp (float-sign elt))))
      (-zerop (elt) (and (zerop elt) (minusp (float-sign elt)))))

  (test ieee-fp.0015.atan2-special-case.zero-arg
        (+zerop (atan +0.0 +0.0))
        (-zerop (atan -0.0 +0.0))
        (+pi-p  (atan +0.0 -0.0))
        (-pi-p  (atan -0.0 -0.0))
        (without-fpe-traps
          (map () (lambda (n)
                    ;; (atan +-0 +(anything-but/nan))  -> +-0
                    (is (+zerop (atan +0.0 n)))
                    (is (-zerop (atan -0.0 n)))
                    ;; (atan +-0 -(anything-but/nan))  -> +-pi
                    (is (+pi-p (atan +0.0 (- n))))
                    (is (-pi-p (atan -0.0 (- n))))
                    ;; (atan +-(anything-but/0+nan) 0) -> +-pi/2
                    (is (+pi/2-p (atan n +0.0)))
                    (is (+pi/2-p (atan n -0.0)))
                    (is (-pi/2-p (atan (- n) +0.0)))
                    (is (-pi/2-p (atan (- n) -0.0))))
               (remove-if-not #'plusp (append *floats* *ieee-fp.inf*)))))

  (test ieee-fp.0016.atan2-special-case.inf-arg
        ;; (atan +-inf +inf) -> +-pi/4
        (let ((+inf ext:single-float-positive-infinity)
              (-inf ext:single-float-negative-infinity))
          (is (approx= (atan +inf +inf) (* +1/4 pi)))
          (is (approx= (atan -inf +inf) (* -1/4 pi)))
          (is (approx= (atan +inf -inf) (* +3/4 pi)))
          (is (approx= (atan -inf -inf) (* -3/4 pi)))
          (map () (lambda (n)
                    (is (+zerop (atan n +inf)))
                    (is (-zerop (atan (- n) +inf)))
                    (is (+pi-p (atan n -inf)))
                    (is (-pi-p (atan (- n) -inf)))
                    (unless (zerop n)
                      (+pi/2-p (atan +inf n))
                      (-pi/2-p (atan -inf n))))
               (remove-if-not #'plusp *floats*)))))

(test ieee-fp.0017.infinity-sinh/cosh/tanh
      (for-all-infinities -infinity +infinity
        (type-and-value-check (sinh -infinity) -infinity)
        (type-and-value-check (sinh +infinity) +infinity)
        (type-and-value-check (cosh -infinity) +infinity)
        (type-and-value-check (cosh +infinity) +infinity)
        (type-and-value-check (tanh -infinity) -1)
        (type-and-value-check (tanh +infinity) +1)
        #|(without-fpe-traps
          (type-and-value-check (sinh (complex 0 -infinity)) (complex (ext:nan) (ext:nan)))
          (type-and-value-check (sinh (complex 0 +infinity)) (complex (ext:nan) (ext:nan)))
          (type-and-value-check (cosh (complex 0 -infinity)) (complex (ext:nan) (ext:nan)))
          (type-and-value-check (cosh (complex 0 +infinity)) (complex (ext:nan) (ext:nan)))
          (type-and-value-check (tanh (complex 0 -infinity)) (complex (ext:nan) (ext:nan)))
          (type-and-value-check (tanh (complex 0 +infinity)) (complex (ext:nan) (ext:nan))))|#))

(test ieee-fp.0018.infinity-asinh/acosh/atanh
      (for-all-infinities -infinity +infinity
        (type-and-value-check (asinh -infinity) -infinity)
        (type-and-value-check (asinh +infinity) +infinity)
        #| (without-fpe-traps
          (complex-equality #'approx= (acosh -infinity) +infinity pi))|#
        (type-and-value-check (acosh +infinity) +infinity)
        #| (complex-equality #'approx= (atanh -infinity) 0 (/ pi 2))
        (complex-equality #'approx= (atanh +infinity) 0 (/ pi 2))|#))

(test ieee-fp.0019.infinity-+/-
      (for-all-infinities -infinity +infinity
        (type-and-value-check (+ -infinity) -infinity)
        (type-and-value-check (+ +infinity) +infinity)

        (type-and-value-check (+ -infinity -infinity) -infinity)
        (without-fpe-traps
          (type-and-value-check (+ -infinity +infinity) (ext:nan))
          (type-and-value-check (+ +infinity -infinity) (ext:nan)))
        (type-and-value-check (+ +infinity +infinity) +infinity)

        (type-and-value-check (- -infinity) +infinity)
        (type-and-value-check (- +infinity) -infinity)

        (without-fpe-traps
          (type-and-value-check (- -infinity -infinity) (ext:nan)))
        (type-and-value-check (- +infinity -infinity) +infinity)
        (type-and-value-check (- -infinity +infinity) -infinity)
        (without-fpe-traps
          (type-and-value-check (- +infinity +infinity) (ext:nan)))
        (for-all-number-subtypes (x real)
          (type-and-value-check (+ x -infinity) -infinity)
          (type-and-value-check (+ x +infinity) +infinity)
          (type-and-value-check (+ -infinity x) -infinity)
          (type-and-value-check (+ +infinity x) +infinity)

          (type-and-value-check (- x -infinity) +infinity)
          (type-and-value-check (- x +infinity) -infinity)
          (type-and-value-check (- -infinity x) -infinity)
          (type-and-value-check (- +infinity x) +infinity))))

(test ieee-fp.0020.infinity-*//
      (for-all-infinities -infinity +infinity
        (type-and-value-check (* -infinity) -infinity)
        (type-and-value-check (* +infinity) +infinity)

        (type-and-value-check (* -infinity -infinity) +infinity)
        (type-and-value-check (* -infinity +infinity) -infinity)
        (type-and-value-check (* +infinity -infinity) -infinity)
        (type-and-value-check (* +infinity +infinity) +infinity)

        (is (eql (/ -infinity) (coerce -0.0 (type-of -infinity))))
        (is (eql (/ +infinity) (coerce +0.0 (type-of +infinity))))

        (without-fpe-traps
          (type-and-value-check (/ -infinity -infinity) (ext:nan))
          (type-and-value-check (/ -infinity +infinity) (ext:nan))
          (type-and-value-check (/ +infinity -infinity) (ext:nan))
          (type-and-value-check (/ +infinity +infinity) (ext:nan)))
        (for-all-number-subtypes (x real)
          (type-and-value-check (* (abs x) -infinity) -infinity)
          (type-and-value-check (* (- (abs x)) -infinity) +infinity)
          (type-and-value-check (* (abs x) +infinity) +infinity)
          (type-and-value-check (* (- (abs x)) +infinity) -infinity)

          (type-and-value-check (* -infinity (abs x)) -infinity)
          (type-and-value-check (* -infinity (- (abs x))) +infinity)
          (type-and-value-check (* +infinity (abs x)) +infinity)
          (type-and-value-check (* +infinity (- (abs x))) -infinity)

          (is (eql (/ (abs x) -infinity) (coerce -0.0 (type-contagion (abs x) -infinity))))
          (is (eql (/ (- (abs x)) -infinity) (coerce +0.0 (type-contagion (- (abs x)) -infinity))))
          (is (eql (/ (abs x) +infinity) (coerce +0.0 (type-contagion (abs x) +infinity))))
          (is (eql (/ (- (abs x)) +infinity) (coerce -0.0 (type-contagion (- (abs x)) +infinity))))

          (type-and-value-check (/ -infinity (abs x)) -infinity)
          (type-and-value-check (/ -infinity (- (abs x))) +infinity)
          (type-and-value-check (/ +infinity (abs x)) +infinity)
          (type-and-value-check (/ +infinity (- (abs x))) -infinity))))

(test ieee-fp.0021.infinity-1+/1-
      (for-all-infinities -infinity +infinity
        (type-and-value-check (1- -infinity) -infinity)
        (type-and-value-check (1- +infinity) +infinity)
        (type-and-value-check (1+ -infinity) -infinity)
        (type-and-value-check (1+ +infinity) +infinity)))

(test ieee-fp.0022.infinity-abs
      (for-all-infinities -infinity +infinity
        (type-and-value-check (abs -infinity) +infinity)
        (type-and-value-check (abs +infinity) +infinity)))

(test ieee-fp.0023.infinity-exp
      (for-all-infinities -infinity +infinity
        (type-and-value-check (exp -infinity) 0)
        (type-and-value-check (exp +infinity) +infinity)))

(test ieee-fp.0024.infinity-expt
      (without-fpe-traps
        (flet ((relaxed-eql (x y)
                 ;; (relaxed-eql +0.0 -0.0) => true
                 (and (eql (type-of x) (type-of y))
                      (= x y))))
          (for-all-infinities -infinity +infinity
            ;; x^∞: |x| < 1
            (for-all-number-subtypes (x (and real (not integer)) 1/2)
              (type-and-value-check (expt x -infinity) +infinity)
              #| (complex-equality #'eql (expt (- x) -infinity)
              (coerce +infinity (type-contagion x -infinity)) ;
              (coerce (ext:nan) (type-contagion x -infinity))) |#
              (type-and-value-check (expt x +infinity) 0)
              #| (complex-equality #'relaxed-eql (expt (- x) +infinity)
              (coerce 0.0 (type-contagion x +infinity)) ;
              (coerce 0.0 (type-contagion x +infinity))) |#)
            ;; x^∞: |x| = 1
            (for-all-number-subtypes (x (or fixnum float) 1)
              (type-and-value-check (expt x -infinity) 1)
              (without-fpe-traps
                (complex-equality #'eql (expt (- x) -infinity)
                                  (coerce (ext:nan) (type-contagion x -infinity))
                                  (coerce (ext:nan) (type-contagion x -infinity))))
              (type-and-value-check (expt x +infinity) 1)
              (without-fpe-traps
                (complex-equality #'eql (expt (- x) +infinity)
                                  (coerce (ext:nan) (type-contagion x +infinity))
                                  (coerce (ext:nan) (type-contagion x +infinity)))))
            ;; x^∞: |x| > 1
            (for-all-number-subtypes (x (or fixnum float) 2)
              (type-and-value-check (expt x -infinity) 0)
              #| (complex-equality #'relaxed-eql (expt (- x) -infinity)
              (coerce 0.0 (type-contagion x -infinity)) ;
              (coerce 0.0 (type-contagion x -infinity))) |#
              (type-and-value-check (expt x +infinity) +infinity)
              #| (complex-equality #'eql (expt (- x) +infinity)
              (coerce +infinity (type-contagion x +infinity)) ;
              (coerce (ext:nan) (type-contagion x +infinity))) |#)
            ;; ∞^y: even y
            (for-all-number-subtypes (y float 2)
              (is (eql (realpart (expt -infinity y))
                       (coerce +infinity (type-contagion -infinity y))))
              (complex-equality #'relaxed-eql (expt -infinity (- y))
                                (coerce 0.0 (type-contagion -infinity y))
                                (coerce 0.0 (type-contagion -infinity y)))
              (type-and-value-check (expt +infinity y) +infinity)
              (type-and-value-check (expt +infinity (- y)) 0.0))
            (for-all-number-subtypes (y fixnum 2)
              (type-and-value-check (expt -infinity y) +infinity)
              (type-and-value-check (expt -infinity (- y)) 0.0)
              (type-and-value-check (expt +infinity y) +infinity)
              (type-and-value-check (expt +infinity (- y)) 0.0))
            ;; ∞^y: odd y
            (for-all-number-subtypes (y float 3)
              ;; Should be -infinity, but C99 complex functions return +infinity 
              #| (is (eql (realpart (expt -infinity y))
              (coerce -infinity (type-contagion -infinity y)))) |#
              (complex-equality #'relaxed-eql (expt -infinity (- y))
                                (coerce 0.0 (type-contagion -infinity y))
                                (coerce 0.0 (type-contagion -infinity y)))
              (type-and-value-check (expt +infinity y) +infinity)
              (type-and-value-check (expt +infinity (- y)) 0.0))
            (for-all-number-subtypes (y fixnum 3)
              (type-and-value-check (expt -infinity y) -infinity)
              (complex-equality #'relaxed-eql (expt -infinity (- y))
                                (coerce 0.0 (type-contagion -infinity y))
                                (coerce 0.0 (type-contagion -infinity y)))
              (type-and-value-check (expt +infinity y) +infinity)
              (type-and-value-check (expt +infinity (- y)) 0.0)) 
            ;; ∞^y: non-integer y
            (for-all-number-subtypes (y (and real (not integer)) 5/2)
              (complex-equality #'relaxed-eql (expt -infinity (- y))
                                (coerce 0.0 (type-contagion -infinity y))
                                (coerce 0.0 (type-contagion -infinity y)))
              (type-and-value-check (expt +infinity y) +infinity)
              (type-and-value-check (expt +infinity (- y)) 0.0))
            ;; ∞^∞
            #| (complex-equality #'eql (expt -infinity -infinity)
            (coerce 0.0 (type-contagion -infinity -infinity)) ;
            (coerce -0.0 (type-contagion -infinity -infinity))) |#
            #| (complex-equality #'eql (expt -infinity +infinity)
            (coerce +infinity (type-contagion -infinity +infinity)) ;
            (coerce (ext:nan) (type-contagion -infinity +infinity))) |#
            (type-and-value-check (expt +infinity -infinity) 0.0)
            (type-and-value-check (expt +infinity +infinity) +infinity)))))

(test ieee-fp.0025.infinity-log
      (for-all-infinities -infinity +infinity
        (complex-equality #'approx= (log -infinity) +infinity pi)
        (type-and-value-check (log +infinity) +infinity)))

(test ieee-fp.0026.infinity-signum
      (for-all-infinities -infinity +infinity
        (type-and-value-check (signum -infinity) -1)
        (type-and-value-check (signum +infinity) +1)))

(test ieee-fp.0027.infinity-sqrt
      (for-all-infinities -infinity +infinity
        (without-fpe-traps
          (is (= (imagpart (sqrt -infinity)) +infinity)))
        (type-and-value-check (sqrt +infinity) +infinity)))

(test ieee-fp.0028.infinity-cis
      (without-fpe-traps
        (for-all-infinities -infinity +infinity
          (is (eql (cis -infinity)
                   (complex (coerce (ext:nan) (type-of -infinity))
                            (coerce (ext:nan) (type-of -infinity)))))
          (is (eql (cis +infinity)
                   (complex (coerce (ext:nan) (type-of +infinity))
                            (coerce (ext:nan) (type-of +infinity))))))))

(test ieee-fp.0029.infinity-conjugate
      (for-all-infinities -infinity +infinity
        (type-and-value-check (conjugate -infinity) -infinity)
        (type-and-value-check (conjugate +infinity) +infinity)
        #| (for-all-number-subtypes (x real)
          (type-and-value-check (conjugate (complex x -infinity)) (complex x +infinity))
          (type-and-value-check (conjugate (complex x +infinity)) (complex x -infinity))) |#))

(test ieee-fp.0030.infinity-phase
      (for-all-infinities -infinity +infinity
        (is (approx= (phase -infinity) (- pi)))
        (is (approx= (phase +infinity) 0))
        #| (is (approx= (phase (complex 0 -infinity)) (- (/ pi 2))))
        (is (approx= (phase (complex 0 +infinity)) (+ (/ pi 2))))
        (is (approx= (phase (complex -infinity -infinity)) (- (* pi 3/4))))
        (is (approx= (phase (complex -infinity +infinity)) (+ (* pi 3/4))))
        (is (approx= (phase (complex +infinity -infinity)) (- (* pi 1/4))))
        (is (approx= (phase (complex +infinity +infinity)) (+ (* pi 1/4)))) |#))



(test ieee-fp.0031.branch-cuts-signed-zero
  (for-all-number-subtypes (x float (+ 1.0 (random 10.0)))
    ;; branch cuts in [1,infinity)
    (let ((z-above (complex x +0.0))
          (z-below (complex x -0.0)))
      (is (plusp (imagpart (asin z-above))))
      (is (minusp (imagpart (asin z-below))))
      (is (minusp (imagpart (acos z-above))))
      (is (plusp (imagpart (acos z-below))))
      (is (plusp (imagpart (atanh z-above))))
      (is (minusp (imagpart (atanh z-below)))))
    ;; branch cuts in (-infinity,-1]
    (let ((z-above (complex (- x) +0.0))
          (z-below (complex (- x) -0.0)))
      (is (plusp (imagpart (asin z-above))))
      (is (minusp (imagpart (asin z-below))))
      (is (minusp (imagpart (acos z-above))))
      (is (plusp (imagpart (acos z-below))))
      (is (plusp (imagpart (atanh z-above))))
      (is (minusp (imagpart (atanh z-below)))))
    ;; branch cuts in [i,i*infinity)
    (let ((z-left (complex -0.0 x))
          (z-right (complex +0.0 x)))
      (is (minusp (realpart (atan z-left))))
      (is (plusp (realpart (atan z-right))))
      (is (minusp (realpart (asinh z-left))))
      (is (plusp (realpart (asinh z-right)))))
    ;; branch cuts in (-i*infinity,-i]
    (let ((z-left (complex -0.0 (- x)))
          (z-right (complex +0.0 (- x))))
      (is (minusp (realpart (atan z-left))))
      (is (plusp (realpart (atan z-right))))
      (is (minusp (realpart (asinh z-left))))
      (is (plusp (realpart (asinh z-right))))))
  (for-all-number-subtypes (x float (- 1.0 (random 10.0)))
    ;; branch cuts in (-infinity,1]
    (let ((z-above (complex x +0.0))
          (z-below (complex x -0.0)))
      (is (plusp (imagpart (acosh z-above))))
      (is (minusp (imagpart (acosh z-below))))))
  (for-all-number-subtypes (x float (- (random 10.0)))
    ;; branch cuts in (-infinity,0]
    (let ((z-above (complex x +0.0))
          (z-below (complex x -0.0)))
      (is (plusp (imagpart (sqrt z-above))))
      (is (minusp (imagpart (sqrt z-below)))))))

(test ieee-fp.0032.bit-conversion/smoke
  (is (= 3.14 (si:bits-single-float (si:single-float-bits 3.14))))
  (is (= 3.14 (si:bits-double-float (si:double-float-bits 3.14))))
  #-long-float
  (is (= 3.14 (si:bits-long-float (si:long-float-bits 3.14))))
  #+long-float
  (progn (signals error (si:long-float-bits 3.14))
         (signals error (si:bits-long-float 3.14))))

(test ieee-fp.0033.trap-fpe-smoke-test
      (let ((bits (si:trap-fpe 'cl:last t)))
        (unwind-protect
             (dolist (flag '(t nil))
               (finishes (si:trap-fpe t flag))
               (finishes (si:trap-fpe bits flag))
               (finishes (si:trap-fpe 'last flag))
               (loop for sym in '(division-by-zero
                                  floating-point-overflow
                                  floating-point-underflow
                                  floating-point-invalid-operation
                                  floating-point-inexact)
                     do (finishes (si:trap-fpe sym flag) "~s should be a valid EXT:FPE-TRAP condition." sym))
               (loop for sym in '(:last
                                  :division-by-zero
                                  :floating-point-overflow
                                  :floating-point-underflow
                                  :floating-point-invalid-operation
                                  :floating-point-inexact)
                     do (signals error (si:trap-fpe sym flag) "~s should be an invalid EXT:FPE-TRAP condition." sym)))
          (si:trap-fpe bits t))))

(test ieee-fp.0034.decode-float
      (labels ((test-float (proto num res exp sign)
                 (equal (multiple-value-list (decode-float (float num proto)))
                        (list (float res proto) exp (float sign proto))))
               (test-float* (num res exp sign)
                 (is (test-float 1.0f0 num res exp sign))
                 (is (test-float 1.0d0 num res exp sign))
                 (is (test-float 1.0l0 num res exp sign))))
        (test-float* -10.0 0.625 4 -1.0)
        (test-float*  -1.0 0.5   1 -1.0)
        (test-float*  -0.0 0.0   0 -1.0)
        (test-float*  +0.0 0.0   0 +1.0)
        (test-float*  +1.0 0.5   1 +1.0)
        (test-float* +10.0 0.625 4 +1.0)))
