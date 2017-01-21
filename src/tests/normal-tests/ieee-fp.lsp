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

;;; Reported by: Robert Dodier
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/issues/299
(test ieee-fp.0003.b299
  (finishes (< ext:double-float-negative-infinity 1/3))
  (finishes (> ext:double-float-negative-infinity 1/3))
  (finishes (< 1/3 ext:double-float-negative-infinity))
  (finishes (> 1/3 ext:double-float-negative-infinity))
  (finishes (< ext:double-float-negative-infinity (1+ most-positive-fixnum)))
  (finishes (> ext:double-float-negative-infinity (1+ most-positive-fixnum)))
  (finishes (< (1+ most-positive-fixnum) ext:double-float-negative-infinity))
  (finishes (> (1+ most-positive-fixnum) ext:double-float-negative-infinity)))


;;; Reported by: Raymond Toy
;;; Test corner-cases of calculating atan2
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/issues/332

(flet((+pi-p (elt) (= (coerce pi (type-of elt)) elt))
      (-pi-p (elt) (= (coerce pi (type-of elt)) (- elt)))
      (+pi/2-p (elt) (approx= (coerce (/ pi +2) (type-of elt)) elt))
      (-pi/2-p (elt) (approx= (coerce (/ pi -2) (type-of elt)) elt))
      (+zerop (elt) (and (zerop elt) (plusp (float-sign elt))))
      (-zerop (elt) (and (zerop elt) (minusp (float-sign elt)))))

  (test ieee-fp.0004.atan2-special-cases.nan-arg
        (without-fpe-traps
            ;; (atan (anything) nan) -> nan
            ;; (atan nan (anything)) -> nan
            (map () (lambda (n)
                      (is (si:float-nan-p (atan n (si:nan))))
                      (is (si:float-nan-p (atan (si:nan) n))))
                 (append *floats* *ieee-fp.inf* (list (si:nan))))))

  (test ieee-fp.0005.atan2-special-case.zero-arg
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

  (test ieee-fp.0006.atan2-special-case.inf-arg
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
