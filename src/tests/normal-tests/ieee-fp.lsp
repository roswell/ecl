;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Daniel Kochmański
;;;; Created:  2016-08-12
;;;; Contains: IEEE floating point tests
;;;;

(in-package :cl-test)

(suite 'ieee-fp)

(defmacro without-fpe-traps (&body body)
  `(unwind-protect
        (progn
          (si:trap-fpe 'last nil)
          ,@body)
     (si:trap-fpe 'last t)))

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

(defparameter *ieee-fp.negative-inf*
  (list ext:short-float-negative-infinity
        ext:single-float-negative-infinity
        ext:double-float-negative-infinity
        ext:long-float-negative-infinity))

(defparameter *ieee-fp.positive-inf*
  (list ext:short-float-positive-infinity
        ext:single-float-positive-infinity
        ext:double-float-positive-infinity
        ext:long-float-positive-infinity))

(defparameter *ieee-fp.inf*
  (list ext:short-float-negative-infinity
        ext:single-float-negative-infinity
        ext:double-float-negative-infinity
        ext:long-float-negative-infinity
        ext:short-float-positive-infinity
        ext:single-float-positive-infinity
        ext:double-float-positive-infinity
        ext:long-float-positive-infinity))

(defparameter *ieee-fp.anything*
  (append *floats*
          (list -0.0 +0.0 (si:nan))
          *ieee-fp.inf*))

(defparameter *ieee-fp.anything/but-nan*
  (remove-if #'si::float-nan-p *ieee-fp.anything*))

(defparameter *ieee-fp.anything/but-0+nan*
  (remove-if (lambda (x)
               (or (si::float-nan-p x)
                   (si::zerop x)))
             *ieee-fp.anything*))

(defparameter *ieee-fp.anything/but-inf+nan*
  (remove-if (lambda (x)
               (or (si::float-nan-p x)
                   (si::float-infinity-p x)))
             *ieee-fp.anything*))

(defparameter *ieee-fp.anything/but-0+inf+nan*
  (remove-if (lambda (x)
               (or (si::float-nan-p x)
                   (si::float-infinity-p x)
                   (zerop x)))
             *ieee-fp.anything*))

(defun pip (my-pi number)
  (<= (abs (- (abs my-pi) (abs number))) 0.01))

(test ieee-fp.0004.atan2-special-cases.nan-arg
  (without-fpe-traps
    ;; (atan (anything) nan) -> nan
    ;; (atan nan (anything)) -> nan
    (map () (lambda (n)
              (is (si:float-nan-p (atan n (si:nan))))
              (is (si:float-nan-p (atan (si:nan) n))))
         *ieee-fp.anything*)))

(test ieee-fp.0005.atan2-special-case.zero-arg
  ;; (atan +-0 +(anything-but-nan))  -> +-0
  ;; (atan +-0 -(anything-but-nan))  -> +-pi
  (map () (lambda (n)
            (is (zerop (atan -0.0 n)))
            (is (zerop (atan +0.0 n)))
            (is (pip pi (atan +0.0 (- n))))
            (is (pip pi (atan -0.0 (- n)))))
       (remove-if-not #'plusp *ieee-fp.anything/but-nan*))
  ;; (atan +-(anything-but-0/nan) 0) -> +-pi/2
  (map () (lambda (n)
            (is (pip (* +1/2 pi) (atan n +0.0)))
            (is (pip (* -1/2 pi) (atan n -0.0))))
       *ieee-fp.anything/but-0+nan*))

(test ieee-fp.0006.atan2-special-case.inf-arg
  ;; (atan +-inf +inf) -> +-pi/4
  (map () (lambda (n)
            (map () (lambda (m) (is (pip (/ pi 4) (atan n m))))
                 *ieee-fp.positive-inf*))
       *ieee-fp.inf*)
  ;; (atan +-inf -inf) -> +-3pi/4
  (map () (lambda (n)
            (map () (lambda (m) (is (pip (* 3/4 pi) (atan n m))))
                 *ieee-fp.negative-inf*))
       *ieee-fp.inf*)
  (map () (lambda (n)
            ;; (atan +-(anything-but-inf/nan), +inf) -> +-0
            (map () (lambda (m) (is (zerop (atan n m))))
                 *ieee-fp.positive-inf*)
            ;; (atan +-(anything-but-inf/nan), -inf) -> +-pi
            (map () (lambda (m) (is (pip pi (atan n m))))
                 *ieee-fp.negative-inf*))
       *ieee-fp.anything/but-inf+nan*)
  ;; (atan +-inf (anything-but-0/nan/inf)) -> +-pi/2
  (map () (lambda (n)
            (map () (lambda (m) (is (pip (/ pi 2) (atan m n))))
                 *ieee-fp.inf*))
       *ieee-fp.anything/but-0+inf+nan*))
