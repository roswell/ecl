;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Daniel Kochmański
;;;; Created:  2016-08-12
;;;; Contains: IEEE floating point tests
;;;;

(in-package :cl-test)

(suite 'features/ieee-fp)

(test ieee-fp.0001.infinity-eql
  (let ((sfni ext:single-float-negative-infinity)
        (sfpi ext:single-float-positive-infinity)
        (dfni ext:double-float-negative-infinity)
        (dfpi ext:double-float-positive-infinity))
    (is (eql sfni (- sfpi)))
    (is (eql dfni (- dfpi)))
    (is (not (eql sfni (- dfpi))))
    (is (= sfni (- dfpi)))))

(test ieee-fp.0002.printing
  (let ((nums (list ext:single-float-negative-infinity
                    ext:single-float-positive-infinity
                    ext:double-float-negative-infinity
                    ext:double-float-positive-infinity
                    (si:nan)
                    (si:infinity)))
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
