;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Contains: Various regression tests for ECL

(in-package :cl-test)

(suite 'regressions/mixed)


;;; (EXT:PACKAGE-LOCK) returned the wrong value.
;;; Fixed in 77a267c7e42860affac8eddfcddb8e81fccd44e5

(test mix.0001.package-lock
  ;; Don't know the first state
  (ext:package-lock "CL-USER" nil)
  (is-false (ext:package-lock "CL-USER" t))
  (is-true  (ext:package-lock "CL-USER" nil))
  (is-false (ext:package-lock "CL-USER" nil)))


;; Bugs from sourceforge

(test mix.0002.mvb-not-evaled
  (is (eq :ok (block nil
                (tagbody
                   (return (multiple-value-bind ()
                               (go :fail) :bad))
                 :fail
                   (return :ok))))))



(ext:with-clean-symbols (foo)
  (declaim (ftype (function (cons) t)   foo)
           (ftype (function (t cons) t) (setf foo)))

  (defun foo (cons)
    (first cons))

  (defun (setf foo) (value cons)
    (setf (first cons) value))

  (test mix.0003.declaim-type
    (let ((*bar* (cons 'x 'y)))
      (is (eq (foo *bar*) 'x))
      (is (eq (setf (foo *bar*) 'z) 'z) "signals on error:
;; Z is not of type CONS.
;;   [Condition of type TYPE-ERROR]"))))



(test mix.0004.style-warning-argument-order
  (let ((warning nil))
    (is (eq :ok
            (handler-bind
                ((style-warning
                  (lambda (c)
                    (format t "got style-warning: ~s~%" c)
                    (setf warning c))))
              (block nil
                (tagbody
                   (return (multiple-value-bind () (go :fail) :bad))
                 :fail
                   (return :ok))))))
    (is-false warning)))

(test mix.0005.write-hash-readable
  (is (= (hash-table-count
          (read-from-string
           (write-to-string (make-hash-table)
                            :readably t))))))

(test mix.0006.find-package
  (is
   (let ((string ":cl-user"))
     (find-package
      (let ((*package* (find-package :cl)))
        (read-from-string string)))))
  (is
   (let ((string ":cl-user"))
     (let ((*package* (find-package :cl)))
       (find-package
        (read-from-string string))))))



;;; Date: 2016-05-21 (Masataro Asai)
;;; Description:
;;;
;;;     RESTART-CASE investigates the body in an incorrect manner,
;;;     then remove the arguments to SIGNAL, which cause the slots of
;;;     the conditions to be not set properly.
;;;
;;; Bug: https://gitlab.com/embeddable-common-lisp/ecl/issues/247
;;;
(ext:with-clean-symbols (x)
  (define-condition x () ((y :initarg :y)))
  (test mix.0007.restart-case-body
    (is-false (handler-bind ((x (lambda (c) (slot-value c 'y))))
                (restart-case
                    (signal 'x :y 1))))))


;;; Date: 2016-04-21 (Juraj)
;;; Fixed: 2016-06-21 (Daniel Kochmański)
;;; Description:
;;;
;;; Trace did not respect *TRACE-OUTPUT*.
;;;
;;; Bug: https://gitlab.com/embeddable-common-lisp/ecl/issues/236
;;;
(ext:with-clean-symbols (fact)
  (defun fact (n) (if (zerop n) :boom (fact (1- n))))
  (test mix.0008.trace-output
    (is-eql 0
            (length
             (with-output-to-string (*trace-output*)
               (trace fact)
               (fact 3)
               (untrace fact)
               *trace-output*)))))


;;;; Author:   Daniel Kochmański
;;;; Created:  2015-09-21
;;;; Contains: Random state tests
#+ (or)
(def-test mix.0009.random-states (:compile-at :definition-time)
  (is (numberp (random 18)) "Trivial case")
  (is (numberp (random 18 #$1))
      "Check if we can generate random number from a read random ~
      state")
  (is (numberp (random 18 (make-random-state)))
      "Check if we can generate random number from a new random ~
      state")
  (is (numberp (random 18 (make-random-state #$1)))
      "Check if we can copy use copied random state from reader")
  (is (= (random 18 #$1)
         (random 18 #$1)
         (random 18 #$1))
      "Check if the same seed produces the same result")
  (is (let ((*print-readably* t)
            (rs (make-random-state #$1)))
        (equalp
         (format nil "~S" #$1)
         (format nil "~S" rs)))
      "Check if we get the same table from the same seed")
  (is (let* ((*print-readably* t)
             (rs (make-random-state #$1))
             (rs-read (read-from-string
                       (format nil "~S" rs))))
        (equalp
         (format nil "~S" rs-read)
         (format nil "~S" rs)))
      "Check if we can read back the random state"))


;;; Date: 2016-08-04 (jd)
;;; Fixed: 2016-08-04 (jd)
;;; Description:
;;;
;;; file-stream-fd caused internal error if fed with non-file ANSI
;;; stream
;;;
;;; Bug: https://gitlab.com/embeddable-common-lisp/ecl/issues/271
;;;
(test mix.0010.file-stream-fd
  ;; We check the second one only if first test passes. Second test
  ;; caused internal error of ECL and crashed the process preventing
  ;; further tests, so we perform it only on versions after the fix.
  (if (signals simple-type-error (ext:file-stream-fd ""))
      (signals simple-type-error (ext:file-stream-fd
                                  (make-string-output-stream)))
      (fail (ext:file-stream-fd (make-string-output-stream))
            "Not-file stream would cause internal error on this ECL (skipped)")))


