;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Contains: Various regression tests for ECL

(in-package :cl-test)

(suite 'mixed)


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
    (is
     (not (zerop
           (length
            (with-output-to-string (*trace-output*)
              (trace fact)
              (fact 3)
              (untrace fact)
              *trace-output*)))))))


;;;; Author:   Daniel Kochmański
;;;; Created:  2015-09-21
;;;; Contains: Random state tests
(test mix.0009.random-states
  (is (numberp (random 18)) "Can't generate trivial random number")
  (is (numberp (random 18 #$1))
      "Can't generate a number from read (#$1) random state")
  (is (numberp (random 18 (make-random-state)))
      "Can't generate a number from a new random state")
  (is (numberp (random 18 (make-random-state #$1)))
      "Can't generate a number from a new random state from reader")
  (is (= (random 18 #$1)
         (random 18 #$1)
         (random 18 #$1))
      "The same seed produces different results")
  (is (let ((*print-readably* t)
            (rs (make-random-state #$1)))
        (equalp
         (prin1-to-string #$1)
         (prin1-to-string rs)))
      "The same seed gives different random states")
  (is (let* ((*print-readably* t)
             (rs (make-random-state #$1))
             (rs-read (read-from-string
                       (prin1-to-string rs))))
        (equalp
         (prin1-to-string rs-read)
         (prin1-to-string rs)))
      "Can't read back a random state"))


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


;;;; Author:   Daniel Kochmański
;;;; Created:  2016-09-07
;;;; Contains: External process interaction API
;;;;
(test mix.0011.run-program
  (let ((p (nth-value 2 (ext:run-program #-windows "sleep"
                                         #+windows "timeout"
                                         (list "3") :wait nil))))
    (is (eql :running (ext:external-process-wait p nil))
        "process doesn't run")
    (ext:terminate-process p)
    (sleep 1)
    (multiple-value-bind (status code)
        (ext:external-process-wait p nil)
      (is (eql :signaled status)
          "status is ~s, should be ~s" status :signalled)
      (is (eql ext:+sigterm+ code)
          "signal code is ~s, should be ~s" code ext:+sigterm+))
    (finishes (ext:terminate-process p))))


;;; Date: 2016-12-20
;;; Reported by: Kris Katterjohn
;;; Fixed: Daniel Kochmański
;;; Description:
;;;
;;;   atan signalled `division-by-zero' exception when the second
;;;   argument was signed zero. Also inconsistent behavior on invalid
;;;   operation (atan 0.0 0.0).
;;;
;;; Bug: https://gitlab.com/embeddable-common-lisp/ecl/issues/329
(test mix.0012.atan-signed-zero
  (finishes (atan 1.0 -0.0)))


;;; Date: 2016-12-21
;;; Description:
;;;
;;;   `sleep' sues `ECL_WITHOUT_FPE_BEGIN' which didn't restore fpe
;;;   correctly.
;;;
;;; Bug: https://gitlab.com/embeddable-common-lisp/ecl/issues/317
(test mix.0013.sleep-without-fpe
  (sleep 0.1)
  (let ((a 1.0)
        (b 0.0))
    ;; nb: normally operation signals `division-by-zero', but OSX
    ;; signals `floating-point-overflow'. It's OK I suppose.
    (signals arithmetic-error (/ a b))))


;;; Data: 2017-01-20
;;; Description:
;;;
;;;   `dolist' macroexpansion yields result which doesn't have a
;;;   correct scope.
;;;
;;; Bug: https://gitlab.com/embeddable-common-lisp/ecl/issues/348
(test mix.0014.dolist
  (is-false
   (nth-value 1
     (compile nil
              (lambda ()
                (dolist (s '("foo" "bar" "baz") s)
                  (declare (type string s))
                  (check-type s string)
                  (format nil "~s" s))))))
  (finishes (eval '(dolist (e '(1 2 3 4) e)
                    (print e)
                    (go :next)
                    (print 'skip)
                    :next))))
