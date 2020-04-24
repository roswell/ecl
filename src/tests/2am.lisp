;;; Copyright (c) 2014 James M. Lawrence
;;; Copyright (c) 2016 Daniel Kochmański
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

#| to avoid conflict with the library name package 2am-ecl |#
(defpackage #:2am-ecl
  (:use #:cl)
  (:export #:test #:is #:signals #:finishes #:run #:suite))

(in-package #:2am-ecl)

(defvar *tests* nil "A name of the default tests suite.")
(defvar *suites* (make-hash-table) "A collection of test suites.")
(defvar *hierarchy* (make-hash-table) "A hierarchy of test suites.")
(defvar *failures* nil)
(defvar *crashes* nil)
(defvar *test-name* nil)
(defvar *test-count* nil)
(defvar *pass-count* nil)
(defvar *fail-count* nil)
(defvar *running* nil)
(defvar *last-fail* nil)

(define-condition test-failure (simple-condition)
  ((test-name :initarg :name
              :accessor test-name)))

(defun suite (&optional (name *tests* name-p) (sub nil sub-p))
  "Sets the current suite to the `name'."
  (assert (symbolp name))
  (assert (typep sub 'sequence))
  (when name-p
    (setf *tests* name))
  (when sub-p
    (setf (gethash *tests* *hierarchy*) sub))
  *tests*)

(defsetf suite (name) (tests suites)
  "Resets the suite to contain the provided tests and suites"
  `(progn
     (assert (typep ,tests 'sequence))
     (assert (typep ,suites 'sequence))
     (setf (gethash ,name *suites*) ,tests
           (gethash ,name *hierarchy*) ,suites)
     tests))

(defun %shuffle (vector)
  (loop for i downfrom (- (length vector) 1) to 1
        do (rotatef (aref vector i) (aref vector (random (1+ i)))))
  vector)

(defun shuffle (sequence)
  (%shuffle (map 'vector #'identity sequence)))

(defun report (test-count pass-count fail-count crashes)
  (let ((num-check (+ pass-count fail-count)))
    (if *running*
        (format t "~&Did ~s test~:p (~s crashed), ~s check~:p.~%" test-count crashes num-check)
        (format t "~&Test ~s: ~s check~:p.~%" *test-name* num-check))
    (unless (zerop num-check)
      (let ((passed% (round (* 100 (/ pass-count num-check))))
            (failed% (round (* 100 (/ fail-count num-check)))))
        (format t "   Pass: ~s (~2D%)~%" pass-count passed%)
        (format t "   Fail: ~s (~2D%)~%" fail-count failed%))))
  (unless (= fail-count crashes 0)
    (format t "~%Failure details:~%")
    (format t "--------------------------------~%")
    (maphash (lambda (test fails)
               (format t " ~A:~%" test)
               (dolist (fail (reverse fails))
                 (if (typep fail 'test-failure)
                     (format t "   FAIL: ")
                     (format t "   CRASH [~A]: " (type-of fail)))
                 (format t "~A~%" fail))
               (format t "~&--------------------------------~%"))
             *failures*)))

(defun %run (fn)
  (let ((*test-count* 0)
        (*pass-count* 0)
        (*fail-count* 0)
        (*failures* (make-hash-table))
        (*crashes* 0))
    (multiple-value-prog1 (funcall fn)
      (report *test-count* *pass-count* *fail-count* *crashes*))))

(defun %run-suite (name)
  (let ((visited nil)
        (functions nil))
    (labels ((traverse (name)
               (unless (member name visited)
                 (push name visited)
                 (push (lambda ()
                         (format t "~&--- Running test suite ~s~%" name)
                         (map nil #'funcall (shuffle
                                             (gethash name *suites*))))
                       functions)
                 (map nil #'traverse (shuffle
                                      (gethash name *hierarchy*))))))
      (traverse name))
    (nreverse functions)))

(defun run (&optional (tests (gethash nil *suites*)))
  "Run each test in the sequence `tests'. Default is `*tests*'."
  (let ((*running* t))
    (etypecase tests
      (symbol
       (%run (lambda ()
               (map nil #'funcall (%run-suite tests)))))
      (list
       (%run (lambda ()
               (map nil #'funcall (shuffle tests)))))))
  (values))

(defun call-test (fn)
  (format t "~&Running test ~s " *test-name*)
  (finish-output)
  (if *running*
      (handler-case
          (progn (incf *test-count*)
                 (funcall fn))
        (serious-condition (c)
          (write-char #\X)
          (incf *crashes*)
          (push c (gethash *test-name* *failures*))))
      (%run fn))
  (values))

(defmacro test (name &body body)
  "Define a test function and add it to `*tests*'."
  `(progn
     (defun ,name ()
       (let ((*test-name* ',name))
         (call-test (lambda () ,@body))))
     (pushnew ',name (gethash *tests* *suites*))
     ',name))

(defun passed ()
  (write-char #\.)
  (when *pass-count*
    (incf *pass-count*))
  T)

(defun failed (c)
  (write-char #\f)
  (when *fail-count*
    (incf *fail-count*))
  (when *failures*
    (push c (gethash *test-name* *failures*)))
  (setf *last-fail* c)
  nil)

(defmacro is (form &rest args
              &aux
                (fmt-ctrl (format nil "~s~@[~%~A~]" form (car args)))
                (fmt-args (cdr args)))
  "Assert that `form' evaluates to non-nil."
  `(if ,form
       (passed)
       (failed (make-condition 'test-failure
                               :name *test-name*
                               :format-control ,fmt-ctrl
                               :format-arguments (list ,@fmt-args)))))

(defun %signals (expected fn &rest args)
  (flet ((handler (condition)
           (cond ((typep condition expected)
                  (return-from %signals (passed)))
                 ((typep condition 'serious-condition)
                  (return-from %signals
                    (let ((fmt-ctrl
                            (if args
                                (car args)
                                "Expected to signal ~s, but got ~s:~%~a"))
                          (fmt-args
                            (if args
                                (cdr args)
                                (list expected (type-of condition) condition))))
                      (failed (make-condition 'test-failure
                                              :name *test-name*
                                              :format-control fmt-ctrl
                                              :format-arguments fmt-args)))))
                 (t #|ignore non-serious unexpected conditions|#))))
    (handler-bind ((condition #'handler))
      (funcall fn)))
  (let ((fmt-ctrl (if args (car args) "Expected to signal ~s, but got nothing"))
        (fmt-args (if args (cdr args) `(,expected))))
   (failed (make-condition 'test-failure
                           :name *test-name*
                           :format-control fmt-ctrl
                           :format-arguments fmt-args))))

(defmacro signals (condition form &rest args)
  "Assert that `body' signals a condition of type `condition'."
  `(%signals ',condition (lambda () ,form) ,@args))

(defmacro finishes (form &rest args)
  (if args
      `(handler-case (progn ,form (passed))
         (serious-condition (c)
           (failed (make-condition 'test-failure
                                   :name *test-name*
                                   :format-control ,(car args)
                                   :format-arguments (list ,@(cdr args))))))
      `(handler-case (progn ,form (passed))
         (serious-condition (c)
           (failed (make-condition 'test-failure
                                   :name *test-name*
                                   :format-control "Expected to finish, but got ~s"
                                   :format-arguments (list (type-of c))))))))
