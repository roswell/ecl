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
  (:export #:test #:test-with-timeout #:is #:signals #:finishes
           #:run #:suite))

(in-package #:2am-ecl)

(defvar *tests* nil "A name of the default tests suite.")
(defvar *suites* (make-hash-table) "A collection of test suites.")
(defvar *hierarchy* (make-hash-table) "A hierarchy of test suites.")
(defvar *stats* nil "Collection of test statistics.")
(defvar *running* nil)
(defvar *test-name* nil)
(defvar *last-fail* nil)
(defvar *default-timeout* 60.0 "Default timeout in seconds.")

(defstruct test-stats
  (failures (make-hash-table))
  (crashes 0)
  (test-count 0)
  (pass-count 0)
  (fail-count 0))

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

(defun report (stats &aux
                       (test-count (test-stats-test-count stats))
                       (pass-count (test-stats-pass-count stats))
                       (fail-count (test-stats-fail-count stats))
                       (crashes (test-stats-crashes stats))
                       (failures (test-stats-failures stats)))
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
             failures)))

(defun %run (fn)
  (let ((*stats* (make-test-stats)))
    (multiple-value-prog1 (funcall fn)
      (report *stats*))))

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

(defun call-test (name fn)
  (let ((*test-name* name))
    (format t "~&Running test ~s " *test-name*)
    (finish-output)
    (if *running*
        (handler-case
            (progn (incf (test-stats-test-count *stats*))
                   (funcall fn))
          (serious-condition (c)
            (write-char #\X)
            (incf (test-stats-crashes *stats*))
            (push c (gethash *test-name* (test-stats-failures *stats*)))))
        (%run fn))
    (values)))

(defmacro test (name &body body)
  `(progn
     (defun ,name ()
       (call-test ',name (lambda () ,@body)))
     (pushnew ',name (gethash *tests* *suites*))
     ',name))

(defun kill-processes (process-list &optional original)
  "Kills a list of processes, which may be the difference between two lists."
  (let ((process-list (set-difference process-list original)))
    (when (member mp:*current-process* process-list)
      (error "Found myself in the kill list"))
    (mapc #'mp:process-kill process-list)
    process-list))

#+threads
(defun call-test-with-timeout (name timeout fn)
  (let* ((all-processes (mp:all-processes))
         (finished nil)
         (runner (mp:process-run-function
                  "runner"
                  #'(lambda (stats running)
                      (let ((*stats* stats)
                            (*running* running))
                        (call-test name fn)
                        (setf finished t)))
                  *stats* *running*)))
    (loop with *test-name* = name
          with timestep = 0.2
          for time from 0.0 upto timeout by timestep
          do (if finished
                 (return)
                 (sleep timestep))
          finally (mp:process-kill runner)
                  (failed (make-condition 'test-failure
                                          :name name
                                          :format-control "Timeout after ~A seconds"
                                          :format-arguments (list timeout)))
                  (return-from call-test-with-timeout))
    (mp:process-join runner)
    (let ((leftovers (kill-processes (mp:all-processes) all-processes)))
      (when leftovers
        (format t "~%;;; Stray processes: ~A~%" leftovers)))))

#+threads
(defmacro test-with-timeout (name-and-timeout &body body)
  (let (name timeout)
    (if (listp name-and-timeout)
        (setf name (first name-and-timeout)
              timeout (second name-and-timeout))
        (setf name name-and-timeout
              timeout '*default-timeout*))
    `(progn
       (defun ,name ()
         (call-test-with-timeout ',name ,timeout (lambda () ,@body)))
       (pushnew ',name (gethash *tests* *suites*))
       ',name)))

(defun passed ()
  (write-char #\.)
  (when *stats*
    (incf (test-stats-pass-count *stats*)))
  T)

(defun failed (c)
  (write-char #\f)
  (when *stats*
    (incf (test-stats-fail-count *stats*))
    (push c (gethash *test-name* (test-stats-failures *stats*))))
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
                                   :format-control "Expected to finish, but got ~s:~%~a"
                                   :format-arguments (list (type-of c) c)))))))
