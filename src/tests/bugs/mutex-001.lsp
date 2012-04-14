;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Multithreading API regression tests

(in-package :cl-test)

;;; Date: 12/04/2012
;;;	Non-recursive mutexes should signal an error when they
;;;	cannot be relocked.
(deftest mutex-001-recursive-error
    (let* ((mutex (mp:make-lock :name 'mutex-001-recursive-error)))
      (and
       (mp:get-lock mutex)
       (eq (mp:lock-owner mutex) mp:*current-process*)
       (handler-case
	   (progn (mp:get-lock mutex) nil)
	 (error (c) t))
       (mp:giveup-lock mutex)
       (null (mp:lock-owner mutex))
       (zerop (mp:lock-count mutex))
       t))
  t)

;;; Date: 12/04/2012
;;;	Recursive locks increase the counter.
(deftest mutex-002-recursive-count
    (let* ((mutex (mp:make-lock :name 'mutex-002-recursive-count :recursive t)))
      (and
       (loop for i from 1 upto 10
	  always (and (mp:get-lock mutex)
		      (= (mp:lock-count mutex) i)
		      (eq (mp:lock-owner mutex) mp:*current-process*)))
       (loop for i from 9 downto 0
	  always (and (eq (mp:lock-owner mutex) mp:*current-process*)
		      (mp:giveup-lock mutex)
		      (= (mp:lock-count mutex) i)))
       (null (mp:lock-owner mutex))
       (zerop (mp:lock-count mutex))
       t))
  t)


;;; Date: 12/04/2012
;;;	When multiple threads compete for a mutex, they should
;;;	all get the same chance of accessing the resource
;;;
(deftest mutex-001-fairness
    (let* ((mutex (mp:make-lock :name 'mutex-001-fairness))
	   (nthreads 10)
	   (count 10)
	   (counter (* nthreads count))
	   (array (make-array count :element-type 'fixnum :initial-element 0)))
      (flet ((slave (n)
	       (loop with continue = t
		  for i from 1 by 1
		  while continue do
		    (mp:get-lock mutex)
		    (cond ((plusp counter)
			   (decf counter)
			   (setf (aref array n) i))
			  (t
			   (setf continue nil)))
		    (mp:giveup-lock mutex))))
	;; Launch all agents. They will be locked
	(let ((all-processes
	       (mp:with-lock (mutex)
		 (loop for n from 0 below nthreads
		    collect (mp:process-run-function n #'slave n)
		    ;; ... and give them some time to block on this mutex
		    finally (sleep 1)))))
	  ;; Now they are released and operate. They should all have
	  ;; the same share of counts.
	  (loop for p in all-processes
	     do (mp:process-join p))
	  (loop for i from 0 below nthreads
	     always (= (aref array i) count)))))
  t)
