;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Multithreading API regression tests

(in-package :cl-test)

;;; Date: 14/04/2012
;;;	Ensure that at creation name and counter are set
(deftest sem-make-and-counter
    (loop with name = "sem-001-make-and-counter"
       for count from 0 to 10
       for sem = (mp:make-semaphore :name name :count count)
       always (and (eq (mp:semaphore-name sem) name)
		   (= (mp:semaphore-count sem) count)
		   (zerop (mp:semaphore-wait-count sem))))
  t)

;;; Date: 14/04/2012
;;;	Ensure that signal changes the counter by the specified amount
(deftest sem-signal-semaphore-count
    (loop with name = "sem-002-signal-semaphore-count"
       for count from 0 to 10
       always (loop for delta from 0 to 10
		 for sem = (mp:make-semaphore :name name :count count)
		 always (and (= (mp:semaphore-count sem) count)
			     (null (mp:signal-semaphore sem delta))
			     (= (mp:semaphore-count sem ) (+ count delta)))))
  t)

;;; Date: 14/04/2012
;;;	A semaphore with a count of zero blocks a process
(def-mp-test sem-signal-one-process
    (let* ((flag nil)
	   (sem (mp:make-semaphore :name "sem-signal-one"))
	   (a-process (mp:process-run-function
		       "sem-signal-one-process"
		       #'(lambda ()
			   (mp:wait-on-semaphore sem)
			   (setf flag t)))))
      (and (null flag)
	   (mp:process-active-p a-process)
	   (progn (mp:signal-semaphore sem) (sleep 0.2) flag)
	   (= (mp:semaphore-count sem) 0)))
  t)

;;; Date: 14/04/2012
;;;	We can signal multiple processes
(def-mp-test sem-signal-n-processes
    (loop for count from 1 upto 10 always
	 (let* ((counter 0)
		(lock (mp:make-lock :name "sem-signal-n-processes"))
		(sem (mp:make-semaphore :name "sem-signal-n-processs"))
		(all-process
		 (loop for i from 1 upto count
		    collect (mp:process-run-function
			     "sem-signal-n-processes"
			     #'(lambda ()
				 (mp:wait-on-semaphore sem)
				 (mp:with-lock (lock) (incf counter)))))))
	   (and (zerop counter)
		(every #'mp:process-active-p all-process)
		(= (mp:semaphore-wait-count sem) count)
		(progn (mp:signal-semaphore sem count) (sleep 0.2)
		       (= counter count))
		(= (mp:semaphore-count sem) 0))))
  t)

;;; Date: 12/04/2012
;;;	It is possible to kill processes waiting for a semaphore.
;;;
(def-mp-test sem-interruptible
    (loop with sem = (mp:make-semaphore :name "sem-interruptible")
       with flag = nil
       for count from 1 to 10
       for all-processes = (loop for i from 1 upto count
			      collect (mp:process-run-function
				       "sem-interruptible"
				       #'(lambda ()
					   (mp:wait-on-semaphore sem)
					   (setf flag t))))
       always (and (progn (sleep 0.2) (null flag))
		   (every #'mp:process-active-p all-processes)
		   (= (mp:semaphore-wait-count sem) count)
		   (mapc #'mp:process-kill all-processes)
		   (progn (sleep 0.2) (notany #'mp:process-active-p all-processes))
		   (null flag)
		   (zerop (mp:semaphore-wait-count sem))
		   t))
  t)
