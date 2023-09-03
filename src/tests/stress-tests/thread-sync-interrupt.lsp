;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Marius Gerbershagen
;;;; Created:  Sun Aug 13 20:04:54 CEST 2023
;;;; Contains: Interrupt tests for thread synchronization primitives

(in-package :cl-test)

(suite 'thread-sync-interrupt)

;;; These tests may fail because in general thread synchronization
;;; primitives provided by the operating system are not guaranteed to
;;; be interrupt safe. There is no way around this, so this cannot
;;; work in every configuration. As of writing this, the
;;; implementations on Linux and Windows are interrupt safe but on
;;; BSDs generally not. Therefore, the main use of these tests is to
;;; check for regressions introduced by changes in ECL.


;; Locks

;;; Date: 04/09/2009
;;; From: Matthew Mondor
;;; Fixed: 05/09/2009 (Juanjo)
;;; Description:
;;;
;;;     When a WITH-LOCK is interrupted, it is not able to release
;;;     the resulting lock and an error is signaled.
;;;
(test-with-timeout mp.mutex.with-lock
    (let ((flag 0)
          (lock (mp:make-lock :name "mutex.with-lock" :recursive nil)))
      (mp:with-lock (lock)
        (let ((background-process
               (mp:process-run-function
                "mutex.with-lock"
                #'(lambda ()
                    (handler-case
                        (progn
                          (setf flag 1)
                          (mp:with-lock (lock)
                            (setf flag 2)))
                      (error (c)
                        (princ c)(terpri)
                        (setf flag c)))
                    (setf flag 3)))))
          ;; The background process should not be able to get
          ;; the lock, and will simply wait. Now we interrupt it
          ;; and the process should gracefully quit, without
          ;; signalling any serious condition
          (loop until (/= flag 0))
          (sleep 0.1)
          (is (mp:process-kill background-process))
          (mp:process-join background-process)
          (is (= flag 1))))))

;;; Date: 12/04/2012
;;;     It is possible to kill processes waiting for a lock.
;;;
(test-with-timeout mp.mutex.interruptible
  (let ((mutex (mp:make-lock :name "mutex.interruptible"))
        (flag 0))
    (mp:get-lock mutex)
    (let ((sleeper-thread
            (mp:process-run-function
             "mutex.interruptible"
             #'(lambda ()
                 (setf flag 1)
                 (mp:with-lock (mutex)
                   (setf flag 2))))))
      (loop until (/= flag 0))
      (sleep 0.1)
      (is (mp:process-active-p sleeper-thread))
      (mp:process-kill sleeper-thread)
      (mp:process-join sleeper-thread)
      (is (= flag 1))
      (is (eq (mp:lock-owner mutex) mp:*current-process*)))
    (mp:giveup-lock mutex)))

;;; Date: 14/04/2012
;;;     It is possible to kill processes waiting for a semaphore.
;;;
(test-with-timeout mp.sem.interruptible
  (loop with sem = (mp:make-semaphore :name "sem.interruptible")
        with flag = nil
        for count from 1 to 10
        for all-processes = (loop for i from 1 upto count
                                  collect (mp:process-run-function
                                           "sem.interruptible"
                                           #'(lambda ()
                                               (mp:wait-on-semaphore sem)
                                               (setf flag t))))
        do (loop until (= (mp:semaphore-wait-count sem) count))
           (is (null flag))
           (is (every #'mp:process-active-p all-processes))
           (mapc #'mp:process-kill all-processes)
           (mapc #'mp:process-join all-processes)
           (is (null flag))
           ;; Usually, the wait count should be zero at this point. We may
           ;; get higher values since the interrupt doesn't lock the mutex
           ;; associated to the semaphore and thus multiple threads may write
           ;; the wait count at the same time. However, interrupts are provided
           ;; only for debugging purposes, for which this behaviour is acceptable.
           (is (<= (mp:semaphore-wait-count sem) count))))

;;; Date: 14/04/2012
;;;     When we kill a process, it is removed from the wait queue.
;;;
(test-with-timeout mp.sem.interrupt-updates-queue
    (let* ((sem (mp:make-semaphore :name "sem.interrupt-updates-queue"))
           (process (mp:process-run-function
                     "sem.interrupt-updates-queue"
                     #'(lambda () (mp:wait-on-semaphore sem)))))
      (loop until (= (mp:semaphore-wait-count sem) 1))
      (is (mp:process-active-p process))
      (mp:process-kill process)
      (mp:process-join process)
      ;; In contrast to the previous test, if we interrupt only a single thread
      ;; the wait count must be correct, since only a single thread is writing.
      (is (zerop (mp:semaphore-wait-count sem)))))

;;; Date: 14/04/2012
;;;     When we kill a process, it signals another one. This is tricky,
;;;     because we need the awake signal to arrive _after_ the process is
;;;     killed, but the process must still be in the queue for the semaphore
;;;     to awake it.
;;;
(test-with-timeout mp.sem.interrupted-resignals
  (let* ((sem (mp:make-semaphore :name "sem.interrupted-resignals"))
         (flag1 nil)
         (flag2 nil)
         (process1 (mp:process-run-function
                    "sem.interrupted-resignals-1"
                    #'(lambda ()
                        (unwind-protect
                             (mp:wait-on-semaphore sem)
                          (loop repeat (* 60 100) do (sleep 1/100))
                          (setf flag1 t)))))
         (process2 (mp:process-run-function
                    "sem.interrupted-resignals-2"
                    #'(lambda ()
                        (mp:wait-on-semaphore sem)
                        (setf flag2 t)))))
    (loop until (= (mp:semaphore-wait-count sem) 2))
    (is (mp:process-active-p process1))
    (is (mp:process-active-p process2))
    ;; We kill the process but ensure it is still running
    (mp:process-kill process1)
    (is (mp:process-active-p process1))
    (is (null flag1))
    ;; Wait until the process is no longer waiting for the semaphore
    (loop until (= (mp:semaphore-wait-count sem) 1))
    ;; ... then awake it and the other process should start working
    (mp:signal-semaphore sem)
    (mp:process-join process2)
    (is (zerop (mp:semaphore-wait-count sem)))
    (is flag2)
    ;; Finally we kill the first process (which will by this time be
    ;; stuck in the unwind-protect call) again.
    (mp:process-kill process1)
    (mp:process-join process1)
    (is (null flag1))))

(test-with-timeout mp.cv.interruptible
  (let* ((mutex (mp:make-lock :name "cv.interruptible"))
         (cv (mp:make-condition-variable))
         (flag 0)
         (waiting-process
          (mp:process-run-function
           "cv.interruptible"
           (lambda ()
             (mp:get-lock mutex)
             (setf flag 1)
             (mp:condition-variable-wait cv mutex)
             (setf flag 2)
             (error "We shouldn't have gotten to this point")))))
    (loop until (mp:with-lock (mutex) (/= flag 0)))
    (sleep 0.1)
    (is (mp:process-kill waiting-process))
    (mp:process-join waiting-process)
    (is (null (mp:lock-owner mutex)))
    (is (= flag 1))))

