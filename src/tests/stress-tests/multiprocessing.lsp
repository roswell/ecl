;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;; Author: Daniel Kochmański
;; Contains: Multiprocessing stress tests

(in-package :cl-test)

(suite 'stress)

(defparameter *runs* 5)


;; Semaphores

;; Submitted by James M. Lawrence
;; 
;; Notes: couldn't reproduce on 64-bit machine, but author uses 32-bit
(test semaphore.wait/signal
  (let* ((message-count 10000)
         (worker-count 64)
         (to-workers (mp:make-semaphore))
         (from-workers (mp:make-semaphore))
         (threads
          (loop repeat worker-count
             collect (mp:process-run-function
                      "test"
                      (lambda ()
                        (loop
                           (mp:wait-on-semaphore to-workers)
                           (mp:signal-semaphore from-workers)))))))
    (dotimes (i *runs*)
      (loop repeat message-count
         do (mp:signal-semaphore to-workers))
      (loop repeat message-count
         do (mp:wait-on-semaphore from-workers))
      (is (zerop (mp:semaphore-count to-workers)))
      (is (zerop (mp:semaphore-count from-workers)))
      (finish-output))
    (mapcar #'mp:process-kill threads)))


;; Implementation of a semaphore using locks and condition variables

;; Submitted by James M. Lawrence
;; 
;; Notes: couldn't reproduce on 64-bit machine, but author uses 32-bit
(defstruct sema
  (count 0)
  (lock (mp:make-lock :recursive nil))
  (cvar (mp:make-condition-variable)))

(defun inc-sema (sema)
  (mp:with-lock ((sema-lock sema))
    (incf (sema-count sema))
    (mp:condition-variable-signal (sema-cvar sema))))

(defun dec-sema (sema)
  (mp:with-lock ((sema-lock sema))
    (loop (cond ((plusp (sema-count sema))
                 (decf (sema-count sema))
                 (return))
                (t
                 (mp:condition-variable-wait
                  (sema-cvar sema) (sema-lock sema)))))))

(test semaphore/condition-wait
  (let* ((message-count 10000)
         (worker-count 64)
         (to-workers (make-sema))
         (from-workers (make-sema))
         (threads
          (loop repeat worker-count
             collect (mp:process-run-function
                      "test"
                      (lambda ()
                        (loop
                           (dec-sema to-workers)
                           (inc-sema from-workers)))))))
    (dotimes (i *runs*)
      (loop repeat message-count
         do (inc-sema to-workers))
      (loop repeat message-count
         do (dec-sema from-workers))
      (is (zerop (sema-count to-workers)))
      (is (zerop (sema-count from-workers)))
      (finish-output))
    (mapcar #'mp:process-kill threads)))


;; Implementation of a queue using locks and condition variables

;; Submitted by James M. Lawrence
;; 
;; Notes: couldn't reproduce on 64-bit machine, but author uses 32-bit
(defstruct (raw-queue (:conc-name nil))
  (head nil)
  (tail nil))

(defun push-raw-queue (value queue)
  (let ((new (cons value nil)))
    (if (head queue)
        (setf (cdr (tail queue)) new)
        (setf (head queue) new))
    (setf (tail queue) new)))

(defun pop-raw-queue (queue)
  (let ((node (head queue)))
    (if node
        (multiple-value-prog1 (values (car node) t)
          (when (null (setf (head queue) (cdr node)))
            (setf (tail queue) nil))
          (setf (car node) nil
                (cdr node) nil))
        (values nil nil))))

(defstruct queue
  (impl (make-raw-queue))
  (lock (mp:make-lock))
  (cvar (mp:make-condition-variable)))

(defun push-queue (object queue)
  (mp:with-lock ((queue-lock queue))
    (push-raw-queue object (queue-impl queue))
    (mp:condition-variable-signal (queue-cvar queue))))

(defun pop-queue (queue)
  (mp:with-lock ((queue-lock queue))
    (loop (multiple-value-bind (value presentp)
              (pop-raw-queue (queue-impl queue))
            (if presentp
                (return value)
                (mp:condition-variable-wait
                 (queue-cvar queue)
                 (queue-lock queue)))))))

(defun qtest (message-count worker-count)
  (dotimes (i *runs*)
    (let ((to-workers (make-queue))
          (from-workers (make-queue)))
      (loop repeat worker-count
         do (mp:process-run-function
             "test"
             (lambda ()
               (loop
                  (let ((message (pop-queue to-workers)))
                    (push-queue message from-workers)
                    (unless message (return)))))))
      (loop repeat message-count do (push-queue t to-workers))
      (loop repeat message-count do (pop-queue from-workers))
      (loop repeat worker-count do (push-queue nil to-workers))
      (loop repeat worker-count do (pop-queue from-workers))
      (format t ".")
      (finish-output))))

(test qtest.1 (finishes (let ((*runs* (* *runs* 10)))
                          (qtest 0 64))))     ; => segfault
(test qtest.2 (finishes (let ((*runs* (* *runs* 10)))
                          (qtest 1 64))))     ; => hang
(test qtest.3 (finishes (qtest 10000 64)))    ; => error "Attempted to recursively lock..."



