;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;; Author: Daniel Kochmański
;; Contains: Multiprocessing stress tests


;; Submitted by James M. Lawrence
;; 
;; Notes: couldn't reproduce on 64-bit machine, but author uses 32-bit
;; This test uses infinite loop, this should be fixed.
(defun test (message-count worker-count)
  (let ((to-workers (mp:make-semaphore))
        (from-workers (mp:make-semaphore)))
    (loop repeat worker-count
          do (mp:process-run-function
              "test"
              (lambda ()
                (loop
                   (mp:wait-on-semaphore to-workers)
                   (mp:signal-semaphore from-workers)))))
    (loop
       (loop repeat message-count
             do (mp:signal-semaphore to-workers))
       (loop repeat message-count
             do (mp:wait-on-semaphore from-workers))
       (assert (zerop (mp:semaphore-count to-workers)))
       (assert (zerop (mp:semaphore-count from-workers)))
       (format t ".")
       (finish-output))))

(defun run ()
  (test 10000 64))

(run)


;; Submitted by James M. Lawrence
;; 
;; Notes: couldn't reproduce on 64-bit machine, but author uses 32-bit
;; This test uses infinite loop, this should be fixed.
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

(defun test (message-count worker-count)
  (let ((to-workers (make-sema))
        (from-workers (make-sema)))
    (loop repeat worker-count
          do (mp:process-run-function
              "test"
              (lambda ()
                (loop
                   (dec-sema to-workers)
                   (inc-sema from-workers)))))
    (loop
       (loop repeat message-count
             do (inc-sema to-workers))
       (loop repeat message-count
             do (dec-sema from-workers))
       (assert (zerop (sema-count to-workers)))
       (assert (zerop (sema-count from-workers)))
       (format t ".")
       (finish-output))))

(defun run ()
  (test 10000 64))

(run)


;; Submitted by James M. Lawrence
;; 
;; Notes: couldn't reproduce on 64-bit machine, but author uses 32-bit
;; This test uses infinite loop, this should be fixed.
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

;;;; queue

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

;;;; qtest

(defun qtest (message-count worker-count)
  (loop (let ((to-workers (make-queue))
              (from-workers (make-queue)))
          (loop repeat worker-count
                do (mp:process-run-function
                    "test"
                    (lambda ()
                      (loop (let ((message (pop-queue to-workers)))
                              (push-queue message from-workers)
                              (unless message (return)))))))
          (loop repeat message-count do (push-queue t to-workers))
          (loop repeat message-count do (pop-queue from-workers))
          (loop repeat worker-count do (push-queue nil to-workers))
          (loop repeat worker-count do (pop-queue from-workers))
          (format t ".")
          (finish-output))))

(qtest 0 64)             ; => segfault
(qtest 1 64)             ; => hang
(qtest 10000 64)         ; => error "Attempted to recursively lock..."


