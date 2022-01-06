;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Multithreading API regression tests

(in-package :cl-test)

(suite 'mp)

;;; Important Note:
;;;
;;; Testing multithreading primitives such as locks or semaphores
;;; often requires synchronizing multiple threads. To keep the tests
;;; as simple as possible, this is synchronization is often done by
;;; using busy waits. As a consequence, test failures may manifest as
;;; timeouts. Some tests for semaphores and barriers also use mutexes
;;; for synchronization purposes and will fail if mutexes don't work
;;; correctly. Nearly all tests also assume that creating, killing or
;;; joining threads works properly, which is not tested separately.


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
;;;     Non-recursive mutexes should signal an error when they
;;;     cannot be relocked.
(test-with-timeout mp.mutex.recursive-error
  (let* ((mutex (mp:make-lock :name 'mutex-001-recursive-error)))
    (is (mp:get-lock mutex))
    (is (eq (mp:lock-owner mutex) mp:*current-process*))
    (signals error (mp:get-lock mutex))
    (is (mp:giveup-lock mutex))
    (is (null (mp:lock-owner mutex)))
    (is (zerop (mp:lock-count mutex)))))

;;; Date: 12/04/2012
;;;     Recursive locks increase the counter.
(test-with-timeout mp.mutex.recursive-count
  (let* ((mutex (mp:make-lock :name 'mutex-002-recursive-count :recursive t)))
    (is (loop for i from 1 upto 10
              always (and (mp:get-lock mutex)
                          (= (mp:lock-count mutex) i)
                          (eq (mp:lock-owner mutex) mp:*current-process*))))
    (is (loop for i from 9 downto 0
              always (and (eq (mp:lock-owner mutex) mp:*current-process*)
                          (mp:giveup-lock mutex)
                          (= (mp:lock-count mutex) i))))
    (is (null (mp:lock-owner mutex)))
    (is (zerop (mp:lock-count mutex)))))


;;; Date: 12/04/2012
;;;     When multiple threads compete for a mutex, they should
;;;     all get the same chance of accessing the resource
;;;
;;; Disabled since underlying OS functions don't guarantee fairness --
;;; mg 2020-08-22
#+(or)
(test-with-timeout mp.mutex.fairness
    (let* ((mutex (mp:make-lock :name "mutex.fairness"))
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
             (is (= (aref array i) count)))))))

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

(test-with-timeout (mp.mutex.timedlock-timeout 30)
  (let ((mutex (mp:make-lock :name "mutex.timedlock-timeout"))
         (flag 0))
    (mp:get-lock mutex)
    (setf flag 1)
    (let ((waiting-process
           (mp:process-run-function
            "mutex.timedlock-timeout"
            (lambda ()
              (when (mp:get-lock mutex 1)
                (error "Grabbing the mutex shouldn't have succeeded"))
              (when (eq (mp:lock-owner mutex) mp:*current-process*)
                (error "Wrong lock owner"))
              (setf flag 2)))))
      (mp:process-join waiting-process)
      (is (eq mp:*current-process* (mp:lock-owner mutex)))
      (is (= flag 2)))))

(test-with-timeout (mp.mutex.timedlock-acquire 30)
  (let ((mutex (mp:make-lock :name "mutex.timedlock-acquire"))
        (flag 0))
    (mp:get-lock mutex)
    (setf flag 1)
    (let ((waiting-process
           (mp:process-run-function
            "mutex.timedlock-acquire"
            (lambda ()
              (setf flag 2)
              (unless (mp:get-lock mutex 60)
                (error "Grabbing the mutex should have succeeded"))
              (when (not (eq (mp:lock-owner mutex) mp:*current-process*))
                (error "Wrong lock owner"))
              (setf flag 3)
              (mp:giveup-lock mutex)))))
      (loop until (> flag 1))
      (sleep 1)
      (mp:giveup-lock mutex)
      (mp:process-join waiting-process)
      (is (= flag 3)))))

;; Semaphores

;;; Date: 14/04/2012
;;;     Ensure that at creation name and counter are set
(test mp.sem.make-and-counter
  (loop with name = "sem.make-and-counter"
        for count from 0 to 10
        for sem = (mp:make-semaphore :name name :count count)
        do (is (eq (mp:semaphore-name sem) name))
           (is (= (mp:semaphore-count sem) count))
           (is (zerop (mp:semaphore-wait-count sem)))))

;;; Date: 14/04/2012
;;;     Ensure that signal changes the counter by the specified amount
(test-with-timeout mp.sem.signal-semaphore-count
  (loop with name = "sem.signal-semaphore-count"
        for count from 0 to 10
        do (loop for delta from 0 to 10
                 for sem = (mp:make-semaphore :name name :count count)
                 do (is (= (mp:semaphore-count sem) count))
                    (is (null (mp:signal-semaphore sem delta)))
                    (is (= (mp:semaphore-count sem ) (+ count delta))))))

;;; Date: 14/04/2012
;;;     A semaphore with a count of zero blocks a process
(test-with-timeout mp.sem.signal-one-process
  (let* ((flag nil)
         (sem (mp:make-semaphore :name "sem.signal-one"))
         (a-process (mp:process-run-function
                     "sem.signal-one-process"
                     #'(lambda ()
                         (mp:wait-on-semaphore sem)
                         (setf flag t)))))
    (is (null flag))
    (is (mp:process-active-p a-process))
    (mp:signal-semaphore sem)
    (mp:process-join a-process)
    (is flag)
    (is (= (mp:semaphore-count sem) 0))))

;;; Date: 14/04/2012
;;;     We can signal multiple processes
(test-with-timeout mp.sem.signal-n-processes
  (loop for count from 1 upto 10 always
       (let* ((counter 0)
              (lock (mp:make-lock :name "sem.signal-n-processes"))
              (sem (mp:make-semaphore :name "sem.signal-n-processs"))
              (all-processes
               (loop for i from 1 upto count
                  collect (mp:process-run-function
                           "sem.signal-n-processes"
                           #'(lambda ()
                               (mp:wait-on-semaphore sem)
                               (mp:with-lock (lock) (incf counter)))))))
         (loop until (= (mp:semaphore-wait-count sem) count))
         (is (zerop counter))
         (is (every #'mp:process-active-p all-processes))
         (mp:signal-semaphore sem count)
         (mapc #'mp:process-join all-processes)
         (is (= counter count)
             "Counter should be ~s (but is ~s)." count counter)
         (is (= (mp:semaphore-count sem) 0)))))

;;; Date: 14/04/2012
;;;     When we signal N processes and N+M are waiting, only N awake
(test-with-timeout mp.sem.signal-only-n-processes
  (loop for m from 1 upto 3 do
       (loop for n from 1 upto 4 do
            (let* ((counter 0)
                   (lock (mp:make-lock :name "sem.signal-n-processes"))
                   (sem (mp:make-semaphore :name "sem.signal-n-processs"))
                   (all-processes
                    (loop for i from 1 upto (+ n m)
                       collect (mp:process-run-function
                                "sem.signal-n-processes"
                                #'(lambda ()
                                    (mp:wait-on-semaphore sem)
                                    (mp:with-lock (lock) (incf counter)))))))
              (loop until (= (mp:semaphore-wait-count sem) (+ m n)))
              (is (zerop counter))
              (is (every #'mp:process-active-p all-processes))
              (mp:signal-semaphore sem n)
              (loop until (= (count-if #'mp:process-active-p all-processes) m))
              (is (= counter n))
              (is (= (mp:semaphore-wait-count sem) m)
                  "Number of threads waiting on semaphore should be ~s (but is ~s)."
                  m (mp:semaphore-wait-count sem))
              (mp:signal-semaphore sem m)
              (mapc #'mp:process-join all-processes)
              (is (= counter (+ n m))
                  "Counter should be ~s (but is ~s)."
                  (+ n m) counter)))))

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

;;; Date: 14/04/2012
;;;     1 producer and N consumers, non-blocking, because the initial count
;;;     is larger than the consumed data.
(test-with-timeout mp.sem.1-to-n-non-blocking
    (loop with counter = 0
       with lock = (mp:make-lock :name "sem.1-to-n-communication")
       for n from 1 to 10
       for m = (round 128 n)
       for length = (* n m)
       for sem = (mp:make-semaphore :name "sem.1-to-n-communication" :count length)
       for producers = (progn
                         (setf counter 0)
                         (loop for i from 0 below n
                            collect (mp:process-run-function
                                     "sem.1-to-n-consumer"
                                     #'(lambda ()
                                         (loop for i from 0 below m
                                            do (mp:wait-on-semaphore sem)
                                            do (mp:with-lock (lock) (incf counter)))))))
       do (mapc #'mp:process-join producers)
          (is (= counter length))
          (is (zerop (mp:semaphore-count sem)))
          (is (zerop (mp:semaphore-wait-count sem)))))

;;; Date: 14/04/2012
;;;     1 producer and N consumers, blocking due to a slow producer.
(test-with-timeout mp.sem.1-to-n-blocking
    (loop with lock = (mp:make-lock :name "sem.1-to-n-communication")
       for n from 1 to 10
       for m = (round 10000 n)
       for length = (* n m)
       for sem = (mp:make-semaphore :name "sem.1-to-n-communication" :count 0)
       for counter = 0
       for producers = (loop for i from 0 below n
                          collect (mp:process-run-function
                                   "sem.1-to-n-consumer"
                                   #'(lambda ()
                                       (loop for i from 0 below m
                                          do (mp:wait-on-semaphore sem))
                                       (mp:with-lock (lock) (incf counter)))))
       do (loop for i from 0 below length
             do (mp:signal-semaphore sem))
       do (mapc #'mp:process-join producers)
          (is (= counter n))
          (is (zerop (mp:semaphore-count sem)))
          (is (zerop (mp:semaphore-wait-count sem)))))

;;; Date: 2021-10-19
;;;
;;;     A smoke test for the new function wait-semaphore.
;;;
(test-with-timeout mp.sem.semaphore-wait/smoke
  (let ((sem (mp:make-semaphore :name "sem.semaphore-wait" :count 3)))
    (flet ((signal-after-fn (count seconds)
             (lambda ()
               (sleep seconds)
               (mp:signal-semaphore sem count))))
      (is (null (mp:semaphore-wait sem 4 0)))
      (is (null (mp:semaphore-wait sem 4 0.1)))
      (is (= 3  (mp:semaphore-wait sem 2 nil)))
      (mp:process-run-function nil (signal-after-fn 1 0.2))
      (is (null (mp:semaphore-wait sem 2 0.1)))
      (is (= 2  (mp:semaphore-wait sem 2 0.2)))
      (mp:process-run-function nil (signal-after-fn 2 0.2))
      (is (= 2  (mp:semaphore-wait sem 1 nil))))))


;; Mailbox

;;; Date: 14/04/2012
;;;     Ensure that at creation name and counter are set, and mailbox is empty.
(test mp.mbox.make-and-counter
  (loop with name = "mbox.make-and-counter"
        for count from 4 to 63
        for mbox = (mp:make-mailbox :name name :count count)
        do (is (eq (mp:mailbox-name mbox) name))
           (is (>= (mp:mailbox-count mbox) count))
           (is (mp:mailbox-empty-p mbox))))

;;; Date: 14/04/2012
;;;     Ensure that the mailbox works in a nonblocking fashion (when the
;;;     number of messages < mailbox size in a single producer and single
;;;     consumer  setting. We do not need to create new threads for this.
(test-with-timeout mp.mbox.nonblocking-io-1-to-1
  (loop with count = 30
        with name = "mbox.nonblocking-io-1-to-1"
        with mbox = (mp:make-mailbox :name name :count count)
        for l from 1 to 10
        for messages = (loop for i from 1 to l
                             do (mp:mailbox-send mbox i)
                             collect i)
        do
        (is (not (mp:mailbox-empty-p mbox)))
        (is (equalp (loop for i from 1 to l
                          collect (mp:mailbox-read mbox))
                    messages))
        (is (mp:mailbox-empty-p mbox))))

;;; Date: 14/04/2012
;;;     The mailbox blocks a process when it saturates the write queue.
(test-with-timeout mp.mbox.blocks-1-to-1
    (let* ((flag nil)
           (mbox (mp:make-mailbox :name "mbox.blocks-1-to-1" :count 32))
           (size (mp:mailbox-count mbox))
           (a-process (mp:process-run-function
                       "mbox.blocks-1-to-1"
                       #'(lambda ()
                           ;; This does not block
                           (loop for i from 1 to size
                              do (mp:mailbox-send mbox i))
                           ;; Here we block
                           (setf flag t)
                           (mp:mailbox-send mbox (1+ size))
                           ;; Now we unblock
                           (setf flag nil)))))
      (sleep 0.2)
      (is (not (mp:mailbox-empty-p mbox))) ; the queue has messages
      (is (mp:process-active-p a-process)) ; the process is active
      (is flag) ; and it is blocked
      (is (loop for i from 1 to (1+ size)  ; messages arrive in order
                always (= i (mp:mailbox-read mbox))))
      (mp:process-join a-process)
      (is (null flag)) ; and process unblocked
      (is (mp:mailbox-empty-p mbox))))

;;; Date: 14/04/2012
;;;     N producers and 1 consumer
(test-with-timeout mp.mbox.n-to-1-communication
    (loop with length = 10000
          with mbox = (mp:make-mailbox :name "mbox.n-to-1-communication" :count 128)
          for n from 1 to 10
          for m = (round length n)
          for messages = (loop for i from 0 below (* n m) collect i)
          for producers = (loop for i from 0 below n
                                do (mp:process-run-function
                                    "mbox.n-to-1-producer"
                                    (let ((proc-no i))
                                      #'(lambda ()
                                          (loop for i from 0 below m
                                                for msg = (+ i (* proc-no m))
                                                do (mp:mailbox-send mbox msg))))))
          do (is (equalp
                  (sort (loop for i from 1 to (* n m)
                              collect (mp:mailbox-read mbox))
                        #'<)
                  messages))
             (is (mp:mailbox-empty-p mbox))))

;;; Date: 14/04/2012
;;;     1 producer and N consumer, but they do not block, because the
;;;     queue is large enough and pre-filled with messages
(test-with-timeout mp.mbox.1-to-n-non-blocking
  (loop
    for n from 1 to 10
    for m = (round 128 n)
    for length = (* n m)
    for mbox = (mp:make-mailbox :name "mbox.1-to-n-non-blocking" :count length)
    for flags = (make-array length :initial-element nil)
    for aux = (loop for i from 0 below length
                    do (mp:mailbox-send mbox i))
    for consumers = (loop for i from 0 below n
                          collect (mp:process-run-function
                                   "mbox.1-to-n-consumer"
                                   #'(lambda ()
                                       (loop for i from 0 below m
                                             for msg = (mp:mailbox-read mbox)
                                             do (setf (aref flags msg) t)))))
    do (mapc #'mp:process-join consumers)
       (is (every #'identity flags))
       (is (mp:mailbox-empty-p mbox))))

;;; Date: 14/04/2012
;;;     1 producer and N consumers, which block, because the producer
;;;     is started _after_ them and is slower.
(test-with-timeout mp.mbox.1-to-n-blocking
  (loop for n from 1 to 10
        for m = (round 10000 n)
        for length = (* n m)
        for mbox = (mp:make-mailbox :name "mp.mbox.1-to-n-blocking" :count length)
        for flags = (make-array length :initial-element nil)
        for consumers = (loop for i from 0 below n
                              collect (mp:process-run-function
                                       "mbox.1-to-n-consumer"
                                       #'(lambda ()
                                           (loop for i from 0 below m
                                                 for msg = (mp:mailbox-read mbox)
                                                 do (setf (aref flags msg) t)))))
        do (loop for i from 0 below length
                 do (mp:mailbox-send mbox i))
        do (mapc #'mp:process-join consumers)
           (is (every #'identity flags))
           (is (mp:mailbox-empty-p mbox))))


;;; Date: 2016-11-10
;;; Description: CLASS-OF called on rwlock crashed lisp process
(test rwlock
  (finishes (class-of (mp:make-rwlock))))


;;; Date: 2016-10-05
;;; From: Daniel Kochmański
;;; Description:
;;;
;;;     HOLDING-LOCK-P verifies, if the current process holds the
;;;     lock.
;;;
(test-with-timeout mp.mutex.holding-lock-p
  (let ((lock (mp:make-lock :name "mutex.holding-lock-p" :recursive nil)))
    (is-false (mp:holding-lock-p lock))
    (mp:with-lock (lock)
      (is-true (mp:holding-lock-p lock))
      (mp:process-run-function
       "mutex.holding-lock-p"
       #'(lambda () (is-false (mp:holding-lock-p lock)))))
    (is-false (mp:holding-lock-p lock))
    (mp:process-run-function
     "mutex.holding-lock-p"
     #'(lambda () (is-false (mp:holding-lock-p lock))))))


;; Atomics

(ext:with-clean-symbols (test-struct test-class *x*)
 (defstruct (test-struct :atomic-accessors)
   (slot1 0)
   (slot2 1 :read-only t))
 (defclass test-class ()
   ((slot1 :initform 0)
    (slot2)))
 (defvar *x*)
;;; Date: 2018-09-21
;;; From: Marius Gerbershagen
;;; Description:
;;;
;;;     Verifies that atomic-update works correctly.
;;;
 (test-with-timeout mp.atomics.atomic-update
       (let* ((n-threads 100)
              (n-updates 1000)
              (n-total (* n-threads n-updates)))
         (let ((cons (cons 0 0))
               (vector (make-array 3 :initial-element 0))
               (object (make-instance 'test-class))
               (struct (make-test-struct)))
           (setf *x* 0)
           (setf (get '*x* 'n) 0)
           (mapc #'mp:process-join
                 (loop repeat n-threads
                    collect (mp:process-run-function
                             ""
                             (lambda ()
                               (loop repeat n-updates do
                                    (mp:atomic-update (cdr cons) #'1+)
                                    (mp:atomic-update (car cons) #'1+)
                                    (mp:atomic-update (svref vector 1) #'1+)
                                    (mp:atomic-update (symbol-value '*x*) #'1+)
                                    (mp:atomic-update (symbol-plist '*x*)
                                                      #'(lambda (plist)
                                                          (list (first plist)
                                                                (1+ (second plist)))))
                                    (mp:atomic-update (slot-value object 'slot1) #'1+)
                                    (mp:atomic-update (test-struct-slot1 struct) #'1+))))))
           (is (car cons) n-total)
           (is (cdr cons) n-total)
           (is (svref vector 1) n-total)
           (is *x* n-total)
           (is (get '*x* 'n) n-total)
           (is (slot-value object 'slot1) n-total)
           (signals error (mp:compare-and-swap (slot-value object 'slot2) 0 1))
           (is (and (eq (mp:compare-and-swap (slot-value object 'slot2) si:unbound 1) si:unbound)
                    (eq (slot-value object 'slot2) 1)))
           (signals error (eval '(mp:compare-and-swap (test-struct-slot2 struct) 1 2))))))


;;; Date: 2018-09-22
;;; From: Marius Gerbershagen
;;; Description:
;;;
;;;     Verifies that atomic-push and atomic-pop work correctly.
;;;
 (test-with-timeout mp.atomics.atomic-push/pop
       (let* ((n-threads 100)
              (n-updates 1000))
         (setf *x* nil)
         (mapc #'mp:process-join
               (loop repeat n-threads
                  collect (mp:process-run-function
                           ""
                           (lambda ()
                             (loop for i below n-updates do
                                  (mp:atomic-push i (symbol-value '*x*)))
                             (loop repeat (1- n-updates) do
                                  (mp:atomic-pop (symbol-value '*x*)))))))
         (is (length *x*) n-threads)))

;;; Date: 2018-09-29
;;; From: Marius Gerbershagen
;;; Description:
;;;
;;;     Verifies that atomic-incf and atomic-decf work correctly.
;;;
 (test-with-timeout mp.atomics.atomic-incf/decf
       (let* ((n-threads 100)
              (n-updates 1000)
              (increment (1+ (random most-positive-fixnum)))
              (final-value (+ (mod (+ (* n-threads n-updates increment)
                                      most-negative-fixnum)
                                   (* -2 most-negative-fixnum))
                              most-negative-fixnum)))
         (let ((cons (cons 0 0))
               (vector (make-array 3 :initial-element 0))
               (object (make-instance 'test-class)))
           (setf *x* 0)
           (mapc #'mp:process-join
                 (loop repeat n-threads
                    collect (mp:process-run-function
                             ""
                             (lambda ()
                               (loop repeat n-updates do
                                    (mp:atomic-incf (cdr cons) increment)
                                    (mp:atomic-incf (car cons) increment)
                                    (mp:atomic-incf (svref vector 1) increment)
                                    (mp:atomic-incf (symbol-value '*x*) increment)
                                    (mp:atomic-incf (slot-value object 'slot1) increment))))))
           (is (car cons) final-value)
           (is (cdr cons) final-value)
           (is (svref vector 1) final-value)
           (is *x* final-value)
           (is (slot-value object 'slot1) final-value)
           (mapc #'mp:process-join
                 (loop repeat n-threads
                    collect (mp:process-run-function
                             ""
                             (lambda ()
                               (loop repeat n-updates do
                                    (mp:atomic-decf (cdr cons) increment)
                                    (mp:atomic-decf (car cons) increment)
                                    (mp:atomic-decf (svref vector 1) increment)
                                    (mp:atomic-decf (symbol-value '*x*) increment)
                                    (mp:atomic-decf (slot-value object 'slot1) increment))))))
           (is (car cons) 0)
           (is (cdr cons) 0)
           (is (svref vector 1) 0)
           (is *x* 0)
           (is (slot-value object 'slot1) 0)))))

;;; Date: 2019-02-05
;;; From: Daniel Kochmański
;;; Description:
;;;
;;;     Verifies that CAS expansion may be removed.
;;;
(ext:with-clean-symbols (*obj* foo)
  (test mp.atomics.defcas/remcas
        (mp:defcas foo (lambda (object old new)
                         (assert (consp object))
                         (setf (car object) old
                               (cdr object) new)))
        (defparameter *obj* (cons nil nil))
        (eval `(mp:compare-and-swap (foo *obj*) :car :cdr))
        (is (eql (car *obj*) :car))
        (is (eql (cdr *obj*) :cdr))
        (mp:remcas 'foo)
        (signals error (eval `(mp:compare-and-swap (foo *obj*) :car :cdr)))))

;;; Date: 2019-02-07
;;; From: Daniel Kochmański
;;; Description:
;;;
;;;     Verifies that CAS modifications honor the package locks.
;;;
(test mp.atomics.cas-locked-package
      (signals package-error (mp:defcas cl:car (lambda (obj old new) nil)))
      (signals package-error (mp:remcas 'cl:car))
      (finishes (mp:defcas cor (lambda (obj old new) nil)))
      (finishes (mp:remcas 'cor)))


;; Barriers

;;; Date: 2020-08-14
;;; From: Daniel Kochmański
;;; Description:
;;;
;;;     Smoke tests for barriers.
;;;

(test mp.barrier.slots
      (let ((barrier (mp:make-barrier 3 :name 'foo)))
        (is (eq 'foo (mp:barrier-name barrier)))
        (is (= 3 (mp:barrier-count barrier)))
        (is (= 0 (mp:barrier-arrivers-count barrier)))))

(let (barrier before-barrier after-barrier all-processes mutex)
  (labels ((try-barrier ()
             (push (mp:process-run-function
                    "try-barrier"
                    (lambda ()
                      (mp:with-lock (mutex)
                        (incf before-barrier))
                      (mp:barrier-wait barrier)
                      (mp:with-lock (mutex)
                        (incf after-barrier))))
                   all-processes))
           (check-barrier (before after arrivers)
             (try-barrier)
             (loop until (= before before-barrier))
             (loop until (= after after-barrier))
             (loop until (= arrivers (mp:barrier-arrivers-count barrier))))
           (wake-barrier ()
             (mp:barrier-unblock barrier :kill-waiting nil))
           (kill-barrier ()
             (mp:barrier-unblock barrier :kill-waiting t)))

    (test-with-timeout mp.barrier.blocking
      (setf barrier (mp:make-barrier 3)
            before-barrier 0
            after-barrier 0
            all-processes nil
            mutex (mp:make-lock))
      (check-barrier 1 0 1)
      (check-barrier 2 0 2)
      (check-barrier 3 3 0)
      (check-barrier 4 3 1)
      (check-barrier 5 3 2)
      (check-barrier 6 6 0)
      ;; clean up
      (mapc #'mp:process-join all-processes))

    (test-with-timeout mp.barrier.unblock-1
      (setf barrier (mp:make-barrier 3)
            before-barrier 0
            after-barrier 0
            all-processes nil
            mutex (mp:make-lock))
      (check-barrier 1 0 1)
      (check-barrier 2 0 2)
      (wake-barrier)
      (mapc #'mp:process-join all-processes)
      (check-barrier 3 2 1)
      (check-barrier 4 2 2)
      (kill-barrier)
      (mapc #'mp:process-join all-processes)
      (setf all-processes nil)
      (check-barrier 5 2 1)
      ;; clean up
      (mapc #'mp:process-kill all-processes)
      (mapc #'mp:process-join all-processes))

    (test-with-timeout mp.barrier.unblock-2
      (setf barrier (mp:make-barrier 3)
            before-barrier 0
            after-barrier 0
            all-processes nil
            mutex (mp:make-lock))
      (mp:barrier-unblock barrier :disable t)
      (check-barrier 1 1 0)
      (check-barrier 2 2 0)
      (check-barrier 3 3 0)
      (check-barrier 4 4 0)
      ;; clean up
      (mapc #'mp:process-join all-processes))

    (test-with-timeout mp.barrier.unblock-3
      (setf barrier (mp:make-barrier 3)
            before-barrier 0
            after-barrier 0
            all-processes nil
            mutex (mp:make-lock))
      (mp:barrier-unblock barrier :reset-count 4)
      (check-barrier 1 0 1)
      (check-barrier 2 0 2)
      (check-barrier 3 0 3)
      (check-barrier 4 4 0)
      (mapc #'mp:process-join all-processes)
      (setf all-processes nil)
      (check-barrier 5 4 1)
      (check-barrier 6 4 2)
      ;; clean up
      (mapc #'mp:process-kill all-processes)
      (mapc #'mp:process-join all-processes))))


;; Condition variables
(test-with-timeout mp.cv.wait-and-signal
  (let* ((mutex (mp:make-lock :name "cv.wait-and-signal"))
         (cv (mp:make-condition-variable))
         (counter-before 0)
         (counter-after 0)
         (n-threads 10)
         (waiting-processes
          (loop repeat n-threads collect
               (mp:process-run-function
                "cv.wait-and-signal"
                (lambda ()
                  (mp:get-lock mutex)
                  (incf counter-before)
                  (mp:condition-variable-wait cv mutex)
                  (when (not (eq (mp:lock-owner mutex) mp:*current-process*))
                    (error "Wrong lock owner"))
                  (incf counter-after)
                  (mp:giveup-lock mutex))))))
    (loop until (mp:with-lock (mutex) (= counter-before n-threads)))
    ;; signal wakes up _at least_ one thread each time it is called (for
    ;; efficiency reasons, the posix specification explicitely allows waking
    ;; up more than one thread)
    (loop with n-signaled = 0 ; minimum number of threads that have woken up
          until (= n-signaled n-threads)
          do (mp:get-lock mutex)
             (is (eq (mp:lock-owner mutex) mp:*current-process*))
             (mp:condition-variable-signal cv)
             (mp:giveup-lock mutex)
             (is (not (eq (mp:lock-owner mutex) mp:*current-process*)))
             (loop until (mp:with-lock (mutex)
                           (and (> counter-after n-signaled)
                                (setf n-signaled counter-after)))))))

(test-with-timeout mp.cv.wait-and-broadcast
  (let* ((mutex (mp:make-lock :name "cv.wait-and-broadcast"))
         (cv (mp:make-condition-variable))
         (counter-before 0)
         (counter-after 0)
         (wakeup nil)
         (n-threads 10)
         (waiting-processes
          (loop repeat n-threads collect
               (mp:process-run-function
                "cv.wait-and-broadcast"
                (lambda ()
                  (mp:get-lock mutex)
                  (incf counter-before)
                  (loop until wakeup ; ignore spurious wakeups (allowed by posix)
                        do (mp:condition-variable-wait cv mutex))
                  (when (not (eq (mp:lock-owner mutex) mp:*current-process*))
                    (error "Wrong lock owner"))
                  (incf counter-after)
                  (mp:giveup-lock mutex))))))
    (loop until (mp:with-lock (mutex) (= counter-before n-threads)))
    ;; broadcast wakes up all threads
    (mp:get-lock mutex)
    (is (eq (mp:lock-owner mutex) mp:*current-process*))
    (setf wakeup t)
    (mp:condition-variable-broadcast cv)
    (mp:giveup-lock mutex)
    (is (not (eq (mp:lock-owner mutex) mp:*current-process*)))
    (mapc #'mp:process-join waiting-processes)
    (is (= counter-after n-threads))))

(test-with-timeout (mp.cv.timedwait-timeout 30) ; whole test times out after 30 seconds
  (let* ((mutex (mp:make-lock :name "cv.timedwait-timeout"))
         (cv (mp:make-condition-variable))
         (flag 0)
         (waiting-process
          (mp:process-run-function
           "cv.timedwait-timeout"
           (lambda ()
             (mp:get-lock mutex)
             (setf flag 1)
             ;; condition variable times out after 1 second, before
             ;; whole test times out
             (mp:condition-variable-timedwait cv mutex 1)
             (when (not (eq (mp:lock-owner mutex) mp:*current-process*))
               (error "Wrong lock owner"))
             (setf flag 2)
             (mp:giveup-lock mutex)))))
    (loop until (mp:with-lock (mutex) (/= flag 0)))
    (mp:process-join waiting-process)
    (is (null (mp:lock-owner mutex)))
    (is (= flag 2))))

(test-with-timeout (mp.cv.timedwait-signal 30) ; whole test times out after 30 seconds
  (let* ((mutex (mp:make-lock :name "cv.timedwait-signal"))
         (cv (mp:make-condition-variable))
         (flag 0)
         (waiting-process
          (mp:process-run-function
           "cv.timedwait-signal"
           (lambda ()
             (mp:get-lock mutex)
             (setf flag 1)
             ;; condition variable times out after 60 seconds, after
             ;; whole test times out
             (mp:condition-variable-timedwait cv mutex 60)
             (when (not (eq (mp:lock-owner mutex) mp:*current-process*))
               (error "Wrong lock owner"))
             (setf flag 2)
             (mp:giveup-lock mutex)))))
    (loop until (mp:with-lock (mutex) (/= flag 0)))
    (mp:get-lock mutex)
    (is (eq (mp:lock-owner mutex) mp:*current-process*))
    (mp:condition-variable-signal cv)
    (mp:giveup-lock mutex)
    (is (not (eq (mp:lock-owner mutex) mp:*current-process*)))
    (mp:process-join waiting-process)
    (is (= flag 2))))

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

