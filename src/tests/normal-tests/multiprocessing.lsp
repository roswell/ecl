;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Multithreading API regression tests

(in-package :cl-test)

(suite 'mp)


;; Auxiliary routines for multithreaded tests

(defun kill-and-wait (process-list &optional original wait)
  "Kills a list of processes, which may be the difference between two lists,
waiting for all processes to finish. Currently it has no timeout, meaning
it may block hard the lisp image."
  (let ((process-list (set-difference process-list original)))
    (when (member mp:*current-process* process-list)
      (error "Found myself in the kill list"))
    (mapc #'mp:process-kill process-list)
    (when wait
      (loop for i in process-list
         do (mp:process-join i)))
    process-list))

(defun mp-test-run (closure)
  (let* ((all-processes (mp:all-processes))
         (output (multiple-value-list (funcall closure))))
    (sleep 0.2) ; time to exit some processes
    (let ((leftovers (kill-and-wait (mp:all-processes) all-processes)))
      (cond (leftovers
             (format t "~%;;; Stray processes: ~A" leftovers))
            (t
             (values-list output))))))

(defmacro def-mp-test (name body expected-value)
  "Runs some test code and only returns the output when the code exited without
creating stray processes."
  (let ((all-processes (gensym))
        (output (gensym))
        (leftover (gensym)))
    `(test ,name
       (is-equal
        (mp-test-run #'(lambda () ,body))
        ,expected-value))))


;; Locks

;;; Date: 04/09/2009
;;; From: Matthew Mondor
;;; Fixed: 05/09/2009 (Juanjo)
;;; Description:
;;;
;;;     When a WITH-LOCK is interrupted, it is not able to release
;;;     the resulting lock and an error is signaled.
;;;
(test mp-0001-with-lock
    (let ((flag t)
          (lock (mp:make-lock :name "mp-0001-with-lock" :recursive nil)))
      (mp:with-lock (lock)
        (let ((background-process
               (mp:process-run-function
                "mp-0001-with-lock"
                #'(lambda ()
                    (handler-case
                        (progn
                          (setf flag 1)
                          (mp:with-lock (lock)
                            (setf flag 2)))
                      (error (c)
                        (princ c)(terpri)
                        (setf flag c)))
                    (setf flag 2)))))
          ;; The background process should not be able to get
          ;; the lock, and will simply wait. Now we interrupt it
          ;; and the process should gracefully quit, without
          ;; signalling any serious condition
          (and (progn (sleep 1)
                      (is (mp:process-kill background-process)))
               (progn (sleep 1)
                      (is (not (mp:process-active-p background-process))))
               (is (eq flag 1)))))))


;; Semaphores

;;; Date: 14/04/2012
;;;     Ensure that at creation name and counter are set
(test sem-make-and-counter
  (is (loop with name = "sem-make-and-counter"
         for count from 0 to 10
         for sem = (mp:make-semaphore :name name :count count)
         always (and (eq (mp:semaphore-name sem) name)
                     (= (mp:semaphore-count sem) count)
                     (zerop (mp:semaphore-wait-count sem))))))

;;; Date: 14/04/2012
;;;     Ensure that signal changes the counter by the specified amount
(test sem-signal-semaphore-count
  (is
   (loop with name = "sem-signal-semaphore-count"
      for count from 0 to 10
      always (loop for delta from 0 to 10
                for sem = (mp:make-semaphore :name name :count count)
                always (and (= (mp:semaphore-count sem) count)
                            (null (mp:signal-semaphore sem delta))
                            (= (mp:semaphore-count sem ) (+ count delta)))))))

;;; Date: 14/04/2012
;;;     A semaphore with a count of zero blocks a process
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
;;;     We can signal multiple processes
(test sem-signal-n-processes
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
         (sleep 0.1) ; let threads settle on semaphore
         (and (is (zerop counter))
              (is (every #'mp:process-active-p all-process))
              (is (= (mp:semaphore-wait-count sem) count)
                  "Number of threads waitng on semaphore should be ~s (but is ~s)."
                   count (mp:semaphore-wait-count sem))
              (is (progn (mp:signal-semaphore sem count)
                         (sleep 0.2)
                         (= counter count))
                  "Counter should be ~s (but is ~s)." count counter)
              (is (= (mp:semaphore-count sem) 0))))))

;;; Date: 14/04/2012
;;;     When we signal N processes and N+M are waiting, only N awake
(test sem-signal-only-n-processes
  (loop for m from 1 upto 3 always
       (loop for n from 1 upto 4 always
            (let* ((counter 0)
                   (lock (mp:make-lock :name "sem-signal-n-processes"))
                   (sem (mp:make-semaphore :name "sem-signal-n-processs"))
                   (all-process
                    (loop for i from 1 upto (+ n m)
                       collect (mp:process-run-function
                                "sem-signal-n-processes"
                                #'(lambda ()
                                    (mp:wait-on-semaphore sem)
                                    (mp:with-lock (lock) (incf counter)))))))
              (sleep 0.1) ; let threads settle on semaphore
              (and (is (zerop counter))
                   (is (every #'mp:process-active-p all-process))
                   (is (= (mp:semaphore-wait-count sem) (+ m n))
                       "Number of threads waiting on semaphore should be ~s (but is ~s)."
                       (+ m n) (mp:semaphore-wait-count sem))
                   (is (progn (mp:signal-semaphore sem n)
                              (sleep 0.02)
                              (= counter n)))
                   (is (= (mp:semaphore-wait-count sem) m)
                       "Number of threads waitng on semaphore should be ~s (but is ~s)."
                       m (mp:semaphore-wait-count sem))
                   (is (progn (mp:signal-semaphore sem m)
                              (sleep 0.02)
                              (= counter (+ n m)))
                       "Counter should be ~s (but is ~s)." (+ n m) counter))))))

;;; Date: 14/04/2012
;;;     It is possible to kill processes waiting for a semaphore.
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

;;; Date: 14/04/2012
;;;     When we kill a process, it is removed from the wait queue.
;;;
(def-mp-test sem-interrupt-updates-queue
    (let* ((sem (mp:make-semaphore :name "sem-interrupt-updates-queue"))
           (process (mp:process-run-function
                     "sem-interrupt-updates-queue"
                     #'(lambda () (mp:wait-on-semaphore sem)))))
      (sleep 0.2)
      (and (= (mp:semaphore-wait-count sem) 1)
           (mp:process-active-p process)
           (progn (mp:process-kill process)
                  (sleep 0.2)
                  (not (mp:process-active-p process)))
           (zerop (mp:semaphore-wait-count sem))
           t))
  t)

;;; Date: 14/04/2012
;;;     When we kill a process, it signals another one. This is tricky,
;;;     because we need the awake signal to arrive _after_ the process is
;;;     killed, but the process must still be in the queue for the semaphore
;;;     to awake it. The way we solve this is by intercepting the kill signal.
;;;
(test sem-interrupted-resignals
  (let* ((sem (mp:make-semaphore :name "sem-interrupted-resignals"))
         (flag1 nil)
         (flag2 nil)
         (process1 (mp:process-run-function
                    "sem-interrupted-resignals"
                    #'(lambda ()
                        (unwind-protect
                             (mp:wait-on-semaphore sem)
                          (sleep 4)
                          (setf flag1 t)
                          ))))
         (process2 (mp:process-run-function
                    "sem-interrupted-resignals"
                    #'(lambda ()
                        (mp:wait-on-semaphore sem)
                        (setf flag2 t)))))
    (sleep 0.2)
    (and (is (= (mp:semaphore-wait-count sem) 2))
         (is (mp:process-active-p process1))
         (is (mp:process-active-p process2))
         ;; We kill the process but ensure it is still running
         (is (progn (mp:process-kill process1)
                    (mp:process-active-p process1)))
         (is (null flag1))
         ;; ... and in the queue
         (is (= (mp:semaphore-wait-count sem) 2))
         ;; We awake it and it should awake the other one
         (is (progn (format t "~%;;; Signaling semaphore")
                    (mp:signal-semaphore sem)
                    (sleep 1)
                    (zerop (mp:semaphore-wait-count sem))))
         (is flag2))))

;;; Date: 14/04/2012
;;;     1 producer and N consumers, non-blocking, because the initial count
;;;     is larger than the consumed data.
(def-mp-test sem-1-to-n-non-blocking
    (loop with counter = 0
       with lock = (mp:make-lock :name "sem-1-to-n-communication")
       for n from 1 to 10
       for m = (round 128 n)
       for length = (* n m)
       for sem = (mp:make-semaphore :name "sem-1-to-n-communication" :count length)
       for producers = (progn
                         (setf counter 0)
                         (loop for i from 0 below n
                            collect (mp:process-run-function
                                     "sem-1-to-n-consumer"
                                     #'(lambda ()
                                         (loop for i from 0 below m
                                            do (mp:wait-on-semaphore sem)
                                            do (mp:with-lock (lock) (incf counter)))))))
       do (mapc #'mp:process-join producers)
       always (and (= counter length)
                   (zerop (mp:semaphore-count sem))
                   (zerop (mp:semaphore-wait-count sem))))
  t)

;;; Date: 14/04/2012
;;;     1 producer and N consumers, blocking due to a slow producer.
(def-mp-test sem-1-to-n-blocking
    (loop with lock = (mp:make-lock :name "sem-1-to-n-communication")
       for n from 1 to 10
       for m = (round 10000 n)
       for length = (* n m)
       for sem = (mp:make-semaphore :name "sem-1-to-n-communication" :count 0)
       for counter = 0
       for producers = (loop for i from 0 below n
                          collect (mp:process-run-function
                                   "sem-1-to-n-consumer"
                                   #'(lambda ()
                                       (loop for i from 0 below m
                                          do (mp:wait-on-semaphore sem))
                                       (mp:with-lock (lock) (incf counter)))))
       do (loop for i from 0 below length
             do (mp:signal-semaphore sem))
       do (mapc #'mp:process-join producers)
       always (and (= counter n)
                   (zerop (mp:semaphore-count sem))
                   (zerop (mp:semaphore-wait-count sem))))
  t)


;; Mutexes
;;; Date: 12/04/2012
;;;     Non-recursive mutexes should signal an error when they
;;;     cannot be relocked.
(test mutex-001-recursive-error
  (is-true
   (let* ((mutex (mp:make-lock :name 'mutex-001-recursive-error)))
     (and
      (mp:get-lock mutex)
      (eq (mp:lock-owner mutex) mp:*current-process*)
      (handler-case
          (progn (mp:get-lock mutex) nil)
        (error (c) t))
      (mp:giveup-lock mutex)
      (null (mp:lock-owner mutex))
      (zerop (mp:lock-count mutex))))))

;;; Date: 12/04/2012
;;;     Recursive locks increase the counter.
(test mutex-002-recursive-count
  (is-true
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
      (zerop (mp:lock-count mutex))))))


;;; Date: 12/04/2012
;;;     When multiple threads compete for a mutex, they should
;;;     all get the same chance of accessing the resource
;;;
(def-mp-test mutex-003-fairness
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

;;; Date: 12/04/2012
;;;     It is possible to kill processes waiting for a lock. We launch a lot of
;;;     processes, 50% of which are zombies: they acquire the lock and do not
;;;     do anything. These processes are then killed, resulting in the others
;;;     doing their job.
;;;
(def-mp-test mutex-004-interruptible
    (let* ((mutex (mp:make-lock :name "mutex-003-fairness"))
           (nprocesses 20)
           (counter 0))
      (flet ((normal-thread ()
               (mp:with-lock (mutex)
                 (incf counter)))
             (zombie-thread ()
               (mp:with-lock (mutex)
                 (loop (sleep 10)))))
        (let* ((all-processes (loop for i from 0 below nprocesses
                                 for zombie = (zerop (mod i 2))
                                 for fn = (if zombie #'zombie-thread #'normal-thread)
                                 collect (cons zombie
                                               (mp:process-run-function
                                                "mutex-003-fairness"
                                                fn))))
               (zombies (mapcar #'cdr (remove-if-not #'car all-processes))))
          (and (zerop counter) ; No proces works because the first one is a zombie
               (kill-and-wait zombies)
               (progn (sleep 0.2) (= counter (/ nprocesses 2)))
               (not (mp:lock-owner mutex))
               t))))
  t)


;; Mailbox

;;; Date: 14/04/2012
;;;     Ensure that at creation name and counter are set, and mailbox is empty.
(test mailbox-make-and-counter
  (is
   (loop with name = "mbox-make-and-counter"
      for count from 4 to 63
      for mbox = (mp:make-mailbox :name name :count count)
      always (and (eq (mp:mailbox-name mbox) name)
                  (>= (mp:mailbox-count mbox) count)
                  (mp:mailbox-empty-p mbox)))))

;;; Date: 14/04/2012
;;;     Ensure that the mailbox works in a nonblocking fashion (when the
;;;     number of messages < mailbox size in a single producer and single
;;;     consumer  setting. We do not need to create new threads for this.
(test mbox-mailbox-nonblocking-io-1-to-1
  (is
   (loop with count = 30
      with name = "mbox-mailbox-nonblocking-io-1-to-1"
      with mbox = (mp:make-mailbox :name name :count count)
      for l from 1 to 10
      for messages = (loop for i from 1 to l
                        do (mp:mailbox-send mbox i)
                        collect i)
      always 
        (and (not (mp:mailbox-empty-p mbox))
             (equalp (loop for i from 1 to l
                        collect (mp:mailbox-read mbox))
                     messages)
             (mp:mailbox-empty-p mbox)))))

;;; Date: 14/04/2012
;;;     The mailbox blocks a process when it saturates the write queue.
(def-mp-test mbox-blocks-1-to-1
    (let* ((flag nil)
           (mbox (mp:make-mailbox :name "mbox-signal-one" :count 32))
           (size (mp:mailbox-count mbox))
           (a-process (mp:process-run-function
                       "mbox-signal-one-process"
                       #'(lambda ()
                           ;; This does not block
                           (loop for i from 1 to size
                              do (mp:mailbox-send mbox i))
                           ;; Here we block
                           (setf flag t)
                           (mp:mailbox-send mbox (1+ size))
                           ;; Now we unblock
                           (setf flag nil)))))
      (sleep 0.2) ; give time for all messages to arrive
      (and (not (mp:mailbox-empty-p mbox)) ; the queue has messages
           (mp:process-active-p a-process) ; the process is active
           flag ; and it is blocked
           (loop for i from 1 to (1+ size) ; messages arrive in order
              always (= i (mp:mailbox-read mbox)))
           (null flag) ; and process unblocked
           (mp:mailbox-empty-p mbox)
           t))
  t)

;;; Date: 14/04/2012
;;;     N producers and 1 consumer
(def-mp-test mbox-n-to-1-communication
    (loop with length = 10000
       with mbox = (mp:make-mailbox :name "mbox-n-to-1-communication" :count 128)
       for n from 1 to 10
       for m = (round length n)
       for messages = (loop for i from 0 below (* n m) collect i)
       for producers = (loop for i from 0 below n
                          do (mp:process-run-function
                              "mbox-n-to-1-producer"
                              (let ((proc-no i))
                                #'(lambda ()
                                    (loop for i from 0 below m
                                       for msg = (+ i (* proc-no m))
                                       do (mp:mailbox-send mbox msg))))))
       always (and (equalp
                    (sort (loop for i from 1 to (* n m)
                             collect (mp:mailbox-read mbox))
                          #'<)
                    messages)
                   (mp:mailbox-empty-p mbox)))
  t)

;;; Date: 14/04/2012
;;;     1 producer and N consumer, but they do not block, because the
;;;     queue is large enough and pre-filled with messages
(test mbox-1-to-n-non-blocking
  (loop
     for n from 1 to 10
     for m = (round 128 n)
     for length = (* n m)
     for mbox = (mp:make-mailbox :name "mbox-1-to-n-communication" :count length)
     for flags = (make-array length :initial-element nil)
     for aux = (loop for i from 0 below length
                  do (mp:mailbox-send mbox i))
     for producers = (loop for i from 0 below n
                        do (mp:process-run-function
                            "mbox-1-to-n-consumer"
                            #'(lambda ()
                                (loop for i from 0 below m
                                   for msg = (mp:mailbox-read mbox)
                                   do (setf (aref flags msg) t)))))
     do (sleep 0.1)
     always (and (is (every #'identity flags))
                 (is (mp:mailbox-empty-p mbox)))))

;;; Date: 14/04/2012
;;;     1 producer and N consumers, which block, because the producer
;;;     is started _after_ them and is slower.
(test mbox-1-to-n-blocking
  (loop for n from 1 to 10
     for m = (round 10000 n)
     for length = (* n m)
     for mbox = (mp:make-mailbox :name "mbox-1-to-n-communication" :count length)
     for flags = (make-array length :initial-element nil)
     for producers = (loop for i from 0 below n
                        do (mp:process-run-function
                            "mbox-1-to-n-consumer"
                            #'(lambda ()
                                (loop for i from 0 below m
                                   for msg = (mp:mailbox-read mbox)
                                   do (setf (aref flags msg) t)))))
     do (loop for i from 0 below length
           do (mp:mailbox-send mbox i))
     do (sleep 0.1)
     always (and (is (every #'identity flags))
                 (is (mp:mailbox-empty-p mbox)))))


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
(test mp-holding-lock-p
  (let ((lock (mp:make-lock :name "mp-holding-lock-p" :recursive nil)))
    (is-false (mp:holding-lock-p lock))
    (mp:with-lock (lock)
      (is-true (mp:holding-lock-p lock))
      (mp:process-run-function
       "mp-holding-lock-p"
       #'(lambda () (is-false (mp:holding-lock-p lock)))))
    (is-false (mp:holding-lock-p lock))
    (mp:process-run-function
     "mp-holding-lock-p"
     #'(lambda () (is-false (mp:holding-lock-p lock))))))
