;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author: Daniel Kochmański
;;;; Created: 2017-04-28
;;;; Contains: hash-table tests

(in-package :cl-test)

(suite 'hash-tables)


;;; weak hash tables interface
(test hash-tables.fill-content
  (let ((ht (make-hash-table)))
    (setf (gethash :qux ht) 3)
    (ext:hash-table-fill ht '((:foo . 1) (:bar . 2)))
    (= (length (ext:hash-table-content ht)) 3)))

(test hash-tables.weak-key
  (let ((ht (make-hash-table :weakness :key))
	(key (make-random-state))
	(val (make-random-state)))
    (setf (gethash key ht) val)
    (destructuring-bind ((k . v)) (ext:hash-table-content ht)
      (is k 'ext:weak-pointer)
      (is v 'random-state))))

(test hash-tables.weak-value
  (let ((ht (make-hash-table :weakness :value))
	(key (make-random-state))
	(val (make-random-state)))
    (setf (gethash key ht) val)
    (destructuring-bind ((k . v)) (ext:hash-table-content ht)
      (is v 'ext:weak-pointer)
      (is k 'random-state))))

(test hash-tables.weak-key-and-value
  (let ((ht (make-hash-table :weakness :key-and-value))
	(key (make-random-state))
	(val (make-random-state)))
    (setf (gethash key ht) val)
    (destructuring-bind ((k . v)) (ext:hash-table-content ht)
      (is k 'ext:weak-pointer)
      (is v 'ext:weak-pointer))))

(test hash-tables.weak-key-or-value
  (let ((ht (make-hash-table :weakness :key-or-value))
	(key (make-random-state))
	(val (make-random-state)))
    (setf (gethash key ht) val)
    (destructuring-bind ((k . v)) (ext:hash-table-content ht)
      (is k 'ext:weak-pointer)
      (is v 'ext:weak-pointer))))

(test hash-tables.weak-nil
  (let ((ht (make-hash-table :weakness nil))
	(key (make-random-state))
	(val (make-random-state)))
    (setf (gethash key ht) val)
    (destructuring-bind ((k . v)) (ext:hash-table-content ht)
      (is k 'random-state)
      (is v 'random-state))))

;;; For some corner cases weak hash-tables returned unnormalized
;;; values. Then we had a regression in gethash which tried to
;;; normalize already normalized values. We clearly need a simple test
;;; whenever hash tables work correctly.
(test hash-tables.weak-faithful
      (let ((hts (list (make-hash-table :weakness nil)
                       (make-hash-table :weakness :key)
                       (make-hash-table :weakness :value)
                       (make-hash-table :weakness :key-or-value)
                       (make-hash-table :weakness :key-and-value)))
            (keys (list :a :b :c))
            (vals (list :x :y :z)))
        ;; ensure basic set/get on null entries
        (mapc (lambda (ht)
                (mapc (lambda (k v)
                        (is (null (gethash k ht)))
                        (setf (gethash k ht) v)
                        (is (eql (gethash k ht) v)
                            "gethash ~s = ~s, should be ~s, weakness is ~s"
                            k (gethash k ht) v (ext:hash-table-weakness ht)))
                      keys vals))
              hts)
        ;; ensure that values get updated for a known key
        (mapc (lambda (ht)
                (setf (gethash :c ht) :z-prim)
                (is (eql (gethash :c ht) :z-prim)))
              hts)
        ;; ensure maphash
        (mapc (lambda (ht)
                (let ((i 0))
                  (maphash (lambda (k v)
                             (incf i)
                             (is (eql v (ecase k
                                          (:a :x)
                                          (:b :y)
                                          (:c :z-prim)))))
                           ht)
                  (is (= i 3))))
              hts)))

(test hash-tables.weak-err
      (signals simple-type-error (make-hash-table :weakness :whatever)))


;;; Synchronization
(test hash-tables.sync
  (let ((ht (make-hash-table :synchronized t)))
    (is-true (ext:hash-table-synchronized-p ht))
    (setf (gethash :foo ht) 3)
    (setf (gethash :bar ht) 4)
    (is (= 2 (hash-table-count ht)))
    (is (= 3 (gethash :foo ht)))
    (is-true (remhash :bar ht))
    (is (= 1 (hash-table-count ht)))))


;;; generic test and hash functions

;;; In this test we provide an equality predicate which distinguishes
;;; only two types of numbers: odd and even. HT is synchronized
;;; because we want also to check, if lock is not hogged by errors
;;; inside our function (we pass string for that purpose).
(test hash-tables.custom
  (flet ((not-so-fancy-equals (x y)
           (if (zerop x)
               (= x y)
               (eql (evenp x) (evenp y))))
         (not-so-fancy-hash (x)
           (cond ((zerop x) 0)
                 ((evenp x) 1)
                 (T 2))))
    (signals error (make-hash-table :test #'not-so-fancy-equals))
    (let ((ht (make-hash-table :test #'not-so-fancy-equals
                               :hash-function #'not-so-fancy-hash
                               :synchronized t)))
      (finishes
        (setf (gethash 13 ht) 42
              (gethash 12 ht) 33
              (gethash 10 ht) 55))
      (is (= (gethash 12 ht) 55))
      (is (= (gethash 1  ht) 42))
      (is (null (gethash 0 ht)))
      (signals error (gethash "foobar" ht))
      (signals error (setf (gethash "foobar" ht) 15))
      (finishes (remhash 3 ht))
      (finishes (hash-table-test ht))
      (is (null (gethash 1 ht)))
      (finishes (setf (gethash 55 ht) 0))
      (is (= (gethash 13 ht) 0)))))


;;; More weak hash table tests. See #761.

;;; Weak hash tables autonormalized GC-ed entries creating holes, which (unlike
;;; remhash) were not filled. That leads to invalid results for GETHASH.
(test hash-tables.weak-collision
  (let ((ht (make-hash-table :weakness :key :size 2 :test #'eq)))
    (loop repeat 15 do (setf (gethash (gensym) ht) 42))
    (setf (gethash :sm ht) t)
    (ext:gc t)
    (is (not (null (gethash :sm ht))))))

;;; MAPHASH mapped also garbage-collected entries leading to dud calls to the
;;; continuation.
(test hash-tables.weak-maphash
  (let ((ht (make-hash-table :weakness :key :size 2 :test #'eq)))
    (loop repeat 4 do (setf (gethash (gensym) ht) 42))
    (setf (gethash :sm ht) t)
    (ext:gc t)
    (maphash (lambda (k v)
               (is (not (null k)))
               (is (not (null v))))
             ht)))

;;;
;;; This test leads to a segfault while running tests, hence it is disabled.
;;; So what's going on?
;;;
;;; 1. si_gc disables interrupts for the env for the time GC_collect is running.
;;; 2. GC_collect calls GC_finalize > GC_make_disappearing_links_disappear
;;; 3. Finally GC_is_marked calls mark_bit_from_hdr that marks env
;;;
;;; Note that this failure happens sometimes with two tests above, but it is
;;; much rarer -- the test below more or less deterministically crashes ECL.
;;;
#+ (or)
(test hash-tables.gc-shakedown
  (dolist (sk '(nil :key :value :key-or-value :key-and-value))
    (let ((ht (make-hash-table :weakness sk :size 2 :test #'eq)))
      (progn
        (progn
          (loop repeat 4 do (setf (gethash (gensym) ht) 42))
          (setf (gethash :sm ht) t)
          (ext:gc t))
        "Crashed on ~a." sk))))
