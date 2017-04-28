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

(test hash-tables.weak-err
  (signals simple-type-error (make-hash-table :weakness :whatever)))
