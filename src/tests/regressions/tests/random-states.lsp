;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Daniel Kochmański
;;;; Created:  2015-09-21
;;;; Contains: Random state tests

(in-package :cl-test)

;; Trivial case
(deftest random-states.0001
    (numberp (random 18))
  T)

;; Check if we can generate random number from a read random state
(deftest random-states.0002
    (numberp (random 18 #$1))
  T)

;; Check if we can generate random number from a new random state
(deftest random-states.0003
    (numberp (random 18 (make-random-state)))
  T)

;; Check if we can copy use copied random state from reader
(deftest random-states.0004
    (numberp (random 18 (make-random-state #$1)))
  T)

;; Check if the same seed produces the same result
(deftest random-states.0005
    (= (random 18 #$1)
       (random 18 #$1)
       (random 18 #$1))
  T)

;; Check if we get the same table from the same seed
(deftest random-states.0005
    (let ((*print-readably* t)
          (rs (make-random-state #$1)))
      (equalp
       (format nil "~S" #$1)
       (format nil "~S" rs)))
  T)

;; Check if we can read back the random state
(deftest random-states.0006
    (let* ((*print-readably* t)
           (rs (make-random-state #$1))
           (rs-read (read-from-string
                     (format nil "~S" rs))))
      (equalp
       (format nil "~S" rs-read)
       (format nil "~S" rs)))
  T)
