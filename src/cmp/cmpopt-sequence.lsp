;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPOPT-SEQUENCE  Optimization of SEQUENCE functions
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defun seq-opt-key-function (key)
  (if key
      #'(lambda (v) `(funcall ,key ,v))
      #'identity))

(defun seq-opt-test-function (test-flag test)
  (cond ((null test-flag)
         (seq-opt-test-function :test '#'eql))
        ((eq test-flag :test-not)
         #'(lambda (v1 v2) `(not (funcall ,test ,v1 ,v2))))
        (t
         #'(lambda (v1 v2) `(funcall ,test ,v1 ,v2)))))

(defun seq-opt-parse-args (function args)
  (loop with key = nil
     with test = ''eql
     with test-flag = nil
     with keyword
     while args
     do (cond ((or (atom args)
                   (null (rest args))
                   (eq keyword '&allow-other-keys)
                   (not (keywordp (setf keyword (pop args)))))
               (return nil))
              ((eq keyword :key)
               (setf key (pop args)))
              ((or (eq keyword :test)
                   (eq keyword :test-not))
               (cond ((null test-flag)
                      (setf test (pop args)
                            test-flag keyword))
                     ((not (eq test-flag keyword))
                      (cmpwarn "Cannot specify :TEST and :TEST-NOT arguments to ~A"
                               function)
                      (return nil))))
              (t (return nil)))
     finally (return (values (seq-opt-key-function key)
                             (seq-opt-test-function test-flag test)
                             key
                             test-flag
                             test))))

(defun expand-member (value list &rest sequence-args)
  (multiple-value-bind (key-function test-function key test-flag test)
      (seq-opt-parse-args 'member sequence-args)
    (unless key
      (when (or (null test-flag) (eq test-flag :test))
        (when (member test '('EQ #'EQ) :test #'equal)
          (return-from expand-member
            `(ffi:c-inline (,value ,list) (:object :object) :object
                           "si_memq(#0,#1)" :one-liner t :side-effects nil)))
        (when (member test '('EQL #'EQL) :test #'equal)
          (return-from expand-member
            `(ffi:c-inline (,value ,list) (:object :object) :object
                           "ecl_memql(#0,#1)" :one-liner t :side-effects nil)))
        (when (member test '('EQUAL #'EQUAL) :test #'equal)
          (return-from expand-member
            `(ffi:c-inline (,value ,list) (:object :object) :object
                           "ecl_member(#0,#1)" :one-liner t :side-effects nil)))))
    #+(or)
    (with-clean-symbols (%value %sublist %elt)
      `(loop with %value = ,value
          with %sublist = ,list
          with %elt
          while %sublist
          when (progn
                 (locally (declare (optimize (safety 0)))
                   (setf %elt (car (optional-type-check %sublist cons))
                         %sublist (cdr (the cons %sublist))))
                 ,(funcall test-function
                           '%value
                           (funcall key-function '%elt)))
          return %sublist))))

(define-compiler-macro member (&whole whole value list &rest sequence-args)
  (if (policy-inline-sequence-functions)
      (or (apply #'expand-member (rest whole))
          whole)
      whole))

(defun expand-assoc (value list &rest sequence-args)
  (multiple-value-bind (key-function test-function key test-flag test)
      (seq-opt-parse-args 'member sequence-args)
    (unless key
      (when (or (null test-flag) (eq test-flag :test))
        (when (member test '('EQ #'EQ) :test #'equal)
          (return-from expand-assoc
            `(ffi:c-inline (,value ,list) (:object :object) :object
                           "ecl_assq(#0,#1)" :one-liner t :side-effects nil)))
        (when (member test '('EQL #'EQL) :test #'equal)
          (return-from expand-assoc
            `(ffi:c-inline (,value ,list) (:object :object) :object
                           "ecl_assql(#0,#1)" :one-liner t :side-effects nil)))
        (when (member test '('EQUAL #'EQUAL) :test #'equal)
          (return-from expand-assoc
            `(ffi:c-inline (,value ,list) (:object :object) :object
                           "ecl_assoc(#0,#1)" :one-liner t :side-effects nil)))
        (when (member test '('EQUALP #'EQUALP) :test #'equal)
          (return-from expand-assoc
            `(ffi:c-inline (,value ,list) (:object :object) :object
                           "ecl_assqlp(#0,#1)" :one-liner t :side-effects nil)))))
    #+(or)
    (with-clean-symbols (%value %sublist %elt)
      `(loop with %value = ,value
          with %sublist = ,list
          with %elt
          with %car
          while %sublist
          when (progn
                 (locally (declare (optimize (safety 0)))
                   (setf %elt (car (optional-type-check %sublist cons))
                         %car (car (optional-type-check %elt cons))
                         %sublist (cdr (the cons %sublist))))
                 ,(funcall test-function
                           '%value
                           (funcall key-function '%car)))
          return %elt))))

(define-compiler-macro assoc (&whole whole value list &rest sequence-args)
  (if (policy-inline-sequence-functions)
      (or (apply #'expand-assoc (rest whole))
          whole)
      whole))
