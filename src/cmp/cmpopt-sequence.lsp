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

(defun constant-function-expression (form)
  (and (consp form)
       (member (first form) '(quote function lambda))))

(defun seq-opt-test-function (test-flag test)
  (cond ((null test-flag)
         (values (seq-opt-test-function :test '#'eql) nil))
        ((eq test-flag :test-not)
         (multiple-value-bind (function init)
             (seq-opt-test-function :test test)
           (values #'(lambda (v1 v2) `(not ,(funcall function v1 v2)))
                   init)))
        ((constant-function-expression test)
         (values #'(lambda (v1 v2) `(funcall ,test ,v1 ,v2))
                 nil))
        (t
         (ext:with-unique-names (test-function)
           (values #'(lambda (v1 v2) `(funcall ,test-function ,v1 ,v2))
                   (list (list test-function test)))))))

(defun seq-opt-key-function (key)
  (cond ((null key)
         (values #'identity nil))
        ((constant-function-expression key)
         (values #'(lambda (elt) `(funcall ,key ,elt))
                 nil))
        (t
         (ext:with-unique-names (key-function)
           (values #'(lambda (elt) `(funcall ,key-function ,elt))
                   (list (list key-function
                               `(or ,key #'identity))))))))

(defun seq-opt-parse-args (function args &key (start-end t))
  (loop with key-flag = nil
     with key = nil
     with init = nil
     with test = ''eql
     with test-flag = nil
     with start = 0
     with end = nil
     with keyword
     while args
     do (cond ((or (atom args)
                   (null (rest args))
                   (eq keyword :allow-other-keys)
                   (not (keywordp (setf keyword (pop args)))))
               (return nil))
              ((eq keyword :key)
               (unless key-flag
                 (setf key (pop args)
                       key-flag t)))
              ((or (eq keyword :test)
                   (eq keyword :test-not))
               (cond ((null test-flag)
                      (setf test (pop args)
                            test-flag keyword))
                     ((not (eq test-flag keyword))
                      (cmpwarn "Cannot specify :TEST and :TEST-NOT arguments to ~A"
                               function)
                      (return nil))))
              ((eq keyword :start)
               (unless start-end
                 (cmpwarn "Unexpected keyword argument ~A in a call to function ~A"
                          keyword function)
                 (return nil))
               (setf start (pop args)))
              ((eq keyword :end)
               (unless start-end
                 (cmpwarn "Unexpected keyword argument ~A in a call to function ~A"
                          keyword function)
                 (return nil))
               (setf end (pop args)))
              ((eq keyword :from-end)
               (unless (null (pop args))
                 (return nil)))
              (t (return nil)))
     finally
       (multiple-value-bind (key-function key-init)
           (seq-opt-key-function key)
         (multiple-value-bind (test-function test-init)
             (seq-opt-test-function test-flag test)
         (return (values key-function
                         test-function
                         (nconc key-init test-init)
                         key-flag
                         test-flag
                         test))))))

;;;
;;; MEMBER
;;;

(defmacro do-in-list ((%elt %sublist list &rest output) &body body)
  `(do* ((,%sublist ,list (cons-cdr ,%sublist)))
        ((null ,%sublist) ,@output)
     (let* ((,%sublist (optional-type-check ,%sublist cons))
            (,%elt (cons-car ,%sublist)))
       ,@body)))

(defun expand-member (value list &rest sequence-args)
  (multiple-value-bind (key-function test-function init
                        key-flag test-flag test)
      (seq-opt-parse-args 'member sequence-args :start-end nil)
    (unless key-flag
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
    (when test-function
      (ext:with-unique-names (%value %sublist %elt %key)
        `(let ((,%value ,value)
               ,@init)
           (do-in-list (,%elt ,%sublist ,list)
             (when ,(funcall test-function %value
                             (funcall key-function %elt))
               (return ,%sublist))))))))

(define-compiler-macro member (&whole whole value list &rest sequence-args)
  (if (policy-inline-sequence-functions)
      (or (apply #'expand-member (rest whole))
          whole)
      whole))

;;;
;;; ASSOC
;;;

(defun expand-assoc (value list &rest sequence-args)
  (multiple-value-bind (key-function test-function init
                        key-flag test-flag test)
      (seq-opt-parse-args 'member sequence-args :start-end nil)
    (unless key-flag
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
    (when test-function
      (ext:with-unique-names (%value %sublist %elt %key %car)
        `(let ((,%value ,value)
               ,@init)
           (do-in-list (,%elt ,%sublist ,list)
             (when ,%elt
               (let ((,%car (cons-car (optional-type-check ,%elt cons))))
                 (when ,(funcall test-function %value
                                 (funcall key-function %car))
                   (return ,%elt))))))))))

(define-compiler-macro assoc (&whole whole value list &rest sequence-args)
  (if (policy-inline-sequence-functions)
      (or (apply #'expand-assoc (rest whole))
          whole)
      whole))
