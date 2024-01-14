;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  CMPOPT-SEQUENCE  Optimization of SEQUENCE functions
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    See file 'LICENSE' for the copyright details.

(in-package "COMPILER")

(defun constant-function-expression (form)
  (and (consp form)
       (member (first form) '(quote function lambda))))

(defun seq-opt-test-function (test-flag test %test)
  (cond ((null test-flag)
         #'(lambda (v1 v2) `(eql ,v1 ,v2)))
        ((eq test-flag :test-not)
         (if (constant-function-expression test)
             #'(lambda (v1 v2) `(not (funcall ,test ,v1 ,v2)))
             #'(lambda (v1 v2) `(not (funcall ,%test ,v1 ,v2)))))
        (t
         (if (constant-function-expression test)
             #'(lambda (v1 v2) `(funcall ,test ,v1 ,v2))
             #'(lambda (v1 v2) `(funcall ,%test ,v1 ,v2))))))

(defun seq-opt-key-function (key %key)
  (cond ((null key)
         #'identity)
        ((constant-function-expression key)
         #'(lambda (elt) `(funcall ,key ,elt)))
        (t
         #'(lambda (elt) `(funcall (or ,%key #'identity) ,elt)))))

#+(or)
(define-compiler-macro si::make-seq-iterator (seq &optional (start 0))
  (ext:with-clean-symbols (%seq %start)
    `(let ((%seq (optional-type-check ,seq sequence))
           (%start ,start))
       (cond ((consp %seq)
              (nthcdr %start %seq))
             ((< %start (length %seq))
              %start)
             (t
              nil)))))

#+(or)
(define-compiler-macro si::seq-iterator-ref (seq iterator)
  (ext:with-clean-symbols (%seq %iterator)
    `(let* ((%seq ,seq)
            (%iterator ,iterator))
       (declare (optimize (safety 0)))
       (if (ext:fixnump %iterator)
           ;; Fixnum iterators are always fine
           (aref %seq %iterator)
           ;; Error check in case we may have been passed an improper list
           (si:cons-car (ext:checked-value cons %iterator))))))

#+(or)
(define-compiler-macro si::seq-iterator-next (seq iterator)
  (ext:with-clean-symbols (%seq %iterator)
    `(let* ((%seq ,seq)
            (%iterator ,iterator))
       (declare (optimize (safety 0)))
       (if (ext:fixnump %iterator)
           (let ((%iterator (1+ (ext:truly-the fixnum %iterator))))
             (declare (fixnum %iterator))
             (and (< %iterator (length (ext:truly-the vector %seq)))
                  %iterator))
           (si:cons-cdr %iterator)))))

(defmacro do-in-seq ((%elt %sequence &key %start %end end output) &body body)
  (ext:with-unique-names (%iterator %counter)
    (let* ((counter (if end
                        `(- (or ,%end most-positive-fixnum) ,%start)
                        0))
           (test (if end
                     `(and ,%iterator (plusp ,%counter))
                     %iterator)))
      `(let* ((,%iterator (si::make-seq-iterator ,%sequence ,%start))
              (,%counter ,counter))
         (declare (:read-only ,%sequence ,%start ,%counter)
                  (ignorable ,%counter)
                  (fixnum ,%counter))
         (loop
            (unless ,test (return ,output))
            (let ((,%elt (si::seq-iterator-ref ,%sequence ,%iterator)))
              ,@body)
            (setf ,%iterator (si::seq-iterator-next ,%sequence ,%iterator))
            ,(when end
               `(decf ,%counter)))))))

;;;
;;; MEMBER
;;;

(defmacro do-in-list ((%elt %sublist %list &rest output) &body body)
  `(do* ((,%sublist ,%list (si:cons-cdr ,%sublist)))
        ((null ,%sublist) ,@output)
     (let* ((,%sublist (optional-type-check ,%sublist cons))
            (,%elt (si:cons-car ,%sublist)))
       ,@body)))

(defmacro define-seq-compiler-macro (name lambda-list &body body)
  (let ((whole (second lambda-list)))
    `(define-compiler-macro* ,name ,lambda-list
       (unless (policy-inline-sequence-functions)
         (return-from ,name ,whole))
       (when (and test test-not)
         (cmpwarn "Cannot specify both :TEST and :TEST-NOT arguments to ~A" ',name)
         (return-from ,name ,whole))
       (let* ((test-flag (cond (test :test)
                               (test-not :test-not)
                               (t nil)))
              (key-function (seq-opt-key-function key %key))
              (test-function (seq-opt-test-function test-flag
                                                    (if test test test-not)
                                                    (if test %test %test-not))))
         (declare (ignorable test-flag key-function test-function))
         ,@body))))

(define-seq-compiler-macro member (&whole whole value list &key key test test-not)
  (unless key
    (when (and (or (null test) (constant-function-expression test))
               (constant-expression-p list))
      (let ((evaluated-list (cmp-eval list)))
        (when (and (si:proper-list-p evaluated-list) (<= (length evaluated-list) 4))
          (return-from member
            `(or ,@(loop for l on evaluated-list
                         for elt = (first l)
                         collect `(and ,(funcall test-function %value `(quote ,elt))
                                       ',l)))))
        (when (or (consp evaluated-list) (symbolp evaluated-list))
          (setf list `',evaluated-list))))
    (when (or (null test-flag) (eq test-flag :test))
      (when (member test '('EQ #'EQ) :test #'equal)
        (return-from member
          `(ffi:c-inline (,%value ,%list) (:object :object) :object
                         "si_memq(#0,#1)" :one-liner t :side-effects nil)))
      (when (or (member test '('EQL #'EQL) :test #'equal) (null test))
        (return-from member
          `(ffi:c-inline (,%value ,%list) (:object :object) :object
                         "ecl_memql(#0,#1)" :one-liner t :side-effects nil)))
      (when (member test '('EQUAL #'EQUAL) :test #'equal)
        (return-from member
          `(ffi:c-inline (,%value ,%list) (:object :object) :object
                         "ecl_member(#0,#1)" :one-liner t :side-effects nil)))))
  (ext:with-unique-names (%sublist %elt)
    `(do-in-list (,%elt ,%sublist ,%list)
       (when ,(funcall test-function %value
                       (funcall key-function %elt))
         (return ,%sublist)))))

;;;
;;; ASSOC
;;;

(define-seq-compiler-macro assoc (&whole whole value list &key key test test-not)
  (unless key
    (when (or (null test-flag) (eq test-flag :test))
      (when (member test '('EQ #'EQ) :test #'equal)
        (return-from assoc
          `(ffi:c-inline (,%value ,%list) (:object :object) :object
                         "ecl_assq(#0,#1)" :one-liner t :side-effects nil)))
      (when (or (member test '('EQL #'EQL) :test #'equal) (null test))
        (return-from assoc
          `(ffi:c-inline (,%value ,%list) (:object :object) :object
                         "ecl_assql(#0,#1)" :one-liner t :side-effects nil)))
      (when (member test '('EQUAL #'EQUAL) :test #'equal)
        (return-from assoc
          `(ffi:c-inline (,%value ,%list) (:object :object) :object
                         "ecl_assoc(#0,#1)" :one-liner t :side-effects nil)))
      (when (member test '('EQUALP #'EQUALP) :test #'equal)
        (return-from assoc
          `(ffi:c-inline (,%value ,%list) (:object :object) :object
                         "ecl_assqlp(#0,#1)" :one-liner t :side-effects nil)))))
  (ext:with-unique-names (%sublist %elt %car)
    `(do-in-list (,%elt ,%sublist ,%list)
       (when ,%elt
         (let ((,%car (si:cons-car (optional-type-check ,%elt cons))))
           (when ,(funcall test-function %value
                           (funcall key-function %car))
             (return ,%elt)))))))

;;;
;;; FIND
;;;

(define-seq-compiler-macro find (&whole whole value sequence &key key test test-not (start 0) end from-end)
  (when from-end
    (return-from find whole))
  (ext:with-unique-names (%elt)
    `(do-in-seq (,%elt ,%sequence :%start ,%start :%end ,%end :end ,end)
       (when ,(funcall test-function %value
                       (funcall key-function %elt))
         (return ,%elt)))))
