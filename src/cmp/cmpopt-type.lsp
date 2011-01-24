;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2008. Juan Jose Garcia-Ripol
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;; CMPOPT-TYPE -- Optimizations for selected types of expresions
;;;;

(in-package "COMPILER")

(defun compute-c1form-type (form)
  (let ((form (c1expr form)))
    (prog1 (c1form-primary-type form)
      (delete-c1forms form))))

(defun safe-type<= (t1 t2)
  (multiple-value-bind (subtypep known-typep)
      (subtypep t1 t2)
    (and subtypep known-typep)))

(defun c1compiler-typecase (args)
  (let* ((expr-type (compute-c1form-type (pop args)))
         (match (find expr-type args :test #'safe-type<= :key #'first)))
    (if match
        (c1progn (rest match))
        (c1nil))))

(defun c1compiler-typecases (args)
  (let* ((all-types (mapcar #'compute-c1form-type (pop args)))
         (match (find all-types args
                      :test #'(lambda (s1 s2) (every #'safe-typep<= s1 s2))
                      :key #'first)))
    (if match
        (c1progn (rest match))
        (c1nil))))

(define-compiler-macro dotimes ((variable limit &rest output) &body body)
  (multiple-value-bind (declarations body)
      (si:process-declarations body nil)
    (ext:with-unique-names (%limit)
      (setf body `(si::while (< ,variable ,%limit)
                    ,@body
                    (setq ,variable (1+ ,variable))))
      `(block nil
         (let ((,%limit ,limit))
           (declare (:read-only ,%limit))
           (ext:compiler-typecase ,limit
             (fixnum (let ((,variable 0))
                       (declare (fixnum ,variable)
                                ,@declarations)
                       ,body))
             (t (let ((,variable 0))
                  (declare ,@declarations)
                  ,body)))
           ,@output)))))
