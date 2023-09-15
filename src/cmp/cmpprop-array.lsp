;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;  Copyright (c) 2010, Juan Jose Garcia Ripoll
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;; Type propagators for arrays.

(in-package "COMPILER")

(defun type-from-array-elt (array &aux name)
  "Input is a lisp type representing a valid subtype of ARRAY. Output is either
the array element type or NIL, denoting that we are not able to compute it. This
version only handles the simplest cases."
  (values (cond ((eq array 'string)
                 'character)
                ((eq array 'base-string)
                 'base-char)
                ((member (setf array (si::expand-deftype array))
                         '(array vector simple-array))
                 t)
                ((atom array)
                 (setf array 'array)
                 t)
                ((eq (setf name (first array)) 'OR)
                 `(OR ,@(mapcar #'type-from-array-elt (rest array))))
                ((eq (setf name (first array)) 'AND)
                 `(AND ,@(mapcar #'type-from-array-elt (rest array))))
                ((not (member (first array)
                              '(array vector simple-array)))
                 (setf array 'array)
                 t)
                ((null (rest array))
                 t)
                (t
                 (let ((x (second array)))
                   (if (eq x '*) t x))))
          array))

(def-type-propagator si::aset (fname array-type &rest indices-and-object)
  (multiple-value-bind (elt-type array-type)
      (type-from-array-elt array-type)
    (values (cons array-type
                  (nconc (make-list (1- (length indices-and-object))
                                    :initial-element 'si::index)
                         (list elt-type)))
            elt-type)))

(def-type-propagator aref (fname array-type &rest indices)
  (multiple-value-bind (elt-type array-type)
      (type-from-array-elt array-type)
    (values (list* array-type (make-list (length indices)
                                         :initial-element 'si::index))
            elt-type)))

(def-type-propagator si::row-major-aset (fname array-type index obj)
  (declare (ignore index obj))
  (multiple-value-bind (elt-type array-type)
      (type-from-array-elt array-type)
    (values (list array-type 'si::index elt-type)
            elt-type)))

(def-type-propagator row-major-aref (fname array-type index)
  (declare (ignore index))
  (multiple-value-bind (elt-type array-type)
      (type-from-array-elt array-type)
    (values (list array-type 'si::index) elt-type)))
