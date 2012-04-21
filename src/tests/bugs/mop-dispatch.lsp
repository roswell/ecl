;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Sat Apr 23 10:18:00 CEST 2012
;;;; Contains: Metaobject Protocol tests

(in-package :cl-test)

;;; Date: 23/04/2012
;;; Description:
;;;
;;;	COMPUTE-APPLICABLE-METHODS-USING-CLASSES works with one and
;;;	two methods and no EQL.
;;;
(deftest mop-c-a-m-u-c-two-methods
    (progn
      (fmakunbound 'mop-fn)
      (defgeneric mop-fn (a)
	(:method ((a number)) (cos a))
	(:method ((a symbol)) a))
      (let ((m1 (compute-applicable-methods #'mop-fn (list 1.0)))
	    (m2 (compute-applicable-methods #'mop-fn (list 'a))))
	(flet ((f (class)
		 (multiple-value-list (clos:compute-applicable-methods-using-classes
				       #'mop-fn (list (find-class class))))))
	  (and (equalp (f 'number) (list m1 t))
	       (equalp (f 'real) (list m1 t))
	       (equalp (f 'symbol) (list m2 t))
	       (equalp (f 'cons) '(nil t))
	       t))))
  t)

;;; Date: 23/04/2012
;;; Description:
;;;
;;;	COMPUTE-APPLICABLE-METHODS-USING-CLASSES fails with EQL specializers
;;;	when one of the specializers is covered by the classes.
;;;
(deftest mop-c-a-m-u-c-fails-with-eql
    (progn
      (fmakunbound 'mop-fn)
      (defgeneric mop-fn (a)
	(:method ((a (eql 1))) 1)
	(:method ((a (eql 'a))) 2)
	(:method ((a float)) 3))
      (let ((m1 (compute-applicable-methods #'mop-fn (list 1)))
	    (m2 (compute-applicable-methods #'mop-fn (list 'a)))
	    (m3 (compute-applicable-methods #'mop-fn (list 1.0))))
	(flet ((f (class)
		 (multiple-value-list (clos:compute-applicable-methods-using-classes
				       #'mop-fn (list (find-class class))))))
	  (and (equalp (f 'integer) (list nil nil))
	       (equalp (f 'number) (list nil nil))
	       (equalp (f 'symbol) (list nil nil))
	       (equalp (f 'float) (list m3 t))
	       (= (length m1) 1)
	       (= (length m2) 1)
	       (= (length m3) 1)
	       t))))
  t)