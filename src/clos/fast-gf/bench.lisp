(in-package "CL-USER")

;;; This is a simple benchmark that is used to compare performance of calling
;;; functions vs funcallable objects vs generic functions. The last one have
;;; multiple variants concerning the complexity of method combinations and the
;;; number of possible dispatch outcomes.

(defmacro measuring ((&key n) &body body)
  )

(declaim (notinline f1 f2))
(declaim (inline f2))

(defun f1 (a b c)
  (declare (ignore a b c)))

(defgeneric f2 (arg1 arg2 arg3)
  (:method ((arg1 integer) (arg2 integer) arg3)
    (declare (ignore arg1 arg2 arg3)))
  (:method ((arg1 fixnum) (arg2 fixnum) arg3)
    (declare (ignore arg1 arg2 arg3)))
  (:method ((arg1 string) (arg2 integer) arg3)
    (declare (ignore arg1 arg2 arg3)))
  (:method ((arg1 string) (arg2 string) arg3)
    (declare (ignore arg1 arg2 arg3)))
  (:method ((arg1 (eql 42)) (arg2 integer) arg3)
    (declare (ignore arg1 arg2 arg3)))
  (:method ((arg1 integer) (arg2 (eql 13)) arg3)
    (declare (ignore arg1 arg2 arg3)))
  (:method ((arg1 integer) (arg2 symbol) arg3)
    (declare (ignore arg1 arg2 arg3))))
