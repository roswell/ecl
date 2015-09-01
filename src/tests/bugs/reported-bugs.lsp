;-*- Mode:     Lisp -*-
;;;; Author:   Daniel Kochmański
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Reported bugs which doesn't belong anywhere

(in-package :cl-test)


;; sf 282
(deftest reported-bugs.mvb-not-evaled
    (assert
     (eq :ok
         (block nil
           (tagbody
              (return (multiple-value-bind ()
                          (go :fail) :bad))
            :fail
              (return :ok)))))
  nil)


;; sf262

(declaim (ftype (function (cons)   t)       foo))
(declaim (ftype (function (t cons) t) (setf foo)))

(defun foo (cons)
  (first cons))

(defun (setf foo) (value cons)
  (setf (first cons) value))

(defvar *c* (cons 'x 'y))

(deftest reported-bugs.declaim-type.1
    (foo *c*) ;; correctly returns 'x
  'x)

;; signals an error:
;; Z is not of type CONS.
;;   [Condition of type TYPE-ERROR]
(deftest reported-bugs.declaim-type.2
    (assert (eq 'z
                (setf (foo *c*) 'z)))
  nil)


;; sf272

(compile nil
         `(lambda (x)
            (1+ (the (values integer string)
                     (funcall x)))))

(deftest reported-bugs.style-warning-argument-order.1
    (let ((warning nil))
      (assert
       (eq :ok
           (handler-bind
               ((style-warning
                 (lambda (c)
                   (format t "got style-warning: ~s~%" c)
                   (setf warning c))))
             (block nil
               (tagbody
                  (return (multiple-value-bind () (go :fail) :bad))
                :fail
                  (return :ok))))))
      (assert (not warning)))
  nil)


;; sf272

(print 
  (write-to-string (make-hash-table)
                   :readably t))

(deftest reported-bugs.write-hash-readable
    (hash-table-count
     (read-from-string 
      (write-to-string (make-hash-table)
                       :readably t)))
  0)


;; sf286

(deftest reported-bugs.find-package.1
    (assert
     (let ((string ":cl-user"))
       (find-package
        (let ((*package* (find-package :cl)))
          (read-from-string string)))))
  nil)

(deftest reported-bugs.find-package.2
    (assert
     (let ((string ":cl-user"))
       (let ((*package* (find-package :cl)))
         (find-package
          (read-from-string string)))))
  nil)


