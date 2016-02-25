;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Contains: Various regression tests for ECL

(in-package :cl-test)


;;; (EXT:PACKAGE-LOCK) returned the wrong value.
;;; Fixed in 77a267c7e42860affac8eddfcddb8e81fccd44e5

(deftest mixed-0001-package-lock
  (progn
    ;; Don't know the first state
    (ext:package-lock "CL-USER" nil)
    (values
     (ext:package-lock "CL-USER" t)
     (ext:package-lock "CL-USER" nil)
     (ext:package-lock "CL-USER" nil)))
  nil t nil)


;; Bugs from sourceforge

(deftest mixed.0002.mvb-not-evaled
    (assert
     (eq :ok
         (block nil
           (tagbody
              (return (multiple-value-bind ()
                          (go :fail) :bad))
            :fail
              (return :ok)))))
  nil)



(declaim (ftype (function (cons)   t)       mixed.0003.foo))
(declaim (ftype (function (t cons) t) (setf mixed.0003.foo)))

(defun mixed.0003.foo (cons)
  (first cons))

(defun (setf mixed.0003.foo) (value cons)
  (setf (first cons) value))

(defvar mixed.0003.*c* (cons 'x 'y))

(deftest mixed.0003.declaim-type.1
    (mixed.0003.foo mixed.0003.*c*) ;; correctly returns x
  x)

;; signals an error:
;; Z is not of type CONS.
;;   [Condition of type TYPE-ERROR]
(deftest mixed.0004.declaim-type.2
    (assert (eq 'z
                (setf (mixed.0003.foo mixed.0003.*c*) 'z)))
  nil)

(compile nil
         `(lambda (x)
            (1+ (the (values integer string)
                     (funcall x)))))



(deftest mixed.0005.style-warning-argument-order
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

(deftest mixed.0006.write-hash-readable
    (hash-table-count
     (read-from-string
      (write-to-string (make-hash-table)
                       :readably t)))
  0)

(deftest mixed.0007.find-package.1
    (assert
     (let ((string ":cl-user"))
       (find-package
        (let ((*package* (find-package :cl)))
          (read-from-string string)))))
  nil)

(deftest mixed.0008.find-package.2
    (assert
     (let ((string ":cl-user"))
       (let ((*package* (find-package :cl)))
         (find-package
          (read-from-string string)))))
  nil)


