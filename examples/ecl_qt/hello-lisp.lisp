
(defpackage :hello-lisp
  (:use :cl :lparallel))

(in-package :hello-lisp) ;;package name hello-lisp


(setf lparallel:*kernel* (lparallel:make-kernel 4))

(lparallel:defpun pfib (n)
  (if (< n 2)
      n
      (plet ((a (pfib (- n 1)))
             (b (pfib (- n 2))))
            (+ a b))))


(defun qsort (seq pred)
  (if (null seq) nil
      (let* ((pivot (first seq))
             (left (remove-if-not (lambda (x)
                                    (funcall pred x pivot))
                                  (cdr seq)))
             (right (remove-if (lambda (x)
                                 (funcall pred x pivot))
                               (cdr seq))))
        (append (qsort left pred)
                (list pivot)
                (qsort right pred)))))


(defun say-hello ()
  "Bonjour, lisp!")
