(defpackage #:my-application
  (:use #:cl)
  (:export #:main))

(in-package #:my-application)

(defun main ()
  (format t "Hello from my-application~%"))
