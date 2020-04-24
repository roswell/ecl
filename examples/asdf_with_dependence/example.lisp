(in-package #:example)

(defun test-function (n)
  (format t "Factorial of ~a is: ~a~%" n (alexandria:factorial n)))
