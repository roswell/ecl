(in-package #:cl-user)

(defmacro define-function (name &body body)
  `(defun ,name (&aux
                   (argc (si:argc))
                   (argv (ext:command-args)))
     (declare (ignorable argc argv))
     ,@body))

(define-function arg-test
  (if (= argc *args-number*)
      (quit 0)
      (quit 1)))

(define-function print-test
  (terpri *standard-output*)
  (princ "Hello stdout" *standard-output*)
  (terpri *error-output*)
  (princ "Hello stderr" *error-output*))

(define-function io/err
  (princ "Welcome to ITP(NR) - Intelligent Test Program (not really)!")
  (print argc *error-output*)

  (princ "Type your SEXP: ")
  (let ((result (read *standard-input* nil :eof)))
    (princ result *error-output*)
    (cond ((eq result :eof)
           (princ "No? Shame...")
           (quit 1))
          (:otherwise
           "Thank you. Your wish has been heard loud and clear."
           (quit 0)))))

(define-function terminate
  ;; timeout is for case of zombies, this process should be killed
  ;; from the outside.
  (sleep 10)
  (quit 0))

(define-function suspend
  (do () (nil)
    (print "heartbit")
    (sleep 1)
    (print "boombaya")
    (sleep 1)))
