(in-package #:cl-user)

(progn
  (print "hello")
  (print "bye!"))

;; (princ "hello")

;; (if (zerop (random 2))
;;     (call-true)
;;     (call-false))

;; (block my-block
;;   (princ "start block!")
;;   (when (zerop (random 2))
;;     (return-from my-block 42))
;;   (princ "exit block!"))

#+ (or)
(defvar *xxx*
  (loop for i from 0 below 5 do
        collect i))

(let ((i 5))
  (tagbody
     (princ "start")
   :again
     (unless (zerop i)
       (decf i)
       (go :again))
     (princ  "end"))
  i)

;; (tagbody
;;  :label-1
;;    (print "hello world!")
;;  :label-2
;;    (setf *xxx* (go :label-1))
;;    (print "hello world!")
;;  :label-3
;;    (when *xxx*
;;      (go :label-2))
;;    (print "hello world!"))

;; (defun call-true ()
;;   (princ "local function call-true called!"))
