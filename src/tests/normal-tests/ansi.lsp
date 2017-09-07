;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

(in-package :cl-test)

(suite 'ansi+)


;; HyperSpec – 3.*

;;;;;;;;;;;;;;;;;;;
;; Deftype tests ;;
;;;;;;;;;;;;;;;;;;;

(ext:with-clean-symbols (ordinary1 ordinary2)
  (test ansi.0001.ordinary
    (deftype ordinary1 ()
      `(member nil t))

    (deftype ordinary2 (a b)
      (if a 'CONS `(INTEGER 0 ,b)))

    (is-true  (typep T  'ordinary1))
    (is-false (typep :a 'ordinary1))
    (is-false (typep T '(ordinary2 nil 3)))
    (is-true  (typep 3 '(ordinary2 nil 4)))
    (is-false (typep T '(ordinary2 T nil)))
    (is-true  (typep '(1 . 2) '(ordinary2 T nil)))))

(ext:with-clean-symbols (opt)
  (test ansi.0002.optional
    (deftype opt (a &optional b)
      (if a 'CONS `(INTEGER 0 ,b)))

    (is-true (typep 5 '(opt nil)))
    (is-false (typep 5 '(opt nil 4)))))

(ext:with-clean-symbols (nest)
  (test ansi.0003.nested
    (deftype nest ((a &optional b) c . d)
      (assert (listp d))
      `(member ,a ,b ,c))
    (is-true  (typep  1 '(nest (1 2) 3 4 5 6)))
    (is-false (typep  1 '(nest (2 2) 3 4 5 6)))
    (is-true  (typep '* '(nest (3) 3)))
    (is-true  (typep  3 '(nest (2) 3)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 12.2.* Numbers tests ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(test ansi.12.2.incf
  (let ((foo 0)
        (bar 0))
    (flet ((inc () (incf foo)))
      (incf (the fixnum bar) (inc)))
    (is (= foo 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; 19.* Pathname tests ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Issue #103 ;; logical-pathname-translations not translating
;; https://gitlab.com/embeddable-common-lisp/ecl/issues/103
(test ansi.0004.wildcards
  (setf (logical-pathname-translations "prog")
        '(("CODE;*.*.*" "/tmp/prog/")))
  (is (equal
       (namestring (translate-logical-pathname "prog:code;documentation.lisp"))
       "/tmp/prog/documentation.lisp")))

;; Issue #351 ;; (probe-file #P"/") maybe shouldn't return nil
;; https://gitlab.com/embeddable-common-lisp/ecl/issues/351
#-windows
(test ansi.19.probe-file
  (is (probe-file #P"/"))
  (is (probe-file #P"/tmp"))
  (is (probe-file #P"/tmp/")))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; 22.* Format tests   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cl-user::fmt (stream argument colonp atsignp &rest params)
  (declare (ignore argument colonp atsignp))
  (format stream "~S~%" params))

;;; Tests for CDR 7
;;; See: https://common-lisp.net/project/cdr/document/7/index.html
(test ansi.22.cdr-7
  ;; trailing commas
  (let ((expected (format nil "(1 2)~%")))
    (is-equal expected (format nil "~1,2/fmt/" t))
    (is-equal expected (format nil "~1,2,/fmt/" t))
    (is-equal expected (format nil "~1,2:/fmt/" t))
    (is-equal expected (format nil "~1,2,:/fmt/" t)))
  ;; final V parameters
  (let ((expected-1 (format nil "(1 T)~%"))
        (expected-2 (format nil "(1 NIL)~%")))
    (is-equal expected-1 (format nil "~1,v/fmt/" t t))
    (is-equal expected-1 (format nil "~1,v,/fmt/" t t))
    (is-equal expected-1 (format nil "~1,v:/fmt/" t t))
    (is-equal expected-1 (format nil "~1,v,:/fmt/" t t))

    (is-equal expected-2 (format nil "~1,v/fmt/" nil t))
    (is-equal expected-2 (format nil "~1,v,/fmt/" nil t))
    (is-equal expected-2 (format nil "~1,v:/fmt/" nil t))
    (is-equal expected-2 (format nil "~1,v,:/fmt/" nil t))))


;;;;;;;;;;;;;;;;;;;;;;;
;; 23.* Reader tests ;;
;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (defstruct example-struct a)
  (test ansi.0005.sharp-s-reader
    (finishes
      (read-from-string
       "(#1=\"Hello\" #S(cl-test::example-struct :A #1#))"))))


