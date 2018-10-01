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
;; 21.2 Stream tests   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Issue #452: broadcast-streams
;;; FILE-STRING-LENGTH for broadcast-streams (non-empty) broken
;;; https://gitlab.com/embeddable-common-lisp/ecl/issues/452
;;;
;;; We also fix the non-conformance where we pick the first (not the last)
;;; component when we delegate question.
(test ansi.21-2.broadcast-non-empty
  (ensure-directories-exist *tmp-dir*)
  (let ((path (merge-pathnames "sxx.txt" *tmp-dir*)))
    (with-open-file (sxx path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (let ((broadcast (make-broadcast-stream sxx)))
        (finishes (file-position broadcast))
        (finishes (file-length broadcast))
        (finishes (file-string-length broadcast "jd"))))))

(test ansi.21-2.file-string-length=nil
  (let ((stream (make-string-output-stream)))
    (finishes (file-position stream))
    (signals error (file-length stream))
    ;; Undefined behavior, should either signal error (because stream is not
    ;; file output character stream), return NIL (because can't be determined)
    ;; or return integer (because progress can be determined).
    #+ (or) (signals error (file-string-length stream "jd"))
    #+ (or) (is (typep (file-string-length stream "jd") '(or null integer)))))

;;; file-* should be passed to the /last/ component.
(test ansi.21-2.last-component
  (ensure-directories-exist *tmp-dir*)
  (let ((first-stream (make-string-output-stream)))
    (with-open-file (last-stream (merge-pathnames "ss.txt" *tmp-dir*)
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (format last-stream "Hello world!~%")
      (finish-output last-stream)     ; for buffered streams
      (is (= (file-length last-stream) 13))
      (let ((broadcast (make-broadcast-stream first-stream last-stream)))
        (is (= 13 (file-length broadcast) (file-length last-stream)))
        (is (= 13 (file-position broadcast) (file-position last-stream)))
        (is (= 2
               (file-string-length broadcast "jd")
               (file-string-length last-stream "jd")))))))


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


