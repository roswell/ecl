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


;;; 6. Iteration

;;; Regression test for #605.
(test ansi.6.1.1.7-destructuring
  (finishes
   (loop with (a b) = '(1)
         do (return (list a b))))
  (finishes
   (loop with (a b . rest) = '(1)
         do (return (list a b rest))))
  (is-equal '(1 nil 2 nil)
            (loop with (a b) = '(1)
                  for  (c d) = '(2)
                  do (return (list a b c d))))
  (is-equal '(1 nil 2 nil nil)
            (loop with (a b . rest) = '(1)
                  for  (c d) = '(2)
                  do (return (list a b c d rest))))
  (is-equal '(1 2 nil)
            (loop for (a (b) ((c))) ='(1 (2))
                  do (return (list a b c))))
  (signals error
           (loop for (a (b)) ='(1 2)
                 do (return (list a b)))))


;;; 8. Structures
(ext:with-clean-symbols
    (my-struct make-my-struct my-struct-2 make-my-struct-2 my-struct-compatible-type)
  (test ansi.8.redefine-compatible
        (let (foo-1 foo-2 foo-3 foo-4)
          (defstruct (my-struct (:constructor make-my-struct)) slot-1 slot-2)
          (setq foo-1 (make-my-struct :slot-1 3 :slot-2 4))
          (finishes (defstruct (my-struct (:constructor make-my-struct))
                      (slot-1 nil)
                      (slot-2 t)))
          (setq foo-2 (make-my-struct :slot-1 3 :slot-2 4))
          (finishes (defstruct (my-struct (:constructor make-my-struct))
                      (slot-1 3)
                      (slot-2 4)))
          (setq foo-3 (make-my-struct))
          (finishes (defstruct (my-struct (:constructor make-my-struct))
                      (slot-1 8 :type t :read-only nil)
                      (slot-2 8 :type t :read-only nil)))
          (setq foo-4 (make-my-struct :slot-1 3 :slot-2 4))
          (is (equalp foo-1 foo-2))
          (is (equalp foo-2 foo-3))
          (is (equalp foo-3 foo-4)))
        (deftype my-struct-compatible-type () `(integer 0 10))
        (defstruct (my-struct-2 (:constructor make-my-struct-2))
          (slot-1 nil :type my-struct-compatible-type :read-only t))
        (finishes
         (defstruct my-struct-2
           (slot-1 nil :type (integer 0 10) :read-only t)))
        (finishes
         (defstruct my-struct-2
           (slot-1 4 :type (integer 0 10) :read-only t)))
        (finishes
         (defstruct my-struct-2
           (slot-1 4 :type (integer 0 10) :read-only nil)))))

(ext:with-clean-symbols (my-struct make-my-struct)
  (test ansi.8.redefine-incompatible
        (defstruct (my-struct (:constructor make-my-struct)) slot-1 slot-2)
        ;; different slot type
        (signals error (defstruct (my-struct (:constructor make-my-struct))
                         (slot-1 nil :type integer)
                         (slot-2 t)))
        ;; too many slots
        (signals error (defstruct (my-struct (:constructor make-my-struct)) slot-1 slot-2 slot-3))
        ;; too few slots
        (signals error (defstruct (my-struct (:constructor make-my-struct)) slot-1))
        (finishes (make-my-struct))))



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


