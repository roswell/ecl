;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Contains: Various regression tests for ECL

(in-package :cl-test)

(suite 'mixed)


;;; (EXT:PACKAGE-LOCK) returned the wrong value.
;;; Fixed in 77a267c7e42860affac8eddfcddb8e81fccd44e5

(test mix.0001.package-lock
  ;; Don't know the first state
  (ext:package-lock "CL-USER" nil)
  (is-false (ext:package-lock "CL-USER" t))
  (is-true  (ext:package-lock "CL-USER" nil))
  (is-false (ext:package-lock "CL-USER" nil)))


;; Bugs from sourceforge

(test mix.0002.mvb-not-evaled
  (is (eq :ok (block nil
                (tagbody
                   (return (multiple-value-bind ()
                               (go :fail) :bad))
                 :fail
                   (return :ok))))))



(ext:with-clean-symbols (foo)
  (declaim (ftype (function (cons) t)   foo)
           (ftype (function (t cons) t) (setf foo)))

  (defun foo (cons)
    (first cons))

  (defun (setf foo) (value cons)
    (setf (first cons) value))

  (test mix.0003.declaim-type
    (let ((*bar* (cons 'x 'y)))
      (is (eq (foo *bar*) 'x))
      (is (eq (setf (foo *bar*) 'z) 'z) "signals on error:
;; Z is not of type CONS.
;;   [Condition of type TYPE-ERROR]"))))



(test mix.0004.style-warning-argument-order
  (let ((warning nil))
    (is (eq :ok
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
    (is-false warning)))

(test mix.0005.write-hash-readable
  (is (= (hash-table-count
          (read-from-string
           (write-to-string (make-hash-table)
                            :readably t))))))

(test mix.0006.find-package
  (is
   (let ((string ":cl-user"))
     (find-package
      (let ((*package* (find-package :cl)))
        (read-from-string string)))))
  (is
   (let ((string ":cl-user"))
     (let ((*package* (find-package :cl)))
       (find-package
        (read-from-string string))))))



;;; Date: 2016-05-21 (Masataro Asai)
;;; Description:
;;;
;;;     RESTART-CASE investigates the body in an incorrect manner,
;;;     then remove the arguments to SIGNAL, which cause the slots of
;;;     the conditions to be not set properly.
;;;
;;; Bug: https://gitlab.com/embeddable-common-lisp/ecl/issues/247
;;;
(ext:with-clean-symbols (x)
  (define-condition x () ((y :initarg :y)))
  (test mix.0007.restart-case-body
    (is-false (handler-bind ((x (lambda (c) (slot-value c 'y))))
                (restart-case
                    (signal 'x :y 1))))))


;;; Date: 2016-04-21 (Juraj)
;;; Fixed: 2016-06-21 (Daniel Kochmański)
;;; Description:
;;;
;;; Trace did not respect *TRACE-OUTPUT*.
;;;
;;; Bug: https://gitlab.com/embeddable-common-lisp/ecl/issues/236
;;;
(ext:with-clean-symbols (fact)
  (defun fact (n) (if (zerop n) :boom (fact (1- n))))
  (test mix.0008.trace-output
    (is
     (not (zerop
           (length
            (with-output-to-string (*trace-output*)
              (trace fact)
              (fact 3)
              (untrace fact)
              *trace-output*)))))))


;;;; Author:   Daniel Kochmański
;;;; Created:  2015-09-21
;;;; Contains: Random state tests
(test mix.0009.random-states
  (is (numberp (random 18)) "Can't generate trivial random number")
  (is (numberp (random 18 #$1))
      "Can't generate a number from read (#$1) random state")
  (is (numberp (random 18 (make-random-state)))
      "Can't generate a number from a new random state")
  (is (numberp (random 18 (make-random-state #$1)))
      "Can't generate a number from a new random state from reader")
  (is (= (random 18 #$1)
         (random 18 #$1)
         (random 18 #$1))
      "The same seed produces different results")
  (is (let ((*print-readably* t)
            (rs (make-random-state #$1)))
        (equalp
         (prin1-to-string #$1)
         (prin1-to-string rs)))
      "The same seed gives different random states")
  (is (let* ((*print-readably* t)
             (rs (make-random-state #$1))
             (rs-read (read-from-string
                       (prin1-to-string rs))))
        (equalp
         (prin1-to-string rs-read)
         (prin1-to-string rs)))
      "Can't read back a random state"))


;;; Date: 2016-08-04 (jd)
;;; Fixed: 2016-08-04 (jd)
;;; Description:
;;;
;;; file-stream-fd caused internal error if fed with non-file ANSI
;;; stream
;;;
;;; Bug: https://gitlab.com/embeddable-common-lisp/ecl/issues/271
;;;
(test mix.0010.file-stream-fd
  ;; We check the second one only if first test passes. Second test
  ;; caused internal error of ECL and crashed the process preventing
  ;; further tests, so we perform it only on versions after the fix.
  (if (signals simple-type-error (ext:file-stream-fd ""))
      (signals simple-type-error (ext:file-stream-fd
                                  (make-string-output-stream)))
      (fail (ext:file-stream-fd (make-string-output-stream))
            "Not-file stream would cause internal error on this ECL (skipped)")))


;;; Date: 2016-12-20
;;; Reported by: Kris Katterjohn
;;; Fixed: Daniel Kochmański
;;; Description:
;;;
;;;   atan signalled `division-by-zero' exception when the second
;;;   argument was signed zero. Also inconsistent behavior on invalid
;;;   operation (atan 0.0 0.0).
;;;
;;; Bug: https://gitlab.com/embeddable-common-lisp/ecl/issues/329
(test mix.0012.atan-signed-zero
  (finishes (atan 1.0 -0.0)))


;;; Date: 2016-12-21
;;; Description:
;;;
;;;   `sleep' uses `ECL_WITHOUT_FPE_BEGIN' which didn't restore fpe
;;;   correctly.
;;;
;;; Bug: https://gitlab.com/embeddable-common-lisp/ecl/issues/317
#+floating-point-exceptions
(test mix.0013.sleep-without-fpe
  (sleep 0.1)
  (let ((a 1.0)
        (b 0.0))
    ;; nb: normally operation signals `division-by-zero', but OSX
    ;; signals `floating-point-overflow'. It's OK I suppose.
    (signals arithmetic-error (/ a b))))


;;; Date: 2017-01-20
;;; Description:
;;;
;;;   `dolist' macroexpansion yields result which doesn't have a
;;;   correct scope.
;;;
;;; Bug: https://gitlab.com/embeddable-common-lisp/ecl/issues/348
(test mix.0014.dolist
  (is-false
   (nth-value 1
     (compile nil
              (lambda ()
                (dolist (s '("foo" "bar" "baz") s)
                  (declare (type string s))
                  (check-type s string)
                  (format nil "~s" s))))))
  (finishes (eval '(dolist (e '(1 2 3 4) e)
                    (print e)
                    (go :next)
                    (print 'skip)
                    :next))))


;;; Date: 2017-07-02
;;; Description:
;;;
;;;   Function `ecl_new_binding_index' called `si_set_finalizer',
;;;   which resetted `env->nvalues' leading to invalid binding in mvb
;;;   during the first function run.
;;;
;;; Bug: https://gitlab.com/embeddable-common-lisp/ecl/issues/233
(test mix.0015.mvb
  (with-compiler ("aux-cl-0003.lsp" :load t)
    `(progn
       (defvar mix.0015.v1 'booya)
       (defun mix.0015.fun ()
         (let ((share_t))
           (multiple-value-bind (mix.0015.v1 woops)
               (case share_t
                 ((nil)
                  (values 1 2)))
             woops)))))
  (ignore-errors
    (delete-file "aux-cl-0003.lsp")
    (delete-file "aux-cl-0003.fas")
    (delete-file "aux-cl-0003.fasc"))
  (is-eql 2 (mix.0015.fun)))

;;; Date: 2018-05-08
;;; Description:
;;;
;;;   Better handling of fifos.
;;;
;;; Bug: https://gitlab.com/embeddable-common-lisp/ecl/issues/242
#-windows
(test mix.0016.fifo-tests
  (ext:run-program "mkfifo" '("my-fifo") :output t)
  ;; 1) reader (first) and writer (inside)
  (with-open-file (stream "my-fifo" :nonblock t)
    (is (null (file-length stream)))
    (is (null (listen stream)))
    (is (eql :foo (read-line stream nil :foo)))
    (is (eql :fifo (ext:file-kind stream nil)))
    (with-open-file (stream2 "my-fifo" :direction :output :nonblock t)
      ;; Even for output it should not block on Unix.
      (finishes (write-line "foobar" stream2)))
    ;; there is nobody on the other side, data is lost
    (is (equal :foo (read-line stream nil :foo))))
  ;; 2) writer (first) and reader (second)
  (signals file-error (open "my-fifo" :direction :output :nonblock t))
  (with-open-file (stream "my-fifo" :direction :input :nonblock t)
    (is (eql :foo (read-line stream nil :foo))))
  ;; clean up
  (delete-file "my-fifo"))


;;; Date: 2018-12-02
;;; Description:
;;;
;;;   Serialization/Deserialization tests
#+externalizable
(test mix.0017.serialization
  (let* ((vector (make-array 4 :element-type 'ext:byte16 :initial-contents #(1 2 3 4)))
         (object-table
          ;; vector of (object . compare-function)
          (vector '(nil . eql) ; empty list
                  '('(1 2) . equalp) ; non-empty list
                  '(#\q . eql) ; character
                  '(42 . eql) ; fixnum
                  (cons (+ 10 most-positive-fixnum) 'eql) ; bignum
                  '(2/3 . eql) ; ratio
                  '(12.3f4 . eql) ; floats
                  '(13.2d4 . eql)
                  '(14.2l3 . eql)
                  '(#c(4 7) . eql) ; complexes
                  '(#c(1.0f0 2.0f0) . eql)
                  '(#c(1.0d0 2.0d0) . eql)
                  '(#c(1.0l0 2.0l0) . eql)
                  '(#.(find-package "COMMON-LISP-USER") . eq) ; package
                  '(q . eql) ; symbol
                  ;; hash-table
                  (cons (let ((ht (make-hash-table)))
                          (setf (gethash :foo ht) :abc)
                          (setf (gethash :bar ht) :def)
                          ht)
                        #'(lambda (x y)
                            (loop for key being the hash-keys of x
                               if (not (eq (gethash key x)
                                           (gethash key y)))
                               return nil
                               finally (return t))))
                  ;; array
                  (cons (let ((a (make-array '(2 2) :initial-element 0)))
                          (setf (aref a 0 0) 'q)
                          (setf (aref a 0 1) 1/5)
                          a)
                        'equalp)
                  (cons vector 'equalp) ; non-displaced vector
                  ;; displaced vector
                  (cons (make-array 3 :element-type 'ext:byte16
                                    :displaced-to vector
                                    :displaced-index-offset 1)
                        #'(lambda (x y)
                            (and (equalp x y)
                                 (equalp (multiple-value-list (array-displacement x))
                                         (multiple-value-list (array-displacement y))))))
                  '("a∩b∈c" . equal) ; string
                  (cons (make-string 3 :initial-element #\q :element-type 'base-char) 'equal) ; base-string
                  (cons (make-array 6 :element-type 'bit :initial-contents #(0 1 0 1 1 0)) 'equal) ; bit-vector
                  ;; stream: not externalizable?
                  ;; random-state
                  (cons (let ((r (make-random-state)))
                          (random 10 r)
                          r)
                        'equalp)
                  ;; readtable: not externalizable
                  '(#P"/foo/bar/whatever.gif" . equal) ; pathname
                  ;; TODO: other objects
                  ))
         (to-be-serialized
          (map 'vector #'first object-table))
         (deserialized (si::deserialize (si::serialize to-be-serialized))))
    (is-true (= (length to-be-serialized) (length deserialized)))
    (loop for i below (length to-be-serialized)
       do (is-true (funcall (cdr (elt object-table i))
                            (elt to-be-serialized i)
                            (elt deserialized i))))))

;;; Date: 2019-12-26
;;; Description:
;;;
;;;   Stack overflow detection and recovery
(test mix.0018.stack-overflow
  (signals ext:stack-overflow (labels ((f (x) (f (1+ x))))
                                (f 1))))

;;; Date 2020-04-22
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/-/merge_requests/197
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/-/issues/576
;;; Description:
;;;
;;;     Ensure that with-input-from-string and with-output-to-string
;;;     close the streams that they provide.
(test mix.0019.with-string-io-close-streams
  (let (stream-var)
    (with-input-from-string (inner-stream-var "test")
      (setf stream-var inner-stream-var)
      (is (open-stream-p stream-var)))
    (is (not (open-stream-p stream-var)))
    (with-output-to-string (inner-stream-var)
      (setf stream-var inner-stream-var)
      (is (open-stream-p stream-var)))
    (is (not (open-stream-p stream-var)))))

;;;; Author:   Tarn W. Burton
;;;; Created:  2021-06-05
;;;; Contains: *ed-functions* tests
(test mix.0020.ed-functions
  (let ((ext:*ed-functions* (list (lambda (x)
                                    (equal x "foo"))
                                  (lambda (x)
                                    (equal x "bar"))
                                  (lambda (x)
                                    (when (equal x "baz")
                                      (error 'file-error :pathname x))))))
    (is (ed "foo"))
    (is (ed "bar"))
    (signals simple-error (ed "qux"))
    (signals file-error (ed "baz"))))

;;;; Author:   Tarn W. Burton
;;;; Created:  2021-12-21
;;;; Contains: pretty printer tests for real valued columns
(defclass pp-stream-test (gray:fundamental-character-output-stream)
  ((column :accessor gray:stream-line-column
           :initform (random .5))
   (value :accessor pp-stream-test-value
          :initform (make-array 10 :adjustable t :fill-pointer 0
                                :element-type 'character))))
(defmethod gray:stream-write-char ((stream pp-stream-test) char)
  (if (eql char #\Newline)
      (setf (gray:stream-line-column stream) (random .5))
      (incf (gray:stream-line-column stream)))
  (vector-push-extend char (pp-stream-test-value stream)))
(test mix.0021.pretty-printer
  (let ((stream (make-instance 'pp-stream-test))
        (*print-right-margin* 15))
    (pprint '(let ((fu 1) (bar 2)) (+ fu bar 7))
            stream)
    (is-eql (sys:file-column stream) 15)
    (is (gray:stream-advance-to-column stream 20))
    (write-char #\A stream)
    (is-equal (pp-stream-test-value stream)
            "
(LET ((FU 1)
      (BAR 2))
  (+ FU BAR 7))    A")))

;; Created: 2022-04-27
;; Contains: a smoke test for a new operator si:adjust-vector
(test mix.0022.adjust-vector
  (let ((vector (si:make-vector t 10 t nil nil nil)))
    (si:adjust-vector vector 20)
    (is (= 20 (array-total-size vector)))))

;;; Created: 2022-11-10
;;; Contains: a test for a logarithm of a very small ratio
(test mix.0023.log-small-ratio
  (finishes (log (/ 1 6319748715279270675921934218987893281199411530039296))))
