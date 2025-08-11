;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Daniel Kochmański
;;;; Created:  2025-07-22
;;;; Contains: Stream tests (for encodings see the suite "eformat")

(in-package #:cl-test)

(suite 'stream)

(deftest stream.unread-signals-error ()
  (let ((stream (make-string-input-stream "jd")))
    (signals error (unread-char #\x stream))
    (is (char= #\j (read-char stream)))
    (is (char= #\d (read-char stream)))
    (finishes (unread-char #\d stream))
    (is (char= #\d (read-char stream)))
    (is (eql (read-char stream nil :eof) :eof)))
  (let* ((vector (make-array 2 :element-type 'ext:byte8 :initial-contents '(1 2)))
         (stream (ext:make-sequence-input-stream vector)))
    (signals error (ext:unread-byte stream 0))
    (is (= 1 (read-byte stream)))
    (is (= 2 (read-byte stream)))
    (finishes (ext:unread-byte stream 2))
    (is (= 2 (read-byte stream)))
    (is (eql (read-byte stream nil :eof) :eof))))

(defun make-input-stream-tester (key read peek back)
  ;; (READ STREAM EOF-VALUE)
  ;; (PEEK STREAM EOF-VALUE)
  ;; (BACK STREAM ELT-VALUE)
  (lambda (stream vector)
    (labels ((call (name fun elt &optional (exp elt))
               (let ((out (funcall fun stream elt)))
                 (is (eql out exp) "~a: ~s not eql to ~s" name out exp)))
             (test (elt)
               (call :peek peek (funcall key elt))
               (call :read read (funcall key elt))
               (call :back back (funcall key elt) nil)
               (call :peek peek (funcall key elt))
               (call :read read (funcall key elt))))
      (loop for i across vector
            do (test i)
            finally (call :read-eof read :eof)
                    (call :peek-eof peek :eof)))))

(setf (fdefinition 'test-byte-input-stream)
      (make-input-stream-tester
       #'identity
       (lambda (s i) (cl:read-byte s nil i))
       (lambda (s i) (ext:peek-byte s i))
       (lambda (s i) (ext:unread-byte s i))))

(setf (fdefinition 'test-char-input-stream)
      (make-input-stream-tester
       #'code-char
       (lambda (s i) (cl:read-char s nil i))
       (lambda (s i) (cl:peek-char nil s nil i))
       (lambda (s i) (cl:unread-char i s))))

(setf (fdefinition 'test-bivalent-input-stream)
      (make-input-stream-tester
       #'identity
       (lambda (s i)
         (if (zerop (random 2))
             (cl:read-byte s nil i)
             (let ((out (cl:read-char s nil i)))
               (if (characterp out)
                   (char-code out)
                   out))))
       (lambda (s i)
         (if (zerop (random 2))
             (ext:peek-byte s i)
             (let ((out (cl:peek-char nil s nil i)))
               (if (characterp out)
                   (char-code out)
                   out))))
       (lambda (s i)
         (if (zerop (random 2))
             (ext:unread-byte s i)
             (cl:unread-char (code-char i) s)))))

(defun test-byte-output-stream (stream n)
  (dotimes (i n)
    (finishes (cl:write-byte (char-code #\x) stream))))

(defun test-char-output-stream (stream n)
  (dotimes (i n)
    (finishes (cl:write-char #\X stream))))

(defun test-bivalent-output-stream (stream n)
  (dotimes (i n)
    (if (zerop (random 2))
        (finishes (cl:write-byte (char-code #\a) stream))
        (finishes (cl:write-char #\A stream)))))

(defun make-sequence-io-stream (vector &optional format)
  (make-two-way-stream
   (ext:make-sequence-input-stream vector :external-format format)
   (ext:make-sequence-output-stream vector :external-format format)))

;;; Smoke test for extensions EXT:PEEK-BYTE and EXT:UNREAD-BYTE.
(deftest stream.smoke-read-byte ()
  (let* ((values (loop repeat 16 collect (random 255)))
         (vector (make-array 16 :element-type '(unsigned-byte 8)
                                :initial-contents values
                                :fill-pointer 8))
         (stream (make-sequence-io-stream vector)))
    (test-byte-input-stream stream vector)
    (test-byte-output-stream stream 8)))

(deftest stream.smoke-read-char ()
  (let* ((values (map 'vector #'char-code "ABCDEFGHIJKLMNOP"))
         (vector (make-array 16 :element-type '(unsigned-byte 8)
                                :initial-contents values
                                :fill-pointer 8))
         (stream (make-sequence-io-stream vector :ascii)))
    (test-char-input-stream stream vector)
    (test-char-output-stream stream 8)))

(deftest stream.smoke-bivalent ()
  (let* ((values (map 'vector #'char-code "ABCDEFGHIJKLMNOP"))
         (vector (make-array 16 :element-type '(unsigned-byte 8)
                                :initial-contents values
                                :fill-pointer 8))
         (stream (make-sequence-io-stream vector :ascii)))
    (test-bivalent-input-stream stream vector)
    (test-bivalent-output-stream stream 8)))

;;; Ensure that we make a "clean" error (i.e not a segfault) when bivalent
;;; stream has bytes that can't be casted to characters.
(deftest stream.error-bivalent ()
  (let* ((values (loop repeat 16 collect char-code-limit))
         (vector (make-array 16 :element-type '(unsigned-byte 64)
                                :initial-contents values
                                :fill-pointer 8))
         (stream (make-sequence-io-stream vector nil)))
    (signals error (test-char-input-stream stream vector))
    (finishes (test-byte-input-stream stream vector))
    (finishes (test-char-output-stream stream 8))
    (finishes (test-char-input-stream stream (subseq vector 8 16)))))

;;; Ensure that MAKE-SEQUENCE-INPUT-STREAM and MAKE-SEQUENCE-OUTPUT-STREAM can
;;; take bytes that are larger than any character.

(deftest stream.binary-sequence ()
  (loop with values = (loop for i from 0 below 16 collect (char-code #\Z))
        for (elt-type . format) in '(((unsigned-byte 8) . nil)
                                     ((unsigned-byte 16) . nil)
                                     ((unsigned-byte 32) . nil)
                                     ((unsigned-byte 64) . nil)
                                     ((unsigned-byte 16) . :ucs-2)
                                     ((unsigned-byte 32) . :ucs-4))
        for vector = (make-array 16 :element-type elt-type
                                    :initial-contents values
                                    :fill-pointer 12)
        for stream = (finishes (make-sequence-io-stream vector format))
        when (and stream (null format))
          do (finishes (test-byte-input-stream stream vector))
             (finishes (test-byte-output-stream stream 4))))

;;; Ensure that MAKE-SEQUENCE-INPUT-STREAM and MAKE-SEQUENCE-OUTPUT-STREAM can
;;; take byte and char sequences and use them as bivalent streams.

(deftest stream.bivalent-sequence ()
  (loop with char-values = "0123456789abcdef"
        with byte-values = (map 'vector #'char-code char-values)
        for (elt-type values format) in `(((unsigned-byte  8) ,byte-values nil)
                                          ((unsigned-byte 16) ,byte-values nil)
                                          ((unsigned-byte 32) ,byte-values nil)
                                          ((unsigned-byte 64) ,byte-values nil)
                                          (character          ,char-values :default))
        for vector = (make-array 16 :element-type elt-type
                                    :initial-contents values
                                    :fill-pointer 12)
        for result = (subseq byte-values 0 12)
        for stream = (finishes (make-sequence-io-stream vector format))
        when stream
          do (finishes (test-bivalent-input-stream stream result))
             (finishes (test-bivalent-output-stream stream 4))))

;;; Ensure that we punt on invalid sequence stream types.

(deftest stream.invalid-sequence ()
  (loop with values = (loop for i from 0 below 16 collect i)
        for (elt-type . format) in '((t . nil)
                                     (single-float . nil)
                                     (double-float . nil)
                                     (long-float   . nil)
                                     (si:complex-single-float  . nil)
                                     (si:complex-double-float  . nil)
                                     (si:complex-long-float  . nil))
        for vector = (make-array 16 :element-type elt-type :initial-contents values)
        do (signals error (make-sequence-io-stream vector format))))

(deftest stream.bidirectional-vector-with-fill-pointer ()
  (let* ((values (map 'vector #'char-code "ABCDEFGHIJKLMNOP"))
         (vector (make-array 16 :element-type '(unsigned-byte 8)
                                :initial-contents values
                                :fill-pointer 8))
         (stream (make-sequence-io-stream vector :ascii)))
    (dotimes (v 8)
      (finishes (read-char stream)))
    (signals error (read-char stream))
    (dotimes (v 4) (write-char #\x stream))
    (dotimes (v 2) (write-char #\y stream))
    (dotimes (v 4) (eql #\x (read-char stream)))
    (dotimes (v 2) (eql #\y (read-char stream)))
    (signals error (read-char stream))
    (dotimes (v 2) (finishes (write-char #\z stream)))
    (signals error (write-char #\z stream))
    (dotimes (v 2) (eql #\z (read-char stream)))
    (signals error (read-char stream))))
