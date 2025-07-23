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
    (is (eql (read-char stream) :eof))))

