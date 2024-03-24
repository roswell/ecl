;;; bsd-2-clause, (c) 2022, Daniel Kochmański

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "hunchentoot"))

(defpackage #:webserver
  (:use #:cl #:hunchentoot))
(in-package #:webserver)

(defvar *acceptor*
  (make-instance 'hunchentoot:easy-acceptor :port 8888))

(print `(serving ,(uiop/os:getcwd)))
(push (create-folder-dispatcher-and-handler "/" (uiop/os:getcwd)) *dispatch-table*)
(start *acceptor*)
