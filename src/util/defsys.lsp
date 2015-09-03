;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;
;;; **********************************************************************
;;; (c) Copyright G. Attardi, 1990.  All rights reserved.
;;; **********************************************************************

(defparameter *util-directory*  "")

(sbt:defsystem util

     :modules
     '((system  t       t       ())     ; a system building tool
       )

     :directory *util-directory*

     :pathname-types  '("lsp" . "o"))
