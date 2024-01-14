;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2008. Juan Jose Garcia-Ripol
;;;;
;;;;    See file 'LICENSE' for the copyright details.
;;;;
;;;; CMPOPT-TYPE -- Optimizations for selected types of expresions
;;;;

(in-package "COMPILER")

(define-compiler-macro dotimes ((variable limit &rest output) &body body)
  (multiple-value-bind (declarations body)
      (si:process-declarations body nil)
    (ext:with-unique-names (%limit)
      `(block nil
         (let ((,%limit ,limit))
           (declare (:read-only ,%limit))
           (ext:compiler-typecase ,%limit
             (fixnum
              ;; %LIMIT will be type checked by the compiler to be
              ;; a fixnum. We may thus just increase the counter.
              (let ((,variable 0))
                (declare (fixnum ,variable)
                         ,@declarations)
                (si::while (< ,variable ,%limit)
                  ,@body
                  (locally (declare (optimize (safety 0)))
                    (setq ,variable (1+ ,variable))))
                ,@output))
             (t
              (let ((,variable 0))
                (declare ,@declarations)
                (si::while (< ,variable ,%limit)
                  ,@body
                  (setq ,variable (1+ ,variable)))
                ,@output))))))))
