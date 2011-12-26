;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2008. Juan Jose Garcia-Ripol
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;; CMPOPT-TYPE -- Optimizations for selected types of expresions
;;;;

(in-package "COMPILER")

(define-compiler-macro dotimes ((variable limit &rest output) &body body)
  (multiple-value-bind (declarations body)
      (si:process-declarations body nil)
    (ext:with-unique-names (%limit)
      (setf body `(si::while (< ,variable ,%limit)
                    ,@body
                    (setq ,variable (1+ ,variable))))
      `(block nil
         (let ((,%limit ,limit))
           (declare (:read-only ,%limit))
           (ext:compiler-typecase ,%limit
             (fixnum (let ((,variable 0))
                       (declare (fixnum ,variable)
                                ,@declarations)
                       ,body))
             (t (let ((,variable 0))
                  (declare ,@declarations)
                  ,body)))
           ,@output)))))
