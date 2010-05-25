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

(defun c1compiler-typecase (args)
  (or (loop with expr-type = (let ((form (c1expr (pop args))))
                               (prog1 (c1form-primary-type form)
                                 (delete-c1forms form)))
         with subtypep
         with known-typep
         for (type . body) in args
         when (multiple-value-bind (subtypep known-typep)
                  (subtypep expr-type type)
                (and subtypep known-typep))
         return (c1progn body))
      (c1nil)))

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
           (ext:compiler-typecase ,limit
             (fixnum (let ((,variable 0))
                       (declare (fixnum ,variable)
                                ,@declarations)
                       ,body))
             (t (let ((,variable 0))
                  (declare ,@declarations)
                  ,body)))
           ,@output)))))

(put-sysprop 'EXT:COMPILER-TYPECASE 'C1SPECIAL 'C1COMPILER-TYPECASE)
