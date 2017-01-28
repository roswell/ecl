;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  CMPFUN  Library functions.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi and William F. Schelter.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.


(in-package "COMPILER")

(defun c1apply (args)
  (check-args-number 'APPLY args 2)
  (flet ((default-apply (fun arguments)
           (let ((form (c1funcall (list* '#'APPLY fun arguments))))
             (when (and (consp fun) (eq (first fun) 'FUNCTION))
               (let* ((fname (second fun))
                      (type (get-return-type fname)))
                 (when type
                   (setf (c1form-type form) type))))
             form)))
    (let* ((fun (first args))
           (arguments (rest args)))
      (cond ((eql (first (last arguments)) 'clos::.combined-method-args.)
             ;; Uses frames instead of lists as last argumennt
             (default-apply fun arguments))
            ((and (consp fun)
                  (eq (first fun) 'LAMBDA))
             (optimize-funcall/apply-lambda (cdr fun) arguments t))
            ((and (consp fun)
                  (eq (first fun) 'EXT::LAMBDA-BLOCK))
             (setf fun (macroexpand-1 fun))
             (optimize-funcall/apply-lambda (cdr fun) arguments t))
            ((and (consp fun)
                  (eq (first fun) 'FUNCTION)
                  (consp (second fun))
                  (member (caadr fun) '(LAMBDA EXT::LAMBDA-BLOCK)))
             (c1apply (list* (second fun) arguments)))
            (t
             (default-apply fun arguments))))))
