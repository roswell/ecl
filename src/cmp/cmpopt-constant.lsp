;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll.
;;;;
;;;;    See file 'LICENSE' for the copyright details.

;;;;
;;;; CMPOPT-CONSTANTS  Constant expressions.
;;;;

(in-package "COMPILER")

(defun constant-expression-p (form &optional (env *cmp-env*))
  (or (constantp form env)
      (and (consp form)
           (let ((head (car form)))
             (or (member head '(IF OR AND NULL NOT PROGN))
                 (and (si:get-sysprop head 'pure)
                      (inline-possible head))))
           (loop for c in (rest form)
              always (constant-expression-p c env)))))

(defun extract-constant-value (form &optional failure (env *cmp-env*))
  (if (constant-expression-p form env)
      (handler-case (cmp-eval form env)
        (error (c) failure))
      failure))

(defun constant-value-p (form &optional (env *cmp-env*))
  (if (constant-expression-p form env)
      (handler-case
          (values t (cmp-eval form env))
        (error (c) (values nil form)))
      (values nil form)))
