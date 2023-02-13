;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2021, Daniel Kochmański
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;

(in-package #:compiler)

;;;; Following special forms are provided:
;;;;
;;;;    (WITH-STACK {form}*)
;;;;            Executes given forms, restoring the lisp stack on output.
;;;;    (STACK-PUSH form)
;;;;    (STACK-PUSH-VALUES form)
;;;;    (STACK-POP nvalues)

(defun c1with-stack (forms)
  (let* ((var (pop forms))
         (body (c1expr `(let ((,var (innermost-stack-frame))) ,@forms))))
    (make-c1form* 'WITH-STACK
                  :type (c1form-type body)
                  :args body)))

(defun c1innermost-stack-frame (args)
  `(ffi:c-inline () () :object "_ecl_inner_frame"
                       :one-liner t :side-effects nil))

(defun c1stack-push (args)
  `(progn
     (ffi:c-inline ,args (t t) :void "ecl_stack_frame_push(#0,#1)"
                               :one-liner t :side-effects t)
     1))

(defun c1stack-push-values (args)
  (let ((frame-var (pop args))
        (form (pop args)))
    (make-c1form* 'STACK-PUSH-VALUES :type '(VALUES)
                  :args
                  (c1expr form)
                  (c1expr `(ffi:c-inline (,frame-var) (t)
                                         :void "ecl_stack_frame_push_values(#0)"
                                         :one-liner t :side-effects t)))))

(defun c1stack-pop (args)
  `(ffi:c-inline ,args (t) (values &rest t)
                 "cl_env_copy->values[0]=ecl_stack_frame_pop_values(#0);"
                 :one-liner nil :side-effects t))

(defun c1apply-from-stack-frame (args)
  `(ffi:c-inline ,args (t t) (values &rest t)
                 "cl_env_copy->values[0]=ecl_apply_from_stack_frame(#0,#1);"
                 :one-liner nil :side-effects t))
