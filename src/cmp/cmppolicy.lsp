;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;; CMPPOLICY -- Code generation choices
;;;;

(in-package #-new-cmp "COMPILER" #+new-cmp "C-ENV")

(defun cmp-env-all-optimizations (&optional (env *cmp-env*))
  (or (cmp-env-search-declaration 'optimize)
      (list *debug* *safety* *space* *speed*)))

(defun cmp-env-optimization (property &optional (env *cmp-env*))
  (let ((x (cmp-env-all-optimizations env)))
    (case property
      (debug (first x))
      (safety (second x))
      (space (third x))
      (speed (fourth x)))))

(defun policy-assume-right-type (&optional (env *cmp-env*))
  (< (cmp-env-optimization 'safety env) 2))

(defun policy-check-stack-overflow (&optional (env *cmp-env*))
  "Do we add a stack check to every function?"
  (>= (cmp-env-optimization 'safety env) 2))

(defun policy-inline-slot-access-p (&optional (env *cmp-env*))
  "Do we inline access to structures and sealed classes?"
  (or (< (cmp-env-optimization 'safety env) 2)
      (<= (cmp-env-optimization 'safety env) (cmp-env-optimization 'speed env))))

(defun policy-check-all-arguments-p (&optional (env *cmp-env*))
  "Do we assume that arguments are the right type?"
  (> (cmp-env-optimization 'safety env) 1))

(defun policy-automatic-check-type-p (&optional (env *cmp-env*))
  "Do we generate CHECK-TYPE forms for function arguments with type declarations?"
  (and *automatic-check-type-in-lambda*
       (>= (cmp-env-optimization 'safety env) 1)))

(defun policy-assume-types-dont-change-p (&optional (env *cmp-env*))
  "Do we assume that type and class definitions will not change?"
  (<= (cmp-env-optimization 'safety env) 1))

(defun policy-open-code-aref/aset-p (&optional (env *cmp-env*))
  "Do we inline access to arrays?"
  (< (cmp-env-optimization 'debug env) 2))

(defun policy-open-code-accessors (&optional (env *cmp-env*))
  "Do we inline access to object slots, including conses and arrays?"
  (< (cmp-env-optimization 'debug env) 2))

(defun policy-array-bounds-check-p (&optional (env *cmp-env*))
  "Check access to array bounds?"
  (>= (cmp-env-optimization 'safety env) 1))

(defun policy-evaluate-forms (&optional (env *cmp-env*))
  "Pre-evaluate a function that takes constant arguments?"
  (<= (cmp-env-optimization 'debug env) 1))

(defun policy-use-direct-C-call (&optional (env *cmp-env*))
  "Emit direct calls to a function whose C name is known"
  (<= (cmp-env-optimization 'debug env) 1))

(defun policy-global-var-checking (&optional (env *cmp-env*))
  "Do we have to read the value of a global variable even if it is discarded?
Also, when reading the value of a global variable, should we ensure it is bound?"
  (>= (cmp-env-optimization 'safety env) 1))

(defun policy-global-function-checking (&optional (env *cmp-env*))
  "Do we have to read the binding of a global function even if it is discarded?"
  (>= (cmp-env-optimization 'safety env) 1))

(defun policy-debug-variable-bindings (&optional (env *cmp-env*))
  "Shall we create a vector with the bindings of each LET/LET*/LAMBDA form?"
  ;; We can only create variable bindings when the function has an IHS frame!!!
  (and (policy-debug-ihs-frame env)
       (>= (cmp-env-optimization 'debug env) 3)))

(defun policy-debug-ihs-frame (&optional (env *cmp-env*))
  "Shall we create an IHS frame so that this function shows up in backtraces?"
  ;; Note that this is a prerequisite for registering variable bindings.
  (or (>= (cmp-env-optimization 'debug env) 2)
      (first (cmp-env-search-declaration 'policy-debug-ihs-frame))))

(defun policy-check-nargs (&optional (env *cmp-env*))
  (>= (cmp-env-optimization 'safety) 1))

(defun safe-compile ()
  (>= (cmp-env-optimization 'safety) 2))

(defun compiler-check-args ()
  (>= (cmp-env-optimization 'safety) 1))

(defun compiler-push-events ()
  (>= (cmp-env-optimization 'safety) 3))
