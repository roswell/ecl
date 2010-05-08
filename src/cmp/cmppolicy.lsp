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

(eval-when (:compile-toplevel :execute)
  (defparameter +last-optimization-bit+ 0)
  (defmacro define-policy (&whole whole name &rest conditions)
    (let* ((test (ash 1 +last-optimization-bit+))
           (constant-name
            (intern (concatenate 'string "+POLICY-" (symbol-name name) "+")
                    (find-package "C")))
           (function-name
            (intern (concatenate 'string "POLICY-" (symbol-name name))
                    (find-package "C")))
           (doc (find-if #'stringp conditions)))
      (incf +last-optimization-bit+)
      (loop with conditions = (remove doc conditions)
         for case = (pop conditions)
         while case
         collect
           (case case
             (:on
              `(>= (cmp-env-optimization ',(pop conditions)) ,(pop conditions)))
             (:off
              `(< (cmp-env-optimization ',(pop conditions)) ,(pop conditions)))
             (:requires
              (pop conditions))
             (otherwise
              (error "Syntax error in macro~%  ~A"
                     `(define-policy ,@whole))))
         into expressions
         finally
           (return
(print
             `(progn
                (defparameter ,constant-name ,test ,@(and doc (list doc)))
                (defun ,function-name (&optional (env *cmp-env*))
                  ,@(and doc (list doc))
                  (and ,@expressions))))))))
)

(define-policy assume-right-type :off safety 2)

;(defun policy-assume-right-type (&optional (env *cmp-env*))
;  (< (cmp-env-optimization 'safety env) 2))

(define-policy check-stack-overflow :on safety 2
  "Aadd a stack check to every function")

;(defun policy-check-stack-overflow (&optional (env *cmp-env*))
;  "Do we add a stack check to every function?"
;  (>= (cmp-env-optimization 'safety env) 2))

(define-policy inline-slot-access :on speed 1 :off debug 2 :off safety 2
  "Inline access to structures and sealed classes")

;(defun policy-inline-slot-access-p (&optional (env *cmp-env*))
;  "Do we inline access to structures and sealed classes?"
;  (or (< (cmp-env-optimization 'safety env) 2)
;      (<= (cmp-env-optimization 'safety env) (cmp-env-optimization 'speed env))))

(define-policy check-all-arguments :on safety 2
  "Verify that arguments have always the right type")

;(defun policy-check-all-arguments-p (&optional (env *cmp-env*))
;  "Do we assume that arguments are the right type?"
;  (> (cmp-env-optimization 'safety env) 1))

(define-policy automatic-check-type :on safety 1
  "Generate CHECK-TYPE forms for function arguments with type declarations")

;(defun policy-automatic-check-type-p (&optional (env *cmp-env*))
;  "Generate CHECK-TYPE forms for function arguments with type declarations"
;  (and *automatic-check-type-in-lambda*
;       (>= (cmp-env-optimization 'safety env) 1)))

(define-policy assume-types-dont-change :off safety 1
  "Assume that type and class definitions will not change")

;(defun policy-assume-types-dont-change-p (&optional (env *cmp-env*))
;  "Assume that type and class definitions will not change"
;  (<= (cmp-env-optimization 'safety env) 1))

(define-policy open-code-aref/aset :off space 2 :off debug 2
  "Inline access to arrays")

;(defun policy-open-code-aref/aset-p (&optional (env *cmp-env*))
;  "Inline access to arrays"
;  (< (cmp-env-optimization 'debug env) 2))

(define-policy open-code-accessors :off debug 2
  "Inline access to object slots, including conses and arrays")

;(defun policy-open-code-accessors (&optional (env *cmp-env*))
;  "Inline access to object slots, including conses and arrays"
;  (< (cmp-env-optimization 'debug env) 2))

(define-policy array-bounds-check :on safety 1
  "Check out of bounds access to arrays")

;(defun policy-array-bounds-check-p (&optional (env *cmp-env*))
;  "Check out of bounds access to arrays"
;  (>= (cmp-env-optimization 'safety env) 1))

(define-policy evaluate-forms :off debug 1
  "Pre-evaluate a function that takes constant arguments")

;(defun policy-evaluate-forms (&optional (env *cmp-env*))
;  "Pre-evaluate a function that takes constant arguments"
;  (<= (cmp-env-optimization 'debug env) 1))

(define-policy use-direct-C-call :off debug 2
  "Emit direct calls to a function whose C name is known")

;(defun policy-use-direct-C-call (&optional (env *cmp-env*))
;  "Emit direct calls to a function whose C name is known"
;  (<= (cmp-env-optimization 'debug env) 1))

(define-policy global-var-checking :on safety 2
  "Read the value of a global variable even if it is discarded, ensuring it is bound")

;(defun policy-global-var-checking (&optional (env *cmp-env*))
;  "Read the value of a global variable even if it is discarded, ensuring it is bound"
;  (>= (cmp-env-optimization 'safety env) 1))

(define-policy global-function-checking :on safety 2
  "Read the binding of a global function even if it is discarded")

;(defun policy-global-function-checking (&optional (env *cmp-env*))
;  "Read the binding of a global function even if it is discarded"
;  (>= (cmp-env-optimization 'safety env) 1))

(define-policy debug-variable-bindings :on safety 3
  :requires (policy-debug-ihs-frame env)
  ;; We can only create variable bindings when the function has an IHS frame!!!
  "Create a debug vector with the bindings of each LET/LET*/LAMBDA form?")

;(defun policy-debug-variable-bindings (&optional (env *cmp-env*))
;  "Create a debug vector with the bindings of each LET/LET*/LAMBDA form?"
;  (and (policy-debug-ihs-frame env)
;       (>= (cmp-env-optimization 'debug env) 3)))

(define-policy debug-ihs-frame :on debug 3
  "Let the functions appear in backtraces")

;(defun policy-debug-ihs-frame (&optional (env *cmp-env*))
;  "Let the functions appear in backtraces"
;  ;; Note that this is a prerequisite for registering variable bindings.
;  (or (>= (cmp-env-optimization 'debug env) 2)
;      (first (cmp-env-search-declaration 'policy-debug-ihs-frame))))

(define-policy check-nargs :on safety 1
  "Check that the number of arguments a function receives is within bounds")

(defun policy-check-nargs (&optional (env *cmp-env*))
  (>= (cmp-env-optimization 'safety) 1))

(defun safe-compile ()
  (>= (cmp-env-optimization 'safety) 2))

(defun compiler-check-args ()
  (>= (cmp-env-optimization 'safety) 1))

(defun compiler-push-events ()
  (>= (cmp-env-optimization 'safety) 3))
