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

(eval-when (:compile-toplevel :execute)
  (defconstant +optimization-quality-orders+ '(debug safety speed space)))

(eval-when (:compile-toplevel :execute)
  (defparameter *optimization-quality-switches*
    (loop with hash = (make-hash-table :size 64 :test #'eq)
       for name in +optimization-quality-orders+
       for i from 0 by 4
       for list = (loop with mask = (ash #b1111 i)
                     for level from 0 to 3
                     for bits = (ash 1 (+ level i))
                     collect (cons bits (logxor bits mask)))
       do (setf (gethash name hash) list)
       finally (return hash)))
  (setf (gethash 'compilation-speed *optimization-quality-switches*)
        '#1=((0 . 0) . #1#)))

(eval-when (:load-toplevel)
  (defparameter *optimization-quality-switches*
    #.*optimization-quality-switches*))

#.`(eval-when (:compile-toplevel :execute :load-toplevel)
  ,@(loop for name in +optimization-quality-orders+
       for i from 0 by 4
       for fun-name = (intern (concatenate 'string
                                           "POLICY-TO-" (symbol-name name) "-LEVEL"))
       collect `(defun ,fun-name (policy)
                  (loop for level from 0 to 3
                     when (logbitp (+ level ,i) policy)
                     return level))))

(defun optimization-quality-switches (type index)
  (nth index (gethash type *optimization-quality-switches*)))

(defun compute-policy (arguments old-bits)
  (let* ((bits old-bits))
    (dolist (x arguments)
      (let (flags name value)
        (cond ((symbolp x)
               (setq flags (optimization-quality-switches x 3)
                     value 3
                     name x))
              ((or (not (consp x))
                   (not (consp (cdr x)))
                   (not (numberp (second x)))
                   (not (<= 0 (second x) 3))))
              (t
               (setf name (first x)
                     value (second x)
                     flags (optimization-quality-switches name (second x)))))
        (if (null flags)
            (cmpwarn "Illegal or unknown OPTIMIZE proclamation ~s" x)
            (setf bits (logandc2 (logior bits (car flags)) (cdr flags))))))
    bits))

(defun default-policy ()
  (compute-policy `((space ,*space*)
                    (safety ,*safety*)
                    (debug ,*debug*)
                    (speed ,*speed*))
                  0))

(defun cmp-env-policy (env)
  (or (first (cmp-env-search-declaration 'optimization env))
      (default-policy)))

(defun cmp-env-add-optimizations (decl &optional (env *cmp-env*))
  (let* ((old (cmp-env-policy env))
         (new (compute-policy decl old)))
    (cmp-env-add-declaration 'optimization (list new) env)))

(defun policy-declaration-name-p (name)
  (and (gethash name *optimization-quality-switches*) t))

(defun maybe-add-policy (decl &optional (env *cmp-env*))
  (let ((record (gethash (first decl) *optimization-quality-switches*)))
    (when (and record (consp decl) (eql (list-length decl) 1))
      (let* ((old (cmp-env-policy env))
             (new (compute-policy (list (first decl)) old)))
        (cmp-env-add-declaration 'optimization (list new) env)))))

(defun add-default-optimizations (env)
  (if (cmp-env-search-declaration 'optimization env)
      env
      (cmp-env-add-declaration 'optimization (list (default-policy)) env)))

(defun cmp-env-all-optimizations (&optional (env *cmp-env*))
  (let ((o (cmp-env-policy env)))
    (list (policy-to-debug-level o)
          (policy-to-safety-level o)
          (policy-to-space-level o)
          (policy-to-speed-level o))))

(defun cmp-env-optimization (property &optional (env *cmp-env*))
  (let ((o (cmp-env-policy env)))
    (case property
      (debug (policy-to-debug-level o))
      (safety (policy-to-safety-level o))
      (space (policy-to-space-level o))
      (speed (policy-to-speed-level o)))))

(eval-when (:compile-toplevel :execute)
  (defparameter +last-optimization-bit+ 17)
  (defun policy-bits (quality level)
    (loop for i from level to 3
       sum (car (optimization-quality-switches quality i))))
  (defun policy-declaration-name (base)
    (intern (symbol-name base) (find-package "EXT")))
  (defun policy-function-name (base)
    (intern (concatenate 'string "POLICY-" (symbol-name base))
            (find-package "C")))
  (defmacro define-policy (&whole whole name &rest conditions)
    (let* ((test (ash 1 +last-optimization-bit+))
           (declaration-name (policy-declaration-name name))
           (function-name (policy-function-name name))
           (doc (find-if #'stringp conditions)))
      (export declaration-name (find-package "EXT"))
      ;; If it is an alias, just copy the bits
      ;; Register as an optimization quality with its own flags
      (let* ((circular-list (list (cons test 0)))
             (flags-list (list* (cons 0 test)
                                circular-list)))
        (rplacd circular-list circular-list)
        (incf +last-optimization-bit+)
        (setf (gethash declaration-name *optimization-quality-switches*)
              flags-list))
      ;; Scan the definition and correct the flags
      (loop with extra = '()
         with slow = '()
         with bits-on = test
         with bits-off = 0
         with conditions = (remove doc conditions)
         for case = (pop conditions)
         while case
         do
           (case case
             (:alias
              (let* ((alias (first conditions)))
                (setf (gethash declaration-name *optimization-quality-switches*)
                      (gethash (policy-declaration-name alias)
                               *optimization-quality-switches*))
                (return `(defun ,function-name (&optional (env *cmp-env*))
                           ,@(and doc (list doc))
                           (,(policy-function-name alias) env)))))
             (:anti-alias
              (let* ((alias (first conditions))
                     (bits (gethash (policy-declaration-name alias)
                               *optimization-quality-switches*)))
                (setf bits (list (second bits)
                                 (first bits)))
                (rplacd (cdr bits) (cdr bits))
                (setf (gethash declaration-name *optimization-quality-switches*)
                      bits)
                (return `(defun ,function-name (&optional (env *cmp-env*))
                           ,@(and doc (list doc))
                           (not (,(policy-function-name alias) env))))))
             (:on
              (push `(>= (cmp-env-optimization ',(first conditions) env)
                         ,(second conditions))
                    slow)
              (setf bits-on (logior (policy-bits (pop conditions)
                                                 (pop conditions))
                                    bits-on)))
             (:off
              (push `(< (cmp-env-optimization ',(first conditions) env)
                        ,(second conditions))
                    slow)
              (setf bits-off (logior (policy-bits (pop conditions)
                                                  (pop conditions))
                                     bits-off)))
             (:requires
              (push (pop conditions) extra))
             (otherwise
              (error "Syntax error in macro~%  ~A"
                     `(define-policy ,@whole))))
         finally
           (progn
             (when (zerop (logandc2 bits-on test))
               (setf bits-on (logior (policy-bits 'speed 0) bits-on)))
             (return
               `(defun ,function-name (&optional (env *cmp-env*))
                  ,@(and doc (list doc))
                  (let ((bits (cmp-env-policy env)))
                    (and (logtest bits ,bits-on)
                         (not (logtest bits ,bits-off))
                         ,@extra)))))))))


;;
;; ERROR CHECKING POLICY
;;

(define-policy assume-no-errors :off safety 2)

(define-policy assume-right-type :alias assume-no-errors)

(define-policy type-assertions :anti-alias assume-no-errors
  "Generate type assertions when inlining accessors and other functions.")

(define-policy check-stack-overflow :on safety 2
  "Add a stack check to every function")

(define-policy ext:check-arguments-type :on safety 1
  "Generate CHECK-TYPE forms for function arguments with type declarations")

(define-policy array-bounds-check :on safety 1
  "Check out of bounds access to arrays")

(define-policy global-var-checking :alias assume-no-errors
  "Read the value of a global variable even if it is discarded, ensuring it is bound")

(define-policy global-function-checking :alias assume-no-errors
  "Read the binding of a global function even if it is discarded")

(define-policy check-nargs :on safety 1 :on ext:check-arguments-type 1
  "Check that the number of arguments a function receives is within bounds")

;;
;; INLINING POLICY
;;

(define-policy assume-types-dont-change :off safety 1
  "Assume that type and class definitions will not change")

(define-policy inline-slot-access :on speed 1 :off debug 2 :off safety 2
  "Inline access to structures and sealed classes")

(define-policy inline-accessors :off debug 2
  "Inline access to object slots, including conses and arrays")

(define-policy open-code-aref/aset :alias inline-accessors
  "Inline access to arrays")

(define-policy evaluate-forms :off debug 1
  "Pre-evaluate a function that takes constant arguments")

(define-policy use-direct-C-call :off debug 2
  "Emit direct calls to a function whose C name is known")

(define-policy inline-type-checks :off space 2
  "Expand TYPEP and similar forms in terms of simpler functions, such as FLOATP,
INTGERP, STRINGP.")

(define-policy inline-sequence-functions :off space 2
  "Inline functions such as MAP, MEMBER, FIND, etc")


;;
;; DEBUG POLICY
;;

(define-policy debug-variable-bindings :on safety 3
  :requires (policy-debug-ihs-frame env)
  ;; We can only create variable bindings when the function has an IHS frame!!!
  "Create a debug vector with the bindings of each LET/LET*/LAMBDA form?")

(define-policy debug-ihs-frame :on debug 3
  "Let the functions appear in backtraces")

(defun safe-compile ()
  (>= (cmp-env-optimization 'safety) 2))

(defun compiler-push-events ()
  (>= (cmp-env-optimization 'safety) 3))
