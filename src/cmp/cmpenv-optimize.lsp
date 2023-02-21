
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See the file 'LICENSE' for the copyright details.

(in-package #:compiler)

(defun default-policy ()
  (compute-policy `((space ,*space*)
                    (safety ,*safety*)
                    (debug ,*debug*)
                    (speed ,*speed*)
                    (compilation-speed ,*compilation-speed*))
                  0))

(defun cmp-env-policy (env)
  (or (first (cmp-env-search-declaration 'optimization env))
      (default-policy)))

(defun add-default-optimizations (env)
  (if (cmp-env-search-declaration 'optimization env)
      env
      (cmp-env-add-declaration 'optimization (list (default-policy)) env)))

(defun cmp-env-add-optimizations (decl &optional (env *cmp-env*))
  (let* ((old (cmp-env-policy env))
         (new (compute-policy decl old)))
    (cmp-env-add-declaration 'optimization (list new) env)))

(defun maybe-add-policy (decl &optional (env *cmp-env*))
  (when (and (consp decl)
             (<= (list-length decl) 2)
             (gethash (first decl) *optimization-quality-switches*))
    (let* ((name (first decl))
           (value (if (or (endp (rest decl)) (second decl))
                      (if (standard-optimization-quality-p name)
                          3
                          1)
                      0))
           (old-policy (cmp-env-policy env))
           (new-policy (compute-policy (list (list name value)) old-policy)))
      (cmp-env-add-declaration 'optimization (list new-policy) env))))

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

(defun safe-compile ()
  (>= (cmp-env-optimization 'safety) 2))

(defun compiler-push-events ()
  (>= (cmp-env-optimization 'safety) 3))



;;
;; ERROR CHECKING POLICY
;;

(define-policy ext:assume-no-errors
  "All bets are off."
  (:off safety 1))

(define-policy-alias ext:assume-right-type
  "Don't insert optional runtime type checks for known types."
  (:alias ext:assume-no-errors))

(define-policy-alias ext:type-assertions
  "Generate type assertions when inlining accessors and other functions."
  (:anti-alias ext:assume-no-errors))

(define-policy ext:check-stack-overflow
  "Add a stack check to every function"
  (:on safety 2))

(define-policy ext:check-arguments-type
  "Generate CHECK-TYPE forms for function arguments with type declarations."
  (:on safety 1))

(define-policy ext:array-bounds-check
  "Check out of bounds access to arrays."
  (:on safety 1))

(define-policy ext:global-var-checking
  "Read the value of a global variable even if it is discarded, ensuring it is bound."
  (:on safety 3))

(define-policy ext:global-function-checking
  "Read the binding of a global function even if it is discarded."
  (:on safety 3))

(define-policy ext:check-nargs
  "Check that the number of arguments a function receives is within bounds."
  (:on safety 1)
  (:only-on ext:check-arguments-type))

(define-policy ext:the-is-checked
  "THE is equivalent to EXT:CHECKED-VALUE. Otherwise THE is equivalent to EXT:TRULY-THE."
  (:on safety 1))

;;
;; INLINING POLICY
;;

(define-policy ext:assume-types-dont-change
  "Assume that type and class definitions will not change."
  (:off safety 1))

(define-policy ext:inline-slot-access
  "Inline access to structures and sealed classes."
  (:on speed 1)
  (:off debug 2)
  (:off safety 2))

(define-policy ext:inline-accessors
  "Inline access to object slots, including conses and arrays."
  (:off debug 2)
  (:off space 2))

(define-policy ext:inline-bit-operations
  "Inline LDB and similar functions."
  (:off space 2))

(define-policy-alias ext:open-code-aref/aset
  "Inline access to arrays."
  (:alias ext:inline-accessors))

(define-policy ext:evaluate-forms
  "Pre-evaluate a function that takes constant arguments."
  (:off debug 1))

(define-policy ext:use-direct-C-call
  "Emit direct calls to a function whose C name is known."
  (:off debug 2))

(define-policy ext:inline-type-checks
  "Expand TYPEP and similar forms in terms of simpler functions, such as FLOATP, INTGERP, STRINGP."
  (:off space 2))

(define-policy ext:inline-sequence-functions
  "Inline functions such as MAP, MEMBER, FIND, etc."
  (:off space 2))

;;
;; DEBUG POLICY
;;

(define-policy ext:debug-variable-bindings
  "Create a debug vector with the bindings of each LET/LET*/LAMBDA form."
  ;; We can only create variable bindings when the function has an IHS frame!!!
  (:requires (policy-debug-ihs-frame env))
  (:on debug 3))

(define-policy ext:debug-ihs-frame
  "Let the functions appear in backtraces."
  (:on debug 3))
