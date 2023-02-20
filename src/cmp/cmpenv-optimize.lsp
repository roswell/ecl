
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
    (let* ((old (cmp-env-policy env))
           (flag (if (or (endp (rest decl)) (second decl)) 3 0))
           (new (compute-policy (list (list (first decl) flag)) old)))
      (cmp-env-add-declaration 'optimization (list new) env))))

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

(define-policy ext:assume-no-errors :off safety 1)

(define-policy ext:assume-right-type :alias ext:assume-no-errors)

(define-policy ext:type-assertions :anti-alias ext:assume-no-errors
  "Generate type assertions when inlining accessors and other functions.")

(define-policy ext:check-stack-overflow :on safety 2
  "Add a stack check to every function")

(define-policy ext:check-arguments-type :on safety 1
  "Generate CHECK-TYPE forms for function arguments with type declarations")

(define-policy ext:array-bounds-check :on safety 1
  "Check out of bounds access to arrays")

(define-policy ext:global-var-checking :on safety 3
  "Read the value of a global variable even if it is discarded, ensuring it is bound")

(define-policy ext:global-function-checking :on safety 3
  "Read the binding of a global function even if it is discarded")

(define-policy ext:check-nargs :on safety 1 :only-on ext:check-arguments-type 1
  "Check that the number of arguments a function receives is within bounds")

(define-policy ext:the-is-checked :on safety 1
  "THE is equivalent to EXT:CHECKED-VALUE. Otherwise THE is equivalent to EXT:TRULY-THE.")

;;
;; INLINING POLICY
;;

(define-policy ext:assume-types-dont-change :off safety 1
  "Assume that type and class definitions will not change")

(define-policy ext:inline-slot-access :on speed 1 :off debug 2 :off safety 2
  "Inline access to structures and sealed classes")

(define-policy ext:inline-accessors :off debug 2 :off space 2
  "Inline access to object slots, including conses and arrays")

(define-policy ext:inline-bit-operations :off space 2
  "Inline LDB and similar functions")

(define-policy ext:open-code-aref/aset :alias ext:inline-accessors
  "Inline access to arrays")

(define-policy ext:evaluate-forms :off debug 1
  "Pre-evaluate a function that takes constant arguments")

(define-policy ext:use-direct-C-call :off debug 2
  "Emit direct calls to a function whose C name is known")

(define-policy ext:inline-type-checks :off space 2
  "Expand TYPEP and similar forms in terms of simpler functions, such as FLOATP,
INTGERP, STRINGP.")

(define-policy ext:inline-sequence-functions :off space 2
  "Inline functions such as MAP, MEMBER, FIND, etc")

;;
;; DEBUG POLICY
;;

(define-policy ext:debug-variable-bindings :on debug 3
  :requires (policy-debug-ihs-frame env)
  ;; We can only create variable bindings when the function has an IHS frame!!!
  "Create a debug vector with the bindings of each LET/LET*/LAMBDA form?")

(define-policy ext:debug-ihs-frame :on debug 3
  "Let the functions appear in backtraces")
