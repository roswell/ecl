
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See file 'LICENSE' for the copyright details.

(in-package #:compiler)

(defun default-policy ()
  (compute-policy `((space ,*space*)
                    (safety ,*safety*)
                    (debug ,*debug*)
                    (speed ,*speed*))
                  0))

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
