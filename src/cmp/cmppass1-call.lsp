;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

(in-package #:compiler)

(defun not-a-closure-p (fname)
  (declare (si::c-local))
  (not (and (fboundp fname) (nth-value 1 (function-lambda-expression (fdefinition fname))))))

(defun function-form-p (form)
  (declare (si::c-local))
  (if (and (consp form)
           (eq (first form) 'CL:FUNCTION))
      (prog1 t
        (check-args-number 'CL:FUNCTION (rest form) 1 1))
      nil))

(defun lambda-form-p (form)
  (if (function-form-p form)
      (let ((fval (second form)))
        (and (consp fval)
             (member (car fval) '(CL:LAMBDA EXT:LAMBDA-BLOCK))
             t))
      nil))

(defun unoptimized-funcall (fun arguments)
  (let ((fun-form (c1expr fun))
        (fun-args (c1args* arguments)))
    (make-c1form* 'FCALL
                  :sp-change t
                  :side-effects t
                  :args fun-form fun-args nil :unknown)))

(defun optimized-lambda-call (lambda-form arguments apply-p)
  (multiple-value-bind (bindings body)
      (transform-funcall/apply-into-let* lambda-form arguments apply-p)
    `(let* ,bindings ,@body)))

(defun try-optimized-lambda-call (fun args apply-p)
  (if (lambda-form-p fun)
      (optimized-lambda-call (second fun) args apply-p)
      nil))

(defun try-macro-expression-call (fun args)
  (unless (consp fun)
    (return-from try-macro-expression-call nil))
  (let* ((name (first fun))
         (fd (and (symbolp name)
                  ;; We do not want to macroexpand 'CL:THE
                  (not (eq name 'CL:THE))
                  (cmp-macro-function name))))
    (if fd
        (c1funcall (list* (cmp-expand-macro fd fun) args))
        nil)))

(defun try-function-special-call (fun args)
  (unless (function-form-p fun)
    (return-from try-function-special-call nil))
  (let ((fname (second fun)))
    (if (si:valid-function-name-p fname)
        (c1call fname args nil)
        (cmperr "Malformed function name: ~A." fname))))

(defun c1funcall (args)
  (check-args-number 'CL:FUNCALL args 1)
  (destructuring-bind (fun . arguments) args
    (or ;; (FUNCALL   (LAMBDA ...) ...) or (FUNCALL   (EXT:LAMBDA-BLOCK ...) ...)
        ;; (FUNCALL #'(LAMBDA ...) ...) or (FUNCALL #'(EXT:LAMBDA-BLOCK ...) ...)
        (try-optimized-lambda-call fun arguments nil)
        ;; (FUNCALL macro-expression ...)
        (try-macro-expression-call fun arguments)
        ;; (FUNCALL (FUNCTION function-name) ...)
        (try-function-special-call fun arguments)
        ;; (FUNCALL lisp-expression ...) or (FUNCALL atomic-expression ...)
        (unoptimized-funcall fun arguments))))

;;; Further optimization opportunities:
;;; - expand macros
;;; - expand compiler macros
;;; - rely on the function proclamations
;;; - rely on the inferred argument type
(defun try-spill-values-funcall (fun arguments)
  (flet ((values-form (arg)
           (setq arg (chk-symbol-macrolet arg))
           (if (or (atom arg)
                   (eq (car arg) 'CL:VALUES))
               arg
               (return-from try-spill-values-funcall nil))))
    (loop for arg in arguments
          for val = (values-form arg)
          if (atom val)
            collect val into values
          else
            append (cdr val) into values
          finally (return (c1funcall (list* fun values))))))

(defun try-macro-expression-mvc (fun arguments)
  (unless (consp fun)
    (return-from try-macro-expression-mvc nil))
  (let* ((name (first fun))
         (fd (and (symbolp name)
                  ;; We do not want to macroexpand 'CL:THE
                  (not (eq name 'CL:THE))
                  (cmp-macro-function name))))
    (if fd
        (c1multiple-value-call (list* (cmp-expand-macro fd fun) arguments))
        nil)))

(defun try-function-special-mvc (fun arguments)
  (unless (function-form-p fun)
    (return-from try-function-special-mvc nil))
  (let ((fname (second fun)))
    (if (si:valid-function-name-p fname)
        (ext:if-let ((funob (local-function-ref fname)))
          (make-c1form* 'MCALL
                        :sp-change t ; conservative estimate
                        :side-effects t ; conservative estimate
                        :args (c1expr fun) (c1args* arguments) funob :local)
          (make-c1form* 'MCALL
                        :sp-change (function-may-change-sp fname)
                        :side-effects (function-may-have-side-effects fname)
                        :args (c1expr fun) (c1args* arguments) fname :global))
        (if (lambda-form-p fun)
            nil
            (cmperr "Malformed function name: ~A." fname)))))

(defun unoptimized-mvc (fun arguments)
  (let ((fun-form (c1expr fun))
        (fun-args (c1args* arguments)))
    (make-c1form* 'MCALL
                  :sp-change t
                  :side-effects t
                  :args fun-form fun-args nil :unknown)))

(defun c1multiple-value-call (args &aux forms)
  (check-args-number 'CL:MULTIPLE-VALUE-CALL args 1)
  (destructuring-bind (fun . arguments) args
    (or ;; (M-V-C expression (VALUES A B) (VALUES X Y Z) ...)
        ;; => (FUNCALL expression A B X Y Z ...)
        (try-spill-values-funcall fun arguments)
        ;; (M-V-C macro-expression ...)
        (try-macro-expression-mvc fun arguments)
        ;; (M-V-C (FUNCTION function-name) ...)
        (try-function-special-mvc fun arguments)
        ;; (M-V-C lisp-expression ...) or (M-V-C atomic-expression ...)
        (unoptimized-mvc fun arguments))))

(defun c1apply (args)
  (check-args-number 'CL:APPLY args 2)
  (flet ((default-apply (fun arguments)
           (let ((form (c1funcall (list* '(function CL:APPLY) fun arguments))))
             (when (function-form-p fun)
               (let* ((fname (second fun))
                      (type (get-return-type fname)))
                 (when type
                   (setf (c1form-type form) type))))
             form))
         (last-argument-is-frame-p (arguments)
           ;; Uses frames instead of lists as last argument. ;; gross hack!
           (eql (first (last arguments)) 'clos:.combined-method-args.)))
    (destructuring-bind (fun . arguments) args
      (if (last-argument-is-frame-p arguments)
          (default-apply fun arguments)
          (or (try-optimized-lambda-call fun arguments t)
              (default-apply fun arguments))))))

(defun c1call (fname args macros-allowed &aux fd success can-inline)
  (cond ((setq fd (local-function-ref fname))
         (c1call-local fname fd args))
        ((and macros-allowed            ; macrolet
              (setq fd (cmp-env-search-macro fname)))
         (cmp-expand-macro fd (list* fname args)))
        ((and (setq can-inline (inline-possible fname))
              (setq fd (compiler-macro-function fname))
              (progn
                (multiple-value-setq (fd success)
                  (cmp-expand-compiler-macro fd fname args))
                success))
         fd)
        ((and can-inline
              (progn
                (multiple-value-setq (fd success)
                  (clos-compiler-macro-expand fname args))
                success))
         fd)
        ((and macros-allowed            ; global macro
              (setq fd (macro-function fname)))
         (cmp-expand-macro fd (list* fname args)))
        ((and (setq can-inline (declared-inline-p fname))
              (plusp *inline-max-depth*)
              (<= (cmp-env-optimization 'space) 1))
         (cond ((and (setq fd (find fname *global-funs* :key #'fun-name :test #'same-fname-p))
                     (not (fun-closure fd)))
                (cmpnote "Inlining ~a" fname)
                (inline-local (fun-lambda-expression fd) fd args))
               ((and (function-form-p can-inline)
                     (not-a-closure-p fname))
                (let ((*inline-max-depth* (1- *inline-max-depth*)))
                  (cmpnote "Inlining ~a" fname)
                  `(funcall ,can-inline ,@args)))
               (t (c1call-global fname args))))
        (t (c1call-global fname args))))

(defun inline-local (lambda fun args)
  (declare (si::c-local))
  (let ((*inline-max-depth* (1- *inline-max-depth*))
        (setjmps *setjmps*)
        (*cmp-env* (cmp-env-copy)))
    ;; To inline the function, we transform it into a let* statement.
    (multiple-value-bind (bindings body)
        (transform-funcall/apply-into-let* lambda args nil)
      (multiple-value-bind (let-vars let-inits specials other-decls body)
          (process-let-bindings 'LET* bindings body)
        ;; We have to compile the function body in the same
        ;; environment in which the function was defined to get
        ;; inlining of closures right.
        (let ((*cmp-env* (cmp-env-copy (fun-cmp-env fun))))
          (mapc #'cmp-env-register-var let-vars)
          (process-let-body 'LET* let-vars let-inits specials other-decls body setjmps))))))

(defun c1call-local (fname fun args)
  (declare (si::c-local))
  (let ((lambda (fun-lambda-expression fun)))
    (when (and lambda
               (declared-inline-p fname)
               (plusp *inline-max-depth*))
      (return-from c1call-local (inline-local lambda fun args))))
  (make-c1form* 'FCALL
                :sp-change t ; conservative estimate
                :side-effects t ; conservative estimate
                :args (c1expr `(function ,fname)) (c1args* args) fun :local))

(defun c1call-global (fname args)
  (let* ((forms (c1args* args)))
    ;; If all arguments are constants, try to precompute the function
    ;; value. We abort when the function signals an error or the value
    ;; is not printable.
    (let ((value (c1call-constant-fold fname forms)))
      (when value
        (return-from c1call-global value)))
    ;; Otherwise emit a global function call
    (make-c1form* 'FCALL
                  :sp-change (function-may-change-sp fname)
                  :side-effects (function-may-have-side-effects fname)
                  :args (c1expr `(function ,fname)) forms fname :global)))

(defun c1call-constant-fold (fname forms)
  (when (and (si:get-sysprop fname 'pure)
             (policy-evaluate-forms)
             (inline-possible fname))
    (handler-case
        (loop with all-values = '()
              with constant-p
              with v
              for form in forms
              do (if (multiple-value-setq (constant-p v)
                       (c1form-constant-p form))
                     (push v all-values)
                     (return nil))
              finally
                 (return
                   (let ((results (multiple-value-list (apply fname (nreverse all-values)))))
                     (if (endp (rest results))
                         (c1constant-value (first results))
                         (let ((results (mapcar #'c1constant-value results)))
                           (when (every #'identity results)
                             (make-c1form* 'CL:VALUES :args results)))))))
      (error (c) (cmpdebug "Can't constant-fold ~s ~s: ~a~%" fname forms c)))))

;;; Transform a (funcall lambda-form arguments) or (apply lambda-form
;;; arguments) expression into an equivalent let* statement. Returns
;;; the bindings and body as two values.
(defun transform-funcall/apply-into-let* (lambda-form arguments apply-p
                                          &aux apply-list apply-var
                                          let-vars extra-stmts all-keys)
  (when (eq (first lambda-form) 'ext:lambda-block)
    (setf lambda-form (macroexpand-1 lambda-form)))
  (multiple-value-bind (requireds optionals rest key-flag keywords
                                  allow-other-keys aux-vars)
      (cmp-process-lambda-list (second lambda-form))
    (when apply-p
      (setf apply-list (first (last arguments))
            apply-var (gensym)
            arguments (butlast arguments)))
    ;; 1. check number of arguments
    (let* ((n-args-wanted-min (first requireds))
           (n-args-wanted-max (if (or key-flag allow-other-keys rest)
                                  call-arguments-limit
                                  (+ (first requireds) (first optionals))))
           (apply-constant-args-p (and apply-p (constantp apply-list)
                                       (listp (ext:constant-form-value apply-list))))
           (n-args-got-min (if apply-constant-args-p
                               (+ (length arguments)
                                  (length (ext:constant-form-value apply-list)))
                               (length arguments)))
           (n-args-got-max (cond ((and apply-p (not apply-constant-args-p))
                                  nil)  ; unknown maximum number of arguments
                                 (t n-args-got-min))))
      (when (and n-args-got-max (< n-args-got-max n-args-wanted-min))
        (too-few-args lambda-form n-args-wanted-min n-args-got-min))
      (when (> n-args-got-min n-args-wanted-max)
        (too-many-args lambda-form n-args-wanted-max n-args-got-max)))
    ;; 2. construct forms to evaluate arguments in order
    (setf arguments (copy-list arguments))
    (do ((scan arguments (cdr scan)))
        ((endp scan))
      (let ((form (first scan)))
        (unless (constantp form)
          (let ((aux-var (gensym)))
            (push `(,aux-var ,form) let-vars)
            (setf (car scan) aux-var)))))
    (when apply-var
      (push `(,apply-var ,apply-list) let-vars))
    ;; 3. process required parameters
    (dolist (i (cdr requireds))
      (push (list i
                  (cond (arguments
                         (pop arguments))
                        (apply-p
                         `(if ,apply-var
                              (pop ,apply-var)
                              (si::dm-too-few-arguments nil)))))
            let-vars))
    ;; 4. process optional parameters
    (do ((scan (cdr optionals) (cdddr scan)))
        ((endp scan))
      (let ((opt-var (first scan))
            (opt-flag (third scan))
            (opt-value (second scan)))
        (cond (arguments
               (setf let-vars
                     (list* `(,opt-var ,(pop arguments))
                            `(,opt-flag t)
                            let-vars)))
              (apply-p
               (setf let-vars
                     (list* `(,opt-var (if ,apply-var
                                           (pop ,apply-var)
                                           ,opt-value))
                            `(,opt-flag ,apply-var)
                            let-vars)))
              (t
               (setf let-vars
                     (list* `(,opt-var ,opt-value)
                            `(,opt-flag nil)
                            let-vars))))))
    ;; 5. process rest parameter
    (when (or key-flag allow-other-keys)
      (unless rest
        (setf rest (gensym))))
    (cond (rest
           (push `(,rest ,(if arguments
                              (if apply-p
                                  `(list* ,@arguments ,apply-var)
                                  `(list ,@arguments))
                              (if apply-p apply-var nil)))
                 let-vars))
          (apply-p
           (push `(when ,apply-var
                    (si::dm-too-many-arguments nil))
                 extra-stmts)))
    ;; 6. process keyword parameters
    (do ((scan (cdr keywords) (cddddr scan)))
        ((endp scan))
      (let ((keyword (first scan))
            (key-var (second scan))
            (key-value (third scan))
            (key-flag (or (fourth scan) (gensym))))
        (push keyword all-keys)
        (setf let-vars
              (list*
               `(,key-var (if (eq ,key-flag 'si::missing-keyword)
                              (prog1 ,key-value
                                     ,(when (fourth scan) `(setf ,key-flag nil)))
                              (prog1 ,key-flag
                                     ,(when (fourth scan) `(setf ,key-flag t)))))
               `(,key-flag (si::search-keyword ,rest ,keyword))
               let-vars))))
    (when (and key-flag (not allow-other-keys))
      (push `(si::check-keyword ,rest ',all-keys) extra-stmts))
    ;; 7. construct body
    (pop aux-vars)
    (loop while aux-vars
          do (push (list (pop aux-vars) (pop aux-vars)) let-vars))
    (values (nreverse (delete-if-not #'first let-vars))
            `(,@(and apply-var `((declare (ignorable ,apply-var))))
              ,@(multiple-value-bind (decl body)
                    (si::find-declarations (cddr lambda-form))
                  (append decl extra-stmts body))))))

(defun optimize-funcall/apply-lambda (lambda-form arguments apply-p)
  (multiple-value-bind (bindings body)
      (transform-funcall/apply-into-let* lambda-form arguments apply-p)
    `(let* ,bindings ,@body)))
