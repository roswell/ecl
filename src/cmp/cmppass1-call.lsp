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

(defun unoptimized-long-call (fun arguments)
  (let ((frame (gensym))
        (f-arg (gensym)))
    `(with-stack ,frame
       (let ((,f-arg ,fun))
         ,@(loop for i in arguments collect `(stack-push ,frame ,i))
         (si::apply-from-stack-frame ,frame ,f-arg)))))

(defun unoptimized-funcall (fun arguments)
  (let ((l (length arguments)))
    (if (<= l si::c-arguments-limit)
        (make-c1form* 'FUNCALL :sp-change t :side-effects t
                               :args (c1expr fun) (c1args* arguments))
        (unoptimized-long-call fun arguments))))

(defun macroexpand-lambda-block (lambda)
  (if (eq (first lambda) 'EXT::LAMBDA-BLOCK)
      (macroexpand-1 lambda)
      lambda))

(defun c1funcall (args)
  (check-args-number 'FUNCALL args 1)
  (let ((fun (first args))
        (arguments (rest args))
        fd)
    (cond ;; (FUNCALL (LAMBDA ...) ...) or (FUNCALL (EXT::LAMBDA-BLOCK ...) ...)
      ((and (consp fun)
            (member (first fun) '(LAMBDA EXT::LAMBDA-BLOCK)))
       (optimize-funcall/apply-lambda (macroexpand-lambda-block fun)
                                      arguments nil))
      ;; (FUNCALL atomic-expression ...)
      ((atom fun)
       (unoptimized-funcall fun arguments))
      ;; (FUNCALL macro-expression ...)
      ((let ((name (first fun)))
         (setq fd (and (symbolp name)
                       ;; We do not want to macroexpand 'THE
                       (not (eq name 'THE))
                       (cmp-macro-function name))))
       (c1funcall (list* (cmp-expand-macro fd fun) arguments)))
      ;; (FUNCALL lisp-expression ...)
      ((not (eq (first fun) 'FUNCTION))
       (unoptimized-funcall fun arguments))
      ;; (FUNCALL #'GENERALIZED-FUNCTION-NAME ...)
      ((si::valid-function-name-p (setq fun (second fun)))
       (c1call fun arguments nil))
      ;; (FUNCALL #'(LAMBDA ...) ...) or (FUNCALL #'(EXT::LAMBDA-BLOCK ...) ...)
      ((and (consp fun)
            (member (first fun) '(LAMBDA EXT::LAMBDA-BLOCK)))
       (optimize-funcall/apply-lambda (macroexpand-lambda-block fun)
                                      arguments nil))
      (t
       (cmperr "Malformed function name: ~A" fun)))))

(defun c1multiple-value-call (args &aux forms)
  (check-args-number 'MULTIPLE-VALUE-CALL args 1)
  (cond
    ;; (M-V-C #'FUNCTION) => (FUNCALL #'FUNCTION)
    ((endp (rest args))
     (c1funcall args))
    ;; (M-V-C #'FUNCTION (VALUES A ... Z)) => (FUNCALL #'FUNCTION A ... Z)
    ((and (= (length args) 2)
          (consp (setq forms (second args)))
          (eq 'VALUES (first forms)))
     (c1funcall (list* (first args) (rest forms))))
    ;; More complicated case.
    (t
     (let ((function (gensym))
           (frame (gensym)))
       `(with-stack ,frame
          (let* ((,function ,(first args)))
            ,@(loop for i in (rest args)
                    collect `(stack-push-values ,frame ,i))
            (si::apply-from-stack-frame ,frame ,function)))))))

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
                  (member (first fun) '(LAMBDA EXT::LAMBDA-BLOCK)))
             (optimize-funcall/apply-lambda (macroexpand-lambda-block fun)
                                            arguments t))
            ((and (consp fun)
                  (eq (first fun) 'FUNCTION)
                  (consp (second fun))
                  (member (caadr fun) '(LAMBDA EXT::LAMBDA-BLOCK)))
             (c1apply (list* (second fun) arguments)))
            (t
             (default-apply fun arguments))))))

(defun c1call (fname args macros-allowed &aux fd success can-inline)
  (cond ((> (length args) si::c-arguments-limit)
         (if (and macros-allowed
                  (setf fd (cmp-macro-function fname)))
             (cmp-expand-macro fd (list* fname args))
             ;; When it is a function and takes many arguments, we will
             ;; need a special C form to call it. It takes extra code for
             ;; handling the stack
             (unoptimized-long-call `#',fname args)))
        ((setq fd (local-function-ref fname))
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
              (consp can-inline)
              (eq (first can-inline) 'function)
              (plusp *inline-max-depth*)
              (<= (cmp-env-optimization 'space) 1))
         (let ((*inline-max-depth* (1- *inline-max-depth*)))
           (cmpnote "Inlining ~a" fname)
           `(funcall ,can-inline ,@args)))
        (t (c1call-global fname args))))

(defun inline-local (lambda fun args)
  (declare (si::c-local))
  (let ((*inline-max-depth* (1- *inline-max-depth*))
        (setjmps *setjmps*)
        (*cmp-env* (cmp-env-copy)))
    ;; To inline the function, we transform it into a let* statement.
    (multiple-value-bind (bindings body)
        (transform-funcall/apply-into-let* (macroexpand-lambda-block lambda)
                                           args nil)
      (multiple-value-bind (let-vars let-inits specials other-decls body)
          (process-let-bindings 'LET* bindings body)
        ;; We have to compile the function body in the same
        ;; environment in which the function was defined to get
        ;; inlining of closures right.
        (let ((*cmp-env* (cmp-env-copy (fun-cmp-env fun))))
          (mapc #'push-vars let-vars)
          (process-let-body 'LET* let-vars let-inits specials other-decls body setjmps))))))

(defun c1call-local (fname fun args)
  (declare (si::c-local))
  (let ((lambda (fun-lambda-expression fun)))
    (when (and lambda
               (declared-inline-p fname)
               (plusp *inline-max-depth*))
      (return-from c1call-local (inline-local lambda fun args))))
  (let* ((forms (c1args* args))
         (return-type (or (get-local-return-type fun) 'T))
         (arg-types (get-local-arg-types fun)))
    ;; Add type information to the arguments.
    (when arg-types
      (let ((fl nil))
        (dolist (form forms)
          (cond ((endp arg-types) (push form fl))
                (t (push (and-form-type (car arg-types) form (car args)
                                        :safe "In a call to ~a" fname)
                         fl)
                   (pop arg-types)
                   (pop args))))
        (setq forms (nreverse fl))))
    (make-c1form* 'CALL-LOCAL
                  :sp-change t ; conservative estimate
                  :side-effects t ; conservative estimate
                  :type return-type
                  :args fun forms)))

(defun c1call-global (fname args)
  (let* ((forms (c1args* args)))
    ;; If all arguments are constants, try to precompute the function
    ;; value. We abort when the function signals an error or the value
    ;; is not printable.
    (let ((value (c1call-constant-fold fname forms)))
      (when value
        (return-from c1call-global value)))
    ;; Otherwise emit a global function call
    (make-c1form* 'CALL-GLOBAL
                  :sp-change (function-may-change-sp fname)
                  :side-effects (function-may-have-side-effects fname)
                  :type (propagate-types fname forms)
                  :args fname forms
                  ;; loc and type are filled by c2expr
                  )))

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
                         (c1constant-value (first results) :only-small-values nil)
                         (let ((results (mapcar (lambda (r)
                                                  (c1constant-value r :only-small-values nil))
                                                results)))
                           (when (every #'identity results)
                             (make-c1form* 'values :args results)))))))
      (error (c) (cmpdebug "Can't constant-fold ~s ~s: ~a~%" fname forms c)))))

;;; Transform a (funcall lambda-form arguments) or (apply lambda-form
;;; arguments) expression into an equivalent let* statement. Returns
;;; the bindings and body as two values.
(defun transform-funcall/apply-into-let* (lambda-form arguments apply-p
                                          &aux body apply-list apply-var
                                          let-vars extra-stmts all-keys)
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
                                       (listp (constant-form-value apply-list))))
           (n-args-got-min (if apply-constant-args-p
                               (+ (length arguments)
                                  (length (constant-form-value apply-list)))
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
