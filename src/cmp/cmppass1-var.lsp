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

(defun c1let (args)
  (check-args-number 'LET args 1)
  (let ((bindings (pop args)))
    (cond ((null bindings)
           (c1locally args))
          ((atom bindings)
           (invalid-let-bindings 'LET bindings))
          ((null (rest bindings))
           (c1let/let* 'let* bindings args))
          (t
           (loop :with temp
              :for b :in bindings
                :if (atom b)
                  :collect b :into real-bindings :and
                  :collect b :into names
                :else
                  :collect (setf temp (gensym "LET")) :into temp-names    :and
                  :collect (cons temp (cdr b))        :into temp-bindings :and
                  :collect (list (car b) temp)        :into real-bindings :and
                  :collect (car b)                    :into names
                :do
                  (cmpck (member (car names) (cdr names) :test #'eq)
                         "LET: The variable ~s occurs more than once in the LET."
                         (car names))
              :finally
                (return (c1let/let* 'let*
                                    (nconc temp-bindings real-bindings)
                                    `((declare (ignorable ,@temp-names)
                                               (:read-only ,@temp-names))
                                      ,@args)))))
          (t
           (c1let/let* 'let bindings args)))))

(defun c1let* (args)
  (check-args-number 'LET* args 1)
  (let ((bindings (pop args)))
    (cond ((null bindings)
           (c1locally args))
          ((atom bindings)
           (invalid-let-bindings 'LET* bindings))
          (t
           (c1let/let* 'let* bindings args)))))

;; Processing of a let form is split in two stages:
;; - processing bindings
;; - processing the body
;; This allows reusing the below functions for inlined closures. These
;; are transformed in a let statement for which the body needs to be
;; compiled in a different lexical environment than the bindings.
(defun c1let/let* (let/let* bindings body)
  (let* ((setjmps *setjmps*)
         (*cmp-env* (cmp-env-copy)))
    (multiple-value-bind (vars forms specials other-decls body)
        (process-let-bindings let/let* bindings body)
      (process-let-body let/let* vars forms specials other-decls body setjmps))))

(defun invalid-let-bindings (let/let* bindings)
  (cmperr "Syntax error in ~A bindings:~%~4I~A"
          let/let* bindings))

(defun process-let-bindings (let/let* bindings body)
  (multiple-value-bind (body specials types ignoreds other-decls)
      (c1body body nil)
    (let ((vars '())
          (forms '()))
      (do ((b bindings)
           name form)
          ((atom b)
           (unless (null b)
             (invalid-let-bindings let/let* bindings)))
        (if (symbolp (setf form (pop b)))
            (setf name form form nil)
            (progn
              (check-args-number "LET/LET* binding" form 1 2)
              (setf name (first form) form (rest form))))
        (let* ((var (c1make-var name specials ignoreds types))
               (type (var-type var))
               (init (cond ((null form)
                            (default-init var))
                           ((trivial-type-p type)
                            (c1expr (first form)))
                           (t
                            (c1expr `(checked-value ,type ,(first form)))))))
          ;; :read-only variable handling. Beppe
          (when (read-only-variable-p name other-decls)
            (if (global-var-p var)
                (cmpwarn "Found :READ-ONLY declaration for global var ~A"
                         name)
                (setf (var-type var) (c1form-primary-type init)))
            (multiple-value-bind (constantp value)
                (c1form-constant-p init)
              (when constantp
                (cmp-env-register-symbol-macro name `',value)
                (setf var nil))))
          (when var
            (push var vars)
            (push init forms)
            (when (eq let/let* 'LET*) (push-vars var)))))
      (setf vars (nreverse vars)
            forms (nreverse forms))
      (when (eq let/let* 'LET)
        (mapc #'push-vars vars))
      (check-vdecl (mapcar #'var-name vars) types ignoreds)
      (values vars forms specials other-decls body))))

(defun process-let-body (let/let* vars forms specials other-decls body setjmps)
  (mapc #'cmp-env-declare-special specials)
  (setf body (c1decl-body other-decls body))
  ;; Try eliminating unused variables, replace constant ones, etc.
  (multiple-value-setq (vars forms)
    (c1let-optimize-read-only-vars vars forms body))
  ;; Verify that variables are referenced and assign final boxed / unboxed type
  (mapc #'check-vref vars)
  (let ((sp-change (some #'global-var-p vars)))
    (make-c1form* let/let*
                  :type (c1form-type body)
                  :volatile (not (eql setjmps *setjmps*))
                  :local-vars vars
                  :args vars forms body)))

(defun c1let-optimize-read-only-vars (all-vars all-forms body)
  (loop with base = (list body)
     for vars on all-vars
     for forms on (nconc all-forms (list body))
     for var = (first vars)
     for form = (first forms)
     for rest-vars = (cdr vars)
     for rest-forms = (cdr forms)
     for read-only-p = (and (null (var-set-nodes var))
                            (null (var-functions-reading var))
                            (null (var-functions-setting var))
                            (not (global-var-p var)))
     when read-only-p
     do (fix-read-only-variable-type var form rest-forms)
     unless (and read-only-p
                (or (c1let-unused-variable-p var form)
                    (c1let-constant-value-p var form rest-vars rest-forms)
                    (c1let-constant-variable-p var form rest-vars rest-forms)
                    #+(or)
                    (c1let-can-move-variable-value-p var form rest-vars rest-forms)))
     collect var into used-vars and
     collect form into used-forms
     finally (return (values used-vars used-forms))))

(defun fix-read-only-variable-type (var form rest-forms)
  (and-form-type (var-type var) form (var-name var) :unsafe "In LET body")
  (let ((form-type (c1form-primary-type form)))
    (setf (var-type var) form-type)
    (update-variable-type var form-type)))

(defun c1let-unused-variable-p (var form)
  ;; * (let ((v2 e2)) e3 e4) => (let () e3 e4)
  ;;   provided
  ;;   - v2 does not appear in body
  ;;   - e2 produces no side effects
  (when (and (= 0 (var-ref var))
             (not (member (var-kind var) '(special global)))
             (not (form-causes-side-effect form)))
    (unless (var-ignorable var)
      (cmpdebug "Removing unused variable ~A" (var-name var)))
    (delete-c1forms form)
    t))

(defun c1let-constant-value-p (var form rest-vars rest-forms)
  ;;  (let ((v1 e1) (v2 e2) (v3 e3)) (expr e4 v2 e5))
  ;;  - v2 is a read only variable
  ;;  - the value of e2 is not modified in e3 nor in following expressions
  (when (and (eq (c1form-name form) 'LOCATION)
             (loc-in-c1form-movable-p (c1form-arg 0 form)))
    (cmpdebug "Replacing variable ~A by its value ~A" (var-name var) form)
    (nsubst-var var form)
    t))

(defun c1let-constant-variable-p (var form rest-vars rest-forms)
  ;;  (let ((v1 e1) (v2 e2) (v3 e3)) (expr e4 v2 e5))
  ;;  - v2 is a read only variable
  ;;  - the value of e2 is not modified in e3 nor in following expressions
  (when (eq (c1form-name form) 'VAR)
    (let ((other-var (c1form-arg 0 form)))
      (unless (or (global-var-p other-var)
                  (member other-var rest-vars)
                  (var-changed-in-form-list other-var rest-forms))
        (cmpdebug "Replacing variable ~A by its value ~A" (var-name var) form)
        (nsubst-var var form)
        t))))

(defun c1let-can-move-variable-value-p (var form rest-vars rest-forms)
  ;;  (let ((v1 e1) (v2 e2) (v3 e3)) (expr e4 v2 e5))
  ;;  can become
  ;;  (let ((v1 e1) (v3 e3)) (expr e4 e2 e5))
  ;;  provided
  ;;  - v2 appears only once
  ;;  - v2 appears only in body
  ;;  - e2 does not affect v1 nor e3, e3 does not affect e2
  ;;  - e4 does not affect e2
  (when (and (= 1 (var-ref var))
             (not (form-causes-side-effect form))
             ;; it does not refer to special variables which
             ;; are changed in the LET form
             (notany #'(lambda (v) (var-referenced-in-form v form)) rest-vars)
             (replaceable var rest-forms))
    (cmpdebug "Replacing variable ~A by its value ~A" (var-name var) form)
    (nsubst-var var form)
    t))

(defun c1make-var (name specials ignores types)
  (cmpck (not (symbolp name)) "The variable ~s is not a symbol." name)
  (cmpck (constantp name) "The constant ~s is being bound." name)
  (let ((ignorable (cdr (assoc name ignores)))
        (kind 'LEXICAL)                 ; we rely on check-vref to fix it
        (type (assoc name types)))
    (cond ((null type)
           (setq type 'T))
          ((machine-c-type-p (setq type (cdr type)))
           (setf kind type
                 type (rep-type->lisp-type type))))
    (cond ((or (member name specials) (special-variable-p name))
           (unless (eq kind 'LEXICAL)
             (cmperr "Special variable ~A cannot be declared to have C type ~A"
                     name type))
           (when (eq type 'T)
             (setf type (or (si:get-sysprop name 'CMP-TYPE) 'T)))
           (c1make-global-variable name :kind 'SPECIAL :type type))
          (t
           (make-var :name name :type type :loc 'OBJECT
                     :kind kind :ignorable ignorable
                     :ref 0)))))

(defun c1var (name)
  (let* ((var (c1vref name))
         (output (make-c1form* 'VAR
                               :type (var-type var)
                               :args var)))
      (add-to-read-nodes var output)
      output))

;;; A variable reference (vref for short) is a list: pair
;;;     ( var-object ) Beppe(ccb) ccb-reference )

(defun c1vref (name)
  (multiple-value-bind (var cfb unw)
      (cmp-env-search-var name)
    (declare (ignore unw))
    (cond ((null var)
           (c1make-global-variable name :warn t
                                        :type (or (si:get-sysprop name 'CMP-TYPE) t)))
          ((not (var-p var))
           ;; symbol-macrolet
           (baboon :format-control "c1vref: ~s is not a variable."
                   :format-arguments (list name)))
          (t
           (case (var-kind var)
             ((SPECIAL GLOBAL))
             ((CLOSURE))
             ((LEXICAL)
              (when cfb
                (setf (var-ref-clb var) t
                      (var-loc var) 'OBJECT)))
             (t
              (when cfb
                (cmperr "Variable ~A declared of C type cannot be referenced across function boundaries."
                        (var-name var)))))
           var))))

(defun c1make-global-variable (name &key
                               (type (or (si:get-sysprop name 'CMP-TYPE) t))
                               (kind 'GLOBAL)
                               (warn nil))
  (let* ((var (make-var :name name :kind kind :type type :loc (add-symbol name))))
    (when warn
      (unless (or (constantp name)
                  (special-variable-p name)
                  (member name *undefined-vars*))
        (undefined-variable name)
        (push name *undefined-vars*)))
    var))

(defun c1setq (args)
  (let ((l (length args)))
    (cmpck (oddp l) "SETQ requires an even number of arguments.")
    (cond ((zerop l) (c1nil))
          ((= l 2) (c1setq1 (first args) (second args)))
          (t
           (c1progn
            (loop while args
               collect `(setq ,(pop args) ,(pop args))))))))

(defun c1setq1 (name form)
  (cmpck (not (symbolp name)) "The variable ~s is not a symbol." name)
  (cmpck (constantp name) "The constant ~s is being assigned a value." name)
  (setq name (chk-symbol-macrolet name))
  (if (symbolp name)
      (let* ((name (c1vref name))
             (type (var-type name))
             (form (c1expr (if (trivial-type-p type)
                               form
                               `(checked-value ,type ,form)))))
        (add-to-set-nodes name (make-c1form* 'SETQ
                                             :type (c1form-type form)
                                             :args name form)))
      `(setf ,name ,form)))

(defun c1progv (args)
  (check-args-number 'PROGV args 2)
  (let ((symbols (c1expr (first args)))
        (values (c1expr (second args)))
        (forms (c1progn (cddr args))))
    (make-c1form* 'PROGV :type (c1form-type forms)
                         :args symbols values forms)))

(defun c1psetq (old-args &aux (args nil) (use-psetf nil))
  ;; A first pass ensures that none of the assigned locations is
  ;; a SETF form. Otherwise we have to resort to PSETF.
  (do ((l old-args))
      ((endp l))
    (let ((var (pop l)))
      (cmpck (not (symbolp var))
             "The variable ~s is not a symbol." var)
      (cmpck (endp l)
           "No form was given for the value of ~s." var)
      (setq var (chk-symbol-macrolet var))
      (setq args (nconc args (list var (pop l))))
      (if (symbolp var)
          (cmpck (constantp var)
                 "The constant ~s is being assigned a value." var)
          (setq use-psetf t))))
  (when use-psetf
    (return-from c1psetq `(psetf ,@args)))
  ;; In the second pass we compile the variable references and the
  ;; assignments. Here we may need to create checked values if the
  ;; variables have been proclaimed.
  (do ((vrefs '())
       (forms '()))
      ((endp args)
       (add-to-set-nodes-of-var-list
        vrefs (make-c1form* 'PSETQ :type '(MEMBER NIL)
                            :args (reverse vrefs) (nreverse forms))))
    (let* ((vref (c1vref (pop args)))
           (type (var-type vref))
           (form (pop args)))
      (push vref vrefs)
      (push (c1expr (if (trivial-type-p type)
                        form
                        `(checked-value ,type ,form)))
            forms))))

(defun c1multiple-value-bind (args)
  (check-args-number 'MULTIPLE-VALUE-BIND args 2)
  (let* ((*cmp-env* (cmp-env-copy))
         (variables (pop args))
         (init-form (pop args)))
    (when (= (length variables) 1)
      (return-from c1multiple-value-bind
        `(let* ((,(first variables) ,init-form))
           ,@args)))
    (multiple-value-bind (body ss ts is other-decls)
        (c1body args nil)
      (mapc #'cmp-env-declare-special ss)
      (let* ((vars (loop for name in variables
                         collect (c1make-var name ss is ts))))
        (setq init-form (c1expr init-form))
        (mapc #'push-vars vars)
        (check-vdecl variables ts is)
        (setq body (c1decl-body other-decls body))
        (mapc #'check-vref vars)
        (make-c1form* 'MULTIPLE-VALUE-BIND :type (c1form-type body)
                                           :local-vars vars
                                           :args vars init-form body)))))

(defun c1multiple-value-setq (args &aux (vars nil) (value-bindings nil)
                                     (storing-forms nil) (return-value nil))
  (check-args-number 'MULTIPLE-VALUE-SETQ args 2 2)
  (dolist (var (reverse (first args)))
    ;; check for symbol-macrolets and insert type checks if necessary
    (cmpck (not (symbolp var)) "The variable ~s is not a symbol." var)
    (let* ((var-or-form (chk-symbol-macrolet var))
           (type t))
      (if (symbolp var-or-form)
          (progn
            (cmpck (constantp var-or-form)
                   "The constant ~s is being assigned a value." var-or-form)
            (setf return-value var-or-form)
            (if (or (not (policy-type-assertions))
                    (trivial-type-p
                     (setf type (variable-type-in-env var-or-form))))
                (push var-or-form vars)
                (let ((new-var (gensym)))
                  (push new-var vars)
                  (push new-var value-bindings)
                  (push `(setf ,var-or-form (checked-value ,type ,new-var)) storing-forms))))
          (multiple-value-bind (setf-vars setf-vals stores storing-form get-form)
              (get-setf-expansion var-or-form *cmp-env*)
            (push (first stores) vars)
            (setf value-bindings (nconc (mapcar #'list setf-vars setf-vals)
                                        stores
                                        value-bindings))
            (push storing-form storing-forms)
            (setf return-value get-form)))))
  (let ((value (second args)))
    (cond (value-bindings
           `(let* (,@value-bindings)
              (multiple-value-setq ,vars ,value)
              ,@storing-forms
              ,return-value))
          ((endp vars)
           `(values ,value))
          ((= (length vars) 1)
           `(setq ,(first vars) ,value))
          (t
           (setq value (c1expr value)
                 vars (mapcar #'c1vref vars))
           (add-to-set-nodes-of-var-list
            vars (make-c1form* 'MULTIPLE-VALUE-SETQ :args vars value))))))


;;; FIXME this doesn't belong to the pass (should be part of cmpenv).

(defun read-only-variable-p (v other-decls)
  (dolist (i other-decls nil)
    (when (and (eq (car i) :READ-ONLY)
               (member v (rest i)))
      (return t))))

(defun env-grows (possibily)
  ;; if additional closure variables are introduced and this is not
  ;; last form, we must use a new env.
  (and possibily
       (plusp *env*)
       (dolist (exit *unwind-exit*)
         (case exit
           (RETURN (return NIL))
           (BDS-BIND)
           (t (return T))))))

;; should check whether a form before var causes a side-effect
;; exactly one occurrence of var is present in forms
(defun replaceable (var form)
  (labels ((abort-on-side-effects (form)
             (if (eq (c1form-name form) 'VAR)
                 (when (eq var (first (c1form-args form)))
                   (return-from replaceable t))
                 (when (c1form-side-effects form)
                   (return-from replaceable nil)))))
    (traverse-c1form-tree form #'abort-on-side-effects)
    (baboon :format-control "In REPLACEABLE, variable ~A not found. Form:~%~A"
            :format-arguments (list (var-name var) *current-form*))))

#+not-used
(defun discarded (var form body &aux last)
  (labels ((last-form (x &aux (args (c1form-args x)))
             (case (c1form-name x)
               (PROGN
                 (last-form (car (last (first args)))))
               ((LET LET* FLET LABELS BLOCK CATCH)
                (last-form (car (last args))))
               (VAR (c1form-arg 0 x))
               (t x))))
    (and (not (form-causes-side-effect form))
         (or (< (var-ref var) 1)
             (and (= (var-ref var) 1)
                  (eq var (last-form body))
                  (eq 'TRASH *destination*))))))

(defun nsubst-var (var form)
  (when (var-set-nodes var)
    (baboon :format-control "Cannot replace a variable that is to be changed"))
  (when (var-functions-reading var)
    (baboon :format-control "Cannot replace a variable that forms part of a closure"))
  (dolist (where (var-read-forms var))
    (unless (and (eql (c1form-name where) 'VAR)
                 (eql (c1form-arg 0 where) var))
      (baboon :format-control "VAR-READ-NODES are only C1FORMS of type VAR"))
    (delete-from-read-nodes var where)
    (c1form-replace-with where form))
  (setf (var-ignorable var) 0))

#+not-used
(defun member-var (var list)
  (let ((kind (var-kind var)))
    (if (member kind '(SPECIAL GLOBAL))
        (member var list :test
                #'(lambda (v1 v2)
                    (and (member (var-kind v2) '(SPECIAL GLOBAL))
                         (eql (var-name v1) (var-name v2)))))
        (member var list))))

;;;

(defun make-var (&rest args)
  (let ((var (apply #'%make-var args)))
    (unless (member (var-kind var) '(SPECIAL GLOBAL))
      (when *current-function*
        (push var (fun-local-vars *current-function*))))
    var))

(defun make-lcl-var (&key rep-type (type 'T))
  (unless rep-type
    (setq rep-type (if type (lisp-type->rep-type type) :object)))
  (unless type
    (setq type 'T))
  (make-var :kind rep-type :type type :loc (next-lcl)))

(defun make-temp-var (&optional (type 'T))
  (make-var :kind :object :type type :loc `(TEMP ,(next-temp))))

(defun var-referenced-in-form-list (var form-list)
  (loop for f in form-list
          thereis (var-referenced-in-form var f)))

(defun var-changed-in-form-list (var form-list)
  (loop for f in form-list
     thereis (var-changed-in-form var f)))

;;; FIXME! VAR-REFERENCED-IN-FORM and VAR-CHANGED-IN-FORM are too
;;; pessimistic. One should check whether the functions reading/setting the
;;; variable are actually called from the given node.  The problem arises when
;;; we create a closure of a function, as in
;;;
;;;     (let* ((a 1) (b #'(lambda () (incf a)))) ...)
;;;
;;; To know whether A is changed or read, we would have to track where B is
;;; actually used.

(defun var-referenced-in-form (var form)
  (declare (type var var))
  (or (find-form-in-node-list form (var-read-nodes var))
      (var-functions-reading var)))

(defun var-changed-in-form (var form)
  (declare (type var var))
  (or (find-form-in-node-list form (var-set-nodes var))
      (let ((kind (var-kind var)))
        (if (or (eq kind 'SPECIAL) (eq kind 'GLOBAL))
            (c1form-sp-change form)
            (var-functions-setting var)))))

(defun update-variable-type (var orig-type)
  ;; FIXME! Refuse to update type of variables that are modified
  (when (var-set-nodes var)
    (return-from update-variable-type))
  (let ((type (type-and (var-type var) orig-type)))
    (if (null type)
        (cmpwarn "Variable assigned a value incompatible with its type declaration.~%Variable: ~A~%Expected type: ~A~%Value type: ~A"
                 (var-name var)
                 (var-type var)
                 orig-type)
        (loop for form in (var-read-forms var)
           when (and (eq (c1form-name form) 'VAR)
                     (eq var (c1form-arg 0 form)))
           do (setf (c1form-type form) (type-and type (c1form-primary-type form)))
           finally (setf (var-type var) type)))))

(defun var-read-forms (var)
  (mapcar #'first (var-read-nodes var)))

(defun assert-var-ref-value (var)
  (when *debug-compiler*
    (unless (let ((ref (var-ref var)))
              (or (> ref (/ most-positive-fixnum 2))
                  (= (var-ref var) (+ (length (var-read-nodes var))
                                      (length (var-set-nodes var))))))
      (baboon :format-control "Number of references in VAR ~A unequal to references list"
              :format-arguments (list var)))))

(defun assert-var-not-ignored (var)
  (when (let ((x (var-ignorable var))) (and x (minusp x)))
    (cmpwarn-style "Variable ~A, declared as IGNORE, found in a lisp form."
                   (var-name var))
    (setf (var-ignorable var) nil)))

(defun delete-from-read-nodes (var form)
  (assert-var-ref-value var)
  (setf (var-ref var) (1- (var-ref var))
        (var-read-nodes var) (delete-form-from-node-list form (var-read-nodes var))))

(defun add-to-read-nodes (var form)
  (assert-var-ref-value var)
  (assert-var-not-ignored var)
  (setf (var-ref var) (1+ (var-ref var))
        (var-read-nodes var) (add-form-to-node-list form (var-read-nodes var)))
  (when *current-function*
    (unless (eq *current-function* (var-function var))
      (pushnew *current-function* (var-functions-reading var))
      (pushnew var (fun-referenced-vars *current-function*))))
  form)

(defun add-to-set-nodes (var form)
  (assert-var-ref-value var)
  (assert-var-not-ignored var)
  (setf (var-ref var) (1+ (var-ref var))
        (var-set-nodes var) (add-form-to-node-list form (var-set-nodes var)))
  ;;(push form (var-read-nodes var))
  (when *current-function*
    (unless (eq *current-function* (var-function var))
      (pushnew *current-function* (var-functions-setting var))
      (pushnew var (fun-referenced-vars *current-function*))))
  form)

(defun add-to-set-nodes-of-var-list (var-list form)
  (dolist (v var-list)
    (add-to-set-nodes v form))
  form)

;;; A special binding creates a var object with the kind field SPECIAL,
;;; whereas a special declaration without binding creates a var object with
;;; the kind field GLOBAL.  Thus a reference to GLOBAL may need to make sure
;;; that the variable has a value.

;;;  Bootstrap problem: proclaim needs this function:
;;;
;;; Check if a variable has been declared as a special variable with a global
;;; value.

(defun check-global (name)
  (member name *global-vars*))

(defun special-variable-p (name)
  "Return true if NAME is associated to a special variable in the lexical environment."
  (or (si::specialp name)
      (check-global name)
      (let ((v (cmp-env-search-var name *cmp-env-root*)))
        ;; Fixme! Revise the declamation code to ensure whether
        ;; we also have to consider 'GLOBAL here.
        (and v (eq (var-kind v) 'SPECIAL)))))

(defun local-variable-p (name &optional (env *cmp-env*))
  (let ((record (cmp-env-search-var name env)))
    (and record (var-p record))))

(defun symbol-macro-p (name &optional (env *cmp-env*))
  (let ((record (cmp-env-search-var name env)))
    (and record (not (var-p record)))))

(defun variable-type-in-env (name &optional (env *cmp-env*))
  (let ((var (cmp-env-search-var name)))
    (cond ((var-p var)
           (var-type var))
          ((si:get-sysprop name 'CMP-TYPE))
          (t))))

(defun var-rep-type (var)
  (case (var-kind var)
    ((LEXICAL CLOSURE SPECIAL GLOBAL) :object)
    (t (var-kind var))))

(defun check-vref (var)
  (when (eq (var-kind var) 'LEXICAL)
    (when (and (zerop (var-ref var)) ;;; This field may be -1 (IGNORE). Beppe
               (not (var-ignorable var)))
      (cmpwarn-style "The variable ~s is not used." (var-name var)))
    (when (not (var-ref-clb var))
      ;; if the variable can be stored locally, set it var-kind to its type
      (setf (var-kind var)
            (if (plusp (var-ref var))
                (lisp-type->rep-type (var-type var))
                :OBJECT)))))

(defun push-vars (v)
  (setf (var-index v) (length (cmp-env-variables)))
  (cmp-env-register-var v))

(defun unboxed (var)
  (not (eq (var-rep-type var) :object)))

(defun local (var)
  (and (not (member (var-kind var) '(LEXICAL CLOSURE SPECIAL GLOBAL)))
       (var-kind var)))

(defun global-var-p (var)
  (let ((kind (var-kind var)))
    (or (eq kind 'global)
        (eq kind 'special))))

(defun useful-var-p (var)
  (or (plusp (var-ref var))
      (global-var-p var)))

(defun si::register-global (name)
  (pushnew name *global-vars*)
  (values))
