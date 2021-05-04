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

(defun c1labels (args) (c1labels/flet 'LABELS args))

(defun c1flet (args) (c1labels/flet 'FLET args))

(defun c1labels/flet (origin args)
  (check-args-number origin args 1)
  (let ((new-env (cmp-env-copy))
        (defs '())
        (local-funs '())
        (fnames '())
        body-c1form)
    ;; On a first round, we extract the definitions of the functions,
    ;; and build empty function objects that record the references to
    ;; this functions in the processed body. In the end
    ;;  DEFS = ( { ( fun-object  function-body ) }* ).
    (dolist (def (car args))
      (cmpck (or (endp def)
                 (not (si::valid-function-name-p (car def)))
                 (endp (cdr def)))
             "The local function definition ~s is illegal." def)
      (cmpck (member (car def) fnames)
             "~s: The function ~s was already defined." origin (car def))
      (push (car def) fnames)
      (let* ((name (car def))
             (var (make-var :name name :kind :object))
             (fun (make-fun :name name :var var)))
        (cmp-env-register-function fun new-env)
        (push (cons fun (cdr def)) defs)))

    ;; Now we compile the functions, either in the current environment
    ;; or in an empty environment in which there are no new functions
    (let ((*cmp-env* (cmp-env-copy (if (eq origin 'FLET) *cmp-env* new-env))))
      (dolist (def (nreverse defs))
        (let ((fun (first def)))
          ;; The closure type will be fixed later on by COMPUTE-...
          (push (c1compile-function (rest def) :fun fun)
                local-funs))))

    ;; When we are in a LABELs form, we have to propagate the external
    ;; variables from one function to the other functions that use it.
    (when (eq origin 'LABELS)
      (loop for change = nil
            do (loop for f1 in local-funs
                     for vars = (fun-referenced-vars f1)
                     for funs = (fun-referenced-funs f1)
                     do (loop for f2 in (fun-referencing-funs f1)
                              for c1 = (add-to-fun-referenced-vars f2 vars)
                              for c2 = (add-to-fun-referenced-funs f2 funs)
                              for c3 = (update-fun-closure-type f2)
                              when (or c1 c2 c3)
                                do (setf change t)))
            do (unless change (return))))

    ;; Now we can compile the body itself.
    (let ((*cmp-env* new-env))
      (multiple-value-bind (body ss ts is other-decl)
          (c1body (rest args) t)
        (mapc #'cmp-env-declare-special ss)
        (check-vdecl nil ts is)
        (setq body-c1form (c1decl-body other-decl body))))

    ;; Keep only functions that have been referenced at least once.
    ;; It is not possible to look at FUN-REF before because functions
    ;; in a LABELS can reference each other.
    (setf local-funs (remove-if-not #'plusp local-funs :key #'fun-ref))

    ;; Keep on inspecting the functions until the closure type does not
    ;; change.
    (loop while
          (let ((x nil))
            (loop for f in local-funs
                  when (update-fun-closure-type f)
                    do (setf x t))
            x))

    (if local-funs
        (make-c1form* 'LOCALS :type (c1form-type body-c1form)
                              :args local-funs body-c1form (eq origin 'LABELS))
        body-c1form)))

;;; During Pass1, a lambda-list
;;;
;;; (   { var }*
;;;     [ &optional { var | ( var [ initform [ svar ] ] ) }* ]
;;;     [ &rest var ]
;;;     [ &key { var | ( { var | ( kwd var ) } [initform [ svar ]])}*
;;;             [&allow-other-keys]]
;;;     [ &aux {var | (var [initform])}*]
;;; )
;;;
;;; is transformed into
;;;
;;; (   ( { var }* )                            ; required
;;;     ( { var initform svar }* )              ; optional
;;;     { var | nil }                           ; rest
;;;     allow-other-keys-flag
;;;     ( { kwd-vv-index var initform svar }* ) ; key
;;; )
;;;
;;; where
;;;     svar:   NIL     ; means svar is not supplied
;;;             | var
;;;
;;; &aux parameters will be embedded into LET*.
;;;
;;; c1lambda-expr receives
;;;     ( lambda-list { doc | decl }* . body )
;;; and returns
;;;     ( lambda info-object lambda-list' doc body' )
;;;
;;; Doc is NIL if no doc string is supplied.
;;; Body' is body possibly surrounded by a LET* (if &aux parameters are
;;; supplied) and an implicit block.

(defun c1compile-function (lambda-list-and-body
                           &key (fun (make-fun)) (name (fun-name fun)))
  (let ((lambda (if name
                    `(ext:lambda-block ,name ,@lambda-list-and-body)
                    `(lambda ,@lambda-list-and-body))))
    (setf (fun-name fun) name
          (fun-lambda-expression fun) lambda 
          (fun-parent fun) *current-function*))
  (when *current-function*
    (push fun (fun-child-funs *current-function*)))
  (let* ((*current-function* fun)
         (*cmp-env* (setf (fun-cmp-env fun) (cmp-env-mark 'SI:FUNCTION-BOUNDARY)))
         (setjmps *setjmps*)
         (decl (si::process-declarations (rest lambda-list-and-body)))
         (global (and *use-c-global*
                      (assoc 'SI::C-GLOBAL decl)
                      (setf (fun-global fun) T)))
         (no-entry (assoc 'SI::C-LOCAL decl))
         cfun exported minarg maxarg proclamation-found-p)
    (multiple-value-bind (lambda-expr optional-type-checks keyword-type-checks)
        (c1lambda-expr lambda-list-and-body name
                       (si::function-block-name name))
      (when (and no-entry (policy-debug-ihs-frame))
        (setf no-entry nil)
        (cmpnote "Ignoring SI::C-LOCAL declaration for~%~4I~A~%because the debug level is large" name))
      (unless (eql setjmps *setjmps*)
        (setf (c1form-volatile lambda-expr) t))
      (setf (fun-lambda fun) lambda-expr)
      (if global
          (multiple-value-setq (cfun exported) (exported-fname name))
          (setf cfun (next-cfun "LC~D~A" name) exported nil))
      (if exported
          ;; Check whether the function was proclaimed to have a certain
          ;; number of arguments, and otherwise produce a function with
          ;; a flexible signature.
          (progn
            (multiple-value-setq (minarg maxarg proclamation-found-p)
              (get-proclaimed-narg name))
            (cmpdebug "~&;;; Function ~A proclaimed (~A,~A)" name minarg maxarg)
            (multiple-value-bind (found-minarg found-maxarg)
                (lambda-form-allowed-nargs lambda-expr)
              (if proclamation-found-p
                  ;; sanity check that the proclamation matches the actual
                  ;; number of arguments found
                  (when (or (/= minarg found-minarg) (/= maxarg found-maxarg))
                    (cmperr "Function ~A takes between ~A and ~A arguments, but is proclaimed to take between ~A and ~A arguments"
                            name found-minarg found-maxarg minarg maxarg))
                  ;; no proclamation found, produce function with
                  ;; flexible signature
                  (setf minarg found-minarg))))
          (multiple-value-setq (minarg maxarg)
            (lambda-form-allowed-nargs lambda-expr)))
      #+ecl-min
      (when exported
        (multiple-value-bind (foundp ignored minarg-core maxarg-core)
            (si::mangle-name name)
          (declare (ignore ignored))
          (when foundp
            ;; Core functions declared in symbols_list.h are
            ;; initialized during startup in all_symbols.d
            (setf no-entry t)
            ;; Sanity check that the number of arguments is consistent
            ;; with the declaration in symbols_list.h. Note that the
            ;; information about the maximum number of arguments for
            ;; variadic functions is missing from this declaration.
            (when (or (/= minarg minarg-core)
                      (and (= maxarg minarg)
                           (/= maxarg maxarg-core)))
              (cmperr "Function ~A takes between ~A and ~A arguments, but is declared to take between ~A and ~A arguments in symbols_list.h"
                      name minarg maxarg minarg-core maxarg-core)))))
      (setf (fun-cfun fun) cfun
            (fun-exported fun) exported
            (fun-closure fun) nil
            (fun-minarg fun) minarg
            (fun-maxarg fun) maxarg
            (fun-description fun) (or (function-block-name-declaration decl) name)
            (fun-no-entry fun) no-entry
            (fun-optional-type-check-forms fun) optional-type-checks
            (fun-keyword-type-check-forms fun) keyword-type-checks)
      (loop for child in (fun-child-funs fun)
            do (add-to-fun-referenced-vars fun (fun-referenced-vars child))
            do (add-to-fun-referenced-funs fun (fun-referenced-funs child)))
      (loop for f in (fun-referenced-funs fun)
            do (add-to-fun-referenced-vars fun (fun-referenced-vars f)))
      (update-fun-closure-type fun)
      (when global
        (if (fun-closure fun)
            (cmpnote "Function ~A is global but is closed over some variables.~%~{~A ~}"
                     (fun-name fun) (mapcar #'var-name (fun-referenced-vars fun)))
            (push fun *global-funs*))))
    fun))

(defun c1lambda-expr (lambda-expr function-name block-name
                      &aux doc body ss is ts
                           other-decls
                           new-variables
                           (type-checks '())
                           (*permanent-data* t)
                           (old-env *cmp-env*)
                           (*cmp-env* (cmp-env-copy)))
  (declare (si::c-local))

  (cmpck (endp lambda-expr)
         "The lambda expression ~s is illegal." (cons 'LAMBDA lambda-expr))

  (multiple-value-setq (body ss ts is other-decls doc)
    (c1body (cdr lambda-expr) t))

  (when block-name (setq body (list (cons 'BLOCK (cons block-name body)))))

  (multiple-value-bind (requireds optionals rest key-flag keywords
                        allow-other-keys aux-vars)
      (cmp-process-lambda-list (car lambda-expr))

    (do ((specs (setq requireds (cdr requireds)) (cdr specs)))
        ((endp specs))
      (let* ((name (first specs))
             (var (c1make-var name ss is ts)))
        (push var type-checks)
        (setf (first specs) var)
        (push-vars var)))

    (do ((specs (setq optionals (cdr optionals)) (cdddr specs)))
        ((endp specs))
      (let* ((name (first specs))
             (var (c1make-var name ss is ts))
             (init (second specs))
             (flag (third specs)))
        (setq init (if init
                       (and-form-type (var-type var) (c1expr init) init
                                      :safe "In (LAMBDA ~a...)" function-name)
                       (default-init var)))
        (push var type-checks)
        (push-vars var)
        (when flag
          (push-vars (setq flag (c1make-var flag ss is ts))))
        (setf (first specs) var
              (second specs) init
              (third specs) flag)))

    (when rest
      (push-vars (setq rest (c1make-var rest ss is ts))))

    (do ((specs (setq keywords (cdr keywords)) (cddddr specs)))
        ((endp specs))
      (let* ((key (first specs))
             (name (second specs))
             (var (c1make-var name ss is ts))
             (init (third specs))
             (flag (fourth specs)))
        (setq init (if init
                       (and-form-type (var-type var) (c1expr init) init
                                      :safe "In (LAMBDA ~a...)" function-name)
                       (default-init var)))
        (push var type-checks)
        (push-vars var)
        (when flag
          (push-vars (setq flag (c1make-var flag ss is ts))))
        (setf (second specs) var
              (third specs) init
              (fourth specs) flag)))

    ;; Make other declarations take effect right now
    (setf *cmp-env* (reduce #'add-one-declaration other-decls
                            :initial-value *cmp-env*))

    ;; After creating all variables and processing the initalization
    ;; forms, we will process the body. However, all free declarations,
    ;; that is declarations which do not refer to the function
    ;; arguments, have to be applied to the body. At the same time, we
    ;; replace &aux variables with a LET* form that defines them.
    (multiple-value-bind (required-type-check-forms optional-type-check-forms keyword-type-check-forms new-auxs)
          (extract-lambda-type-checks function-name requireds optionals
                                      keywords ts other-decls)
        (let* ((declarations other-decls)
               (let-vars (loop for spec on (nconc new-auxs aux-vars)
                            by #'cddr
                            for name = (first spec)
                            for init = (second spec)
                            collect (list name init)))
               (new-variables (cmp-env-new-variables *cmp-env* old-env))
               (already-declared-names (set-difference (mapcar #'var-name new-variables)
                                                       (mapcar #'car let-vars))))
       ;; Gather declarations for &aux variables, either special...
       (let ((specials (set-difference ss already-declared-names)))
         (when specials
           (push `(special ,@specials) declarations)))
       ;; ...ignorable...
       (let ((ignorables (loop for (var . expected-uses) in is
                            unless (member var already-declared-names)
                            collect var)))
         (when ignorables
           (push `(ignorable ,@ignorables) declarations)))
       ;; ...or type declarations
       (loop for (var . type) in ts
          unless (member var already-declared-names)
          do (push `(type ,type ,var) declarations))
       ;; ...create the enclosing LET* form for the &aux variables
       (when (or let-vars declarations)
         (setq body `((let* ,let-vars
                        (declare ,@declarations)
                        ,@body))))
       ;; ...wrap around the type checks for required variables, removing
       ;; unnecessary nil's
       (setq body (nconc (delete nil required-type-check-forms) body))
       ;; ...now finally compile the body with the type checks
       (let ((*cmp-env* (cmp-env-copy *cmp-env*)))
         (setf body (c1progn body)))
       ;;
       ;; ...and verify whether all variables are used.
       (dolist (var new-variables)
         (check-vref var))
       (values (make-c1form* 'LAMBDA
                             :local-vars new-variables
                             :args (list requireds optionals rest key-flag keywords
                                         allow-other-keys)
                             doc body)
               (mapcar #'(lambda (x) (if x (c1expr x) nil))
                       optional-type-check-forms)
               (mapcar #'(lambda (x) (if x (c1expr x) nil))
                       keyword-type-check-forms))))))

(defun cmp-process-lambda-list (list)
  (handler-case (si::process-lambda-list list 'function)
    (error (c) (cmperr "Illegal lambda list ~S:~%~A" list c))))

(defun child-function-p (presumed-parent fun)
  (declare (optimize speed))
  (loop for real-parent = (fun-parent fun)
        while real-parent
        do (if (eq real-parent presumed-parent)
               (return t)
               (setf fun real-parent))))

(defun compute-closure-type (fun)
  (declare (si::c-local))
  (let ((lexical-closure-p nil))
    ;; it will have a full closure if it refers external non-global variables
    (dolist (var (fun-referenced-vars fun))
      (cond ((global-var-p var))
            ;; ...across CB
            ((ref-ref-ccb var)
             (return-from compute-closure-type 'CLOSURE))
            (t
             (setf lexical-closure-p t))))
    ;; ...or if it directly calls a function
    (dolist (f (fun-referenced-funs fun))
      (unless (child-function-p fun f)
        ;; .. which has a full closure
        (case (fun-closure f)
          (CLOSURE (return-from compute-closure-type 'CLOSURE))
          (LEXICAL (setf lexical-closure-p t)))))
    ;; ...or the function itself is referred across CB
    (when lexical-closure-p
      (if (or (fun-ref-ccb fun)
              (and (fun-var fun)
                   (plusp (var-ref (fun-var fun)))))
          'CLOSURE
          'LEXICAL))))

(defun update-fun-closure-type-many (function-list)
  (do ((finish nil t)
       (recompute nil))
      (finish
       recompute)
    (dolist (f function-list)
      (when (update-fun-closure-type f)
        (setf recompute t finish nil)))))

(defun prepend-new (l1 l2)
  (loop for f in l1
     do (pushnew f l2))
  l2)

(defun update-fun-closure-type (fun)
  (let ((old-type (fun-closure fun)))
    (when (eq old-type 'closure)
      (return-from update-fun-closure-type nil))
    ;; This recursive algorithm is guaranteed to stop when functions
    ;; do not change.
    (let ((new-type (compute-closure-type fun))
          to-be-updated)
      ;; Same type
      (when (eq new-type old-type)
        (return-from update-fun-closure-type nil))
      (when (fun-global fun)
        (cmpnote "Function ~A is global but is closed over some variables.~%~{~A ~}"
                 (fun-name fun) (mapcar #'var-name (fun-referenced-vars fun))))
      (setf to-be-updated (append (fun-child-funs fun) (fun-referencing-funs fun)))
      (setf (fun-closure fun) new-type)
      ;; All external, non-global variables become of type closure
      (when (eq new-type 'CLOSURE)
        (dolist (var (fun-referenced-vars fun))
          (unless (or (global-var-p var)
                      (eq (var-kind var) new-type))
            (setf (var-ref-clb var) nil
                  (var-ref-ccb var) t
                  (var-kind var) 'CLOSURE
                  (var-loc var) 'OBJECT
                  to-be-updated
                  (prepend-new (var-functions-reading var)
                               (prepend-new (var-functions-setting var)
                                            to-be-updated)))))
        (dolist (f (fun-referenced-funs fun))
          (setf (fun-ref-ccb f) t)))
      ;; If the status of some of the children changes, we have
      ;; to recompute the closure type.
      (when (update-fun-closure-type-many to-be-updated)
        (update-fun-closure-type fun))
      t)))

;;; FIXME these functions doesn't belong to the pass1 module.
(defun local-function-ref (fname &optional build-object)
  (multiple-value-bind (fun cfb unw)
      (cmp-env-search-function fname)
    (declare (ignore unw))
    (when fun
      (when (functionp fun) 
        (when build-object
          ;; Macro definition appears in #'.... This should not happen.
          (cmperr "The name of a macro ~A was found in special form FUNCTION." fname))
        (return-from local-function-ref nil))
      (incf (fun-ref fun))
      (if build-object
          (setf (fun-ref-ccb fun) t)
          (let ((caller *current-function*))
            (when (and caller
                       (not (member fun (fun-referenced-funs caller) :test #'eq)))
              (push fun (fun-referenced-funs caller))
              (push caller (fun-referencing-funs fun)))))
      ;; we introduce a variable to hold the funob
      (let ((var (fun-var fun)))
        (when (and cfb build-object)
          (setf (var-ref-clb var) t)
          (when (not (eq (var-kind var) 'CLOSURE))
            (setf (var-kind var) 'LEXICAL)))))
    fun))

(defun fun-needs-narg (fun)
  (not (fun-fixed-narg fun)))

(defun fun-fixed-narg (fun)
  "Returns true if the function has a fixed number of arguments and it is not a closure.
The function thus belongs to the type of functions that ecl_make_cfun accepts."
  (let (narg)
    (and (not (eq (fun-closure fun) 'CLOSURE))
         (= (fun-minarg fun) (setf narg (fun-maxarg fun)))
         (<= narg si::c-arguments-limit)
         narg)))

(defun add-to-fun-referenced-vars (fun var-list)
  (loop with new-vars = (fun-referenced-vars fun)
     with locals = (fun-local-vars fun)
     with change = nil
     for v in var-list
     when (and (not (member v locals :test #'eq))
               (not (member v new-vars :test #'eq)))
     do (setf change t new-vars (cons v new-vars))
     finally (when change
               (setf (fun-referenced-vars fun) new-vars)
               (return t))))

(defun add-to-fun-referenced-funs (fun fun-list)
  (loop with new-funs = (fun-referenced-funs fun)
     with change = nil
     for f in fun-list
     when (and (not (eq fun f))
               (not (member f new-funs :test #'eq))
               (not (child-function-p fun f)))
     do (setf change t
              new-funs (cons f new-funs)
              (fun-referencing-funs f) (cons fun (fun-referencing-funs f)))
     finally (when change
               (setf (fun-referenced-funs fun) new-funs)
               (return t))))

;;; searches for a (FUNCTION-BLOCK-NAME ...) declaration
(defun function-block-name-declaration (declarations)
  (loop for i in declarations
        if (and (consp i) (eql (car i) 'si::function-block-name)
                (consp (cdr i)))
          return (cadr i)
        finally (return nil)))

(defun exported-fname (name)
  (let (cname)
    (if (and (symbolp name) (setf cname (si:get-sysprop name 'Lfun)))
        (values cname t)
        (values (next-cfun "L~D~A" name) nil))))

(defun lambda-form-allowed-nargs (lambda)
  (let ((minarg 0)
        (maxarg call-arguments-limit))
    (destructuring-bind (requireds optionals rest key-flag keywords a-o-k)
        (c1form-arg 0 lambda)
      (declare (ignore keywords))
      (setf minarg (length requireds))
      (when (and (null rest) (not key-flag) (not a-o-k))
        (setf maxarg (+ minarg (/ (length optionals) 3)))))
    (values minarg maxarg)))
