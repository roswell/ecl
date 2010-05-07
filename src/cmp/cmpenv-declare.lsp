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
;;;; CMPENV-DECLARE -- Declarations for the compiler
;;;;
;;;; Extract, process and incorporate declarations into the compiler
;;;; environment. Unlike proclamations, these are local to the current
;;;; compiled file and do not propagate beyond it.
;;;;

(in-package #-ecl-new "COMPILER" #+ecl-new "C-ENV")

(defun proper-list-p (x &optional test)
  (and (listp x)
       (handler-case (list-length x) (type-error (c) nil))
       (or (null test) (every test x))))

(defun type-name-p (name)
  (or (get-sysprop name 'SI::DEFTYPE-DEFINITION)
      (find-class name nil)
      (get-sysprop name 'SI::STRUCTURE-TYPE)))

(defun validate-alien-declaration (names-list error)
  (dolist (new-declaration names-list)
    (unless (symbolp new-declaration)
      (cmperr "The declaration ~s is not a symbol" new-declaration))
    (when (type-name-p new-declaration)
      (cmperr "Symbol name ~S cannot be both the name of a type and of a declaration"
              new-declaration))))

(defun alien-declaration-p (name &optional (env *cmp-env*))
  (or (member name si::*alien-declarations*)
      (member name (cmp-env-search-declaration 'alien env))))

(defun parse-ignore-declaration (decl-args expected-ref-number tail)
  (declare (si::c-local))
  (loop for name in decl-args
     do (if (symbolp name)
            (push (cons name expected-ref-number) tail)
            (cmpassert (and (consp name)
                            (= (length name) 2)
                            (eq (first name) 'function))
                       "Invalid argument to IGNORE/IGNORABLE declaration:~&~A"
                       name)))
  tail)

(defun collect-declared (type var-list tail)
  (declare (si::c-local))
  (cmpassert (proper-list-p var-list #'symbolp)
             "Syntax error in declaration ~s" decl)
  (loop for var-name in var-list
     do (push (cons var-name type) tail))
  tail)

(defun c1body (body doc-p)
  "Split a function body into a list of forms, a set of declarations,
and a possible documentation string (only accepted when DOC-P is true)."
  (multiple-value-bind (all-declarations body doc specials)
      (si:process-declarations body doc-p)
    (loop with others = '()
       with types = '()
       with ignored = '()
       for decl in all-declarations
       for decl-name = (first decl)
       for decl-args = (rest decl)
       do (cmpassert (and (proper-list-p decl-args) (symbolp decl-name))
                     "Syntax error in declaration ~s" decl)
       do (case decl-name
            (SPECIAL)
            (IGNORE
             (cmpassert (proper-list-p decl-args #'symbolp)
                        "Syntax error in declaration ~s" decl)
             (setf ignored (parse-ignore-declaration decl-args -1 ignored)))
            (IGNORABLE
             (cmpassert (proper-list-p decl-args #'symbolp)
                        "Syntax error in declaration ~s" decl)
             (setf ignored (parse-ignore-declaration decl-args 0 ignored)))
            (TYPE
             (cmpassert (and (consp decl-args)
                             (proper-list-p (rest decl-args) #'symbolp))
                        "Syntax error in declaration ~s" decl)
             (setf types (collect-declared (first decl-args)
                                           (rest decl-args)
                                           types)))
            (OBJECT
             (cmpassert (proper-list-p decl-args #'symbolp)
                        "Syntax error in declaration ~s" decl)
             (setf types (collect-declared 'OBJECT decl-args types)))
            ((OPTIMIZE FTYPE INLINE NOTINLINE DECLARATION SI::C-LOCAL
              SI::C-GLOBAL DYNAMIC-EXTENT IGNORABLE VALUES
              SI::NO-CHECK-TYPE POLICY-DEBUG-IHS-FRAME :READ-ONLY)
             (push decl others))
            (otherwise
             (if (alien-declaration-p decl-name)
                 (push decl others)
                 (multiple-value-bind (ok type)
                     (valid-type-specifier decl-name)
                   (cmpassert ok "Unknown declaration specifier ~s"
                              decl-name)
                   (setf types (collect-declared type decl-args types))))))
       finally (return (values body specials types ignored
                               (nreverse others) doc all-declarations)))))

(defun default-optimization (optimization)
  (ecase optimization
    (speed *speed*)
    (safety *safety*)
    (space *space*)
    (debug *debug*)))

(defun search-optimization-quality (declarations what)
  (dolist (i (reverse declarations)
	   (default-optimization what))
    (when (and (consp i) (eq (first i) 'policy-debug-ihs-frame)
               (eq what 'debug))
      (return 2))      
    (when (and (consp i) (eq (first i) 'optimize))
      (dolist (j (rest i))
	(cond ((consp j)
	       (when (eq (first j) what)
		 (return-from search-optimization-quality (second j))))
	      ((eq j what)
	       (return-from search-optimization-quality 3)))))))

(defun compute-optimizations (arguments env)
  (let ((optimizations (copy-list (cmp-env-all-optimizations env))))
    (dolist (x arguments)
      (when (symbolp x)
        (setq x (list x 3)))
      (if (or (not (consp x))
              (not (consp (cdr x)))
              (not (numberp (second x)))
              (not (<= 0 (second x) 3)))
          (cmpwarn "Illegal OPTIMIZE proclamation ~s" x)
          (let ((value (second x)))
            (case (car x)
              (DEBUG (setf (first optimizations) value))
              (SAFETY (setf (second optimizations) value))
              (SPACE (setf (third optimizations) value))
              (SPEED (setf (fourth optimizations) value))
              (COMPILATION-SPEED)
              (t (cmpwarn "Unknown OPTIMIZE quality ~s" (car x)))))))
    optimizations))

(defun add-one-declaration (env decl)
  "Add to the environment one declarations which is not type, ignorable or
special variable declarations, as these have been extracted before."
  (case (car decl)
    (OPTIMIZE
     (let ((optimizations (compute-optimizations (rest decl) env)))
       (cmp-env-add-declaration 'optimize optimizations env)))
    (POLICY-DEBUG-IHS-FRAME
     (let ((flag (or (rest decl) '(t))))
       (if *current-function*
           (progn
             (cmp-env-add-declaration 'policy-debug-ihs-frame flag
                                      (fun-cmp-env *current-function*))
             env)
           (cmp-env-add-declaration 'policy-debug-ihs-frame
                                    flag env))))
    (FTYPE
     (if (atom (rest decl))
         (cmpwarn "Syntax error in declaration ~a" decl)
         (multiple-value-bind (type-name args)
             (si::normalize-type (second decl))
           (if (eq type-name 'FUNCTION)
               (dolist (v (cddr decl))
                 (setf env (add-function-declaration v (first args)
                                                     (rest args) env)))
               (cmpwarn "In an FTYPE declaration, found ~A which is not a function type."
                        (second decl)))))
     env)
    (INLINE
      (declare-inline (rest decl) env))
    (NOTINLINE
     (setf env (declare-notinline (rest decl) env)))
    (DECLARATION
     (validate-alien-declaration (rest decl) #'cmperr)
     (setf env (cmp-env-extend-declaration 'alien (rest decl) env)))
    ((SI::C-LOCAL SI::C-GLOBAL SI::NO-CHECK-TYPE :READ-ONLY)
     env)
    ((DYNAMIC-EXTENT IGNORABLE)
     ;; FIXME! SOME ARE IGNORED!
     env)
    (otherwise
     (unless (alien-declaration-p (first decl) env)
       (cmpwarn "Unknown declaration specifier ~s" (first decl)))
     env)))

(defun symbol-macro-declaration-p (name type)
  (let* ((record (cmp-env-search-variables name 'si::symbol-macro *cmp-env*)))
    (when (and record (functionp record))
      (let* ((expression (funcall record name nil)))
        (cmp-env-register-symbol-macro name `(the ,type ,expression)))
      t)))

(defun check-vdecl (vnames ts is)
  (loop for (var . type) in ts
     unless (or (member var vnames :test #'eq)
                (symbol-macro-declaration-p var type))
     do (cmpwarn "Declaration of type~&~4T~A~&was found for not bound variable ~s."
                 type var))
  (loop for (var . expected-uses) in is
     unless (member var vnames :test #'eq)
     do (cmpwarn (if (minusp expected-uses)
                     "IGNORE declaration was found for not bound variable ~s."
                     "IGNORABLE declaration was found for not bound variable ~s.")
                 var)))
