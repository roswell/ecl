;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPENV  Environments of the Compiler.

(in-package "C-ENV")

(defun function-arg-types (arg-types &aux (types nil))
  (do ((al arg-types (cdr al)))
      ((or (endp al)
           (member (car al) '(&optional &rest &key)))
       (nreverse types))
      (declare (object al))
      (push (c-types:type-filter (car al)) types)))

(defun proper-list-p (x &optional test)
  (and (listp x)
       (handler-case (list-length x) (type-error (c) nil))
       (or (null test) (every test x))))

;;; The valid return type declaration is:
;;;	(( VALUES {type}* )) or ( {type}* ).

(defun function-return-type (return-types)
  (cond ((endp return-types) t)
        ((and (consp (car return-types))
              (eq (caar return-types) 'VALUES))
         (cond ((not (endp (cdr return-types)))
                (warn "The function return types ~s is illegal." return-types)
                t)
               ((or (endp (cdar return-types))
                    (member (cadar return-types) '(&optional &rest &key)))
                t)
               (t (c-types:type-filter (cadar return-types)))))
        (t (c-types:type-filter (car return-types)))))

(defun add-function-proclamation (fname decl)
  (if (symbolp fname)
      (let* ((arg-types '*)
	     (return-types '*)
	     (l decl))
	(cond ((null l))
	      ((consp l)
	       (setf arg-types (pop l)))
	      (t (warn "The function proclamation ~s ~s is not valid."
		       fname decl)))
	(cond ((null l))
	      ((and (consp l) (null (rest l)))
	       (setf return-types (function-return-type l)))
	      (t (warn "The function proclamation ~s ~s is not valid."
		       fname decl)))
	(if (eq arg-types '*)
	    (sys:rem-sysprop fname 'PROCLAIMED-ARG-TYPES)
	    (sys:put-sysprop fname 'PROCLAIMED-ARG-TYPES arg-types))
	(if (eq return-types '*)
	    (sys:rem-sysprop fname 'PROCLAIMED-RETURN-TYPE)
	    (sys:put-sysprop fname 'PROCLAIMED-RETURN-TYPE return-types)))
      (warn "The function proclamation ~s ~s is not valid." fname decl)))

(defun add-function-declaration (fname arg-types return-types env)
  (if (si::valid-function-name-p fname)
      (cmp-env-register-ftype fname (list arg-types return-types) env)
      (warn "In (DECLARE (FTYPE ...)): ~s is not a valid function name" fname))
  env)

(defun get-arg-types (fname &optional (env *cmp-env*))
  (let ((x (cmp-env-search-ftype fname env)))
    (if x
        (values (first x) t)
        (sys:get-sysprop fname 'PROCLAIMED-ARG-TYPES))))

(defun get-return-type (fname &optional (env *cmp-env*))
  (let ((x (cmp-env-search-ftype fname env)))
    (if x
	(values (second x) t)
	(sys:get-sysprop fname 'PROCLAIMED-RETURN-TYPE))))

(defun get-local-arg-types (fun &optional (env *cmp-env*))
  (let ((x (cmp-env-search-ftype (fun-name fun))))
    (if x
        (values (first x) t)
        (values nil nil))))

(defun get-local-return-type (fun &optional (env *cmp-env*))
  (let ((x (cmp-env-search-ftype (fun-name fun))))
    (if x
        (values (second x) t)
        (values nil nil))))

(defun get-proclaimed-narg (fun &optional (env *cmp-env*))
  (multiple-value-bind (arg-list found)
      (get-arg-types fun env)
    (if found
	(loop for type in arg-list
	   with minarg = 0
	   and maxarg = 0
	   and in-optionals = nil
	   do (cond ((member type '(* &rest &key &allow-other-keys) :test #'eq)
		     (return (values minarg call-arguments-limit)))
		    ((eq type '&optional)
		     (setf in-optionals t maxarg minarg))
		    (in-optionals
		     (incf maxarg))
		    (t
		     (incf minarg)
		     (incf maxarg)))
	   finally (return (values minarg maxarg found)))
	(values 0 call-arguments-limit found))))

;;; Proclamation and declaration handling.

(defun inline-possible (fname &optional (env *cmp-env*))
  (not (or ; (compiler-<push-events)
	;(>= *debug* 2) Breaks compilation of STACK-PUSH-VALUES
        (let ((x (cmp-env-search-declaration 'notinline env)))
          (and x (member fname x :test #'same-fname-p)))
	(member fname *notinline* :test #'same-fname-p)
	(sys:get-sysprop fname 'CMP-NOTINLINE))))

#-:CCL
(defun proclaim (decl &aux decl-name)
  (unless (listp decl)
	  (error "The proclamation specification ~s is not a list" decl))
  (case (setf decl-name (car decl))
    (SPECIAL
     (dolist (var (cdr decl))
       (if (symbolp var)
           (sys:*make-special var)
           (error "Syntax error in proclamation ~s" decl))))
    (OPTIMIZE
     (dolist (x (cdr decl))
       (when (symbolp x) (setq x (list x 3)))
       (if (or (not (consp x))
               (not (consp (cdr x)))
               (not (numberp (second x)))
               (not (<= 0 (second x) 3)))
           (warn "The OPTIMIZE proclamation ~s is illegal." x)
           (case (car x)
		 (DEBUG (setq *debug* (second x)))
                 (SAFETY (setq *safety* (second x)))
                 (SPACE (setq *space* (second x)))
                 (SPEED (setq *speed* (second x)))
                 (COMPILATION-SPEED (setq *speed* (- 3 (second x))))
                 (t (warn "The OPTIMIZE quality ~s is unknown." (car x)))))))
    (TYPE
     (if (consp (cdr decl))
         (proclaim-var (second decl) (cddr decl))
         (error "Syntax error in proclamation ~s" decl)))
    (FTYPE
     (if (atom (rest decl))
	 (error "Syntax error in proclamation ~a" decl)
	 (multiple-value-bind (type-name args)
	     (si::normalize-type (second decl))
	   (if (eq type-name 'FUNCTION)
	       (dolist (v (cddr decl))
		 (add-function-proclamation v args))
	       (error "In an FTYPE proclamation, found ~A which is not a function type."
		      (second decl))))))
    (INLINE
     (dolist (fun (cdr decl))
       (if (si::valid-function-name-p fun)
	   (sys:rem-sysprop fun 'CMP-NOTINLINE)
	   (error "Not a valid function name ~s in proclamation ~s" fun decl))))
    (NOTINLINE
     (dolist (fun (cdr decl))
       (if (si::valid-function-name-p fun)
	   (sys:put-sysprop fun 'CMP-NOTINLINE t)
	   (error "Not a valid function name ~s in proclamation ~s" fun decl))))
    ((OBJECT IGNORE DYNAMIC-EXTENT IGNORABLE)
     ;; FIXME! IGNORED!
     (dolist (var (cdr decl))
       (unless (si::valid-function-name-p var)
	 (error "Not a valid function name ~s in ~s proclamation" fun decl-name))))
    (DECLARATION
     (validate-alien-declaration (rest decl) #'error)
     (setf si::*alien-declarations*
           (nconc (copy-list (rest decl)) si::*alien-declarations*)))
    (SI::C-EXPORT-FNAME
     (dolist (x (cdr decl))
       (cond ((symbolp x)
	      (multiple-value-bind (found c-name)
		  (si::mangle-name x t)
		(if found
		    (warn "The function ~s is already in the runtime. C-EXPORT-FNAME declaration ignored." x)
		    (sys:put-sysprop x 'Lfun c-name))))
	     ((consp x)
	      (destructuring-bind (c-name lisp-name) x
		(if (si::mangle-name lisp-name)
		    (warn "The funciton ~s is already in the runtime. C-EXPORT-FNAME declaration ignored." lisp-name)
		    (sys:put-sysprop lisp-name 'Lfun c-name))))
	     (t
	      (error "Syntax error in proclamation ~s" decl)))))
    ((ARRAY ATOM BASE-CHAR BIGNUM BIT BIT-VECTOR CHARACTER COMPILED-FUNCTION
      COMPLEX CONS DOUBLE-FLOAT EXTENDED-CHAR FIXNUM FLOAT HASH-TABLE INTEGER KEYWORD LIST
      LONG-FLOAT NIL NULL NUMBER PACKAGE PATHNAME RANDOM-STATE RATIO RATIONAL
      READTABLE SEQUENCE SHORT-FLOAT SIMPLE-ARRAY SIMPLE-BIT-VECTOR
      SIMPLE-STRING SIMPLE-VECTOR SINGLE-FLOAT STANDARD-CHAR STREAM STRING
      SYMBOL T VECTOR SIGNED-BYTE UNSIGNED-BYTE FUNCTION)
     (proclaim-var decl-name (cdr decl)))
    (otherwise
     (cond ((multiple-value-bind (ok type)
		(c-types:valid-type-specifier decl-name)
	      (when ok
		(proclaim-var type (rest decl))
		t)))
	   ((let ((proclaimer (sys:get-sysprop (car decl) :proclaim)))
	      (when (functionp proclaimer)
		(mapc proclaimer (rest decl))
		t)))
	   ((alien-declaration-p (first decl)))
	   (t
	    (warn "The declaration specifier ~s is unknown." decl-name))))))

(defun type-name-p (name)
  (or (sys:get-sysprop name 'SI::DEFTYPE-DEFINITION)
      (find-class name nil)
      (sys:get-sysprop name 'SI::STRUCTURE-TYPE)))

(defun validate-alien-declaration (names-list error)
  (declare (si::c-local))
  (dolist (new-declaration names-list)
    (unless (symbolp new-declaration)
      (funcall error "The declaration ~s is not a symbol" new-declaration))
    (when (type-name-p new-declaration)
      (funcall error "Symbol ~S cannot be both the name of a type and of a declaration"
               new-declaration))))

(defun proclaim-var (type vl)
  (setq type (c-types:type-filter type))
  (dolist (var vl)
    (if (symbolp var)
	(let* ((type1 (sys:get-sysprop var 'CMP-TYPE))
               (v (find var *undefined-vars* :key #'var-name))
               (merged (if type1 (type-and type1 type) type)))
	  (unless merged
	    (warn
	     "Proclamation for variable ~A of type~&~4T~A~&is incompatible with previous declaration~&~4~T~A"
	     var type type1)
	    (setq merged T))
	  (sys:put-sysprop var 'CMP-TYPE merged)
	  (when v (setf (var-type v) merged)))
	(warn "The variable name ~s is not a symbol." var))))

(defun c1body (body doc-p &aux
	            (all-declarations nil)
		    (ss nil)		; special vars
		    (is nil)		; ignored vars
		    (ts nil)		; typed vars (var . type)
		    (others nil)	; all other vars
	            doc form)
  (loop
    (when (endp body) (return))
    (setq form (cmp-macroexpand (car body)))
    (cond
     ((stringp form)
      (when (or (null doc-p) (endp (cdr body)) doc) (return))
      (setq doc form))
     ((and (consp form) (eq (car form) 'DECLARE))
      (push form all-declarations)
      (dolist (decl (cdr form))
        (cmpassert (and (proper-list-p decl) (symbolp (first decl)))
		   "Syntax error in declaration ~s" form)
	(let* ((decl-name (first decl))
	       (decl-args (rest decl)))
	  (flet ((declare-variables (type var-list)
		   (cmpassert (proper-list-p var-list #'symbolp)
			      "Syntax error in declaration ~s" decl)
		   (when type
		     (dolist (var var-list)
		       (push (cons var type) ts)))))
	    (case decl-name
	      (SPECIAL
	       (cmpassert (proper-list-p decl-args #'symbolp)
			  "Syntax error in declaration ~s" decl)
	       (setf ss (append decl-args ss)))
	      (IGNORE
	       (cmpassert (proper-list-p decl-args #'symbolp)
			  "Syntax error in declaration ~s" decl)
	       (setf is (append decl-args is)))
	      (TYPE
	       (cmpassert decl-args "Syntax error in declaration ~s" decl)
	       (declare-variables (first decl-args) (rest decl-args)))
	      (OBJECT
	       (declare-variables 'OBJECT decl-args))
	      ;; read-only variable treatment. obsolete!
	      (:READ-ONLY
	       (push decl others))
	      ((OPTIMIZE FTYPE INLINE NOTINLINE DECLARATION SI::C-LOCAL SI::C-GLOBAL
		DYNAMIC-EXTENT IGNORABLE VALUES SI::NO-CHECK-TYPE)
	       (push decl others))
	      (otherwise
	       (multiple-value-bind (ok type)
                   (c-types:valid-type-specifier decl-name)
                 (cmpassert ok "The declaration specifier ~s is unknown." decl-name)
                 (declare-variables type decl-args)))
	      )))))
     (t (return)))
    (pop body))
  (values body ss ts is others doc all-declarations))

(defun default-optimization (optimization)
  (ecase optimization
    (speed *speed*)
    (safety *safety*)
    (space *space*)
    (debug *debug*)))

(defun search-optimization-quality (declarations what)
  (dolist (i (reverse declarations)
	   (default-optimization what))
    (when (and (consp i) (eq (first i) 'optimize))
      (dolist (j (rest i))
	(cond ((consp j)
	       (when (eq (first j) what)
		 (return-from search-optimization-quality (second j))))
	      ((eq j what)
	       (return-from search-optimization-quality 3)))))))

(defun compute-optimizations (arguments env)
  (let ((optimizations (cmp-env-all-optimizations env)))
    (dolist (x arguments)
      (when (symbolp x) (setq x (list x 3)))
      (unless optimizations
        (setq optimizations (cmp-env-all-optimizations)))
      (if (or (not (consp x))
              (not (consp (cdr x)))
              (not (numberp (second x)))
              (not (<= 0 (second x) 3)))
          (cmpwarn "The OPTIMIZE proclamation ~s is illegal." x)
          (let ((value (second x)))
            (case (car x)
              (DEBUG (setf (first optimizations) value))
              (SAFETY (setf (second optimizations) value))
              (SPACE (setf (third optimizations) value))
              (SPEED (setf (fourth optimizations) value))
              (COMPILATION-SPEED)
              (t (cmpwarn "The OPTIMIZE quality ~s is unknown." (car x)))))))
    optimizations))

(defun add-declarations (decls &optional (env *cmp-env*))
  (dolist (decl decls)
    (case (car decl)
      (OPTIMIZE
       (let ((optimizations (compute-optimizations (rest decl) env)))
         (setf env (cmp-env-add-declaration 'optimize optimizations))))
      (FTYPE
       (if (atom (rest decl))
	   (cmpwarn "Syntax error in declaration ~a" decl)
	   (multiple-value-bind (type-name args)
	       (si::normalize-type (second decl))
	     (if (eq type-name 'FUNCTION)
		 (dolist (v (cddr decl))
                   (setf env (add-function-declaration v (first args) (rest args) env)))
		 (cmpwarn "In an FTYPE declaration, found ~A which is not a function type."
			  (second decl))))))
      (INLINE
       (let* ((x (copy-list (cmp-env-search-declaration 'notinline)))
              (names (rest decl)))
         (dolist (fun names)
           (unless (si::valid-function-name-p fun)
             (cmperr "Not a valid function name ~s in declaration ~s" fun decl))
           (setf x (delete fun x :test #'same-fname-p)))
         (setf env (cmp-env-add-declaration 'notinline x))))
      (NOTINLINE
       (let* ((x (cmp-env-search-declaration 'notinline))
              (names (rest decl)))
         (dolist (fun names)
           (if (si::valid-function-name-p fun)
               (push fun x)
               (cmperr "Not a valid function name ~s in declaration ~s" fun decl)))
         (setf env (cmp-env-add-declaration 'notinline x))))
      (DECLARATION
       (validate-alien-declaration (rest decl) #'cmperr)
       (cmp-env-extend-declarations 'alien (rest decl)))
      ((SI::C-LOCAL SI::C-GLOBAL SI::NO-CHECK-TYPE))
      ((DYNAMIC-EXTENT IGNORABLE)
       ;; FIXME! SOME ARE IGNORED!
       )
      (:READ-ONLY)
      (otherwise
       (unless (alien-declaration-p (first decl))
	 (cmpwarn "The declaration specifier ~s is unknown." (car decl))))))
  env)

(defun check-vdecl (vnames ts is)
  (dolist (x ts)
    (unless (member (car x) vnames)
      (cmpwarn "Type declaration was found for not bound variable ~s."
               (car x))))
  (dolist (x is)
    (unless (member x vnames)
      (cmpwarn "Ignore declaration was found for not bound variable ~s." x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPILER ENVIRONMENT
;;;

(defmacro cmp-env-new ()
  '(cons nil nil))

(defun cmp-env-copy (&optional (env *cmp-env*))
  (cons (car env) (cdr env)))

(defmacro cmp-env-variables (&optional (env '*cmp-env*))
  `(car ,env))

(defmacro cmp-env-functions (&optional (env '*cmp-env*))
  `(cdr ,env))

(defun cmp-env-cleanups (env)
  (loop with specials = '()
	with end = (cmp-env-variables env)
	with cleanup-forms = '()
	with aux
	for records-list on (cmp-env-variables *cmp-env*)
	until (eq records-list end)
	do (let ((record (first records-list)))
	     (cond ((atom record))
		   ((and (symbolp (first record))
			 (eq (second record) :special))
		    (push (fourth record) specials))
		   ((eq (first record) :cleanup)
		    (push (second record) cleanup-forms))))
	finally (progn
		  (unless (eq records-list end)
		    (error "Inconsistency in environment."))
		  (return (values specials
                                  (apply #'nconc (mapcar #'copy-list cleanup-forms)))))))

(defun cmp-env-register-var (var &optional (env *cmp-env*) (boundp t))
  (push (list (var-name var)
	      (if (member (var-kind var) '(special global))
		  :special
		  t)
	      boundp
	      var)
	(cmp-env-variables env))
  env)

(defun cmp-env-declare-special (name &optional (env *cmp-env*))
  (cmp-env-register-var (c::c1make-global-variable name :warn nil :kind 'SPECIAL)
			env nil)
  env)

(defun cmp-env-add-declaration (type arguments &optional (env *cmp-env*))
  (push (list* :declare type arguments) 
        (cmp-env-variables env))
  env)

(defun cmp-env-extend-declaration (type arguments &optional (env *cmp-env*))
  (let ((x (cmp-env-search-declaration type)))
    (cmp-env-add-declaration type (append arguments x) env)
    env))

(defun cmp-env-register-function (fun &optional (env *cmp-env*))
  (push (list (fun-name fun) 'function fun)
	(cmp-env-functions env))
  env)

(defun cmp-env-register-macro (name function &optional (env *cmp-env*))
  (push (list name 'si::macro function)
	(cmp-env-functions env))
  env)

(defun cmp-env-register-ftype (name declaration &optional (env *cmp-env*))
  (push (list* :declare name declaration)
        (cmp-env-functions env))
  env)

(defun cmp-env-register-symbol-macro (name form &optional (env *cmp-env*))
  (push (list name 'si::symbol-macro #'(lambda (whole env) form))
	(cmp-env-variables env))
  env)

(defun cmp-env-register-block (blk &optional (env *cmp-env*))
  (push (list :block (blk-name blk) blk)
	(cmp-env-variables env))
  env)

(defun cmp-env-register-tag (name tag &optional (env *cmp-env*))
  (push (list :tag (list name) tag)
	(cmp-env-variables env))
  env)

(defun cmp-env-register-cleanup (form &optional (env *cmp-env*))
  (push (list :cleanup (copy-list form)) (cmp-env-variables env))
  env)

(defun cmp-env-search-function (name &optional (env *cmp-env*))
  (let ((ccb nil)
	(clb nil)
	(unw nil)
	(found nil))
    (dolist (record (cmp-env-functions env))
      (cond ((eq record 'CB)
	     (setf ccbb t))
	    ((eq record 'LB)
	     (setf clb t))
	    ((eq record 'UNWIND-PROTECT)
	     (setf unw t))
	    ((atom record)
	     (baboon))
	    ;; We have to use EQUAL because the name can be a list (SETF whatever)
	    ((equal (first record) name)
	     (setf found (first (last record)))
	     (return))))
    (values found ccb clb unw)))

(defun cmp-env-search-variables (type name env)
  (let ((ccb nil)
	(clb nil)
	(unw nil)
	(found nil))
    (dolist (record (cmp-env-variables env))
      (cond ((eq record 'CB)
	     (setf ccb t))
	    ((eq record 'LB)
	     (setf clb t))
	    ((eq record 'UNWIND-PROTECT)
	     (setf unw t))
	    ((atom record)
	     (baboon))
	    ((not (eq (first record) type)))
	    ((eq type :block)
	     (when (eq name (second record))
	       (setf found record)
	       (return)))
	    ((eq type :tag)
	     (when (member name (second record) :test #'eql)
	       (setf found record)
	       (return)))
	    ((eq (second record) 'si::symbol-macro)
	     (when (eq name 'si::symbol-macro)
	       (setf found record))
	     (return))
	    (t
	     (setf found record)
	     (return))))
    (values (first (last found)) ccb clb unw)))

(defun cmp-env-search-block (name &optional (env *cmp-env*))
  (cmp-env-search-variables :block name env))

(defun cmp-env-search-tag (name &optional (env *cmp-env*))
  (cmp-env-search-variables :tag name env))

(defun cmp-env-search-symbol-macro (name &optional (env *cmp-env*))
  (cmp-env-search-variables name 'si::symbol-macro env))

(defun cmp-env-search-var (name &optional (env *cmp-env*))
  (cmp-env-search-variables name t env))

(defun cmp-env-search-macro (name &optional (env *cmp-env*))
  (let ((f (cmp-env-search-function name env)))
    (if (functionp f) f nil)))

(defun cmp-env-search-ftype (name &optional (env *cmp-env*))
  (dolist (i env nil)
    (when (and (consp i)
               (eq (pop i) :declare)
               (same-fname-p (pop i) name))
      (return i))))

(defun cmp-env-mark (mark &optional (env *cmp-env*))
  (cons (cons mark (car env))
	(cons mark (cdr env))))

(defun cmp-env-new-variables (new-env old-env)
  (loop for i in (ldiff (cmp-env-variables *cmp-env*)
			(cmp-env-variables old-env))
	when (and (consp i) (var-p (fourth i)))
	collect (fourth i)))

(defun cmp-env-search-declaration (kind &optional (env *cmp-env*))
  (loop for i in (car env)
     when (and (consp i)
               (eq (first i) :declare)
               (eq (second i) kind))
     return (cddr i)))

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

(defun policy-assume-right-type (&optional (env *cmp-env*))
  (< (cmp-env-optimization 'safety env) 2))

(defun policy-check-stack-overflow (&optional (env *cmp-env*))
  "Do we add a stack check to every function?"
  (>= (cmp-env-optimization 'safety env) 2))

(defun policy-inline-slot-access-p (&optional (env *cmp-env*))
  "Do we inline access to structures and sealed classes?"
  (or (< (cmp-env-optimization 'safety env) 2)
      (<= (cmp-env-optimization 'safety env) (cmp-env-optimization 'speed env))))

(defun policy-check-all-arguments-p (&optional (env *cmp-env*))
  "Do we assume that arguments are the right type?"
  (> (cmp-env-optimization 'safety env) 1))

(defun policy-automatic-check-type-p (&optional (env *cmp-env*))
  "Do we generate CHECK-TYPE forms for function arguments with type declarations?"
  (and *automatic-check-type-in-lambda*
       (>= (cmp-env-optimization 'safety env) 1)))

(defun policy-assume-types-dont-change-p (&optional (env *cmp-env*))
  "Do we assume that type and class definitions will not change?"
  (<= (cmp-env-optimization 'safety env) 1))

(defun policy-open-code-aref/aset-p (&optional (env *cmp-env*))
  "Do we inline access to arrays?"
  (< (cmp-env-optimization 'debug env) 2))

(defun policy-open-code-accessors (&optional (env *cmp-env*))
  "Do we inline access to object slots, including conses and arrays?"
  (< (cmp-env-optimization 'debug env) 2))

(defun policy-array-bounds-check-p (&optional (env *cmp-env*))
  "Check access to array bounds?"
  (>= (cmp-env-optimization 'safety env) 1))

(defun policy-evaluate-forms (&optional (env *cmp-env*))
  "Pre-evaluate a function that takes constant arguments?"
  (<= (cmp-env-optimization 'debug env) 1))

(defun alien-declaration-p (name)
  (or (member name (cmp-env-search-declaration 'alien) :test #'eq)
      (member name si:*alien-declarations*)))

(defun policy-global-var-checking (&optional (env *cmp-env*))
  "Do we have to read the value of a global variable even if it is discarded?
Also, when reading the value of a global variable, should we ensure it is bound?"
  (>= (cmp-env-optimization 'safety env) 1))

(defun policy-global-function-checking (&optional (env *cmp-env*))
  "Do we have to read the binding of a global function even if it is discarded?"
  (>= (cmp-env-optimization 'safety env) 1))

(defun policy-debug-variable-bindings (&optional (env *cmp-env*))
  "Shall we create a vector with the bindings of each LET/LET*/LAMBDA form?"
  ;; We can only create variable bindings when the function has an IHS frame!!!
  (and (policy-debug-ihs-frame env)
       (>= (cmp-env-optimization 'debug env) 3)))

(defun policy-debug-ihs-frame (&optional (env *cmp-env*))
  "Shall we create an IHS frame so that this function shows up in backtraces?"
  ;; Note that this is a prerequisite for registering variable bindings. Hence,
  ;; it has to be recorded in a special variable.
  (>= (fun-debug *current-function*) 2))

(defun policy-check-nargs (&optional (env *cmp-env*))
  (>= (cmp-env-optimization 'safety) 1))

(defmacro safe-compile ()
  `(>= (cmp-env-optimization 'safety) 2))

