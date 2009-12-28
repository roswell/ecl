;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPTOP  --  Compiler top-level.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defmacro with-t1expr ((init-name) &rest body)
  `(t1loop #'(lambda ()
               (flet ((t1expr (form)
                        (let ((*current-toplevel-form* form)
                              (*compile-toplevel* t)
                              (*compile-time-too* nil))
                          (setf *top-level-forms* (nreconc (t1expr* 'trash form)
                                                           *top-level-forms*)))))
                 ,@body))
           ,init-name))

(defparameter +init-function-name+ (gensym "ENTRY-POINT"))

(defun t1loop (body init-name)
  (let* ((only-argument (make-var :name (gensym "CBLOCK")
                                  :type T
                                  :kind :object
                                  :loc `(LCL 1)
                                  :ref 1))
         (fun (make-fun :name +init-function-name+
                        :cfun init-name
                        :minarg 1 :maxarg 1 :closure nil
                        :global t :no-entry t :exported t))
         (*current-function* fun)
         (*lcl* 1)
         (*last-label* 0)
         (*cmp-env* (cmp-env-register-var only-argument (cmp-env-new)))
         (*permanent-data* nil))
    (setf *top-level-forms* nil)
    (funcall body)
    (setf (fun-lambda fun) (nconc (c1function-prologue fun)
                                  (nreverse *top-level-forms*)
                                  (c1function-epilogue fun))
          (fun-last-lcl fun) *lcl*
          (fun-last-label fun) *last-label*
          *top-level-forms* fun)))

(defvar *toplevel-forms-to-print*
  '(defun defmacro defvar defparameter defclass defmethod defgeneric))

(defun t1expr* (destination form)
  ;(let ((*print-level* 3)) (print form))
  (catch *cmperr-tag*
    (when (consp form)
      (let ((fun (car form)) (args (cdr form)) fd)
	(when (member fun *toplevel-forms-to-print*)
	  (print-current-form))
	(cond
            ((consp fun) (t1ordinary destination form))
            ((not (symbolp fun))
	     (cmperr "~s is illegal function." fun))
	    ((eq fun 'QUOTE)
	     (t1ordinary destination 'NIL))
	    ((setq fd (get-sysprop fun 'T1))
	     (funcall fd destination args))
	    ((or (get-sysprop fun 'C1) (get-sysprop fun 'C1SPECIAL))
             (t1ordinary destination form))
	    ((and (setq fd (compiler-macro-function fun))
		  (inline-possible fun)
		  (let ((success nil))
		    (multiple-value-setq (fd success)
		      (cmp-expand-macro fd form))
		    success))
	     (t1expr* destination fd))
	    ((setq fd (cmp-macro-function fun))
	     (t1expr* destination (cmp-expand-macro fd form)))
	    (t (t1ordinary destination form))
	   )))))

(defun t1/c1expr (destination form)
  (cond ((not *compile-toplevel*)
	 (c1translate destination form))
	((atom form)
	 (t1ordinary destination form))
	(t
	 (t1expr* destination form))))	

(defun emit-local-funs (fun)
  (format t "~&;;; Adding ~A" (fun-child-funs fun))
  (loop with *compile-time-too* = nil
     with *compile-toplevel* = nil
     with emitted-local-funs = (make-hash-table :test #'eql)
     with pending = (fun-child-funs fun)
     while pending
     do (let ((f (pop pending)))
          (when (gethash f emitted-local-funs)
            (error "Doubly emitted function ~A" f))
          (t3local-fun f)
          (format t "~&;;; Adding ~A" (fun-child-funs f))
          (setf (gethash f emitted-local-funs) t
                pending (append (fun-child-funs f) pending)))))

(defun ctop-write (name h-pathname data-pathname
		        &key shared-data
			&aux def top-output-string
			(*volatile* "volatile "))

  ;(let ((*print-level* 3)) (pprint *top-level-forms*))
  (wt-nl1 "#include \"" (si::coerce-to-filename h-pathname) "\"")
  ;; All lines from CLINES statements are grouped at the beginning of the header
  ;; Notice that it does not make sense to guarantee that c-lines statements
  ;; are produced in-between the function definitions, because two functions
  ;; might be collapsed into one, or we might not produce that function at all
  ;; and rather inline it.
  (do ()
      ((null *clines-string-list*))
    (wt-nl-h (pop *clines-string-list*)))
  (wt-nl-h "#ifdef __cplusplus")
  (wt-nl-h "extern \"C\" {")
  (wt-nl-h "#endif")
  (when si::*compiler-constants*
    (wt-nl-h "#include <string.h>"))
  ;;; Initialization function.
  (let* ((c-output-file *compiler-output1*)
	 (*compiler-output1* (make-string-output-stream))
	 (*compiler-declared-globals* (make-hash-table)))
    (unless shared-data
      (wt-nl1 "#include \"" (si::coerce-to-filename data-pathname) "\""))

    ;; Type propagation phase
    (when *do-type-propagation*
      (setq *compiler-phase* 'p1propagate)
      (dolist (form *top-level-forms*)
        (p1propagate form nil))
      (dolist (fun *local-funs*)
        (propagate-function-types fun)))

    (setq *compiler-phase* 't2)

    ;; Optimization passes
    (execute-pass 'pass-consistency)
    (execute-pass 'pass-delete-no-side-effects)
    (execute-pass 'pass-delete-unused-bindings)
    (execute-pass 'pass-decide-var-rep-types)
    (execute-pass 'pass-assign-labels)

    ;; Emit entry function
    (let ((*compile-to-linking-call* nil))
      (t3local-fun *top-level-forms*))

    ;; Now emit the rest
    (let ((*compiler-output1* c-output-file))
      (emit-local-funs *top-level-forms*))

    (setq top-output-string (get-output-stream-string *compiler-output1*)))

  ;; Declarations in h-file.
  (wt-nl-h "static cl_object Cblock;")
  (let ((num-objects (data-size)))
    (if (zerop num-objects)
	(progn
	  (wt-nl-h "#undef ECL_DYNAMIC_VV")
	  (wt-nl-h "#define compiler_data_text \"\"")
	  (wt-nl-h "#define compiler_data_text_size 0")
	  (wt-nl-h "#define VM 0")
	  (wt-nl-h "#define VMtemp 0")
	  (wt-nl-h "#define VV NULL"))
	(progn
	  (wt-nl-h "#define VM " (data-permanent-storage-size))
	  (wt-nl-h "#define VMtemp "  (data-temporary-storage-size))
	  (wt-nl-h "#ifdef ECL_DYNAMIC_VV")
	  (wt-nl-h "static cl_object *VV;")
	  (wt-nl-h "#else")
	  (wt-nl-h "static cl_object VV[VM];")
	  (wt-nl-h "#endif"))))

  (dolist (l *linking-calls*)
    (let* ((c-name (fourth l))
	   (var-name (fifth l)))
      (wt-nl-h "static cl_object " c-name "(cl_narg, ...);")
      (wt-nl-h "static cl_object (*" var-name ")(cl_narg, ...)=" c-name ";")))

  ;;; Initial functions for linking calls.
  (dolist (l *linking-calls*)
    (let* ((var-name (fifth l))
	   (c-name (fourth l))
	   (lisp-name (third l)))
      (wt-nl1 "static cl_object " c-name "(cl_narg narg, ...)"
	      "{TRAMPOLINK(narg," lisp-name ",&" var-name ",Cblock);}")))

  (wt-nl-h "#ifdef __cplusplus")
  (wt-nl-h "}")
  (wt-nl-h "#endif")

  (when (and (listp *static-constants*)
             (setf *static-constants* (nreverse *static-constants*)))
    (wt-nl-h "/*")
    (wt-nl-h " * Statically defined constants")
    (wt-nl-h " */")
    (loop for (value name builder) in (reverse *static-constants*)
          do (terpri *compiler-output2*)
          do (funcall builder name value *compiler-output2*)))

  (output-cfuns *compiler-output2*)

  (setq *compiler-phase* 't3)

  ;;; Callbacks
  (when *callbacks*
    (wt-nl-h "#include <ecl/internal.h>")
    (dolist (x *callbacks*)
      (apply #'t3-defcallback x)))

  (wt-nl top-output-string))

(defun c1eval-when (destination args)
  (check-args-number 'EVAL-WHEN args 1)
  (let ((load-flag nil)
	(compile-flag nil)
	(execute-flag nil))
    (dolist (situation (car args))
      (case situation
	((LOAD :LOAD-TOPLEVEL) (setq load-flag t))
	((COMPILE :COMPILE-TOPLEVEL) (setq compile-flag t))
	((EVAL :EXECUTE)
	 (if *compile-toplevel*
	     (setq compile-flag (or *compile-time-too* compile-flag))
	     (setq execute-flag t)))
	(otherwise (cmperr "The EVAL-WHEN situation ~s is illegal."
			   situation))))
    (cond ((not *compile-toplevel*)
	   (c1progn destination (and execute-flag (rest args))))
	  (load-flag
	   (let ((*compile-time-too* compile-flag))
	     (c1progn destination (rest args))))
	  (compile-flag
	   (cmp-eval (cons 'PROGN (rest args)))
	   (c1progn destination 'NIL))
	  (t
	   (c1progn destination 'NIL)))))

(defun exported-fname (name)
  (let (cname)
    (if (and (symbolp name)
             (not (member name *notinline*))
             (setf cname (get-sysprop name 'Lfun)))
        (values cname t)
        (values (next-cfun "L~D~A" name) nil))))

(defun new-defun (new &optional no-entry)
  (push new *global-funs*))

(defun print-function (x)
  (format t "~%<a FUN: ~A, CLOSURE: ~A, LEVEL: ~A, ENV: ~A>"
	  (fun-name x) (fun-closure x) (fun-level x) (fun-env x)))

(defun rep-type (type)
  (case type
    (FIXNUM "cl_fixnum ")
    (CHARACTER "unsigned char ")
    (SINGLE-FLOAT "float ")
    (DOUBLE-FLOAT "double ")
    (otherwise "cl_object ")))

(defun t1ordinary (destination form)
  (when *compile-time-too* (cmp-eval form))
  (let* ((*compile-toplevel* nil)
         (*compile-time-too* nil))
    (add-load-time-values (c1translate destination form))))

(defun add-load-time-values (form)
  (let ((previous (nconc *load-time-values* *make-forms*)))
    (setf *load-time-values* nil
          *make-forms* nil)
    (nconc previous form)))

(defun c1load-time-value (destination args)
  (check-args-number 'LOAD-TIME-VALUE args 1 2)
  (let ((form (first args))
	loc)
    (cond ((not (listp *load-time-values*))
	   ;; When using COMPILE, we set *load-time-values* to 'VALUES and
	   ;; thus signal that we do not want to compile these forms, but
	   ;; just to retain their value.
	   (return-from c1load-time-value (c1constant-value destination
                                                            (cmp-eval form) :always t)))
          ((typep form '(or list symbol))
	   (setf loc (data-empty-loc))
	   (setf *load-time-values* (nconc *load-time-values*
                                           (c1translate loc form))))
	  (t
	   (setf loc (add-object (cmp-eval form)))))
    (c1set-loc destination loc)))

(defun parse-cvspecs (x &aux (cvspecs nil))
  (dolist (cvs x (nreverse cvspecs))
    (cond ((symbolp cvs)
           (push (list :OBJECT (string-downcase (symbol-name cvs))) cvspecs))
          ((stringp cvs) (push (list :OBJECT cvs) cvspecs))
          ((and (consp cvs)
                (member (car cvs) '(OBJECT CHAR INT FLOAT DOUBLE)))
           (dolist (name (cdr cvs))
             (push (list (car cvs)
                         (cond ((symbolp name)
                                (string-downcase (symbol-name name)))
                               ((stringp name) name)
                               (t (cmperr "The C variable name ~s is illegal."
                                          name))))
                   cvspecs)))
          (t (cmperr "The C variable specification ~s is illegal." cvs)))))

(defun locative-type-from-var-kind (kind)
  (cdr (assoc kind
              '((:object . "_ecl_object_loc")
                (:fixnum . "_ecl_fixnum_loc")
                (:char . "_ecl_base_char_loc")
                (:float . "_ecl_float_loc")
                (:double . "_ecl_double_loc")
                ((special global closure replaced discarded lexical) . NIL)))))

(defun t3local-fun (fun)
  (print-emitting fun)
  (let* ((*current-function* fun)
         (*lcl* (fun-last-lcl fun))
         (*last-label* (fun-last-label fun))
	 (*lex* 0)
         (*max-lex* 0)
	 (*env* (fun-env fun))		; continue growing env
	 (*max-env* *env*)
         (*env-lvl* 0)
	 (*level* (if (eq (fun-closure fun) 'LEXICAL)
                      (fun-level fun)
                      0))
         (*volatile* (if (fun-volatile-p fun) "volatile " ""))
         (*permanent-data* t))
    (c2translate (fun-lambda fun))))

;;; ----------------------------------------------------------------------
;;; Optimizer for FSET. Removes the need for a special handling of DEFUN as a
;;; toplevel form and also allows optimizing calls to DEFUN or DEFMACRO which
;;; are not toplevel, but which create no closures.
;;;
;;; The idea is as follows: when the function or macro to be defined is not a
;;; closure, we can use the auxiliary C functions c_def_c_*() instead of
;;; creating a closure and invoking si_fset(). However until the C2 phase of
;;; the compiler we do not know whether a function is a closure, hence the need
;;; for a c2fset.
;;;
(defun c1fset (destination args)
  (destructuring-bind (fname def &optional (macro nil) (pprint nil))
      args
    (let* ((unoptimized (c1call-global destination 'SI:FSET
                                       (list fname def macro pprint)))
           (fun-form (c1translate 'VALUE0 def)))
      (if (and (eq destination 'TRASH)
               (= (length fun-form) 1)
               (setf fun-form (first fun-form))
               (eq (c1form-name fun-form) 'FUNCTION)
	       (not (eq (c1form-arg 0 fun-form) 'GLOBAL)))
	  (let ((fun-object (c1form-arg 2 fun-form)))
            (setf (fun-child-funs *current-function*)
                  (delete fun-object (fun-child-funs *current-function*)))
	    (cond ((fun-no-entry fun-object)
                   (when macro
                     (cmperr "Declaration C-LOCAL used in macro ~a" (fun-name fun)))
                   (make-c1form* 'SI:FSET :args fun-object nil nil nil nil))
                  ((and (typep macro 'boolean)
                        (typep pprint '(or integer null))
                        (consp fname)
                        (eq (first fname) 'quote))
                   (make-c1form* 'SI:FSET
                                 :args
                                 fun-object ;; Function object
                                 (add-object (second fname) :permanent t :duplicate t)
                                 macro
                                 pprint
                                 unoptimized))))
          unoptimized))))

(defun c2fset (fun fname macro pprint c1forms)
  (when (fun-no-entry fun)
    (wt-nl "(void)0; /* No entry created for "
	   (format nil "~A" (fun-name fun))
	   " */")
    ;; FIXME! Look at c2function!
    (new-local fun)
    (return-from c2fset))
  (when (fun-closure fun)
    (return-from c2fset (c2call-global destination 'SI:FSET c1forms)))
  (let ((*inline-blocks* 0)
	(loc (data-empty-loc)))
    (push (list loc fname fun) *global-cfuns-array*)
    ;; FIXME! Look at c2function!
    (new-local fun)
    (wt-nl (if macro "ecl_cmp_defmacro(" "ecl_cmp_defun(")
	   loc ");")
    (close-inline-blocks)))

(defun output-cfuns (stream)
  (let ((n-cfuns (length *global-cfuns-array*)))
    (wt-nl-h "/*")
    (wt-nl-h " * Exported Lisp functions")
    (wt-nl-h " */")
    (wt-nl-h "#define compiler_cfuns_size " n-cfuns)
    (if (zerop n-cfuns)
        (wt-nl-h "#define compiler_cfuns NULL")
        (progn
          (format stream "~%static const struct ecl_cfun compiler_cfuns[] = {~
~%~t/*t,m,narg,padding,name,block,entry*/");
          (loop for (loc fname-loc fun) in (nreverse *global-cfuns-array*)
                do (let* ((cfun (fun-cfun fun))
                          (minarg (fun-minarg fun))
                          (maxarg (fun-maxarg fun))
                          (narg (if (= minarg maxarg) maxarg nil)))
                     (format stream "~%{0,0,~D,0,MAKE_FIXNUM(~D),MAKE_FIXNUM(~D),(cl_objectfn)~A,Cnil,MAKE_FIXNUM(~D)},"
                             (or narg -1) (second loc) (second fname-loc)
                             cfun (fun-file-position fun))))
          (format stream "~%};")))))

;;; ----------------------------------------------------------------------

;;; Pass 1 top-levels.

(put-sysprop 'COMPILER-LET 'T1 'c1compiler-let)
(put-sysprop 'EVAL-WHEN 'T1 'c1eval-when)
(put-sysprop 'PROGN 'T1 'c1progn)
(put-sysprop 'MACROLET 'T1 'c1macrolet)
(put-sysprop 'LOCALLY 'T1 'c1locally)
(put-sysprop 'SYMBOL-MACROLET 'T1 'c1symbol-macrolet)
