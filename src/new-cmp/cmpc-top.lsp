;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;;  CMPC-TOP -- Dump all lisp forms and data
;;;;

(in-package "C-BACKEND")

(defparameter +init-function-name+ (gensym "ENTRY-POINT"))

(defun ctop-write (name h-pathname data-pathname
                   &key shared-data
                   &aux def top-output-string
                   (*volatile* "volatile "))

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
    (c-backend-passes)

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

  (output-cfuns *compiler-output2*)

  (setq *compiler-phase* 't3)

  ;;; Callbacks
  (when *callbacks*
    (wt-nl-h "#include <ecl/internal.h>")
    (dolist (x *callbacks*)
      (apply #'t3-defcallback x)))

  (wt-nl top-output-string))

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

(defun emit-local-funs (fun)
  (loop with *compile-time-too* = nil
     with *compile-toplevel* = nil
     with emitted-local-funs = (make-hash-table :test #'eql)
     with pending = (fun-child-funs fun)
     while pending
     do (let ((f (pop pending)))
          (when (gethash f emitted-local-funs)
            (error "Doubly emitted function ~A" f))
          (t3local-fun f)
          (setf (gethash f emitted-local-funs) t
                pending (append (fun-child-funs f) pending)))))

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

