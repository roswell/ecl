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

(defun c2locals (c1form funs body labels ;; labels is T when deriving from labels
                 &aux
                 (*env* *env*)
                 (*inline-blocks* 0)
                 (*env-lvl* *env-lvl*))
  (declare (ignore c1form labels))
  ;; create location for each function which is returned,
  ;; either in lexical:
  (loop with env-grows = nil
     with closed-vars = '()
     for fun in funs
     for var = (fun-var fun)
     when (plusp (var-ref var))
     do (case (var-kind var)
          ((lexical closure)
           (push var closed-vars)
           (unless env-grows
             (setq env-grows (var-ref-ccb var))))
          (otherwise
           (maybe-open-inline-block)
           (bind (next-lcl) var)
           (wt-nl "cl_object " *volatile* var ";")))
     finally
     ;; if we have closed variables
       (when (env-grows env-grows)
         (maybe-open-inline-block)
         (let ((env-lvl *env-lvl*))
           (wt "cl_object " *volatile* "env" (incf *env-lvl*) " = env" env-lvl ";")))
     ;; bind closed locations because of possible circularities
       (loop for var in closed-vars
          do (bind *vv-nil* var)))
  ;; create the functions:
  (mapc #'new-local funs)
  ;; - then assign to it
  (loop for fun in funs
     for var = (fun-var fun)
     when (plusp (var-ref var))
     do (set-var (list 'MAKE-CCLOSURE fun) var))
  (c2expr body)
  (close-inline-blocks))

;;; Mechanism for sharing code.
(defun new-local (fun)
  (declare (type fun fun))
  (case (fun-closure fun)
    (CLOSURE
     (setf (fun-level fun) 0
           (fun-env fun) *env*))
    (LEXICAL
     ;; Only increase the lexical level if there have been some
     ;; new variables created. This way, the same lexical environment
     ;; can be propagated through nested FLET/LABELS.
     (setf (fun-level fun) (if (plusp *lex*) (1+ *level*) *level*)
           (fun-env fun) 0))
    (otherwise
     (setf (fun-level fun) 0
           (fun-env fun) 0)))
  (push fun *local-funs*))

#| Steps:
 1. defun creates declarations for requireds + va_alist
 2. c2lambda-expr adds declarations for:
        unboxed requireds
        lexical optionals (+ supplied-p), rest, keywords (+ supplied-p)
    Lexical optionals and keywords can be unboxed if:
        a. there is more then one reference in the body
        b. they are not referenced in closures
 3. binding is performed for:
        special or unboxed requireds
        optionals, rest, keywords
 4. the function name is optionally pushed onto the IHS when
    the caller asks for it.
|#

(defun c2lambda-expr
    (lambda-list body cfun fname description use-narg required-lcls closure-type
                 optional-type-check-forms keyword-type-check-forms
                 &aux (requireds (first lambda-list))
                 (optionals (second lambda-list))
                 (rest (third lambda-list)) rest-loc
                 (key-flag (fourth lambda-list))
                 (keywords (fifth lambda-list))
                 (allow-other-keys (sixth lambda-list))
                 (nreq (length requireds))
                 (nopt (/ (length optionals) 3))
                 (nkey (/ (length keywords) 4))
                 (varargs (or optionals rest key-flag allow-other-keys))
                 (fname-in-ihs-p (or (policy-debug-variable-bindings)
                                     (and (policy-debug-ihs-frame)
                                          (or description fname))))
                 simple-varargs
                 (*permanent-data* t)
                 (*unwind-exit* *unwind-exit*)
                 (*env* *env*)
                 (*inline-blocks* 0)
                 (last-arg))
  (declare (fixnum nreq nkey))

  (if (and fname ;; named function
           ;; no required appears in closure,
           (dolist (var requireds t)
             (declare (type var var))
             (when (var-ref-ccb var) (return nil)))
           (null optionals)  ;; no optionals,
           (null rest)       ;; no rest parameter, and
           (null key-flag))  ;; no keywords.
    (setf *tail-recursion-info* (cons *tail-recursion-info* requireds))
    (setf *tail-recursion-info* nil))

  ;; check number of arguments
  (wt-maybe-check-num-arguments use-narg
                                nreq
                                (if (or rest key-flag allow-other-keys)
                                    nil
                                    (+ nreq nopt))
                                fname)

  ;; If the number of required arguments exceeds the number of variables we
  ;; want to pass on the C stack, we pass some of the arguments to the list
  ;; of optionals, which will eventually get passed in the lisp stack.
  (when (> nreq si::c-arguments-limit)
    (setf nopt (+ nopt (- nreq si::c-arguments-limit))
          nreq si::c-arguments-limit)
    (setf optionals (nconc (loop for var in (subseq requireds si::c-arguments-limit)
                              nconc (list var *c1nil* NIL))
                           optionals)
          requireds (subseq requireds 0 si::c-arguments-limit)
          varargs t))

  ;; For each variable, set its var-loc.
  ;; For optional and keyword parameters, and lexical variables which
  ;; can be unboxed, this will be a new LCL.
  ;; The bind step later will assign to such variable.
  (labels ((wt-decl (var)
             (let ((lcl (next-lcl (var-name var))))
               (wt-nl)
               (wt (rep-type->c-name (var-rep-type var)) " " *volatile* lcl ";")
               lcl))
           (do-decl (var)
             (when (local var) ; no LCL needed for SPECIAL or LEX
               (setf (var-loc var) (wt-decl var)))))
    ;; Declare unboxed required arguments
    (loop for var in requireds
       when (unboxed var)
       do (setf (var-loc var) (wt-decl var)))
    ;; dont create rest or varargs if not used
    (when (and rest (< (var-ref rest) 1)
               (not (eq (var-kind rest) 'SPECIAL)))
      (setq rest nil
            varargs (or optionals key-flag allow-other-keys)))
    ;; Declare &optional variables
    (do ((opt optionals (cdddr opt)))
        ((endp opt))
      (do-decl (first opt))
      (when (third opt) (do-decl (third opt))))
    ;; Declare &rest variables
    (when rest (setq rest-loc (wt-decl rest)))
    ;; Declare &key variables
    (do ((key keywords (cddddr key)))
        ((endp key))
      (do-decl (second key))
      (when (fourth key) (do-decl (fourth key)))))

  ;; Declare and assign the variable arguments pointer
  (when varargs
    (flet ((last-variable ()
             (cond (required-lcls
                    (first (last required-lcls)))
                   ((eq closure-type 'LEXICAL)
                    (format nil "lex~D" (1- *level*)))
                   (t "narg"))))
      (if (setq simple-varargs (and (not (or rest key-flag allow-other-keys))
                                    (<= (+ nreq nopt) si::c-arguments-limit)))
          (wt-nl "va_list args; va_start(args,"
                 (last-variable)
                 ");")
          (wt-nl "ecl_va_list args; ecl_va_start(args,"
                 (last-variable) ",narg," nreq ");"))))

  ;; Bind required argumens. Produces C statements for unboxed variables,
  ;; which is why it is done after all declarations.
  (mapc #'bind required-lcls requireds)

  (when fname-in-ihs-p
    (let ((fname (get-object (or description fname))))
      (open-inline-block)
      (setf *ihs-used-p* t)
      (push 'IHS *unwind-exit*)
      (when (policy-debug-variable-bindings)
        (build-debug-lexical-env (reverse requireds) t))
      (wt-nl "ecl_ihs_push(cl_env_copy,&ihs," fname ",_ecl_debug_env);")))

  ;; Bind optional parameters as long as there remain arguments.
  (when optionals
    ;; When binding optional values, we use two calls to BIND. This means
    ;; 'BDS-BIND is pushed twice on *unwind-exit*, which results in two calls
    ;; to bds_unwind1(), which is wrong. A simple fix is to save *unwind-exit*
    ;; which is what we do here.
    (let ((va-arg-loc (if simple-varargs 'VA-ARG 'CL-VA-ARG)))
      ;; counter for optionals
      (wt-nl-open-brace)
      (wt-nl "int i = " nreq ";")
      (do ((opt optionals (cdddr opt))
           (type-check optional-type-check-forms (cdr type-check)))
          ((endp opt))
        (wt-nl "if (i >= narg) {")
        (let ((*opened-c-braces* (1+ *opened-c-braces*)))
          (bind-init (second opt) (first opt))
          (when (third opt)
            (bind *vv-nil* (third opt))))
        (wt-nl "} else {")
        (let ((*opened-c-braces* (1+ *opened-c-braces*))
              (*unwind-exit* *unwind-exit*))
          (wt-nl "i++;")
          (bind va-arg-loc (first opt))
          (if (car type-check)
              (c2expr* (car type-check)))
          (when (third opt)
            (bind *vv-t* (third opt))))
        (wt-nl "}"))
      (wt-nl-close-brace)))

  (when (or rest key-flag allow-other-keys)
    (cond ((not (or key-flag allow-other-keys))
           (wt-nl rest-loc " = cl_grab_rest_args(args);"))
          (t
           (cond (keywords
                  (wt-nl-open-brace) ;; Brace [1]
                  (wt-nl "cl_object keyvars[" (* 2 nkey) "];")
                  (wt-nl "cl_parse_key(args," nkey "," cfun "keys,keyvars"))
                 (t
                  (wt-nl "cl_parse_key(args,0,NULL,NULL")))
           ;; This explicit coercion is required to remove the "volatile"
           ;; declaration on some variables.
           (if rest (wt ",(cl_object*)&" rest-loc) (wt ",NULL"))
           (wt (if allow-other-keys ",TRUE);" ",FALSE);"))))
    (when rest
      (bind rest-loc rest)))

  (when varargs
    (wt-nl (if simple-varargs "va_end(args);" "ecl_va_end(args);")))

  ;;; Bind keywords.
  (do ((kwd keywords (cddddr kwd))
       (all-kwd nil)
       (KEYVARS[i] `(KEYVARS 0))
       (i 0 (1+ i))
       (type-check keyword-type-check-forms (cdr type-check)))
      ((endp kwd)
       (when all-kwd
         (wt-nl-h "#define " cfun "keys (&" (add-keywords (nreverse all-kwd)) ")")
         (wt-nl-close-brace))) ;; Matches [1]
    (declare (fixnum i))
    (push (first kwd) all-kwd)
    (let ((key (first kwd))
          (var (second kwd))
          (init (third kwd))
          (flag (fourth kwd)))
      (cond ((and (eq (c1form-name init) 'LOCATION)
                  (eq (c1form-arg 0 init) *vv-nil*))
             ;; no initform
             ;; ECL_NIL has been set in keyvars if keyword parameter is not supplied.
             (setf (second KEYVARS[i]) i)
             (bind KEYVARS[i] var))
            (t
             ;; with initform
             (setf (second KEYVARS[i]) (+ nkey i))
             (wt-nl "if (Null(") (wt-loc KEYVARS[i]) (wt ")) {")
             (let ((*unwind-exit* *unwind-exit*)
                   (*opened-c-braces* (1+ *opened-c-braces*)))
               (bind-init init var))
             (wt-nl "} else {")
             (let ((*opened-c-braces* (1+ *opened-c-braces*)))
               (setf (second KEYVARS[i]) i)
               (bind KEYVARS[i] var)
               (if (car type-check)
                   (c2expr* (car type-check))))
             (wt-nl "}")))
      (when flag
        (setf (second KEYVARS[i]) (+ nkey i))
        (bind KEYVARS[i] flag))))

  (when *tail-recursion-info*
    (push 'TAIL-RECURSION-MARK *unwind-exit*)
    (wt-nl1 "TTL:"))

  ;;; Now the parameters are ready, after all!
  (c2expr body)

  (close-inline-blocks))

(defun wt-maybe-check-num-arguments (use-narg minarg maxarg fname)
 (when (and (policy-check-nargs) use-narg)
   (flet ((wrong-num-arguments ()
            (if fname
                (wt " FEwrong_num_arguments(" (get-object fname) ");")
                (wt " FEwrong_num_arguments_anonym();"))))
     (if (and maxarg (= minarg maxarg))
         (progn (wt-nl "if (ecl_unlikely(narg!=" minarg "))")
                (wrong-num-arguments))
         (progn
           (when (plusp minarg)
             (wt-nl "if (ecl_unlikely(narg<" minarg "))")
             (wrong-num-arguments))
           (when maxarg
             (wt-nl "if (ecl_unlikely(narg>" maxarg "))")
             (wrong-num-arguments)))))
   (open-inline-block)))
