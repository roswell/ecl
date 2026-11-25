;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2021, Daniel Kochmański
;;;;
;;;;    See file 'LICENSE' for the copyright details.
;;;;

(in-package #:compiler)

(defun c2locals (c1form funs body labels ;; labels is T when deriving from labels
                 &aux
                   (*env* *env*)
                   (*env-lvl* *env-lvl*))
  (declare (ignore c1form labels))
  (let ((vars (loop for fun in funs
                    for var = (fun-var fun)
                    when (useful-var-p var)
                      do (vlet var)
                      and collect var)))
    ;; Update local functions and register them for emitting.
    (map nil #'update-function-env funs)
    ;; Ensure variables where we will store functions.
    (push-instruction :let-vars vars)
    ;; Assign local functions to variables.
    (push-instruction :locals funs)
    (c2expr body)
    (push-instruction :let-exit)))

(defun update-function-env (fun)
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
  (register-function fun))

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
     c-compatible-variadic-signature
     &aux (requireds (first lambda-list))
       (optionals (second lambda-list))
       (rest (third lambda-list)) rest-loc
       (key-flag (fourth lambda-list))
       (keywords (fifth lambda-list))
       (allow-other-keys (sixth lambda-list))
       (nreq (length requireds))
       (nopt (/ (length optionals) 3))
       (nkey (/ (length keywords) 4))
       (varargs (or optionals rest key-flag allow-other-keys
                    (and c-compatible-variadic-signature requireds)))
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
                                 nconc (list var (c1nil) NIL))
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
               (wt (host-type->c-name (var-host-type var)) " " *volatile* lcl ";")
               lcl))
           (do-decl (var)
             (when (local-var-p var) ; no LCL needed for SPECIAL or LEX
               (setf (var-loc var) (wt-decl var)))))
    ;; Declare unboxed required arguments
    (loop for var in requireds
          when (or (unboxed-var-p var) c-compatible-variadic-signature)
            do (do-decl var))
    ;; dont create rest or varargs if not used
    (when (and rest (< (var-ref rest) 1)
               (not (eq (var-kind rest) 'SPECIAL)))
      (setq rest nil
            varargs (or varargs optionals key-flag allow-other-keys)))
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
             (cond ((and required-lcls (not c-compatible-variadic-signature))
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
                 (last-variable) ",narg,"
                 (if c-compatible-variadic-signature
                     0
                     nreq)
                 ");"))
      (when c-compatible-variadic-signature
        (setf required-lcls
              (loop repeat nreq
                    collect (if simple-varargs 'VA-ARG 'CL-VA-ARG))))))

  ;; Bind required arguments. Produces C statements for unboxed
  ;; variables, which is why it is done after all declarations.
  (mapc #'bind required-lcls requireds)

  (when fname-in-ihs-p
    (setf *ihs-used-p* t)
    (push 'IHS *unwind-exit*)
    (let ((fname-obj (get-object (or description fname) :test #'equal)))
      (if (policy-debug-variable-bindings)
          (push-instruction :build-debug-funenv (reverse requireds) fname-obj)
          (push-instruction :build-debug-funenv nil fname-obj))))

  ;; When binding optional and keyword parameters, we have duplicate calls to
  ;; BIND to assign default value when the argument is not present, i.e:
  ;;
  ;;   if(i>=narg) BIND
  ;;   else        BIND
  ;;
  ;; that may result in duplicate 'BDS-BIND entries in *UNWIND-EXIT* and in too
  ;; many calls bds_unwind1(). To avoid that we rebind *UNWIND-EXIT* in one
  ;; branch and discard erroneous entries.

  ;; Bind optional parameters as long as there remain arguments.
  (when optionals
    (let ((va-arg-loc (if simple-varargs 'VA-ARG 'CL-VA-ARG)))
      ;; counter for optionals
      (with-inline-blocks ()
        (open-inline-block)
        (wt-nl "int i = " nreq ";")
        (do ((opt optionals (cdddr opt))
             (type-check optional-type-check-forms (cdr type-check)))
            ((endp opt))
          (let ((var (first opt))
                (init (second opt))
                (flag (third opt)))
            (wt-nl "if (i >= narg) {")
            (let ((*opened-c-braces* (1+ *opened-c-braces*)))
              (c2bind-init var init)
              (when flag
                (bind *vv-nil* flag)))
            (wt-nl "} else {")
            (let ((*opened-c-braces* (1+ *opened-c-braces*))
                  (*unwind-exit* *unwind-exit*))
              (wt-nl "i++;")
              (bind va-arg-loc var)
              (when (car type-check)
                (c2expr* (car type-check)))
              (when flag
                (bind *vv-t* flag)))
            (wt-nl "}"))))))

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
           (if rest
               (wt ",(cl_object*)&" rest-loc)
               (wt ",NULL"))
           (if allow-other-keys
               (wt ",TRUE);")
               (wt ",FALSE);"))))
    (when rest
      (bind rest-loc rest)))

  (when varargs
    (wt-nl (if simple-varargs "va_end(args);" "ecl_va_end(args);")))

  ;;; Bind keywords.
  ;;; Keys are bound from a variable assigned while binding &rest parameters.
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
             (bind KEYVARS[i] var)
             (when (car type-check)
               (c2expr* (car type-check))))
            (t
             ;; with initform
             (setf (second KEYVARS[i]) (+ nkey i))
             (wt-nl "if (Null(") (wt-loc KEYVARS[i]) (wt ")) {")
             (let ((*unwind-exit* *unwind-exit*)
                   (*opened-c-braces* (1+ *opened-c-braces*)))
               (c2bind-init var init))
             (wt-nl "} else {")
             (let ((*opened-c-braces* (1+ *opened-c-braces*)))
               (setf (second KEYVARS[i]) i)
               (bind KEYVARS[i] var)
               (when (car type-check)
                 (c2expr* (car type-check))))
             (wt-nl "}")))
      (when flag
        (setf (second KEYVARS[i]) (+ nkey i))
        (bind KEYVARS[i] flag))))
  (when *tail-recursion-info*
    (setf *tail-recursion-mark* (next-label t))
    (push *tail-recursion-mark* *unwind-exit*)
    (wt-label *tail-recursion-mark*))
  ;;; Now the parameters are ready, after all!
  (c2expr body)
  (close-inline-blocks))
