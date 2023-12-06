;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2021, Daniel Kochmański
;;;;
;;;;    See file 'LICENSE' for the copyright details.
;;;;

(in-package #:compiler)

(defun c2let-replaceable-var-ref-p (var form rest-forms)
  (when (and (eq (c1form-name form) 'VARIABLE)
             (null (var-set-nodes var))
             (local var))
    (let ((var1 (c1form-arg 0 form)))
      (declare (type var var1))
      ;; FIXME We should be able to replace variable even if they are referenced
      ;; across functions.  We just need to keep track of their uses.
      (when (and (local var1)
                 (eq (unboxed var) (unboxed var1))
                 (not (var-changed-in-form-list var1 rest-forms)))
        (cmpdebug "Replacing variable ~a by its value" (var-name var))
        (nsubst-var var form)
        t))))

(defun locative-type-from-var-kind (kind)
  (cdr (assoc kind
              '((:object . "_ecl_object_loc")
                (:fixnum . "_ecl_fixnum_loc")
                (:char . "_ecl_base_char_loc")
                (:float . "_ecl_float_loc")
                (:double . "_ecl_double_loc")
                (:long-double . "_ecl_long_double_loc")
                #+complex-float (:csfloat . "_ecl_csfloat_loc")
                #+complex-float (:cdfloat . "_ecl_cdfloat_loc")
                #+complex-float (:clfloat . "_ecl_clfloat_loc")
                #+sse2 (:int-sse-pack . "_ecl_int_sse_pack_loc")
                #+sse2 (:float-sse-pack . "_ecl_float_sse_pack_loc")
                #+sse2 (:double-sse-pack . "_ecl_double_sse_pack_loc")
                ((special global closure lexical) . NIL)))))

(defun build-debug-lexical-env (var-locations &optional first)
  #-:msvc ;; FIXME! Problem with initialization of statically defined vectors
  (let* ((filtered-locations '())
         (filtered-codes '()))
    ;; Filter out variables that we know how to store in the debug information
    ;; table. This excludes among other things closures and special variables.
    (loop for var in var-locations
          for name = (let ((*package* (find-package "KEYWORD")))
                       (format nil "\"~S\"" (var-name var)))
          for code = (locative-type-from-var-kind (var-kind var))
          for loc = (var-loc var)
          when (and code (consp loc) (eq (first loc) 'LCL))
            do (progn
                 (push (cons name code) filtered-codes)
                 (push loc filtered-locations)))
    ;; Generate two tables, a static one with information about the variables,
    ;; including name and type, and dynamic one, which is a vector of pointer to
    ;; the variables.
    (when filtered-codes
      (setf *ihs-used-p* t)
      (wt-nl "static const struct ecl_var_debug_info _ecl_descriptors[]={")
      (loop for (name . code) in filtered-codes
            for i from 0
            do (wt-nl (if (zerop i) "{" ",{") name "," code "}"))
      (wt "};")
      (wt-nl "const cl_index _ecl_debug_info_raw[]={")
      (wt-nl (if first "(cl_index)(ECL_NIL)," "(cl_index)(_ecl_debug_env),")
             "(cl_index)(_ecl_descriptors)")
      (loop for var-loc in filtered-locations
            do (wt ",(cl_index)(&" var-loc ")"))
      (wt "};")
      (wt-nl "ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,"
             (+ 2 (length filtered-locations))
             ",,);")
      (unless first
        (wt-nl "ihs.lex_env = _ecl_debug_env;")))
    filtered-codes))

(defun pop-debug-lexical-env ()
  (wt-nl "ihs.lex_env = _ecl_debug_env;"))

(defun c2let* (c1form vars forms body
               &aux
               (*volatile* (c1form-volatile* c1form))
               (*unwind-exit* *unwind-exit*)
               (*env* *env*)
               (*env-lvl* *env-lvl*)
               (*inline-blocks* 0))
  ;; Replace read-only variables when it is worth doing it.
  (loop for var in vars
     for rest-forms on (append forms (list body))
     for form = (first rest-forms)
     unless (c2let-replaceable-var-ref-p var form rest-forms)
     collect var into used-vars and
     collect form into used-forms
     finally (setf vars used-vars forms used-forms))

  ;; Emit C definitions of local variables
  (loop for var in vars
     for kind = (local var)
     do (when kind
          (maybe-open-inline-block)
          (bind (next-lcl (var-name var)) var)
          (wt-nl *volatile* (host-type->c-name kind) " " var ";")))

  ;; Create closure bindings for closed-over variables
  (when (some #'var-ref-ccb vars)
    (maybe-open-inline-block)
    (let ((env-lvl *env-lvl*))
      (wt-nl *volatile* "cl_object env" (incf *env-lvl*) " = env" env-lvl ";")))

  ;; Assign values
  (loop for form in forms
     for var in vars
     do (case (var-kind var)
          ((LEXICAL CLOSURE SPECIAL GLOBAL)
           (case (c1form-name form)
             (LOCATION (bind (c1form-arg 0 form) var))
             (VARIABLE (bind (c1form-arg 0 form) var))
             (t (bind-init form var))))
          (t ; local var
           (let ((*destination* var)) ; nil (ccb)
             (c2expr* form)))))
          
  ;; Optionally register the variables with the IHS frame for debugging
  (if (policy-debug-variable-bindings)
      (let ((*unwind-exit* *unwind-exit*))
        (with-lexical-scope ()
          (ext:if-let ((env (build-debug-lexical-env vars)))
            (progn
              (push 'IHS-ENV *unwind-exit*)
              (c2expr body)
              (pop-debug-lexical-env))
            (c2expr body))))
      (c2expr body))
  (close-inline-blocks))

(defun c2multiple-value-bind (c1form vars init-form body)
  (declare (ignore c1form))
  (let* ((*unwind-exit* *unwind-exit*)
         (*env-lvl* *env-lvl*)
         (*env* *env*)
         (*lcl* *lcl*)
         (labels nil)
         (env-grows nil)
         (*inline-blocks* 0)
         min-values max-values)
    ;; 1) Retrieve the number of output values.
    (multiple-value-setq (min-values max-values)
      (c1form-values-number init-form))
    ;; 2) For all variables which are not special and do not belong to a
    ;;    closure, make a local C variable.
    (dolist (var vars)
      (declare (type var var))
      (let ((kind (local var)))
        (if kind
            (when (useful-var-p var)
              (maybe-open-inline-block)
              (bind (next-lcl) var)
              (wt-nl (host-type->c-name kind) " " *volatile* var ";")
              (wt-comment (var-name var)))
            (unless env-grows (setq env-grows (var-ref-ccb var))))))
    ;; 3) If there are closure variables, set up an environment.
    (when (setq env-grows (env-grows env-grows))
      (let ((env-lvl *env-lvl*))
        (maybe-open-inline-block)
        (wt-nl "volatile cl_object env" (incf *env-lvl*)
               " = env" env-lvl ";")))
    ;; 4) Assign the values to the variables, compiling the form and binding the
    ;;    variables in the process.
    (do-m-v-setq vars init-form t)
    ;; 5) Compile the body. If there are bindings of special variables, these
    ;;    bindings are undone here.
    (c2expr body)
    ;; 6) Close the C expression.
    (close-inline-blocks)))

(defun c2location (c1form loc)
  (unwind-exit (precise-loc-type loc (c1form-primary-type c1form))))

;;; When LOC is not NIL, then the variable is a constant.
(defun c2variable (c1form var loc)
  (unwind-exit (precise-loc-type
                (if (and loc (not (numberp (vv-location loc))))
                    loc
                    (or (try-const-c-inliner var) var))
                (c1form-primary-type c1form))))

(defun c2setq (c1form vref form)
  (declare (ignore c1form))
  ;; First comes the assignement
  (let ((*destination* vref))
    (c2expr* form))
  ;; Then the returned value
  (if (eq (c1form-name form) 'LOCATION)
      (c2location form (c1form-arg 0 form))
      (unwind-exit vref)))

(defun c2progv (c1form symbols values body)
  (declare (ignore c1form))
  (let* ((*lcl* *lcl*)
         (lcl (next-lcl))
         (sym-loc (make-lcl-var))
         (val-loc (make-lcl-var)))
    (with-lexical-scope ()
      (wt-nl "cl_object " sym-loc ", " val-loc "; cl_index " lcl ";")
      (let ((*destination* sym-loc)) (c2expr* symbols))
      (let ((*destination* val-loc)) (c2expr* values))
      (let ((*unwind-exit* (cons lcl *unwind-exit*)))
        (wt-nl lcl " = ecl_progv(cl_env_copy, " sym-loc ", " val-loc ");")
        (c2expr body)))))

(defun c2psetq (c1form vrefs forms
                &aux (*lcl* *lcl*) (saves nil) (braces *opened-c-braces*))
  (declare (ignore c1form))
  ;; similar to inline-args
  (do ((vrefs vrefs (cdr vrefs))
       (forms forms (cdr forms))
       (var) (form))
      ((null vrefs))
    (setq var (first vrefs)
          form (car forms))
    (if (or (var-changed-in-form-list var (rest forms))
            (var-referenced-in-form-list var (rest forms)))
        (case (c1form-name form)
          (LOCATION (push (cons var (c1form-arg 0 form)) saves))
          (otherwise
            (if (local var)
                (let* ((host-type (var-host-type var))
                       (host-type-c-name (host-type->c-name host-type))
                       (temp (make-lcl-var :host-type host-type)))
                  (wt-nl-open-brace)
                  (wt-nl host-type-c-name " " *volatile* temp ";")
                  (let ((*destination* temp)) (c2expr* form))
                  (push (cons var temp) saves))
                (let ((*destination* (make-temp-var)))
                  (c2expr* form)
                  (push (cons var *destination*) saves)))))
        (let ((*destination* var))
          (c2expr* form))))
  (dolist (save saves)
    (set-var (cdr save) (car save)))
  (wt-nl-close-many-braces braces)
  (unwind-exit *vv-nil*))

;;; bind must be called for each variable in a lambda or let, once the value
;;; to be bound has been placed in loc.
;;; bind takes care of setting var-loc.

(defun bind (loc var)
  ;; loc can be either (LCL n), 'VA-ARGS, (KEYVARS n), (CAR n),
  ;; a constant, or (VAR var) from a let binding. ; ccb
  (declare (type var var))
  (case (var-kind var)
    (CLOSURE
     (let ((var-loc (var-loc var)))
       (unless (typep var-loc 'fixnum)
         ;; first binding: assign location
         (setq var-loc (next-env))
         (setf (var-loc var) var-loc))
       (when (zerop var-loc) (wt-nl "env" *env-lvl* " = ECL_NIL;"))
       (wt-nl "CLV" var-loc " = env" *env-lvl* " = CONS(")
       (wt-coerce-loc :object loc)
       (wt ",env" *env-lvl* ");")
       (wt-comment (var-name var))))
    (LEXICAL
     (let ((var-loc (var-loc var)))
       (unless (consp var-loc)
         ;; first binding: assign location
         (setq var-loc (next-lex))
         (setf (var-loc var) var-loc))
       (wt-nl) (wt-lex var-loc) (wt " = ")
       (wt-coerce-loc :object loc)
       (wt ";"))
     (wt-comment (var-name var)))
    ((SPECIAL GLOBAL)
     (bds-bind loc var))
    (t
     (cond ((not (eq (var-loc var) 'OBJECT))
            ;; already has location (e.g. optional in lambda list)
            ;; check they are not the same
            (unless (equal (var-loc var) loc)
              (wt-nl var " = ")
              (wt-coerce-loc (var-host-type var) loc)
              (wt ";")))
           ((and (consp loc) (eql (car loc) 'LCL))
            ;; set location for lambda list requireds
            (setf (var-loc var) loc))
           (t
            (baboon :format-control "bind: unexpected var-kind (~s) and loc (~s)."
                    :format-arguments (list (var-kind var) loc)))))))

;;; Used by let*, defmacro and lambda's &aux, &optional, &rest, &keyword
(defun bind-init (form var)
  (let ((kind (var-kind var)))
    (if (member kind '(CLOSURE LEXICAL SPECIAL GLOBAL))
        ;; Binding these variables is complicated and involves lexical
        ;; environments, global environments, etc. If we use `(BIND var)
        ;; as destination, BIND might receive the wrong environment.
        (with-inline-blocks ()
          (let ((locs (coerce-locs (inline-args (list form)))))
            (bind (first locs) var)
            ;; Notice that we do not need to update *UNWIND-EXIT* because BIND
            ;; does it for us.
            ))
        ;; The simple case of a variable which is local to a function.
        (let ((*destination* `(BIND ,var)))
          (c2expr* form)))))

(defun bds-bind (loc var)
  ;; Optimize the case (let ((*special-var* *special-var*)) ...)
  (cond ((and (var-p loc)
              (member (var-kind loc) '(global special))
              (eq (var-name loc) (var-name var)))
         (wt-nl "ecl_bds_push(cl_env_copy," (var-loc var) ");"))
        (t
         (wt-nl "ecl_bds_bind(cl_env_copy," (var-loc var) ",")
         (wt-coerce-loc :object loc)
         (wt ");")))
  (push 'BDS-BIND *unwind-exit*)
  (wt-comment (var-name var)))

(defun bind-or-set (loc v use-bind)
  (declare (si::c-local))
  (cond ((not use-bind)
         (set-var loc v))
        ((or (plusp (var-ref v))
             (member (var-kind v) '(SPECIAL GLOBAL)))
         (bind loc v))))

(defun values-loc-or-value0 (i)
  (declare (si::c-local))
  (if (plusp i)
      (list 'VALUE i)
      'VALUE0))

(defun do-m-v-setq (vars form use-bind)
  ;; This routine moves values from the multiple-value stack into the
  ;; variables VARS. The amount of values is not known (or at least we only
  ;; know that there is some number between MIN-VALUES and MAX-VALUES).  If
  ;; the values are to be created with BIND, then USED-BIND=T.  The output of
  ;; this routine is a location containing the first value (typically, the
  ;; name of the first variable).
  (declare (si::c-local))
  (when (= (length vars) 1)
    (let ((*destination* (first vars)))
      (c2expr* form)
      (return-from do-m-v-setq *destination*)))

  ;; Store the values in the values stack + value0. Try guessing how
  ;; many they are.
  (multiple-value-bind (min-values max-values)
      (c1form-values-number form)
    (declare (ignore max-values))

    ;; We save the values in the value stack + value0
    (let ((*destination* 'LEAVE))
      (c2expr* form))

    ;; At least we always have NIL value0
    (setf min-values (max 1 min-values))

    (let* ((*lcl* *lcl*)
           (useful-extra-vars (some #'useful-var-p (nthcdr min-values vars)))
           (nr (make-lcl-var :type :int)))
      (with-lexical-scope ()
        (when useful-extra-vars
          ;; Make a copy of env->nvalues before assigning to any variables
          (wt-nl "const int " nr " = cl_env_copy->nvalues;"))

        ;; We know that at least MIN-VALUES variables will get a value
        (dotimes (i min-values)
          (when vars
            (let ((v (pop vars))
                  (loc (values-loc-or-value0 i)))
              (bind-or-set loc v use-bind))))

        ;; Assign to other variables only when the form returns enough values
        (when useful-extra-vars
          (let ((tmp (make-lcl-var)))
            (wt-nl "cl_object " tmp ";")
            (loop for v in vars
                  for i from min-values
                  for loc = (values-loc-or-value0 i)
                  do (when (useful-var-p v)
                       (wt-nl tmp " = (" nr "<=" i ")? ECL_NIL : " loc ";")
                       (bind-or-set tmp v use-bind)))))))
    'VALUE0))

(defun c2multiple-value-setq (c1form vars form)
  (declare (ignore c1form))
  (unwind-exit (do-m-v-setq vars form nil)))

(defun wt-var (var)
  (declare (type var var))
  (let ((var-loc (var-loc var)))
    (case (var-kind var)
      (CLOSURE (wt-env var-loc))
      (LEXICAL (wt-lex var-loc))
      ((SPECIAL GLOBAL)
       (if (safe-compile)
           (wt "ecl_cmp_symbol_value(cl_env_copy," var-loc ")")
           (wt "ECL_SYM_VAL(cl_env_copy," var-loc ")")))
      (t (wt var-loc)))))

(defun set-var (loc var &aux (var-loc (var-loc var))) ;  ccb
  (unless (var-p var)
    (baboon :format-control "set-var: ~s is not a variable."
            :format-arguments (list var)))
  (case (var-kind var)
    (CLOSURE
     (wt-nl)(wt-env var-loc)(wt " = ")
     (wt-coerce-loc (var-host-type var) loc)
     (wt #\;))
    (LEXICAL
     (wt-nl)(wt-lex var-loc)(wt " = ")
     (wt-coerce-loc (var-host-type var) loc)
     (wt #\;))
    ((SPECIAL GLOBAL)
     (if (safe-compile)
         (wt-nl "ecl_cmp_setq(cl_env_copy," var-loc ",")
         (wt-nl "ECL_SETQ(cl_env_copy," var-loc ","))
     (wt-coerce-loc (var-host-type var) loc)
     (wt ");"))
    (t
     (wt-nl var-loc " = ")
     (wt-coerce-loc (var-host-type var) loc)
     (wt #\;))
    ))

(defun wt-lcl (lcl)
  (unless (numberp lcl)
    (baboon :format-control "wt-lcl: ~s NaN"
            :format-arguments (list lcl)))
  (wt "v" lcl))

(defun wt-lcl-loc (lcl &optional type name)
  (declare (ignore type))
  (unless (numberp lcl)
    (baboon :format-control "wt-lcl-loc: ~s NaN"
            :format-arguments (list lcl)))
  (wt "v" lcl name))

(defun wt-lex (lex)
  (if (consp lex)
      (wt "lex" (car lex) "[" (cdr lex) "]")
      (wt-lcl lex)))

(defun wt-temp (temp)
  (wt "T" temp))

;;; reference to variable of inner closure.
(defun wt-env (clv) (wt "ECL_CONS_CAR(CLV" clv ")"))
