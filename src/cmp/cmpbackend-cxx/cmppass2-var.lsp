;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2021, Daniel Kochmański
;;;;
;;;;    See file 'LICENSE' for the copyright details.
;;;;

(in-package "COMPILER")

(defun let-replaceable-var-ref-p (var form rest-forms)
  (declare (si::c-local))
  (when (and (eq (c1form-name form) 'VARIABLE)
             (null (var-set-nodes var))
             (local-var-p var))
    (let ((var1 (c1form-arg 0 form)))
      (declare (type var var1))
      ;; FIXME We should be able to replace variable even if they are referenced
      ;; across functions.  We just need to keep track of their uses.
      (when (and (local-var-p var1)
                 (eq (unboxed-var-p var) (unboxed-var-p var1))
                 (not (var-changed-in-form-list var1 rest-forms)))
        (cmpdebug "Replacing variable ~a by its value" (var-name var))
        (nsubst-var var form)
        t))))

(defun let-build-debug-lexenv-p (var-locations)
  (declare (si::c-local))
  (and (policy-debug-variable-bindings)
       (some (lambda (var)
               (let ((loc (var-loc var)))
                 (and (useful-var-p var)
                      (locative-type-from-var-kind (var-kind var))
                      (consp loc)
                      (eq (car loc) 'LCL))))
             var-locations)))

;;; Used by let*, defmacro and lambda's &aux, &optional, &rest, &keyword
(defun bind-init (form var)
  (let ((kind (var-kind var)))
    (if (member kind '(CLOSURE LEXICAL SPECIAL GLOBAL))
        ;; Binding these variables is complicated and involves lexical
        ;; environments, global environments, etc. If we use `(BIND var)
        ;; as destination, BIND might receive the wrong environment.
        (let ((*destination* (make-temp-var)))
          (c2expr* form)
          (bind *destination* var))
        ;; The simple case of a variable which is local to a function.
        (let ((*destination* `(BIND ,var)))
          (c2expr* form)))))

(defun c2let* (c1form vars forms body
               &aux
               (*volatile* (c1form-volatile* c1form))
               (*unwind-exit* *unwind-exit*)
               (*env* *env*)
               (*env-lvl* *env-lvl*))
  ;; Replace read-only variables when it is worth doing it.
  (loop for var in vars
        for rest-forms on (append forms (list body))
        for form = (first rest-forms)
        unless (let-replaceable-var-ref-p var form rest-forms)
          collect var into used-vars
          and collect form into used-forms
        finally (setf vars used-vars forms used-forms))
  ;; Make necessary local C variables and create closure bindings for
  ;; closed-over variables.
  (push-instruction :let-vars vars)
  ;; Assign values
  (loop for form in forms
        for var in vars
        do (cond
             ((not (useful-var-p var))
              (let ((*destination* 'TRASH))
                (c2expr* form)))
             ((local-var-p var)
              (let ((*destination* var))
                (c2expr* form)))
             (t
              (case (c1form-name form)
                (LOCATION (bind (c1form-arg 0 form) var))
                (VARIABLE (bind (c1form-arg 0 form) var))
                (t (bind-init form var))))))
  ;; Optionally register the variables with the IHS frame for debugging
  (if (let-build-debug-lexenv-p vars)
      (let ((*unwind-exit* (list* 'IHS-ENV *unwind-exit*))
            (var-locations (remove-if-not #'useful-var-p vars)))
        (push-instruction :build-debug-lexenv var-locations)
        (c2expr body)
        (push-instruction :close-debug-lexenv))
      (c2expr body))
  (push-instruction :let-exit))

(defun c2multiple-value-bind (c1form vars init-form body)
  (declare (ignore c1form))
  (let* ((*unwind-exit* *unwind-exit*)
         (*env-lvl* *env-lvl*)
         (*env* *env*)
         (*lcl* *lcl*))
    ;; Make necessary local C variables and create closure bindings for
    ;; closed-over variables.
    (push-instruction :let-vars vars)
    ;; Assign the values to the variables, compiling the form and binding the
    ;; variables in the process.
    (do-m-v-setq vars init-form t)
    ;; Compile the body. If there are bindings of special variables, these
    ;; bindings are undone here.
    (c2expr body)
    (push-instruction :let-exit)))

(defun c2location (c1form loc)
  (unwind-exit (precise-loc-lisp-type loc (c1form-primary-type c1form))))

;;; When LOC is not NIL, then the variable is a constant.
(defun c2variable (c1form var loc)
  (unwind-exit (precise-loc-lisp-type
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
    (push-instruction :progv sym-loc val-loc lcl)
    (let ((*destination* sym-loc)) (c2expr* symbols))
    (let ((*destination* val-loc)) (c2expr* values))
    (let ((*unwind-exit* (cons lcl *unwind-exit*)))
      (push-instruction :bind-progv sym-loc val-loc lcl)
      (c2expr body))
    (push-instruction :exit-progv)))

(defun c2psetq (c1form vrefs forms &aux (saves nil))
  (declare (ignore c1form))
  ;; similar to inline-args
  (with-inline-blocks ()
    (loop for var in vrefs
          for (form . rest-forms) on forms
          do (if (or (var-changed-in-form-list var rest-forms)
                     (var-referenced-in-form-list var rest-forms))
                 (if (eq (c1form-name form) 'LOCATION)
                     (push (cons var (c1form-arg 0 form)) saves)
                     (let* ((typ (c1form-primary-type form))
                            (rep (var-host-type var))
                            (tmp (emit-inlined-temp-var form typ rep)))
                       (push (cons var tmp) saves)))
                 (let ((*destination* var))
                   (c2expr* form))))
    (dolist (save saves)
      (push-instruction :set-loc (car save) (cdr save))))
  (unwind-exit *vv-nil*))

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
    (push-instruction :m-v-setq vars min-values use-bind)
    'VALUE0))

(defun c2multiple-value-setq (c1form vars form)
  (declare (ignore c1form))
  (unwind-exit (do-m-v-setq vars form nil)))
