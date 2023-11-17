;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

(in-package #:compiler)

(defun c2expr (form)
  (with-c1form-env (form form)
    (let* ((name (c1form-name form))
           (args (c1form-args form))
           (dispatch (gethash name *c2-dispatch-table*)))
      (apply dispatch form args))))

(defun c2expr* (form)
  ;; C2EXPR* compiles the giving expression in a context in which
  ;; other expressions will follow this one. We must thus create
  ;; a possible label so that the compiled forms exit right at
  ;; the point where the next form will be compiled.
  (with-exit-label (*exit*)
    (let (;;(*lex* *lex*)
          (*lcl* *lcl*)
          (*temp* *temp*))
      (c2expr form))))

(defun c2progn (c1form forms)
  (declare (ignore c1form))
  ;; c1progn ensures that the length of forms is not less than 1.
  (do ((l forms (cdr l))
       (lex *lex*))
      ((endp (cdr l))
       (c2expr (car l)))
    (let* ((this-form (first l))
           (name (c1form-name this-form)))
      (let ((*destination* 'TRASH))
        (c2expr* (car l)))
      (setq *lex* lex)  ; recycle lex locations
      ;; Since PROGN does not have tags, any transfer of control means
      ;; leaving the current PROGN statement.
      (when (or (eq name 'CL:GO) (eq name 'CL:RETURN-FROM))
        (return)))))

(defun c2if (c1form fmla form1 form2)
  (declare (ignore c1form))
  ;; FIXME! Optimize when FORM1 or FORM2 are constants
  (cond ((and (eq *destination* 'TRASH)
              (eq (c1form-name form2) 'LOCATION))
         ;; Optimize (IF condition true-branch) or a situation in which
         ;; the false branch can be discarded.
         (with-exit-label (false-label *exit*)
           (let ((*destination* `(JUMP-FALSE ,false-label)))
             (c2expr* fmla))
           (c2expr form1)))
        ((and (eq *destination* 'TRASH)
              (eq (c1form-name form1) 'LOCATION))
         ;; Optimize (IF condition useless-value false-branch) when
         ;; the true branch can be discarded.
         (with-exit-label (true-label *exit*)
           (let ((*destination* `(JUMP-TRUE ,true-label)))
             (c2expr* fmla))
           (c2expr form2)))
        (t
         (with-exit-label (false-label)
           (let ((*destination* `(JUMP-FALSE ,false-label)))
             (c2expr* fmla))
           (c2expr form1))
         (c2expr form2))))

(defun jump-true-destination-p (dest)
  (declare (si::c-local))
  (and (consp dest) (eq (si:cons-car dest) 'JUMP-TRUE)))

(defun jump-false-destination-p (dest)
  (declare (si::c-local))
  (and (consp dest) (eq (si:cons-car dest) 'JUMP-FALSE)))

(defun negate-argument (inlined-arg dest-loc)
  (declare (si::c-local))
  (let* ((loc (second inlined-arg))
         (rep-type (loc-representation-type loc)))
    (apply #'produce-inline-loc
           (list inlined-arg)
           (if (eq (loc-representation-type dest-loc) :bool)
               (case rep-type
                 (:bool '((:bool) (:bool) "(#0)==ECL_NIL" nil t))
                 (:object '((:object) (:bool) "(#0)!=ECL_NIL" nil t))
                 (otherwise (return-from negate-argument nil)))
               (case rep-type
                 (:bool '((:bool) (:object) "(#0)?ECL_NIL:ECL_T" nil t))
                 (:object '((:object) (:object) "Null(#0)?ECL_T:ECL_NIL" nil t))
                 (otherwise (return-from negate-argument *vv-nil*)))))))

(defun c2fmla-not (c1form arg)
  (declare (ignore c1form))
  (let ((dest *destination*))
    (cond ((jump-true-destination-p dest)
           (let ((*destination* `(JUMP-FALSE ,@(cdr dest))))
             (c2expr arg)))
          ((jump-false-destination-p dest)
           (let ((*destination* `(JUMP-TRUE ,@(cdr dest))))
             (c2expr arg)))
          (t
           (let ((*inline-blocks* 0)
                 (*temp* *temp*))
             (unwind-exit (negate-argument (emit-inline-form arg nil) dest))
             (close-inline-blocks))))))

(defun c2fmla-and (c1form butlast last)
  (declare (ignore c1form))
  (if (jump-false-destination-p *destination*)
      (progn
        (mapc #'c2expr* butlast)
        (c2expr last))
      (with-exit-label (normal-exit)
        (with-exit-label (false-label)
          (let ((*destination* `(JUMP-FALSE ,false-label)))
            (mapc #'c2expr* butlast))
          (c2expr last))
        (unwind-exit *vv-nil*))))

(defun c2fmla-or (c1form butlast last)
  (declare (ignore c1form))
  (cond ((jump-true-destination-p *destination*)
         (mapc #'c2expr* butlast)
         (c2expr last))
        ((jump-false-destination-p *destination*)
         (with-exit-label (true-label)
           (let ((*destination* `(JUMP-TRUE ,true-label)))
             (mapc #'c2expr* butlast))
           (c2expr last))
         (unwind-exit *vv-t*))
        (t
         (with-exit-label (common-exit)
           (with-exit-label (normal-exit)
             (dolist (f butlast)
               (let ((*destination* 'VALUE0))
                 (c2expr* f))
               (wt-nl "if (" 'VALUE0 "!=ECL_NIL) ")
               (wt-open-brace) (unwind-jump normal-exit) (wt-nl-close-brace))
             (c2expr last))
           (unwind-exit 'VALUE0)))))

(defun c2mv-prog1 (c1form form body)
  (declare (ignore c1form))
  (wt-nl-open-brace)
  (wt-nl "struct ecl_stack_frame _ecl_inner_frame_aux;")
  (wt-nl *volatile* "cl_object _ecl_inner_frame = ecl_stack_frame_open(cl_env_copy,(cl_object)&_ecl_inner_frame_aux,0);")
  (let ((*unwind-exit* `((STACK "_ecl_inner_frame") ,@*unwind-exit*)))
    (let ((*destination* 'VALUEZ))
      (c2expr* form))
    (wt-nl "ecl_stack_frame_push_values(_ecl_inner_frame);")
    (let ((*destination* 'TRASH))
      (mapc #'c2expr* body))
    (wt-nl "ecl_stack_frame_pop_values(_ecl_inner_frame);"))
  (wt-nl "ecl_stack_frame_close(_ecl_inner_frame);")
  (wt-nl-close-brace)
  (unwind-exit 'VALUEZ))

(defun c2values (c1form forms)
  (declare (ignore c1form))
  (cond
   ;; When the values are not going to be used, then just
   ;; process each form separately.
   ((eq *destination* 'TRASH)
    (mapc #'c2expr* forms)
    ;; We really pass no value, but we need UNWIND-EXIT to trigger all the
    ;; frame-pop and all other exit forms.
    (unwind-exit 'VALUE0))
   ;; For (VALUES) we can replace the output with either NIL (if the value is
   ;; actually used) and set only NVALUES when the value is the output of a
   ;; function.
   ((endp forms)
    (cond ((eq *destination* 'LEAVE)
           (wt-nl "value0 = ECL_NIL;")
           (wt-nl "cl_env_copy->nvalues = 0;")
           (unwind-exit 'LEAVE))
          ((eq *destination* 'VALUEZ)
           (wt-nl "cl_env_copy->values[0] = ECL_NIL;")
           (wt-nl "cl_env_copy->nvalues = 0;")
           (unwind-exit 'VALUEZ))
          (t
           (unwind-exit *vv-nil*))))
   ;; For a single form, we must simply ensure that we only take a single
   ;; value of those that the function may output.
   ((endp (rest forms))
    (let ((form (first forms)))
      (if (or (not (member *destination* '(LEAVE VALUEZ)))
              (c1form-single-valued-p form))
          (c2expr form)
          (progn
            (let ((*destination* 'VALUE0)) (c2expr* form))
            (unwind-exit 'VALUE0)))))
   ;; In all other cases, we store the values in the VALUES vector,
   ;; and force the compiler to retrieve anything out of it.
   (t
    (let* ((nv (length forms))
           (*inline-blocks* 0)
           (*temp* *temp*)
           (forms (nreverse (coerce-locs (inline-args forms)))))
      ;; By inlining arguments we make sure that VL has no call to funct.
      ;; Reverse args to avoid clobbering VALUES(0)
      (wt-nl "cl_env_copy->nvalues = " nv ";")
      (do ((vl forms (rest vl))
           (i (1- (length forms)) (1- i)))
          ((null vl))
        (declare (fixnum i))
        (wt-nl "cl_env_copy->values[" i "] = " (first vl) ";"))
      (unwind-exit 'VALUEZ)
      (close-inline-blocks)))))
