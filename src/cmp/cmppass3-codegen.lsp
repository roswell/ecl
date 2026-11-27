(in-package "COMPILER")

(defgeneric codegen (compiler opecode instruction))

(defmacro define-codegen ((compiler opcode) (instruction) &body body)
  `(defmethod codegen ((compiler (eql ,compiler))
                       (opcode (eql ,opcode))
                       (,instruction instruction))
     ,@body))


;;; cmppass2-eval

(define-codegen (:cxx :clear-values) (instruction)
  (case (instruction-output instruction)
    (VALUEZ
     (wt-nl "cl_env_copy->values[0] = ECL_NIL;")
     (wt-nl "cl_env_copy->nvalues = 0;"))
    (LEAVE
     (wt-nl "value0 = ECL_NIL;")
     (wt-nl "cl_env_copy->nvalues = 0;"))))

;;; CHECKME do we still need to reverse forms if they are inlined?
(define-codegen (:cxx :store-values) (instruction)
  ;; To avoid clobbering VALUES vector inputs are inlined arguments (to avoid
  ;; funcalls) and slots are written from end to write VALUES[0] last.
  (let* ((forms (reverse (instruction-inputs instruction)))
         (nv (length forms)))
    (wt-nl "cl_env_copy->nvalues = " nv ";")
    (loop for v in forms
          for i from (1- nv) by -1 do
            (wt-nl "cl_env_copy->values[" i "] = " v ";"))))

(define-codegen (:cxx :frame-push-values) (instruction)
  (destructuring-bind (frame) (instruction-inputs instruction)
    (wt-nl "ecl_stack_frame_push_values(" frame ");")))

(define-codegen (:cxx :frame-pop-values) (instruction)
  (destructuring-bind (frame) (instruction-inputs instruction)
    (wt-nl "ecl_stack_frame_pop_values(" frame ");")))


;;; cmppass2-call

(define-codegen (:cxx :frame-push) (instruction)
  (destructuring-bind (frame what) (instruction-inputs instruction)
    (wt-nl "ecl_stack_frame_push(" frame "," what ");")))

(define-codegen (:cxx :declare-c-fun) (instruction)
  (destructuring-bind (fun-c-name minarg maxarg)
      (instruction-inputs instruction)
    (multiple-value-bind (val declared)
        (gethash fun-c-name *compiler-declared-globals*)
      (declare (ignore val))
      (unless declared
        (if (= maxarg minarg)
            (progn
              (wt-nl-h "extern cl_object " fun-c-name "(")
              (dotimes (i maxarg)
                (when (> i 0) (wt-h1 ","))
                (wt-h1 "cl_object"))
              (wt-h1 ");"))
            (progn
              (wt-nl-h "extern cl_object " fun-c-name "(cl_narg")
              (dotimes (i (min minarg si:c-arguments-limit))
                (wt-h1 ",cl_object"))
              (wt-h1 ",...);")))
        (setf (gethash fun-c-name *compiler-declared-globals*) 1)))))


;;; cmppass2-exit

(define-codegen (:cxx :move) (instruction)
  (destructuring-bind (into from) (instruction-inputs instruction)
    (set-loc into from)))

;;; INV this function arguments are procured by COMPUTE-UNWIND.
(defun perform-unwind (frs-bind bds-lcl bds-bind stack-frame ihs-p)
  (declare (si::c-local)
           (fixnum frs-bind bds-bind))
  (when (plusp frs-bind)
    (wt-nl "ecl_frs_pop_n(cl_env_copy, " frs-bind ");"))
  (when stack-frame
    (wt-nl "ecl_stack_frame_close(" stack-frame ");"))
  (when bds-lcl
    (wt-nl "ecl_bds_unwind(cl_env_copy," bds-lcl ");"))
  (if (< bds-bind 4)
      (dotimes (n bds-bind)
        (declare (ignorable n))
        (wt-nl "ecl_bds_unwind1(cl_env_copy);"))
      (wt-nl "ecl_bds_unwind_n(cl_env_copy," bds-bind ");"))
  (case ihs-p
    (IHS     (wt-nl "ecl_ihs_pop(cl_env_copy);"))
    (IHS-ENV (wt-nl "ihs.lcl_env = _ecl_debug_env;"))))

(define-codegen (:cxx :unwind) (instruction)
  (destructuring-bind (into from) (instruction-inputs instruction)
    (multiple-value-bind (frs-bind bds-lcl bds-bind stack-frame ihs-p)
        (compute-unwind into from)
      (perform-unwind frs-bind bds-lcl bds-bind stack-frame ihs-p))))

(define-codegen (:cxx :branch) (instruction)
  (destructuring-bind (exit from kind args) (instruction-inputs instruction)
    (ecase kind
      (:jump-t
       (destructuring-bind (loc) args
         (case (loc-host-type loc)
           (:bool     (wt-nl "if (" loc ") "))
           (:object   (wt-nl "if (" loc "!=ECL_NIL) "))
           (otherwise (wt-nl "if ((") (wt-coerce-loc :object loc) (wt ")!=ECL_NIL) ")))))
      (:jump-f
       (destructuring-bind (loc) args
         (case (loc-host-type loc)
           (:bool     (wt-nl "if (!(" loc "))"))
           (:object   (wt-nl "if (Null(" loc "))"))
           (otherwise (wt-nl "if (Null(") (wt-coerce-loc :object loc) (wt "))")))))
      (:jump-eq
       (destructuring-bind (x y) args
         (wt-nl "if (" `(coerce-loc :object ,x) "==" `(coerce-loc :object ,y) ") "))))
    (wt-open-brace)
    (multiple-value-bind (frs-bind bds-lcl bds-bind stack-frame ihs-p)
        (compute-unwind (label-denv exit) from)
      (perform-unwind frs-bind bds-lcl bds-bind stack-frame ihs-p)
      (wt-nl-go exit))
    (wt-nl-close-brace)))

(define-codegen (:cxx :escape) (instruction)
  ;; All these boil down to calling ecl_unwind which unwinds stacks dynamically.
  ;; If we want to implement call/cc, then this is the place where we dispatch.
  #+ (or) (wt-nl "ecl_unwind(cl_env_copy," frs-id ");")
  (destructuring-bind (exit kind) (instruction-inputs instruction)
    (ecase kind
      (:go
       ;; The second argument is passed as a value (index for jump).
       (wt-nl "cl_go(" (tag-var exit) ",ecl_make_fixnum(" (tag-index exit) "));"))
      (:throw
       ;; Unlike GO and RETURN-FROM, the destination is not known at compile time.
       ;; TODO in some cases it is possible to prove the destination CATCH form.
       (wt-nl "cl_throw(" exit ");"))
      (:return-from
       ;; The second argument is used only to signal the error.
       (wt-nl "cl_return_from(" (blk-var exit) "," (get-object (blk-name exit)) ");")))))

;;; JUMP is similar to GOTO, but it allows a fallthough.
(define-codegen (:cxx :jump) (instruction)
  (destructuring-bind (label from) (instruction-inputs instruction)
    (unless (eq label (find-if #'labelp from))
      (wt-nl-go label))))

(define-codegen (:cxx :goto) (instruction)
  (destructuring-bind (label) (instruction-inputs instruction)
    (wt-nl-go label)))

(define-codegen (:cxx :exit) (instruction)
  (destructuring-bind (loc) (instruction-inputs instruction)
    (wt-nl "return " loc ";")))

(define-codegen (:cxx :label) (instruction)
  (destructuring-bind (label) (instruction-inputs instruction)
    (wt-label label)))


;;; cmppass2-cont

;;; WITH-UNWIND-FRAME

(define-codegen (:cxx :frame-unwind) (instruction)
  (destructuring-bind (tag) (instruction-inputs instruction)
    (wt-nl-open-brace)
    (wt-nl "ecl_frs_push(cl_env_copy," tag ");")
    (wt-nl "if (__ecl_frs_push_result!=0) {")))

(define-codegen (:cxx :frame-normal) (instruction)
  (destructuring-bind () (instruction-inputs instruction)
    (wt-nl "} else {")))

(define-codegen (:cxx :frame-finish) (instruction)
  (destructuring-bind () (instruction-inputs instruction)
    (wt-nl "}")
    (wt-nl-close-brace)))

;;; WITH-STACK-FRAME
(define-codegen (:cxx :stack-open) (instruction)
  (destructuring-bind (loc) (instruction-inputs instruction)
    (open-inline-block)
    (let ((hlp "_ecl_inner_frame_aux"))
      (wt-nl "ecl_object " hlp ";")
      (wt-nl *volatile* "cl_object " loc
             "=ecl_stack_frame_open(cl_env_copy,(cl_object)&" hlp ",0);"))))

(define-codegen (:cxx :stack-exit) (instruction)
  (destructuring-bind () (instruction-inputs instruction)
    (close-inline-block)))

;;; BLOCK
(define-codegen (:cxx :block) (instruction)
  (destructuring-bind (blk) (instruction-inputs instruction)
    (let ((blk-var (blk-var blk)))
      (wt-nl-open-brace)
      (when (eq :object (var-kind blk-var))
        ;; The block is referenced across UNWIND-PROTECT boundary.
        (setf (var-loc blk-var) (next-lcl))
        (wt-nl "cl_object " blk-var ";"))
      (when (env-grows (var-ref-ccb blk-var))
        ;; The block is referenced from a closure which may escape.
        (let ((env-lvl *env-lvl*))
          (wt-nl "cl_object " *volatile* "env" (incf *env-lvl*) " = env" env-lvl ";")))
      (bind 'FRAME++ blk-var))))

(define-codegen (:cxx :exit-block) (instruction)
  (destructuring-bind (blk) (instruction-inputs instruction)
    (wt-nl-close-brace)
    (when (var-ref-ccb (blk-var blk))
      (decf *env*))))

;;; TAGBODY
(define-codegen (:cxx :tagbody) (instruction)
  (destructuring-bind (tag-loc) (instruction-inputs instruction)
    (wt-nl-open-brace)
    (when (eq :object (var-kind tag-loc))
      (setf (var-loc tag-loc) (next-lcl))
      (wt-nl "cl_object " tag-loc ";"))
    (when (env-grows (var-ref-ccb tag-loc))
      (let ((env-lvl *env-lvl*))
        (wt-nl "volatile cl_object env" (incf *env-lvl*) " = env" env-lvl ";")))
    (bind 'FRAME++ tag-loc)))

(define-codegen (:cxx :bad-tag) (instruction)
  (destructuring-bind (tag-loc) (instruction-inputs instruction)
    (when (var-ref-ccb tag-loc)
      (wt-nl "ecl_internal_error(\"GO found an inexistent tag\");"))))

(define-codegen (:cxx :exit-tagbody) (instruction)
  (destructuring-bind () (instruction-inputs instruction)
    (wt-nl-close-brace)))

;;; CATCH
(define-codegen (:cxx :catch) (instruction)
  (destructuring-bind () (instruction-inputs instruction)
    (wt-comment "BEGIN CATCH")))

(define-codegen (:cxx :exit-catch) (instruction)
  (destructuring-bind () (instruction-inputs instruction)
    (wt-nl "ecl_frs_pop(cl_env_copy);")
    (wt-comment "END CATCH")))

;;; UNWIND-PROTECT

(define-codegen (:cxx :unwind-protect-begin) (instruction)
  (destructuring-bind () (instruction-inputs instruction)
    (wt-nl "volatile bool unwinding = FALSE;")
    (wt-nl "ecl_frame_ptr next_fr;")))

(define-codegen (:cxx :unwind-protect-abort) (instruction)
  (destructuring-bind () (instruction-inputs instruction)
    (wt-nl "  unwinding = TRUE; next_fr=cl_env_copy->frs_stack.nlj_fr;")))

(define-codegen (:cxx :unwind-protect-clean) (instruction)
  (destructuring-bind (frame) (instruction-inputs instruction)
    (wt-nl "ecl_frs_pop(cl_env_copy);")
    (wt-nl "ecl_stack_frame_push_values(" frame ");")))

(define-codegen (:cxx :unwind-protect-exit) (instruction)
  (destructuring-bind (frame) (instruction-inputs instruction)
    (wt-nl "ecl_stack_frame_pop_values(" frame ");")
    (wt-nl "if (unwinding) ecl_unwind(cl_env_copy,next_fr);")))


;;; cmppass2-var

;;; LET, MULTIPLE-VALUE-BIND and MULTIPLE-VALUE-SETQ

(define-codegen (:cxx :let-vars) (instruction)
  (destructuring-bind (vars) (instruction-inputs instruction)
    (open-inline-block)
    ;; For all variables which are not special and do not belong to a closure,
    ;; make a local C variable.
    (dolist (var vars)
      (declare (type var var))
      (ext:when-let ((kind (local-var-p var)))
        (when (useful-var-p var)
          (wt-nl (host-type->c-name kind) " " *volatile* var ";"))))
    ;; If there are closure variables, set up an environment.
    (when (env-grows (some #'var-ref-ccb vars))
      (let ((env-lvl *env-lvl*))
        (wt-nl "volatile cl_object env" (incf *env-lvl*) " = env" env-lvl ";")))))

(define-codegen (:cxx :m-v-setq) (instruction)
  (destructuring-bind (vars min-values use-bind) (instruction-inputs instruction)
    (let* ((*lcl* *lcl*)
           (useful-extra-vars (some #'useful-var-p (nthcdr min-values vars)))
           (tmp (make-lcl-var))
           (nr (make-lcl-var :type :int)))
      (open-inline-block)
      (when useful-extra-vars
        ;; Make a temporary variable that stores VALUES[i] or ECL_NIL.
        (wt-nl "cl_object " tmp ";")
        ;; Make a copy of env->nvalues before assigning to any variables
        (wt-nl "const int " nr " = cl_env_copy->nvalues;"))
      ;; We know that at least MIN-VALUES variables will get a value.
      ;; VARS without a value are initialized to NIL (#VALS < #VARS).
      (flet ((bind-or-set (loc var)
               (cond ((not use-bind)
                      (set-loc var loc))
                     ((useful-var-p var)
                      (bind loc var)))))
        (loop for v in vars
              for i from 0
              for loc = 'value0 then (list 'VALUE i)
              do (cond
                   ((< i min-values)
                    (bind-or-set loc v))
                   ((useful-var-p v)
                    (wt-nl tmp " = (" nr "<=" i ")? ECL_NIL : " loc ";")
                    (bind-or-set tmp v)))))
      (close-inline-block))))

(define-codegen (:cxx :let-exit) (instruction)
  (destructuring-bind () (instruction-inputs instruction)
    (close-inline-block)))


;;; PROGV

(define-codegen (:cxx :progv) (instruction)
  (destructuring-bind (sym-loc val-loc lcl) (instruction-inputs instruction)
    (open-inline-block)
    (wt-nl "cl_object " sym-loc ", " val-loc "; cl_index " lcl ";")))

(define-codegen (:cxx :bind-progv) (instruction)
  (destructuring-bind (sym-loc val-loc lcl) (instruction-inputs instruction)
    (wt-nl lcl " = ecl_progv(cl_env_copy, " sym-loc ", " val-loc ");")))

(define-codegen (:cxx :exit-progv) (instruction)
  (destructuring-bind () (instruction-inputs instruction)
   (close-inline-block)))


;;; PSETQ

(define-codegen (:cxx :set-loc) (instruction)
  (destructuring-bind (destination loc) (instruction-inputs instruction)
    (set-loc destination loc)))

;;; Debug env

(define-codegen (:cxx :build-debug-lexenv) (instruction)
  (destructuring-bind (var-locations) (instruction-inputs instruction)
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
        (open-inline-block)
        (wt-nl "static const struct ecl_var_debug_info _ecl_descriptors[]={")
        (loop for (name . code) in filtered-codes
              for i from 0
              do (wt-nl (if (zerop i) "{" ",{") name "," code "}"))
        (wt "};")
        (wt-nl "const cl_index _ecl_debug_info_raw[]={")
        (wt-nl "(cl_index)(_ecl_debug_env),"
               "(cl_index)(_ecl_descriptors)")
        (loop for var-loc in filtered-locations
              do (wt ",(cl_index)(&" var-loc ")"))
        (wt "};")
        (wt-nl "ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,"
               (+ 2 (length filtered-locations))
               ",,);")
        (wt-nl "ihs.lcl_env = _ecl_debug_env;"))
      filtered-codes)))

(define-codegen (:cxx :close-debug-lexenv) (instruction)
  (destructuring-bind () (instruction-inputs instruction)
    (close-inline-block)
    ;; POP-DEBUG-LEXICAL-ENV
    (wt-nl "ihs.lcl_env = _ecl_debug_env;")))

(define-codegen (:cxx :build-debug-funenv) (instruction)
  (destructuring-bind (var-locations fname-obj) (instruction-inputs instruction)
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
      (open-inline-block)
      ;; Generate two tables, a static one with information about the variables,
      ;; including name and type, and dynamic one, which is a vector of pointer to
      ;; the variables.
      (when filtered-codes
        (wt-nl "static const struct ecl_var_debug_info _ecl_descriptors[]={")
        (loop for (name . code) in filtered-codes
              for i from 0
              do (wt-nl (if (zerop i) "{" ",{") name "," code "}"))
        (wt "};")
        (wt-nl "const cl_index _ecl_debug_info_raw[]={")
        (wt-nl "(cl_index)(ECL_NIL),"
               "(cl_index)(_ecl_descriptors)")
        (loop for var-loc in filtered-locations
              do (wt ",(cl_index)(&" var-loc ")"))
        (wt "};")
        (wt-nl "ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,"
               (+ 2 (length filtered-locations))
               ",,);")))
    (wt-nl "ecl_ihs_push(cl_env_copy,&ihs," fname-obj ",ECL_NIL,_ecl_debug_env);")))
