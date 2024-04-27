(in-package #:compiler)



(defun wt-print-header (source)
  (wt-comment-nl "Compiler: ~A ~A" (lisp-implementation-type) (lisp-implementation-version))
  #-ecl-min
  (multiple-value-bind (second minute hour day month year)
      (get-decoded-time)
    (declare (ignore second))
    (wt-comment-nl "Date: ~D/~D/~D ~2,'0D:~2,'0D (yyyy/mm/dd)" year month day hour minute)
    (wt-comment-nl "Machine: ~A ~A ~A" (software-type) (software-version) (machine-type)))
  (wt-comment-nl "Source: ~A" source))

(defun compiler-pass/generate-cxx (c-pathname h-pathname data-pathname init-name source)
  (with-cxx-env ()
    ;; After this step we still can add new objects, but objects that are
    ;; already stored in VV or VVtemp must not change the location.
    (optimize-cxx-data *referenced-objects*)
    (setq *compiler-phase* 't2)
    (with-open-file (*compiler-output1* c-pathname :direction :output
                                                   :if-does-not-exist :create
                                                   :if-exists :supersede)
      (with-open-file (*compiler-output2* h-pathname :direction :output
                                                     :if-does-not-exist :create
                                                     :if-exists :supersede)
        (wt-print-header source)
        (wt-nl1 "#include " *cmpinclude*)
        (ctop-write init-name h-pathname data-pathname)
        (terpri *compiler-output1*)
        (terpri *compiler-output2*)))
    (data-c-dump data-pathname)))

;;;;  CMPTOP  --  Compiler top-level.

(defun t2expr (form)
  (check-type form c1form)
  (with-c1form-env (form form)
    (let* ((name (c1form-name form))
           (args (c1form-args form))
           (dispatch (gethash name *t2-dispatch-table*)))
      (if dispatch
          (apply dispatch form args)
          (cmperr "Unhandled T2FORM found at the toplevel:~%~4I~A" form)))))

(defun t2compiler-let (c1form symbols values body)
  (declare (ignore c1form))
  (progv symbols values (c2expr body)))

(defun t2progn (c1form args)
  (declare (ignore c1form))
  (mapc #'t2expr args))

(defun t2ordinary (c1form form)
  (declare (ignore c1form))
  (with-exit-label (*exit*)
    (let ((*destination* 'TRASH))
      (c2expr form))))

(defun t2load-time-value (c1form vv-loc form)
  (declare (ignore c1form))
  (with-exit-label (*exit*)
    (let ((*destination* vv-loc))
      (c2expr form))))

(defun t2make-form (c1form vv-loc form)
  (declare (ignore c1form))
  (with-exit-label (*exit*)
    (let ((*destination* vv-loc))
      (c2expr form))))

(defun t2init-form (c1form vv-loc form)
  (declare (ignore c1form vv-loc))
  (with-exit-label (*exit*)
    (let ((*destination* 'TRASH))
      (c2expr form))))

(defun t2fset (c1form &rest args)
  (declare (ignore args))
  (t2ordinary c1form c1form))

(defun c2fset (c1form fun fname macro-p pprint c1forms)
  (declare (ignore pprint))
  (when (fun-no-entry fun)
    (wt-nl "(void)0; " (format nil "/* No entry created for ~A */" (fun-name fun)))
    ;; FIXME! Look at C2LOCALS!
    (update-function-env fun)
    (return-from c2fset))
  (if (and (not (fun-closure fun))
           (eq *destination* 'TRASH))
      (wt-install-function fname fun macro-p)
      (c2call-global c1form 'SI:FSET c1forms)))


(defun emit-functions (*compiler-output1*)
  (declare (si::c-local))
  ;; Local functions and closure functions
  (do ((*compile-time-too* nil)
       (*compile-toplevel* nil)
       (*emitted-functions* nil))
      ;; repeat until t3function generates no more
      ((eq *emitted-functions* *functions*))
    ;; scan *functions* backwards
    (do ((lfs *functions* (cdr lfs)))
        ((eq (cdr lfs) *emitted-functions*)
         (setq *emitted-functions* lfs)
         (locally (declare (notinline t3function))
           ;; so disassemble can redefine it
           (t3function (first lfs)))))))

(defun ctop-write (init-name h-pathname data-pathname &aux top-output-string)
  (wt-nl "#include \"" (brief-namestring h-pathname) "\"")

  ;; VV might be needed by functions in CLINES.
  (wt-nl-h "#ifdef ECL_DYNAMIC_VV")
  (wt-nl-h "static cl_object *VV;")
  (wt-nl-h "#else")
  (wt-nl-h "static cl_object VV[VM];")
  (wt-nl-h "#endif")
  (output-clines *compiler-output2*)

  (wt-nl-h "#ifdef __cplusplus")
  (wt-nl-h "extern \"C\" {")
  (wt-nl-h "#endif")
  ;;; We rebind the output to ensure that the initialization function is
  ;;; processed first and added last.
  (let ((output (make-string-output-stream)))
    (t3entry-fun init-name output)
    (emit-functions *compiler-output1*)
    (setq top-output-string (get-output-stream-string output)))
  ;; Declarations in h-file.
  (wt-nl-h "static cl_object Cblock;")
  (let ((num-objects (data-size)))
    (if (zerop num-objects)
        (progn
          (wt-nl-h "#undef ECL_DYNAMIC_VV")
          (wt-nl-h "#define compiler_data_text 0")
          (wt-nl-h "#define VM 0")
          (wt-nl-h "#define VMtemp 0")
          (wt-nl-h "#define VV NULL"))
        (progn
          (wt-nl-h "#define VM " (data-permanent-storage-size))
          (wt-nl-h "#define VMtemp "  (data-temporary-storage-size)))))

  (wt-nl-h "#define ECL_DEFINE_SETF_FUNCTIONS ")
  (loop for (name setf-vv name-vv) in *setf-definitions*
        do (wt-h #\\ #\Newline setf-vv "=ecl_setf_definition(" name-vv ",ECL_T);"))

  (wt-nl-h "#ifdef __cplusplus")
  (wt-nl-h "}")
  (wt-nl-h "#endif")

  (unless (null *static-constants*)
    (wt-nl-h "/*")
    (wt-nl-h " * Statically defined constants")
    (wt-nl-h " */")
    (loop for (value vv builder) in (reverse *static-constants*)
          do (terpri *compiler-output2*)
          do (funcall builder (vv-location vv) value *compiler-output2*)))

  (output-cfuns *compiler-output2*)

  (setq *compiler-phase* 't3)

  ;;; Callbacks
  (when *callbacks*
    (wt-nl-h "#include <ecl/internal.h>")
    (dolist (x *callbacks*)
      (apply #'t3callback x)))

  (wt-nl "#include \"" (brief-namestring data-pathname) "\"")
  (wt-nl "#ifdef __cplusplus")
  (wt-nl "extern \"C\"")
  (wt-nl "#endif")
  (wt-nl top-output-string))

(defun wt-function-locals (&optional closure-type)
  ;; FIXME! Are we careful enough with temporary variables that
  ;; we need not make them volatile?
  (when (plusp *max-temp*)
    (wt-nl "cl_object ")
    (dotimes (i *max-temp*)
      (wt "T" i)
      (unless (= (1+ i) *max-temp*) (wt ", ")))
    (wt ";"))
  (when *ihs-used-p*
    (wt-nl "struct ecl_ihs_frame ihs;")
    (wt-nl "const cl_object _ecl_debug_env = ECL_NIL;"))
  ;; There should be no need to mark lex as volatile, since we
  ;; are going to pass pointers of this array around and the compiler
  ;; should definitely keep this in memory.
  (when (plusp *max-lex*)
    (wt-nl "volatile cl_object lex" *level* "[" *max-lex* "];"))

  (unless (eq closure-type 'CLOSURE)
    (wt-nl "cl_object " *volatile* "env0 = ECL_NIL;"))

  (when  (plusp *max-env*)
    ;; Closure structure has to be marked volatile or else GCC may
    ;; optimize away writes into it because it does not know it shared
    ;; with the rest of the world.
    (when *aux-closure*
      (wt-nl "volatile struct ecl_cclosure aux_closure;"))

    (wt-nl "cl_object " *volatile*)
    (loop for i from 0 below *max-env*
          for comma = "" then ", "
          do (wt comma "CLV" i)
          finally (wt ";"))))


(defun t3entry-fun (name *compiler-output1*)
  (with-bir-env (:env 0 :level 0 :volatile "volatile ")
    (wt-nl "ECL_DLLEXPORT void " name "(cl_object flag)")
    (wt-nl-open-brace)
    (wt-nl "const cl_env_ptr cl_env_copy = ecl_process_env();")
    (wt-nl "cl_object value0;")
    (wt-nl "cl_object *VVtemp;")

    (wt-nl "if (flag != OBJNULL){")
    (wt-nl "Cblock = flag;")
    (wt-nl "#ifndef ECL_DYNAMIC_VV")
    (wt-nl "flag->cblock.data = VV;")
    (wt-nl "#endif")
    (when *self-destructing-fasl*
      (wt-nl "flag->cblock.self_destruct=1;"))
    (wt-nl "flag->cblock.data_size = VM;")
    (wt-nl "flag->cblock.temp_data_size = VMtemp;")
    (wt-nl "flag->cblock.data_text = compiler_data_text;")
    (wt-nl "flag->cblock.cfuns_size = compiler_cfuns_size;")
    (wt-nl "flag->cblock.cfuns = compiler_cfuns;")
    (when ext:*source-location*
      (wt-nl "flag->cblock.source = ecl_make_constant_base_string(\""
             (namestring (car ext:*source-location*)) "\",-1);"))
    (wt-nl "return;}")
    (wt-nl "#ifdef ECL_DYNAMIC_VV")
    (wt-nl "VV = Cblock->cblock.data;")
    (wt-nl "#endif")
    ;; With this we ensure creating a constant with the tag
    ;; and the initialization file
    (wt-nl "Cblock->cblock.data_text = (const cl_object *)\"" (init-name-tag name) "\";")
    (wt-nl "VVtemp = Cblock->cblock.temp_data;")
    (wt-nl "ECL_DEFINE_SETF_FUNCTIONS")
    ;; We save the C body of the statement, indented, just in case we need to
    ;; add a {} section with the environment variables.
    (let ((body (let ((*opened-c-braces* (1+ *opened-c-braces*)))
                  (with-output-to-string (*compiler-output1*)
                    (terpri *compiler-output1*)
                    (wt-comment-nl "MAKE-LOAD-FORMs")
                    (dolist (form *make-forms*)
                      (t2expr form))
                    (wt-comment-nl "TOP-LEVEL-FORMs")
                    (dolist (form *top-level-forms*)
                      (t2expr form))))))
      (if (or (plusp *max-lex*)
              (plusp *max-temp*)
              (plusp *max-env*)
              *ihs-used-p*)
          (with-lexical-scope ()
            (wt-function-locals)
            (write-sequence body *compiler-output1*))
          (write-sequence body *compiler-output1*)))
    ;; We process top-level forms before functions to update their
    ;; environments. Then we emit functions before top level forms.
    (wt-nl-close-many-braces 0)))

(defun t3callback (lisp-name c-name c-name-constant return-type return-type-code
                       arg-types arg-type-constants call-type &aux (return-p t))
  (declare (ignore lisp-name))
  (with-bir-env (:env 0 :level 0 :volatile "volatile ")
    (when (eql return-type :void)
      (setf return-p nil))
    (let ((return-type-name (host-type->c-name (ffi::%convert-to-arg-type return-type)))
          (vars (loop for n from 0 below (length arg-types)
                      collect (format nil "var~d" n)))
          (fmod (case call-type
                  ((:cdecl :default) "")
                  (:stdcall "__stdcall ")
                  (t (cmperr "DEFCALLBACK does not support ~A as calling convention"
                             call-type)))))
      (wt-nl-h "static " return-type-name " " fmod c-name "(")
      (wt-nl1  "static " return-type-name " " fmod c-name "(")
      (loop with comma = ""
            for var in vars
            for type in arg-types
            for arg-type-name = (host-type->c-name (ffi::%convert-to-arg-type type))
            do (wt-h comma arg-type-name " " var)
               (wt   comma arg-type-name " " var)
               (setf comma ","))
      (wt ")")
      (wt-h ");")
      (with-lexical-scope ()
        (when return-p
          (wt-nl return-type-name " output;"))
        (wt-nl "const cl_env_ptr cl_env_copy = ecl_process_env();")
        (wt-nl "cl_object aux;")
        (with-stack-frame (frame)
          (loop for var in vars
                and type in arg-types
                and ct in arg-type-constants
                do (wt-nl "ecl_stack_frame_push(" frame "," `(ffi-data-ref ,var ,ct) ");"))
          (wt-nl "aux = ecl_apply_from_stack_frame(" frame ","
                 "ecl_fdefinition(" c-name-constant "));")
          ;; No UNWIND-EXIT, so we must close the frame manually.
          (wt-nl "ecl_stack_frame_close(" frame ");"))
        (when return-p
          (set-loc `(ffi-data-ref "output" ,return-type-code) "aux")
          (wt-nl "return output;"))))))

(defun t3function (fun)
  (declare (type fun fun))
  ;; Compiler note about compiling this function
  (when *compile-print*
    (ext:when-let ((name (or (fun-name fun) (fun-description fun))))
      (format t "~&;;; Emitting code for ~s.~%" name)))
  (with-c1form-env (lambda-expr (fun-lambda fun))
    (let ((*tail-recursion-info* fun)
          (*tail-recursion-mark* nil))
      (with-bir-env (:env (fun-env fun)
                     :level (fun-lexical-levels fun)
                     :volatile (c1form-volatile* lambda-expr))
        (t3function-declaration fun)
        (wt-nl-open-brace)
        (let ((body (t3function-body fun)))
          (wt-function-locals (fun-closure fun))
          (wt-nl "const cl_env_ptr cl_env_copy = ecl_process_env();")
          (when (eq (fun-closure fun) 'CLOSURE)
            (wt-nl "cl_object " *volatile* "env0 = cl_env_copy->function->cclosure.env;"))
          (wt-nl "cl_object " *volatile* "value0;")
          (when (policy-check-stack-overflow)
            (wt-nl "ecl_cs_check(cl_env_copy,value0);"))
          (when (eq (fun-closure fun) 'CLOSURE)
            (t3function-closure-scan fun))
          (write-sequence body *compiler-output1*)
          (wt-nl-close-many-braces 0))
        (when (fun-variadic-entrypoint fun)
          (t3function-variadic-entrypoint fun))))))

(defun t3function-body (fun)
  (let ((string (make-array 2048 :element-type 'character
                                 :adjustable t
                                 :fill-pointer 0)))
    (with-output-to-string (*compiler-output1* string)
      (let ((lambda-expr (fun-lambda fun)))
        (c2lambda-expr (c1form-arg 0 lambda-expr)
                       (c1form-arg 2 lambda-expr)
                       (fun-cfun fun)
                       (fun-name fun)
                       (fun-description fun)
                       (fun-needs-narg fun)
                       (fun-required-lcls fun)
                       (fun-closure fun)
                       (fun-optional-type-check-forms fun)
                       (fun-keyword-type-check-forms fun)
                       (fun-variadic-entrypoint fun))))
    string))

;;; Variadic entrypoints
;;;
;;; For functions which only take required arguments, we can generate
;;; an entrypoint with variadic signature and narg parameter. This
;;; entrypoint checks the number of arguments and then calls the
;;; entrypoint without narg parameter. Callers using direct C calls
;;; can then skip the check for the number of parameters. If no direct
;;; C calls exist, the C compiler will usually inline the fixed
;;; entrypoint and thus optimize away the overhead that this method
;;; incurs over only generating a variadic entrypoint.
;;;
;;; If we use C compatible variadic dispatch, we can likewise generate
;;; an entrypoint with the signature expected by the caller, i.e. a
;;; function with only the narg argument as a fixed parameter and all
;;; other arguments as variadic parameters. This function then checks
;;; the number of arguments and dispatches to the entrypoint where the
;;; required arguments appear as fixed parameters. The advantage over
;;; only generating a variadic entrypoint with generic signature is
;;; again that callers using direct C calls can skip some work.
;;; Typically, C compatible variadic dispatch is used when fixed
;;; arguments can be passed in registers while variadic arguments must
;;; be pushed onto the stack. Thus, direct C calls can skip pushing
;;; the required arguments onto the stack.
(defun fun-variadic-entrypoint (fun)
  (let ((result (fun-variadic-entrypoint-cfun fun)))
    (if (not (eq result :unknown))
        result
        (setf (fun-variadic-entrypoint-cfun fun)
              (and (policy-inline-nargs-check)
                   (not (fun-no-entry fun))
                   (or (not (fun-needs-narg fun))
                       #+c-compatible-variadic-dispatch t)
                   ;; lexical closures will never be called via a
                   ;; variadic entrypoint, no need to create one
                   (not (eq (fun-closure fun) 'lexical))
                   (concatenate 'string (fun-cfun fun) "_va"))))))

(defun t3function-variadic-entrypoint (fun)
  (flet ((wt-return (args)
           (wt-nl "return ")
           (wt-call (fun-cfun fun) args)
           (wt ";")
           (wt-nl-close-many-braces 0)))
    (t3function-header fun
                       (fun-variadic-entrypoint-cfun fun)
                       t
                       #+c-compatible-variadic-dispatch t)
    (wt-nl-open-brace)
    (wt-maybe-check-num-arguments t
                                  (fun-minarg fun)
                                  (fun-maxarg fun)
                                  (fun-name fun))
    #-c-compatible-variadic-dispatch
    (wt-return (fun-required-lcls fun))
    #+c-compatible-variadic-dispatch
    (let ((maxargs (min (fun-maxarg fun) (1+ si:c-arguments-limit))))
      (when (plusp maxargs)
        ;; For C compatible variadic dispatch, we handle both functions
        ;; with variadic and with fixed number of arguments
        (wt-nl "cl_object x[" maxargs "];")
        (wt-nl "va_list args; va_start(args,narg);")
        (loop for i below (fun-minarg fun)
              do (wt-nl "x[" i "] = ") (wt-coerce-loc :object 'VA-ARG) (wt ";"))
        (unless (= (fun-minarg fun) (fun-maxarg fun))
          (wt-nl "for (int i = " (fun-minarg fun) "; i < ")
          (if (<= maxargs si:c-arguments-limit)
              (wt "narg")
              (wt "(narg < " maxargs " ? narg : " maxargs ")"))
          (wt "; i++)")
          (wt-open-brace)
          (wt-nl "x[i] = ")
          (wt-coerce-loc :object 'VA-ARG)
          (wt ";")
          (wt-nl-close-brace))
        (wt-nl "va_end(args);"))
      (let ((args (loop for i below maxargs
                        collect (concatenate 'string "x[" (write-to-string i) "]"))))
        (when (fun-needs-narg fun)
          (push "narg" args))
        (wt-return args)))))

(defun t3function-header (fun cfun needs-narg &optional omit-requireds)
  (let* ((comma "")
         (lambda-expr (fun-lambda fun))
         (lambda-list (c1form-arg 0 lambda-expr))
         (requireds (loop
                       repeat si::c-arguments-limit
                       for arg in (car lambda-list)
                       collect (next-lcl (var-name arg)))))
    (setf (fun-required-lcls fun) requireds)
    (cond ((fun-exported fun)
           (wt-nl-h "ECL_DLLEXPORT cl_object " cfun "(")
           (wt-nl "cl_object " cfun "("))
          (t
           (wt-nl-h "static cl_object " cfun "(")
           (wt-nl "static cl_object " cfun "(")))
    (when needs-narg
      (wt-h *volatile* "cl_narg")
      (wt *volatile* "cl_narg narg")
      (setf comma ", "))
    (dotimes (n (fun-lexical-levels fun))
      (wt-h comma "volatile cl_object  *")
      (wt comma "volatile cl_object *lex" n)
      (setf comma ", "))
    (unless omit-requireds
      (loop for lcl in requireds
            do (wt-h comma "cl_object " *volatile*)
               (wt comma "cl_object " *volatile* lcl)
               (setf comma ", ")))
    (when needs-narg
      (wt-h ", ...")
      (wt ", ..."))
    (wt-h ");")
    (wt ")")))

(defun t3function-declaration (fun)
  (declare (type fun fun))
  (wt-comment-nl (cond ((fun-global fun) "function definition for ~a")
                       ((eq (fun-closure fun) 'CLOSURE) "closure ~a")
                       (t "local function ~a"))
                 (or (fun-name fun) (fun-description fun) 'CLOSURE))
  (let* ((lambda-expr (fun-lambda fun))
         (cmp-env (c1form-env lambda-expr)))
    (wt-comment-nl "optimize speed ~D, debug ~D, space ~D, safety ~D "
                   (cmp-env-optimization 'speed cmp-env)
                   (cmp-env-optimization 'debug cmp-env)
                   (cmp-env-optimization 'space cmp-env)
                   (cmp-env-optimization 'safety cmp-env))
    (t3function-header fun (fun-cfun fun) (fun-needs-narg fun)))
  t)

(defun fun-closure-variables (fun)
  (sort (remove-if
         #'(lambda (x)
             (or
              ;; non closure variable
              (not (ref-ref-ccb x))
              ;; special variable
              (eq (var-kind x) 'special)
              ;; not actually referenced
              (and (not (var-referenced-in-form x (fun-lambda fun)))
                   (not (var-changed-in-form x (fun-lambda fun))))
              ;; parameter of this closure
              ;; (not yet bound, therefore var-loc is OBJECT)
              (eq (var-loc x) 'OBJECT)))
         (fun-referenced-vars fun))
        #'>
        :key #'var-loc))

(defun fun-lexical-levels (fun)
  (if (eq (fun-closure fun) 'LEXICAL)
      (fun-level fun)
      0))

(defun t3function-closure-scan (fun)
  (let ((clv-used (fun-closure-variables fun)))
    (wt-nl "/* Scanning closure data ... */")
    (do ((n (1- (fun-env fun)) (1- n))
         (bs clv-used)
         (first t))
        ((or (minusp n) (null bs)))
      (wt-nl "CLV" n)
      (if first
          (progn (wt " = env0;") (setf first nil))
          (wt " = _ecl_cdr(CLV" (1+ n) ");"))
      (when (= n (var-loc (first bs)))
        (wt-comment (var-name (first clv-used)))
        (pop clv-used)))
    (wt-nl-open-brace)
    (wt " /* ... closure scanning finished */")))

(defun output-cfuns (stream)
  (let ((n-cfuns (length *global-cfuns-array*)))
    (wt-nl-h "/*")
    (wt-nl-h " * Exported Lisp functions")
    (wt-nl-h " */")
    (wt-nl-h "#define compiler_cfuns_size " n-cfuns)
    (if (zerop n-cfuns)
        (wt-nl-h "#define compiler_cfuns NULL")
        (progn
          (format stream "~%static const struct ecl_cfunfixed compiler_cfuns[] = {~
~%~t/*t,m,narg,padding,name=function-location,block=name-location,entry,entry_fixed,file,file_position*/")
          (loop for (loc fname-loc fun) in (nreverse *global-cfuns-array*)
                do (let* ((variadic-entrypoint (fun-variadic-entrypoint-cfun fun))
                          (cfun (or variadic-entrypoint (fun-cfun fun)))
                          (needs-narg (or variadic-entrypoint (fun-needs-narg fun)))
                          (narg (if needs-narg
                                    (if variadic-entrypoint
                                        0
                                        (min (fun-minarg fun) si:c-arguments-limit))
                                    (fun-fixed-narg fun))))
                     (format stream "~%{~A,0,~D,0,ecl_make_fixnum(~D),ecl_make_fixnum(~D),(cl_objectfn)~A,NULL,ECL_NIL,ecl_make_fixnum(~D)},"
                             (if needs-narg "t_cfun" "t_cfunfixed")
                             narg
                             (vv-location loc)
                             (vv-location fname-loc)
                             cfun
                             (fun-file-position fun))))
          (format stream "~%};")))))

(defun wt-install-function (fname fun macro-p)
  (with-inline-blocks ()
    (let ((loc (data-empty-loc*)))
      (push (list loc fname fun) *global-cfuns-array*)
      ;; FIXME! Look at C2LOCALS!
      (update-function-env fun)
      (if macro-p
          (wt-nl "ecl_cmp_defmacro(" loc ");")
          (wt-nl "ecl_cmp_defun(" loc ");"))
      (wt-comment (loc-immediate-value fname)))))
