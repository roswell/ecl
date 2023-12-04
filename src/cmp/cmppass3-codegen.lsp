(in-package "COMPILER")

(defgeneric codegen (compiler opecode instruction))

(defmacro define-codegen ((compiler opcode) (instruction) &body body)
  `(defmethod codegen ((compiler (eql ,compiler))
                       (opcode (eql ,opcode))
                       (,instruction instruction))
     ,@body))


;;; Data

(define-codegen (:cxx :move) (instruction)
  (destructuring-bind (into from) (instruction-inputs instruction)
    (set-loc into from)))

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


;;; Functions

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


;;; Unwinding

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
    (IHS-ENV (wt-nl "ihs.lex_env = _ecl_debug_env;"))))

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
