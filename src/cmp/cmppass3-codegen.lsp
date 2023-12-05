(in-package "COMPILER")

(defgeneric codegen (compiler opecode instruction))

(defmacro define-codegen ((compiler opcode) (instruction) &body body)
  `(defmethod codegen ((compiler (eql ,compiler))
                       (opcode (eql ,opcode))
                       (,instruction instruction))
     ,@body))

(define-codegen (:cxx :move) (instruction)
  (destructuring-bind (into from) (instruction-inputs instruction)
    (set-loc into from)))


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
