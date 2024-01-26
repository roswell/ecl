;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;; Set-loc and Wt-loc.

(in-package "COMPILER")


;;;
;;; Mundane locs
;;;

(defun wt-loc (loc)
  (cond ((consp loc)
         (let ((fd (gethash (car loc) *wt-loc-dispatch-table*)))
           (if fd
               (apply fd (cdr loc))
               (unknown-location 'wt-loc loc))))
        ((symbolp loc)
         (let ((txt (gethash loc *wt-loc-dispatch-table* :not-found)))
           (when (eq txt :not-found)
             (unknown-location 'wt-loc loc))
           (wt txt)))
        ((stringp loc)
         (wt loc))
        ((var-p loc)
         (wt-var loc))
        ((vv-p loc)
         (wt-vv loc))
        (t
         (unknown-location 'wt-loc loc))))

(defun wt-fixnum (value &optional vv)
  (declare (ignore vv))
  (princ value *compiler-output1*)
  ;; Specify explicit type suffix as a workaround for MSVC. C99
  ;; standard compliant compilers don't need type suffixes and choose
  ;; the correct type themselves. Note that we cannot savely use
  ;; anything smaller than a long long here, because we might perform
  ;; some other computation on the integer constant which could
  ;; overflow if we use a smaller integer type (overflows in long long
  ;; computations are taken care of by the compiler before we get to
  ;; this point).
  #+msvc (princ (cond ((typep value (host-type->lisp-type :long-long)) "LL")
                      ((typep value (host-type->lisp-type :unsigned-long-long)) "ULL")
                      (t (baboon :format-control
                                 "wt-fixnum: The number ~A doesn't fit any integer type."
                                 value)))
                *compiler-output1*))

(defun wt-number (value &optional vv)
  (declare (ignore vv))
  (wt value))

(defun wt-character (value &optional vv)
  (declare (ignore vv))
  ;; We do not use the '...' format because this creates objects of type
  ;; 'char' which have sign problems
  (wt (char-code value)))

(defun wt-value (i)
  (wt "cl_env_copy->values[" i "]"))

(defun wt-keyvars (i)
  (wt "keyvars[" i "]"))

(defun wt-the (type loc)
  (declare (ignore type))
  (wt-loc loc))


;;;
;;; CALL-LOC
;;;

(defun wt-call (fun args &optional fname env)
  (if env
      (progn
        (setf *aux-closure* t)
        (wt "(aux_closure.env="env",cl_env_copy->function=(cl_object)&aux_closure,")
        (wt-call fun args)
        (wt ")"))
      (progn
        (wt fun "(")
        (let ((comma ""))
          (dolist (arg args)
            (wt comma arg)
            (setf comma ", ")))
        (wt ")")))
  (when fname (wt-comment fname)))

(defun wt-call-indirect (fun-loc args fname function-p)
  (let ((narg (length args)))
    (if function-p
        (wt "(cl_env_copy->function=" fun-loc ")->cfun.entry(" narg)
        (wt "ecl_function_dispatch(cl_env_copy," fun-loc ")(" narg))
    (dolist (arg args)
      (wt ", " arg))
    (wt ")")
    (when fname
      (wt-comment fname))))

(defun wt-call-stack (loc fname)
  (wt "ecl_apply_from_stack_frame(_ecl_inner_frame," loc ")")
  (when fname
    (wt-comment fname)))

(defun wt-call-normal (fun args type)
  (declare (ignore type))
  (unless (fun-cfun fun)
    (baboon "Function without a C name: ~A" (fun-name fun)))
  (let* ((minarg (fun-minarg fun))
         (maxarg (fun-maxarg fun))
         (fun-c-name (fun-cfun fun))
         (fun-lisp-name (fun-name fun))
         (narg (length args))
         (env nil))
    (case (fun-closure fun)
      (CLOSURE
       (when (plusp *max-env*)
         (setf env (environment-accessor fun))))
      (LEXICAL
       (let ((lex-lvl (fun-level fun)))
         (dotimes (n lex-lvl)
           (let* ((j (- lex-lvl n 1))
                  (x (lex-env-var-name j)))
             (push x args))))))
    (unless (<= minarg narg maxarg)
      (cmperr "Wrong number of arguments for function ~S"
              (or fun-lisp-name 'ANONYMOUS)))
    (when (fun-needs-narg fun)
      (push narg args))
    (wt-call fun-c-name args nil env)))


;;;
;;; FDEFINITION, MAKE-CLOSURE
;;; 
(defun wt-fdefinition (fun-name)
  (let* ((name (si:function-block-name fun-name))
         (package (symbol-package name))
         (safe (or (not (safe-compile))
                   (and (or (eq package (find-package "CL"))
                            (eq package (find-package "CLOS"))
                            (eq package (find-package "SI")))
                        (fboundp fun-name)
                        (functionp (fdefinition fun-name))))))
    (cond
      ((not safe)
       (let ((vv (get-object fun-name)))
         (wt "ecl_fdefinition(" vv ")")))
      ((eq name fun-name)
       ;; #'symbol
       (let ((vv (get-object name)))
         (wt "(" vv "->symbol.gfdef)")))
      (t
       ;; #'(SETF symbol)
       (let ((setf-loc (assoc name *setf-definitions*)))
         (unless setf-loc
           (let* ((setf-vv (data-empty-loc*))
                  (name-vv (get-object name)))
             (setf setf-loc (list name setf-vv name-vv))
             (push setf-loc *setf-definitions*)))
         (wt "ECL_CONS_CAR(" (second setf-loc) ")"))))))

(defun environment-accessor (fun)
  (let* ((env-var (env-var-name *env-lvl*))
         (expected-env-size (fun-env fun)))
    (if (< expected-env-size *env*)
        (format nil "ecl_nthcdr(~D,~A)" (- *env* expected-env-size) env-var)
        env-var)))

(defun wt-make-closure (fun &aux (cfun (fun-cfun fun)))
  (declare (type fun fun))
  (let* ((closure (fun-closure fun))
         narg)
    (cond ((eq closure 'CLOSURE)
           (wt "ecl_make_cclosure_va((cl_objectfn)" cfun ","
               (environment-accessor fun)
               ",Cblock," (min (fun-minarg fun) si:c-arguments-limit) ")"))
          ((eq closure 'LEXICAL)
           (baboon :format-control "wt-make-closure: lexical closure detected."))
          ((setf narg (fun-fixed-narg fun)) ; empty environment fixed number of args
           (wt "ecl_make_cfun((cl_objectfn_fixed)" cfun ",ECL_NIL,Cblock," narg ")"))
          (t ; empty environment variable number of args
           (wt "ecl_make_cfun_va((cl_objectfn)" cfun ",ECL_NIL,Cblock,"
               (min (fun-minarg fun) si:c-arguments-limit) ")")))))

;;;
;;; COERCE-LOC
;;;

(defun wt-to-object-conversion (loc-host-type loc)
  ;; FIXME we can do better for constant locations.
  (let* ((record (host-type-record loc-host-type))
         (coercer (and record (host-type-to-lisp record))))
    (unless coercer
      (cmperr "Cannot coerce C variable of type ~S to lisp object" loc-host-type))
    (wt coercer "(" loc ")")))

(defun wt-from-object-conversion (dest-type loc-type host-type loc)
  (let* ((record (host-type-record host-type))
         (coercer (and record (host-type-from-lisp record))))
    (unless coercer
      (cmperr "Cannot coerce lisp object to C type ~A" host-type))
    (wt (if (or (policy-assume-no-errors)
                (subtypep loc-type dest-type))
            (host-type-from-lisp-unsafe record)
            coercer)
        "(" loc ")")))

(defun wt-coerce-loc (dest-host-type loc)
  (setq dest-host-type (lisp-type->host-type dest-host-type))
                                        ;(print dest-host-type)
                                        ;(print loc)
  (let* ((dest-type (host-type->lisp-type dest-host-type))
         (loc-type (loc-lisp-type loc))
         (loc-host-type (loc-host-type loc)))
    (labels ((coercion-error (&optional (write-zero t))
               (cmpwarn "Unable to coerce lisp object from type (~S,~S)~%~
                        to C/C++ type (~S,~S)"
                        loc-type loc-host-type dest-type dest-host-type)
               (when write-zero
                 ;; It is possible to reach this point due to a bug
                 ;; but also due to a failure of the dead code
                 ;; elimination. Write a zero to ensure that the
                 ;; output is syntactically valid C code and hope for
                 ;; the latter case.
                 (wt "(ecl_miscompilation_error(),0)")))
             (ensure-valid-object-type (a-lisp-type)
               (when (subtypep `(AND ,loc-type ,a-lisp-type) NIL)
                 (coercion-error nil))))
      (when (eq dest-host-type loc-host-type)
        (wt loc)
        (return-from wt-coerce-loc))
      (case dest-host-type
        ((:char :unsigned-char :wchar)
         (case loc-host-type
           ((:char :unsigned-char :wchar)
            (wt "(" (host-type->c-name dest-host-type) ")(" loc ")"))
           ((:object)
            (ensure-valid-object-type dest-type)
            (wt-from-object-conversion dest-type loc-type dest-host-type loc))
           (otherwise
            (coercion-error))))
        ((:float :double :long-double)
         (cond
           ((c-number-host-type-p loc-host-type)
            (wt "(" (host-type->c-name dest-host-type) ")(" loc ")"))
           ((eq loc-host-type :object)
            ;; We relax the check a bit, because it is valid in C to coerce
            ;; between floats of different types.
            (ensure-valid-object-type 'FLOAT)
            (wt-from-object-conversion dest-type loc-type dest-host-type loc))
           (t
            (coercion-error))))
        ((:csfloat :cdfloat :clfloat)
         (cond
           ((c-number-host-type-p loc-host-type)
            (wt "(" (host-type->c-name dest-host-type) ")(" loc ")"))
           ((eq loc-host-type :object)
            ;; We relax the check a bit, because it is valid in C to coerce
            ;; between COMPLEX floats of different types.
            (ensure-valid-object-type 'SI:COMPLEX-FLOAT)
            (wt-from-object-conversion dest-type loc-type dest-host-type loc))
           (t
            (coercion-error))))
        ((:bool)
         (cond
           ((c-number-host-type-p loc-host-type)
            (wt "1"))
           ((eq loc-host-type :object)
            (wt "(" loc ")!=ECL_NIL"))
           (t
            (coercion-error))))
        ((:object)
         (case loc-host-type
           ((:int-sse-pack :float-sse-pack :double-sse-pack)
            (when (>= (cmp-env-optimization 'speed) 1)
              (cmpwarn-style "Boxing a value of type ~S - performance degraded."
                             loc-host-type))))
         (wt-to-object-conversion loc-host-type loc))
        ((:pointer-void)
         (case loc-host-type
           ((:object)
            (wt-from-object-conversion dest-type loc-type dest-host-type loc))
           ((:cstring)
            (wt "(char *)(" loc ")"))
           (otherwise
            (coercion-error))))
        ((:cstring)
         (coercion-error))
        ((:char*)
         (case loc-host-type
           ((:object)
            (wt "ecl_base_string_pointer_safe(" loc ")"))
           ((:pointer-void)
            (wt "(char *)(" loc ")"))
           (otherwise
            (coercion-error))))
        ((:int-sse-pack :float-sse-pack :double-sse-pack)
         (case loc-host-type
           ((:object)
            (wt-from-object-conversion 'ext:sse-pack loc-type dest-host-type loc))
           ;; Implicitly cast between SSE subtypes
           ((:int-sse-pack :float-sse-pack :double-sse-pack)
            (wt (ecase dest-host-type
                  (:int-sse-pack (ecase loc-host-type
                                   (:float-sse-pack "_mm_castps_si128")
                                   (:double-sse-pack "_mm_castpd_si128")))
                  (:float-sse-pack (ecase loc-host-type
                                     (:int-sse-pack "_mm_castsi128_ps")
                                     (:double-sse-pack "_mm_castpd_ps")))
                  (:double-sse-pack (ecase loc-host-type
                                      (:int-sse-pack "_mm_castsi128_pd")
                                      (:float-sse-pack "_mm_castps_pd"))))
                "(" loc ")"))
           (otherwise
            (coercion-error))))
        (t
         ;; At this point we only have coercions to integers
         (cond
           ((not (c-integer-host-type-p dest-host-type))
            (coercion-error))
           ((c-number-host-type-p loc-host-type)
            (wt "(" (host-type->c-name dest-host-type) ")(" loc ")"))
           ((eq :object loc-host-type)
            (ensure-valid-object-type dest-type)
            (wt-from-object-conversion dest-type loc-type dest-host-type loc))
           (t
            (coercion-error))))))))


;;;
;;; INLINE-LOC
;;;

(defun wt-c-inline-loc (output-host-type c-expression coerced-arguments side-effects output-vars)
  (declare (ignore output-host-type side-effects))
  (with-input-from-string (s c-expression)
    (when (and output-vars (not (eq output-vars 'VALUES)))
      (wt-nl))
    (do ((c (read-char s nil nil)
            (read-char s nil nil)))
        ((null c))
      (case c
        (#\@
         (let ((object (read s)))
           (unless (and (consp object) (eq (car object) 'CL:RETURN))
             (cmperr "Used @~s in C-INLINE form. Expected syntax is @(RETURN ...)." object))
           (if (eq output-vars 'VALUES)
               (cmperr "Used @(RETURN ...) in a C-INLINE form with no output values.")
               (let ((ndx (or (second object) 0))
                     (l (length output-vars)))
                 (if (< ndx l)
                     (wt (nth ndx output-vars))
                     (cmperr "Used @(RETURN ~D) in a C-INLINE form with ~D output values."
                             ndx l))))))
        (#\#
         (let* ((k (read-char s))
                (next-char (peek-char nil s nil nil))
                (index (digit-char-p k 36)))
           (cond ((eq k #\#)
                  (wt #\#))
                 ((or (null index) (and next-char (alphanumericp next-char)))
                  (wt #\# k))
                 ((< index (length coerced-arguments))
                  (wt (nth index coerced-arguments)))
                 (t
                  (cmperr "C-INLINE: Variable code exceeds number of arguments")))))
        (otherwise
         (write-char c *compiler-output1*))))))


;;;
;;; SET-LOC
;;;

;;; Setting the location requires some cooperation between the code that returns
;;; the location and the code that assigns it. By default we treat all RVALs as
;;; having a single value (that's how wt-loc dispatches all known locations).
;;;
;;; This "default" changes when the LVAL expects multiple values. In that case
;;; the SET-LOC method checks whether the RVAL may return multiple values:
;;;
;;; - when it does, then we use these values and do nothing
;;; - when unknown, then we update values[0] and leave nvalues as is
;;; - otherwise we update values[0] and reset nvalues = 1
;;;
;;; The "unknown" requires some explanation. The predicate (USES-VALUES loc)
;;; returns true for locations that possibly can return multiple values. The
;;; most representative example are function calls - the number of returned
;;; values may even change at runtime, because the function may be recompiled.
;;;
;;; The contract between the caller and the callee is that the callee will
;;; ensure upon exit, that nvalues contains the correct value, and that the
;;; returned value is the primary value. When the callee returns only a single
;;; value then it does not update VALUES vector to avoid global memory writes.
;;; This is why LVALs that accept multiple values must assign VALUES[0] when the
;;; (USES-VALUES RVAL) returns T. -- jd 2023-12-14

(defun set-unknown-loc (destination loc)
  (unknown-location 'set-loc destination))

(defun set-loc (destination loc &aux fd)
  (cond ((eq destination loc))
        ((symbolp destination)
         (funcall (gethash destination *set-loc-dispatch-table* 'set-unknown-loc)
                  loc))
        ((var-p destination)
         (set-var loc destination))
        ((vv-p destination)
         (set-vv loc destination))
        ((atom destination)
         (unknown-location 'set-loc destination loc))
        (t
         (ext:if-let ((fd (gethash (first destination) *set-loc-dispatch-table*)))
           (apply fd loc (rest destination))
           (progn
             (wt-nl)
             (wt-loc destination)
             (wt " = " (coerce-loc (loc-host-type destination) loc) ";"))))))

(defun set-trash-loc (loc &rest args)
  (declare (ignore args))
  (when (loc-with-side-effects-p loc)
    (wt-nl loc ";")
    t))

(defun set-value0-loc (loc)
  (wt-nl "value0 = " (coerce-loc :object loc) ";"))

(defun set-the-loc (loc type orig-loc)
  (declare (ignore type))
  (set-loc orig-loc loc))

(defun set-leave-loc (loc)
  (cond ((or (eq loc 'VALUEZ) (uses-values loc))
         (wt-nl "value0 = " (coerce-loc :object loc) ";"))
        ((eq loc 'VALUE0)
         (wt-nl "cl_env_copy->nvalues = 1;"))
        ((eq loc 'LEAVE))
        (t
         (wt-nl "value0 = " (coerce-loc :object loc) ";")
         (wt-nl "cl_env_copy->nvalues = 1;"))))

(defun set-valuez-loc (loc)
  (cond ((eq loc 'VALUEZ))
        ((uses-values loc)
         (wt-nl "cl_env_copy->values[0] = " (coerce-loc :object loc) ";"))
        (t
         (wt-nl "cl_env_copy->values[0] = " (coerce-loc :object loc) ";")
         (wt-nl "cl_env_copy->nvalues = 1;"))))

;;;
;;; Foreign data
;;;

(defun wt-ffi-data-ref (data ffi-tag)
  (wt "ecl_foreign_data_ref_elt(&" data "," ffi-tag ")"))

(defun wt-ffi-data-set (value data ffi-tag)
  (wt "ecl_foreign_data_set_elt(&" data "," ffi-tag "," value ");"))
