;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;; CMPPROP Type propagation.

(in-package "COMPILER")

(eval-when (:compile-toplevel :execute)
(defconstant +all-c1-forms+
  '(;; top-level forms
    (ORDINARY           c1form :pure)
    (MAKE-FORM          vv-loc value-c1form :side-effects)
    (INIT-FORM          vv-loc value-c1form :side-effects)
    ;; both-level forms (different semantics)
    (EXT:COMPILER-LET   symbols values body)
    (SI:FSET            function-object vv-loc macro-p pprint-p lambda-form :side-effects)
    (CL:LOAD-TIME-VALUE dest-loc value-c1form :pure :single-valued)
    (CL:PROGN           body :pure)
    ;; sub-level forms
    (LOCATION           loc :pure :single-valued)
    (VAR                var :single-valued)
    (CL:SETQ            var value-c1form :side-effects)
    (CL:PSETQ           var-list value-c1form-list :side-effects)
    (CL:BLOCK           blk-var progn-c1form :pure)

    (CL:PROGV           symbols values form :side-effects)
    (CL:TAGBODY         tag-var tag-body :pure)
    (CL:RETURN-FROM     blk-var nonlocal value :side-effects)
    (FCALL              fun-value (arg-value*) :side-effects)
    (MCALL              fun-value (arg-value*) :side-effects)
    (CALL-LOCAL         obj-fun (arg-value*) :side-effects)
    (CALL-GLOBAL        fun-name (arg-value*))
    (CL:CATCH           catch-value body :side-effects)
    (CL:UNWIND-PROTECT  protected-c1form body :side-effects)
    (CL:THROW           catch-value output-value :side-effects)
    (CL:GO              tag-var nonlocal :side-effects)

    (FFI:C-INLINE       (arg-c1form*)
                        (arg-type-symbol*)
                        output-rep-type
                        c-expression-string
                        side-effects-p
                        one-liner-p)
    (FFI:C-PROGN        variables forms)

    (LOCALS             local-fun-list body labels-p :pure)
    (CL:IF              fmla-c1form true-c1form false-c1form :pure)
    (FMLA-NOT           fmla-c1form :pure)
    (FMLA-AND           * :pure)
    (FMLA-OR            * :pure)
    (CL:LAMBDA          lambda-list doc body-c1form)
    (CL:LET*            vars-list var-init-c1form-list decl-body-c1form :pure)
    (CL:VALUES          values-c1form-list :pure)
    (CL:MULTIPLE-VALUE-SETQ vars-list values-c1form-list :side-effects)
    (CL:MULTIPLE-VALUE-BIND vars-list init-c1form body :pure)

    (CL:FUNCTION        (GLOBAL/CLOSURE) lambda-form fun-object :single-valued)
    (CL:RPLACD          (dest-c1form value-c1form) :side-effects)

    (SI:STRUCTURE-REF   struct-c1form type-name slot-index (:UNSAFE/NIL) :pure)
    (SI:STRUCTURE-SET   struct-c1form type-name slot-index value-c1form :side-effects)

    (MV-PROG1           form body :side-effects)

    (ext:COMPILER-TYPECASE var expressions)
    (ext:CHECKED-VALUE  type value-c1form let-form))))

(defconstant +c1-form-hash+
  #.(loop with hash = (make-hash-table :size 128 :test #'eq)
       for (name . rest) in +all-c1-forms+
       for length = (if (member '* rest) nil (length rest))
       for side-effects = (if (member :side-effects rest)
                              (progn (and length (decf length)) t)
                              nil)
       for movable = (if (member :pure rest)
                         (progn (and length (decf length)) t)
                         nil)
       for single-valued = (if (member :single-valued rest)
                               (progn (and length (decf length)) t)
                               nil)
       do (setf (gethash name hash) (list length side-effects movable single-valued))
       finally (return hash)))

(defconstant +c1-dispatch-alist+
  '((cl:block . c1block) ; c1special
    (cl:return-from . c1return-from) ; c1special
    (cl:funcall . c1funcall) ; c1
    (cl:catch . c1catch) ; c1special
    (cl:unwind-protect . c1unwind-protect) ; c1special
    (cl:throw . c1throw) ; c1special
    (ffi:defcallback . c1-defcallback) ; c1
    (cl:progn . c1progn) ; c1special
    (ext:with-backend . c1with-backend) ; c1special
    (ffi:clines . c1clines) ; c1special
    (ffi:c-inline . c1c-inline) ; c1special
    (ffi:c-progn . c1c-progn) ; c1special
    (cl:flet . c1flet) ; c1special
    (cl:labels . c1labels) ; c1special
    (cl:locally . c1locally) ; c1special
    (cl:macrolet . c1macrolet) ; c1special
    (cl:symbol-macrolet . c1symbol-macrolet) ; c1special

    (cl:if . c1if) ; c1special
    (cl:not . c1not) ; c1special
    (cl:and . c1and) ; c1special
    (cl:or . c1or) ; c1special

    (cl:let . c1let) ; c1special
    (cl:let* . c1let*) ; c1special

    (cl:multiple-value-call . c1multiple-value-call) ; c1special
    (cl:multiple-value-prog1 . c1multiple-value-prog1) ; c1special
    (cl:values . c1values) ; c1
    (cl:multiple-value-setq . c1multiple-value-setq) ; c1
    (cl:multiple-value-bind . c1multiple-value-bind) ; c1

    (ext:compiler-typecase . c1compiler-typecase) ; c1special
    (ext:checked-value . c1checked-value) ; c1special

    (cl:quote . c1quote) ; c1special
    (cl:function . c1function) ; c1special
    (cl:the . c1the) ; c1special
    (ext:truly-the . c1truly-the) ; c1special
    (cl:eval-when . c1eval-when) ; c1special
    (cl:declare . c1declare) ; c1special
    (ext:compiler-let . c1compiler-let) ; c1special

    (cl:tagbody . c1tagbody) ; c1special
    (cl:go . c1go) ; c1special

    (cl:setq . c1setq) ; c1special
    (cl:progv . c1progv) ; c1special
    (cl:psetq . c1psetq) ; c1special

    (cl:load-time-value . c1load-time-value) ; c1

    (cl:apply . c1apply) ; c1
    ))

(defconstant +t1-dispatch-alist+
  '((ext:with-backend . c1with-backend) ; t1

    (cl:defmacro . t1defmacro)
    (ext:compiler-let . c1compiler-let)
    (cl:eval-when . c1eval-when)
    (cl:progn . c1progn)
    (cl:macrolet . c1macrolet)
    (cl:locally . c1locally)
    (cl:symbol-macrolet . c1symbol-macrolet)
    (si:fset . t1fset)
    ))

(defconstant +set-loc-dispatch-alist+
  '((bind . bind)
    (jump-true . set-jump-true)
    (jump-false . set-jump-false)

    (cl:values . set-values-loc)
    (value0 . set-value0-loc)
    (cl:return . set-return-loc)
    (trash . set-trash-loc)

    (cl:the . set-the-loc)
    ))

(defconstant +wt-loc-dispatch-alist+
  '((call-normal . wt-call-normal)
    (call-indirect . wt-call-indirect)
    (call-stack . wt-call-stack)
    (ffi:c-inline . wt-c-inline-loc)
    (coerce-loc . wt-coerce-loc)

    (temp . wt-temp)
    (lcl . wt-lcl-loc)
    (fixnum-value . wt-fixnum)
    (long-float-value . wt-number)
    (double-float-value . wt-number)
    (single-float-value . wt-number)
    (short-float-value . wt-number)
    (csfloat-value . wt-number)
    (cdfloat-value . wt-number)
    (clfloat-value . wt-number)
    (character-value . wt-character)
    (value . wt-value)
    (keyvars . wt-keyvars)
    (cl:the . wt-the)

    (cl:fdefinition . wt-fdefinition)
    (make-cclosure . wt-make-closure)

    (si:structure-ref . wt-structure-ref)

    (cl:nil . "ECL_NIL")
    (cl:t . "ECL_T")
    (cl:return . "value0")
    (cl:values . "cl_env_copy->values[0]")
    (va-arg . "va_arg(args,cl_object)")
    (cl-va-arg . "ecl_va_arg(args)")
    (value0 . "value0")
    ))

(defconstant +c2-dispatch-alist+
  '((cl:block . c2block)
    (cl:return-from . c2return-from)
    (fcall . c2fcall)
    (mcall . c2mcall)
    (call-global . c2call-global)
    (cl:catch . c2catch)
    (cl:unwind-protect . c2unwind-protect)
    (cl:throw . c2throw)
    (cl:progn . c2progn)
    (ffi:c-inline . c2c-inline)
    (ffi:c-progn . c2c-progn)
    (locals . c2locals)
    (call-local . c2call-local)

    (cl:if . c2if)
    (fmla-not . c2fmla-not)
    (fmla-and . c2fmla-and)
    (fmla-or . c2fmla-or)

    (cl:let* . c2let*)

    (cl:values . c2values)
    (cl:multiple-value-setq . c2multiple-value-setq)
    (cl:multiple-value-bind . c2multiple-value-bind)

    (cl:function . c2function)
    (ext:compiler-let . c2compiler-let)

    (mv-prog1 . c2mv-prog1)

    (cl:tagbody . c2tagbody)
    (cl:go . c2go)

    (var . c2var/location)
    (location . c2var/location)
    (cl:setq . c2setq)
    (cl:progv . c2progv)
    (cl:psetq . c2psetq)

    (si:fset . c2fset)

    (ext:compiler-typecase . c2compiler-typecase)
    (ext:checked-value . c2checked-value)
    ))

(defconstant +t2-dispatch-alist+
  '((ext:compiler-let . t2compiler-let)
    (cl:progn . t2progn)
    (ordinary . t2ordinary)
    (cl:load-time-value . t2load-time-value)
    (make-form . t2make-form)
    (init-form . t2init-form)
    (si:fset . t2fset)
    ))

(defconstant +p1-dispatch-alist+
  '((cl:block . p1block)
    (cl:return-from . p1return-from)
    (fcall . p1trivial)
    (mcall . p1trivial)
    (call-global . p1call-global)
    (call-local . p1call-local)
    (cl:catch . p1catch)
    (cl:throw . p1throw)
    (cl:if . p1if)
    (fmla-not . p1fmla-not)
    (fmla-and . p1fmla-and)
    (fmla-or . p1fmla-or)
    (cl:lambda . p1lambda)
    (cl:let* . p1let*)
    (locals . p1locals)
    (cl:multiple-value-bind . p1multiple-value-bind)
    (cl:multiple-value-setq . p1multiple-value-setq)
    (cl:progn . p1progn)
    (cl:progv . p1progv)
    (cl:setq . p1setq)
    (cl:psetq . p1psetq)
    (cl:tagbody . p1tagbody)
    (cl:go . p1go)
    (cl:unwind-protect . p1unwind-protect)
    (ordinary . p1ordinary)
    (si:fset . p1fset)
    (var . p1var)
    (cl:values . p1values)
    (location . p1trivial) ;; Some of these can be improved
    (ffi:c-inline . p1trivial)
    (ffi:c-progn . p1trivial)
    (cl:function . p1trivial)
    (cl:load-time-value . p1trivial)
    (make-form . p1trivial)
    (init-form . p1trivial)
    (mv-prog1 . p1mv-prog1)

    (ext:compiler-typecase . p1compiler-typecase)
    (ext:checked-value . p1checked-value)
    ))

(defun make-dispatch-table (alist)
  (loop with hash = (make-hash-table :size (max 128 (* 2 (length alist)))
                                     :test #'eq)
     for (name . function) in alist
     do (setf (gethash name hash) function)
     finally (return hash)))

(defparameter *c1-dispatch-table* (make-dispatch-table +c1-dispatch-alist+))

(defparameter *t1-dispatch-table* (make-dispatch-table +t1-dispatch-alist+))

(defparameter *c2-dispatch-table* (make-dispatch-table +c2-dispatch-alist+))

(defparameter *set-loc-dispatch-table* (make-dispatch-table +set-loc-dispatch-alist+))

(defparameter *wt-loc-dispatch-table* (make-dispatch-table +wt-loc-dispatch-alist+))

(defparameter *t2-dispatch-table* (make-dispatch-table +t2-dispatch-alist+))

(defparameter *p1-dispatch-table* (make-dispatch-table +p1-dispatch-alist+)
  "Dispatch table for type propagators associated to C1FORMs.")

(defparameter *p0-dispatch-table* (make-dispatch-table '())
  "Type propagators for known functions.")

(defparameter *cinline-dispatch-table* (make-dispatch-table '()))
