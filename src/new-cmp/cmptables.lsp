;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DISPATCH TABLES FOR FRONT-ENDS AND BACK-ENDS
;;;

;;; ----------------------------------------------------------------------
;;; CONSTRUCTORS
;;;

(defun make-dispatch-table (pairs)
  (loop with output = (make-hash-table :size (* 2 (length pairs)) :test #'eq)
     for (name . function) in pairs
     do (setf (gethash name output) function)
     finally (return output)))

(defun extend-dispatch-table (pairs table)
  (loop with output = (make-dispatch-table pairs)
     for k being the hash-key in output using (hash-value v)
     do (setf (gethash output k) v)
     finally (return output)))

;;; ------------------------------------------------------------------
;;; COMMON LISP FORMS TRANSLATORS
;;;

(defconstant +c1-dispatch-data+
 '(
   ;; cmpblock.lsp
   (block . c1block)
   (return-from . c1return-from)

   ;; cmpcall.lsp
   (funcall . c1funcall)
   
   ;; cmpcatch.lsp
   (catch . c1catch)
   (throw . c1throw)
   (unwind-protect . c1unwind-protect)

   ;; cmpcbk.lsp
   ;; (ffi:defcallback . c1-defcallback)

   ;; cmpeval
   (progn . c1progn)

   ;; cmpffi.lsp
   (ffi:clines . c1clines)
   (ffi:c-inline . c1c-inline)

   ;; cmpflet
   (flet . c1flet)
   (labels . c1labels)
   (do-flet/labels . c1do-flet/labels)
   (make-flet/labels-closure . c1make-flet/labels-closure)
   (locally . c1locally)
   (macrolet . c1macrolet)
   (symbol-macrolet . c1symbol-macrolet)

   ;; cmpfun.lsp
   (apply . c1apply)

   ;; cmpif.lsp
   (if . c1if)

   ;; cmplet.lsp
   (let . c1let)
   (let* . c1let*)

   ;; cmpmulti.lsp
   (multiple-value-call . c1multiple-value-call)
   (multiple-value-prog1 . c1multiple-value-prog1)
   (values . c1values)
   (multiple-value-setq . c1multiple-value-setq)
   (multiple-value-bind . c1multiple-value-bind)

   ;; cmpspecial.lsp
   (quote . c1quote)
   (function . c1function)
   (the . c1the)
   (eval-when . c1eval-when)
   (declare . c1declare)
   (ext:compiler-let . c1compiler-let)

   ;; cmpstack.lsp
   (with-stack . c1with-stack)
   (stack-push . c1stack-push)
   (stack-push-values . c1stack-push-values)
   (stack-pop . c1stack-pop)
   (si::apply-from-stack-frame . c1apply-from-stack-frame)

   ;; cmpstructures.lsp
   ;; (sys::structure-ref . c1structure-ref)
   ;; (sys::structure-set . c1structure-set)

   ;; cmptag.lsp
   (tagbody . c1tagbody)
   (go . c1go)

   ;; cmptop.lsp
   (load-time-value . c1load-time-value)
   (si:fset . c1fset)

   ;; cmptranslate.lsp
   (values-ref . c1values-ref)

   ;; cmpvar.lsp
   (setq . c1setq)
   (psetq . c1psetq)
   (progv . c1progv)
   ))

(defparameter +c1-dispatch-table+ (make-dispatch-table +c1-dispatch-data+))

;;; ------------------------------------------------------------------
;;; C/C++ BACKEND
;;;

(defparameter +c2-dispatch-table+
 (make-dispatch-table
  '(
    (set . c2set)
    (set-mv . c2set-mv)
    (values . c2values-op)
    (bind . c2bind)
    (bind-special . c2bind-special)
    (progv . c2progv-op)
    (unbind . c2unbind)
    (progv-exit . c2progv-exit-op)
    (frame-pop . c2frame-pop)
    (frame-set . c2frame-set)
    (frame-save-next . c2frame-save-next)
    (frame-jmp-next . c2frame-jmp-next)
    (frame-id . c2frame-id)
    (jmp . c2jmp)

    (function-prologue . c2function-prologue)
    (function-epilogue . c2function-epilogue)
    
    (bind-required . c2bind-required)
    (varargs-bind . c2varargs-bind-op)
    (varargs-pop . c2varargs-pop-op)
    (varargs-rest . c2varargs-rest-op)
    (varargs-unbind . c2varargs-unbind-op)

    (stack-frame-open . c2stack-frame-open)
    (stack-frame-push . c2stack-frame-push)
    (stack-frame-push-values . c2stack-frame-push-values)
    (stack-frame-pop-values . c2stack-frame-pop-values)
    (stack-frame-apply . c2stack-frame-apply)
    (stack-frame-close . c2stack-frame-close)

    (throw . c2throw-op)
    (return-from . c2return-from-op)
    (go . c2go-op)
    (funcall . c2funcall-op)
    (call-local . c2call-local)
    (call-global . c2call-global)

    (debug-env-open . c2debug-env-open)
    (debug-env-close . c2debug-env-close)
    (debug-env-push-vars . c2debug-env-push-vars)
    (debug-env-pop-vars . c2debug-env-pop-vars)

    ;; cmpffi.lsp
    (ffi:c-inline . c2c-inline)
    
    ;; cmpflet.lsp
    (do-flet/labels . c2do-flet/labels)

    ;; cmpstructures.lsp
    ;; (sys:structure-ref . c2structure-ref)
    ;; (sys:structure-set . c2structure-set)

    ;; cmptop.lsp
    (si:fset . c2fset)
    )))

(defparameter +c2-wt-loc-table+
 (make-dispatch-table
  '(
    ;; cmploc.lsp
    (temp . wt-temp)
    (lcl . wt-lcl-loc)
    (vv . wt-vv)
    (vv-temp . wt-vv-temp)
    (car . wt-car)
    (cdr . wt-cdr)
    (cadr . wt-cadr)
    (fixnum-value . wt-number)
    (character-value . wt-character)
    (long-float-value . wt-number)
    (double-float-value . wt-number)
    (single-float-value . wt-number)
    (value . wt-value)
    (keyvars . wt-keyvars)
    (the . wt-the-loc)

    (nil . wt-nil-loc)
    (t . wt-t-loc)
    (value0 . wt-value0-loc)
    (return . wt-value0-loc)
    (values+value0 . wt-value0-loc)
    (values . wt-values-loc)
    (va-arg . wt-va-arg-loc)
    (cl-va-arg . wt-cl-va-arg-loc)

    ;; cmpbackend.lsp
    (call . wt-call)
    (call-normal . wt-call-normal)
    (call-indirect . wt-call-indirect)

    ;; cmpffi.lsp
    (ffi:c-inline . wt-c-inline-loc)
    (coerce-loc . wt-coerce-loc)

    ;; cmpspecial.ls
    (fdefinition . wt-fdefinition)
    (make-cclosure . wt-make-closure)

    ;; cmpstructures.lsp
    (sys:structure-ref . wt-structure-ref)
    )))

(defparameter +c2-set-loc-table+
 (make-dispatch-table
  '(
    ;; cmpbind.lsp
    (bind . bind)

    ;; cmploc.lsp
    (values . set-values-loc)
    (values+value0 . set-values+value0-loc)
    (value0 . set-value0-loc)
    (return . set-return-loc)
    (actual-return . set-actual-return-loc)
    (trash . set-trash-loc)
    (the . set-the-loc)

    ;; cmpbackend.lsp
    (jmp-true . set-loc-jmp-true)
    (jmp-false . set-loc-jmp-false)
    (jmp-zero . set-loc-jmp-false)
    )))

