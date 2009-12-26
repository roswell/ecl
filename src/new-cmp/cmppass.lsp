;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPPASS  Optimization passes
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia Ripoll.
;;;;
;;;;    ECL is free software; you can redistribute it and/or modify it
;;;;    under the terms of the GNU Library General Public License as
;;;;    published by the Free Software Foundation; either version 2 of
;;;;    the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ALL C1FORMS
;;;
;;;	BIND			(var1 ... varN)
;;;	BIND-REQUIREDS		((var1 . arg1-loc) ... (varN . argN-loc))
;;;	BIND-SPECIAL		destination value-loc
;;;	CALL-LOCAL		destination fun (arg1 ... argN)
;;;	CALL-GLOBAL		destination fun (arg1 ... argN)
;;;	C-INLINE
;;;	DEBUG-ENV-OPEN		fun-name
;;;	DEBUG-ENV-PUSH-VARS	(var1 ... varN)
;;;	DEBUG-ENV-POP-VARS	(var1 ... varN) close-block
;;;	DEBUG-ENV-CLOSE		fun-name
;;;	DO-FLET/LABELS		(fun1 ... funN)
;;;	FRAME-ID		frame-var
;;;	FRAME-JMP-NEXT		frame-var
;;;	FRAME-POP		frame-var
;;;	FRAME-SAVE-NEXT		frame-var
;;;	FRAME-SET		id-loc no-label
;;;	FUNCALL			destination (arg1 ... argN)
;;;	FUNCTION-PROLOGUE	fun
;;;	FUNCTION-EPILOGUE	fun
;;;	GO			tag
;;;	JMP			tag
;;;	PROGV			ndx-loc (var1-loc ... varN-loc) values-loc
;;;	PROGV-EXIT		ndx-loc
;;;	SET			destination source
;;;	SET-MV			(dest-loc1 ... dest-locN) min-args max-args
;;;	SI:STRUCTURE-REF
;;;	SI:STRUCTURE-SET
;;;	STACK-FRAME-OPEN	frame-var
;;;	STACK-FRAME-PUSH	frame-var value-loc
;;;	STACK-FRAME-PUSH-VALUES	frame-var
;;;	STACK-FRAME-POP-VALUES	frame-var
;;;	STACK-FRAME-APPLY	frame-var fun-loc
;;;	STACK-FRAME-CLOSE	frame-var
;;;	RETURN-FROM		block-id-var block-name
;;;	THROW			tag-loc
;;;	UNBIND			(var1 ... varN)
;;;	VALUES			(value1-loc ... valueN-loc)
;;;	VARARGS-BIND		nargs-loc varargs-loc min max nkeys check
;;;	VARARGS-POP		dest-loc nargs-loc varargs-loc
;;;	VARARGS-REST		dest-loc nargs-loc varargs-loc nkeys
;;;				keys-list-loc allow-other-keys
;;;	VARARGS-UNBIND		nargs-loc varargs-loc min max nkeys check
;;;

(in-package "COMPILER")

(defun execute-pass (pass)
  (cmpnote "Executing pass ~A" pass)
  (loop with pending = (list *top-level-forms*)
     for *current-function* = (pop pending)
     for f = *current-function*
     while f
     do (cmpnote "Applying pass ~A on function ~A" pass f)
     do (setf (fun-lambda f) (funcall pass f (fun-lambda f)))
     do (setf pending (append (fun-child-funs f) pending))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DELETE UNUSED FORMS
;;;

(defun pass-delete-no-side-effects (function forms)
  "Going backwards, we examime forms that cause no side effects and whose
output value is not used."
  (nreverse (delete-if #'delete-if-no-side-effects
                       (nreverse forms))))

(defun delete-if-no-side-effects (form)
  (pprint-c1form form) 
  (when (c1form-p form)
    (case (c1form-name form)
      ((LOCATION VAR SYS:STRUCTURE-REF #+clos SYS:INSTANCE-REF)
       t)
      ((BIND UNBIND)
       (every #'unused-variable-p (c1form-arg 0 form)))
      (CALL-GLOBAL
       (let* ((form-args (c1form-args form))
              (destination (first form-args))
              (fname (second form-args))
              (args (third form-args)))
         (cond ((function-may-have-side-effects fname)
                nil)
               ((unused-destination destination)
                (loop for i in args do (eliminate-from-read-nodes i form))
                (eliminate-from-set-nodes destination form)
                t)
               (t nil))))
      (SET
       (let* ((destination (c1form-arg 0 form))
              (source (c1form-arg 1 form)))
         (when (unused-destination destination)
           (eliminate-from-set-nodes destination form)
           (eliminate-from-read-nodes destination form)
           (eliminate-from-set-nodes source form)
           (eliminate-from-read-nodes source form)
           t)))
      (t nil))))

(defun unused-destination (dest)
  (when (var-p dest)
    (print (var-read-nodes dest)))
  (or (eq dest 'trash)
      (and (var-p dest)
           (unused-variable-p dest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DELETE UNUSED BINDINGS
;;;

(defun pass-delete-unused-bindings (function forms)
  "We eliminate all unused variables, including their bindings. Empty BIND/UNBIND
forms are also suppressed."
  (labels ((unused-variable-binding-p (v)
             (unused-variable-p (if (consp v) (car v) v)))
           (unused-bindings (form)
             (and (c1form-p form)
                  (member (c1form-name form) '(BIND UNBIND BIND-REQUIREDS))
                  (let ((new-args (delete-if #'unused-variable-binding-p
                                             (c1form-arg 0 form))))
                    (setf (c1form-args form) (list new-args))
                    (null new-args)))))
    (delete-if #'unused-bindings forms)
    (setf (fun-local-vars fun)
          (delete #'unused-variable-p (fun-local-vars fun))
          (fun-referred-vars fun)
          (delete #'unused-variable-p (fun-referred-vars fun)))))

