;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2021, Daniel Kochmański
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;

(in-package #:compiler)

(defun c2compiler-let (c1form symbols values body)
  (declare (ignore c1form))
  (progv symbols values (c2expr body)))

(defun c2function (c1form kind funob fun)
  (declare (ignore c1form funob))
  (case kind
    (GLOBAL
     (unwind-exit (list 'FDEFINITION fun)))
    (CLOSURE
     ;; XXX: we have some code after baboon – is CLOSURE legal or not?
     (baboon :format-control "c2function: c1form is of unexpected kind.")
     (new-local fun)
     (unwind-exit `(MAKE-CCLOSURE ,fun)))))

;;; Mechanism for sharing code.
(defun new-local (fun)
  ;; returns the previous function or NIL.
  (declare (type fun fun))
  (case (fun-closure fun)
    (CLOSURE
     (setf (fun-level fun) 0 (fun-env fun) *env*))
    (LEXICAL
     ;; Only increase the lexical level if there have been some
     ;; new variables created. This way, the same lexical environment
     ;; can be propagated through nested FLET/LABELS.
     (setf (fun-level fun) (if (plusp *lex*) (1+ *level*) *level*)
           (fun-env fun) 0))
    (otherwise
     (setf (fun-env fun) 0 (fun-level fun) 0)))
  (let ((previous
         nil
         #+(or)
         (dolist (old *local-funs*)
           (when (similar fun old)
             (return old)))))
    (if previous
        (progn
          (if (eq (fun-closure fun) 'CLOSURE)
              (cmpnote "Sharing code for closure")
              (cmpnote "Sharing code for local function ~A" (fun-name fun)))
          (setf (fun-cfun fun) (fun-cfun previous)
                (fun-lambda fun) nil)
          previous)
        (push fun *local-funs*))))

(defun wt-fdefinition (fun-name)
  (let* ((name (si::function-block-name fun-name))
         (package (symbol-package name))
         (safe (or (not (safe-compile))
                   (and (or (eq package (find-package "CL"))
                            (eq package (find-package "CLOS"))
                            (eq package (find-package "SI")))
                        (fboundp fun-name)
                        (functionp (fdefinition fun-name))))))
    (if (eq name fun-name)
        ;; #'symbol
        (let ((vv (add-symbol name)))
          (if safe
              (wt "(" vv "->symbol.gfdef)")
              (wt "ecl_fdefinition(" vv ")")))
        ;; #'(SETF symbol)
        (if safe
            #+(or)
            (let ((set-loc (assoc name *setf-definitions*)))
              (unless set-loc
                (let* ((setf-vv (data-empty-loc))
                       (name-vv (add-symbol name))
                       (setf-form-vv (add-object fun-name)))
                  (setf set-loc (list name setf-vv name-vv setf-form-vv))
                  (push set-loc *setf-definitions*)))
              (wt "ECL_SETF_DEFINITION(" (second set-loc) "," (fourth set-loc) ")"))
            (let ((set-loc (assoc name *setf-definitions*)))
              (unless set-loc
                (let* ((setf-vv (data-empty-loc))
                       (name-vv (add-symbol name)))
                  (setf set-loc (list name setf-vv name-vv))
                  (push set-loc *setf-definitions*)))
              (wt "ECL_CONS_CAR(" (second set-loc) ")"))
            (let ((vv (add-symbol fun-name)))
              (wt "ecl_fdefinition(" vv ")"))))))

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
