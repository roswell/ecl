;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPIF  Conditionals.

(in-package "COMPILER")

(defun c1if (args)
  (check-args-number 'IF args 2 3)
  (let ((f (c1fmla-constant (car args))))
    (case f
      ((T) (c1expr (second args)))
      ((NIL) (if (endp (cddr args)) (c1nil) (c1expr (third args))))
      (otherwise
       (make-c1form* 'IF :args (c1expr f) (c1expr (second args))
		     (if (endp (cddr args)) (c1nil) (c1expr (third args))))))
    ))

(defun c1not (args)
  (check-args-number 'NOT args 1)
  (let* ((value (first args))
         (f (c1fmla-constant value)))
    (if (or (eq f t) (eq f nil))
        (c1expr (not f))
        (let* ((value (c1expr (first args))))
          (make-c1form* 'FMLA-NOT
                        :type '(member t nil)
                        :args value)))))

(defun c1and (args)
  (let ((f (c1fmla-constant `(AND ,@args))))
    (cond ((or (eq f t) (eq f nil) (atom f))
           (c1expr f))
          ((null args)
           (c1t))
          ((null (rest args))
           (c1expr (first args)))
          (t
           (let* ((values (c1args* args))
                  (last (first (last values)))
		  (butlast (nbutlast values)))
             (make-c1form* 'FMLA-AND
                           :type (type-or 'null (c1form-type last))
                           :args butlast last))))))

(defun c1or (args)
  (if (null args)
      (c1nil)
      (let* ((values (c1args* args))
	     (last (first (last values)))
	     (butlast (butlast values)))
	(if butlast
	    (make-c1form* 'FMLA-OR
			 :type (type-or 'null (c1form-type last))
			 :args butlast last)
	    last))))

(defun resolve-constants (list)
  (mapcar #'(lambda (x)
              (if (constantp x)
                  (and (cmp-eval x) t)
                  x))
          list))

(defun c1fmla-constant (fmla &aux f)
  (cond
   ((constant-expression-p fmla)
    (and (cmp-eval fmla) t))
   ((atom fmla)
    fmla)
   ((eq (setf f (car fmla)) 'AND)
    (let* ((simplified (delete t (mapcar #'c1fmla-constant (rest fmla)))))
      (cond ((null simplified)
             t) ; (AND)
            ((rest simplified)
             `(AND ,@simplified))
            (t
             (first simplified)))))
   ((eq f 'OR)
    (let* ((simplified (delete nil (mapcar #'c1fmla-constant (rest fmla)))))
      (cond ((null simplified)
             nil) ; (OR)
            ((rest simplified)
             `(OR ,@simplified))
            (t
             (first simplified)))))
   ((member f '(NOT NULL))
    (when (endp (cdr fmla)) (too-few-args 'not 1 0))
    (unless (endp (cddr fmla))
      (too-many-args 'not 1 (length (cdr fmla))))
    (setq f (c1fmla-constant (second fmla)))
    (case f
      ((T) nil)
      ((NIL) t)
      (t (list 'NOT f))))
   ((or (get-sysprop f 'C1)
        (get-sysprop f 'C1SPECIAL)
        (get-sysprop f 'C1CONDITIONAL))
    fmla)
   ((let ((fd (compiler-macro-function f)))
      (and fd
           (inline-possible f)
           (let ((success nil))
             (multiple-value-setq (fmla success)
               (cmp-expand-macro fd fmla))
             success)
           (c1fmla-constant fmla))))
   ((let ((fd (cmp-macro-function f)))
      (and fd (c1fmla-constant (cmp-expand-macro fd fmla)))))
   (t fmla)))

(eval-when (:compile-toplevel :execute)
(defmacro with-exit-label ((label) &body body)
  `(let* ((,label (next-label))
	  (*unwind-exit* (cons ,label *unwind-exit*)))
     ,@body
     (wt-label ,label))))

(defun c2if (fmla form1 form2)
  ;; FIXME! Optimize when FORM1 or FORM2 are constants
  (with-exit-label (normal-exit)
    (with-exit-label (false-label)
      (let ((*destination* `(JUMP-FALSE ,false-label)))
	(c2expr* fmla))
      (c2expr form1))
    (c2expr form2)))

(defun negate-argument (inlined-arg dest-loc)
  (let* ((loc (second inlined-arg))
         (rep-type (loc-representation-type loc)))
    (apply #'produce-inline-loc
           (list inlined-arg)
           (if (eq (loc-representation-type dest-loc) :bool)
               (case rep-type
                 (:bool '((:bool) (:bool) "(#0)==Cnil" nil t))
                 (:object '((:object) (:bool) "(#0)!=Cnil" nil t))
                 (otherwise (return-from negate-argument nil)))
               (case rep-type
                 (:bool '((:bool) (:object) "(#0)?Cnil:Ct" nil t))
                 (:object '((:object) (:object) "Null(#0)?Ct:Cnil" nil t))
                 (otherwise (return-from negate-argument nil)))))))

(defun c2fmla-not (arg)
  (let ((dest *destination*))
    (cond ((and (consp dest) (eq (car dest) 'JUMP-TRUE))
           (let ((*destination* `(JUMP-FALSE ,@(cdr dest))))
             (c2expr arg)))
          ((and (consp dest) (eq (car dest) 'JUMP-FALSE))
           (let ((*destination* `(JUMP-TRUE ,@(cdr dest))))
             (c2expr arg)))
          (t
           (let ((*inline-blocks* 0)
                 (*temp* *temp*))
             (unwind-exit (negate-argument
                           (emit-inline-form arg t nil)
                           *destination*))
             (close-inline-blocks))))))

(defun jump-true-destination? ()
  (let ((dest *destination*))
    (and (consp dest) (eq (car dest) 'JUMP-TRUE))))

(defun jump-false-destination? ()
  (let ((dest *destination*))
    (and (consp dest) (eq (car dest) 'JUMP-FALSE))))

(defun c2fmla-and (butlast last)
  (if (jump-false-destination?)
      (progn
	(mapc #'c2expr* butlast)
	(c2expr last))
      (with-exit-label (normal-exit)
	(with-exit-label (false-label)
	  (let ((*destination* `(JUMP-FALSE ,false-label)))
	    (mapc #'c2expr* butlast))
	  (c2expr last))
	(unwind-exit nil))))

(defun c2fmla-or (butlast last)
  (cond ((jump-true-destination?)
	 (mapc #'c2expr* butlast)
	 (c2expr last))
	((jump-false-destination?)
	 (with-exit-label (true-label)
	   (let ((*destination* `(JUMP-TRUE ,true-label)))
	     (mapc #'c2expr* butlast))
	   (c2expr last))
	 (unwind-exit t))
	(t
	 (with-exit-label (normal-exit)
	   (dolist (f butlast)
	     (let ((*destination* 'VALUE0))
	       (c2expr* f))
	     (set-jump-true 'VALUE0 normal-exit))
	   (let ((*destination* 'VALUE0))
	     (c2expr* last)))
	 (unwind-exit 'VALUE0))))

(defun set-jump-true (loc label)
  (multiple-value-bind (constantp value)
      (loc-immediate-value-p loc)
    (cond ((not constantp)
           (cond ((eq (loc-representation-type loc) :bool)
                  (wt-nl "if(" loc "){"))
                 (t
                  (wt-nl "if((")
                  (wt-coerce-loc :object loc)
                  (wt ")!=Cnil){")))
           (unwind-no-exit label)
           (wt-nl) (wt-go label)
           (wt "}"))
          ((null value))
          (t
           (unwind-no-exit label)
           (wt-nl) (wt-go label)))))

(defun set-jump-false (loc label)
  (multiple-value-bind (constantp value)
      (loc-immediate-value-p loc)
    (cond ((not constantp)
           (cond ((eq (loc-representation-type loc) :bool)
                  (wt-nl "if(!(" loc ")){"))
                 (t
                  (wt-nl "if((")
                  (wt-coerce-loc :object loc)
                  (wt ")==Cnil){")))
           (unwind-no-exit label)
           (wt-nl) (wt-go label)
           (wt "}"))
          (value)
          (t
           (unwind-no-exit label)
           (wt-nl) (wt-go label)))))

;;; ----------------------------------------------------------------------

(put-sysprop 'if 'c1special 'c1if)
(put-sysprop 'if 'c2 'c2if)
(put-sysprop 'not 'c1 'c1not)
(put-sysprop 'fmla-not 'c2 'c2fmla-not)
(put-sysprop 'and 'c1 'c1and)
(put-sysprop 'fmla-and 'c2 'c2fmla-and)
(put-sysprop 'or 'c1 'c1or)
(put-sysprop 'fmla-or 'c2 'c2fmla-or)
(put-sysprop 'jump-true 'set-loc 'set-jump-true)
(put-sysprop 'jump-false 'set-loc 'set-jump-false)
