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

(defun child-function-p (presumed-parent fun)
  (declare (optimize speed))
  (loop for real-parent = (fun-parent fun)
        while real-parent
        do (if (eq real-parent presumed-parent)
               (return t)
               (setf fun real-parent))))

(defun compute-closure-type (fun)
  (declare (si::c-local))
  (let ((lexical-closure-p nil))
    ;; it will have a full closure if it refers external non-global variables
    (dolist (var (fun-referenced-vars fun))
      (cond ((global-var-p var))
            ;; ...across CB
            ((ref-ref-ccb var)
             (return-from compute-closure-type 'CLOSURE))
            (t
             (setf lexical-closure-p t))))
    ;; ...or if it directly calls a function
    (dolist (f (fun-referenced-funs fun))
      (unless (child-function-p fun f)
        ;; .. which has a full closure
        (case (fun-closure f)
          (CLOSURE (return-from compute-closure-type 'CLOSURE))
          (LEXICAL (setf lexical-closure-p t)))))
    ;; ...or the function itself is referred across CB
    (when lexical-closure-p
      (if (or (fun-ref-ccb fun)
              (and (fun-var fun)
                   (plusp (var-ref (fun-var fun)))))
          'CLOSURE
          'LEXICAL))))

(defun update-fun-closure-type-many (function-list)
  (do ((finish nil t)
       (recompute nil))
      (finish
       recompute)
    (dolist (f function-list)
      (when (update-fun-closure-type f)
        (setf recompute t finish nil)))))

(defun prepend-new (l1 l2)
  (loop for f in l1
     do (pushnew f l2))
  l2)

(defun update-fun-closure-type (fun)
  (let ((old-type (fun-closure fun)))
    (when (eq old-type 'closure)
      (return-from update-fun-closure-type nil))
    ;; This recursive algorithm is guaranteed to stop when functions
    ;; do not change.
    (let ((new-type (compute-closure-type fun))
          to-be-updated)
      ;; Same type
      (when (eq new-type old-type)
        (return-from update-fun-closure-type nil))
      (when (fun-global fun)
        (cmpnote "Function ~A is global but is closed over some variables.~%~{~A ~}"
                 (fun-name fun) (mapcar #'var-name (fun-referenced-vars fun))))
      (setf to-be-updated (append (fun-child-funs fun) (fun-referencing-funs fun)))
      (setf (fun-closure fun) new-type)
      ;; All external, non-global variables become of type closure
      (when (eq new-type 'CLOSURE)
        (dolist (var (fun-referenced-vars fun))
          (unless (or (global-var-p var)
                      (eq (var-kind var) new-type))
            (setf (var-ref-clb var) nil
                  (var-ref-ccb var) t
                  (var-kind var) 'CLOSURE
                  (var-loc var) 'OBJECT
                  to-be-updated
                  (prepend-new (var-functions-reading var)
                               (prepend-new (var-functions-setting var)
                                            to-be-updated)))))
        (dolist (f (fun-referenced-funs fun))
          (setf (fun-ref-ccb f) t)))
      ;; If the status of some of the children changes, we have
      ;; to recompute the closure type.
      (when (update-fun-closure-type-many to-be-updated)
        (update-fun-closure-type fun))
      t)))

(defun local-function-ref (fname &optional build-object)
  (multiple-value-bind (fun cfb unw)
      (cmp-env-search-function fname)
    (declare (ignore unw))
    (when fun
      (when (functionp fun) 
        (when build-object
          ;; Macro definition appears in #'.... This should not happen.
          (cmperr "The name of a macro ~A was found in special form FUNCTION." fname))
        (return-from local-function-ref nil))
      (incf (fun-ref fun))
      (if build-object
          (setf (fun-ref-ccb fun) t)
          (let ((caller *current-function*))
            (when (and caller
                       (not (member fun (fun-referenced-funs caller) :test #'eq)))
              (push fun (fun-referenced-funs caller))
              (push caller (fun-referencing-funs fun)))))
      ;; we introduce a variable to hold the funob
      (let ((var (fun-var fun)))
        (when (and cfb build-object)
          (setf (var-ref-clb var) t)
          (when (not (eq (var-kind var) 'CLOSURE))
            (setf (var-kind var) 'LEXICAL)))))
    fun))

(defun fun-needs-narg (fun)
  (not (fun-fixed-narg fun)))

(defun fun-fixed-narg (fun)
  "Returns true if the function has a fixed number of arguments and it is not a closure.
The function thus belongs to the type of functions that ecl_make_cfun accepts."
  (let (narg)
    (and (not (eq (fun-closure fun) 'CLOSURE))
         (= (fun-minarg fun) (setf narg (fun-maxarg fun)))
         (<= narg si::c-arguments-limit)
         narg)))

(defun add-to-fun-referenced-vars (fun var-list)
  (loop with new-vars = (fun-referenced-vars fun)
     with locals = (fun-local-vars fun)
     with change = nil
     for v in var-list
     when (and (not (member v locals :test #'eq))
               (not (member v new-vars :test #'eq)))
     do (setf change t new-vars (cons v new-vars))
     finally (when change
               (setf (fun-referenced-vars fun) new-vars)
               (return t))))

(defun add-to-fun-referenced-funs (fun fun-list)
  (loop with new-funs = (fun-referenced-funs fun)
     with change = nil
     for f in fun-list
     when (and (not (eq fun f))
               (not (member f new-funs :test #'eq))
               (not (child-function-p fun f)))
     do (setf change t
              new-funs (cons f new-funs)
              (fun-referencing-funs f) (cons fun (fun-referencing-funs f)))
     finally (when change
               (setf (fun-referenced-funs fun) new-funs)
               (return t))))

;;; searches for a (FUNCTION-BLOCK-NAME ...) declaration
(defun function-block-name-declaration (declarations)
  (loop for i in declarations
        if (and (consp i) (eql (car i) 'si::function-block-name)
                (consp (cdr i)))
          return (cadr i)
        finally (return nil)))

(defun exported-fname (name)
  (let (cname)
    (if (and (symbolp name) (setf cname (si:get-sysprop name 'Lfun)))
        (values cname t)
        (values (next-cfun "L~D~A" name) nil))))

(defun function-may-have-side-effects (fname)
  (not (si:get-sysprop fname 'no-side-effects)))

(defun function-may-change-sp (fname)
  (not (or (si:get-sysprop fname 'no-side-effects)
           (si:get-sysprop fname 'no-sp-change))))
