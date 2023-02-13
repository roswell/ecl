;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;; CMPENV-FUN -- Declarations concerning function types and inlining
;;;;

(in-package "COMPILER")

(defun function-arg-types (arg-types &aux (types nil))
  (do ((al arg-types (cdr al)))
      ((or (endp al)
           (member (car al) '(&optional &rest &key)))
       (nreverse types))
      (declare (object al))
    (push (car al) types)))

;;; The valid return type declaration is:
;;;     (( VALUES {type}* )) or ( {type}* ).

(defun proclaim-function (fname decl)
  (if (si:valid-function-name-p fname)
      (let* ((arg-types '*)
             (return-types '*)
             (l decl))
        (cond ((null l))
              ((consp l)
               (setf arg-types (pop l)))
              (t (warn "The function proclamation ~s ~s is not valid."
                       fname decl)))
        (cond ((null l))
              ((or (atom l) (rest l))
               (warn "The function proclamation ~s ~s is not valid."
                     fname decl))
              (t
               (setf return-types (first l))))
        (when (eq arg-types '())
          (setf arg-types '(&optional)))
        (if (eq arg-types '*)
            (si:rem-sysprop fname 'PROCLAIMED-ARG-TYPES)
            (si:put-sysprop fname 'PROCLAIMED-ARG-TYPES arg-types))
        (if (member return-types '(* (VALUES &rest t))
                    :test #'equalp)
            (si:rem-sysprop fname 'PROCLAIMED-RETURN-TYPE)
            (si:put-sysprop fname 'PROCLAIMED-RETURN-TYPE return-types)))
      (warn "The function proclamation ~s ~s is not valid." fname decl)))

(defun add-function-declaration (fname ftype &optional (env *cmp-env*))
  (if (si::valid-function-name-p fname)
      (let ((fun (cmp-env-search-function fname)))
        (if (functionp fun)
            (warn "Found function declaration for local macro ~A" fname)
            (cmp-env-register-ftype fname ftype env)))
      (warn "In (DECLARE (FTYPE ...)): ~s is not a valid function name" fname))
  env)

(defun get-arg-types (fname &optional (env *cmp-env*) (may-be-global t))
  (ext:if-let ((x (cmp-env-search-ftype fname env)))
    (let ((arg-types (first x)))
      (unless (eq arg-types '*)
        (values arg-types t)))
    (when may-be-global
      (let ((fun (cmp-env-search-function fname env)))
        (when (or (null fun) (and (fun-p fun) (fun-global fun)))
          (si:get-sysprop fname 'PROCLAIMED-ARG-TYPES))))))

(defun get-return-type (fname &optional (env *cmp-env*))
  (ext:if-let ((x (cmp-env-search-ftype fname env)))
    (let ((return-types (second x)))
      (unless (eq return-types '*)
        (values return-types t)))
    (let ((fun (cmp-env-search-function fname env)))
      (when (or (null fun) (and (fun-p fun) (fun-global fun)))
        (si:get-sysprop fname 'PROCLAIMED-RETURN-TYPE)))))

(defun get-local-arg-types (fun &optional (env *cmp-env*))
  (ext:if-let ((x (cmp-env-search-ftype (fun-name fun) env)))
    (values (first x) t)
    (values nil nil)))

(defun get-local-return-type (fun &optional (env *cmp-env*))
  (ext:if-let ((x (cmp-env-search-ftype (fun-name fun) env)))
    (values (second x) t)
    (values nil nil)))

(defun get-proclaimed-narg (fun &optional (env *cmp-env*))
  (multiple-value-bind (arg-list found)
      (get-arg-types fun env)
    (if found
        (loop for type in arg-list
           with minarg = 0
           and maxarg = 0
           and in-optionals = nil
           do (cond ((member type '(* &rest &key &allow-other-keys) :test #'eq)
                     (return (values minarg call-arguments-limit found)))
                    ((eq type '&optional)
                     (setf in-optionals t maxarg minarg))
                    (in-optionals
                     (incf maxarg))
                    (t
                     (incf minarg)
                     (incf maxarg)))
           finally (return (values minarg maxarg found)))
        (values 0 call-arguments-limit found))))

;;; Proclamation and declaration handling.

(defun declare-inline (fname &optional (env *cmp-env*) (definition t))
  (unless (si::valid-function-name-p fname)
    (cmperr "Not a valid argument to INLINE declaration~%~4I~A"
            fname))
  (cmp-env-extend-declaration 'INLINE (list (cons fname definition)) env))

(defun declare-notinline (fname &optional (env *cmp-env*))
  (declare-inline fname env nil))

(defun proclaim-inline (fname-list)
  (dolist (fun fname-list)
    (unless (si::valid-function-name-p fun)
      (error "Not a valid function name ~s in INLINE proclamation" fun))
    (unless (si:get-sysprop fun 'INLINE)
      (si:put-sysprop fun 'INLINE t)
      (si:rem-sysprop fun 'NOTINLINE))))

(defun proclaim-notinline (fname-list)
  (dolist (fun fname-list)
    (unless (si::valid-function-name-p fun)
      (error "Not a valid function name ~s in NOTINLINE proclamation" fun))
    (si:rem-sysprop fun 'INLINE)
    (si:put-sysprop fun 'NOTINLINE t)))

(defun declared-inline-p (fname &optional (env *cmp-env*))
  (let* ((x (cmp-env-search-declaration 'inline env))
         (flag (assoc fname x :test #'same-fname-p)))
    (if flag
        (cdr flag)
        (si:get-sysprop fname 'INLINE))))

(defun declared-notinline-p (fname &optional (env *cmp-env*))
  (let* ((x (cmp-env-search-declaration 'inline env))
         (flag (assoc fname x :test #'same-fname-p)))
    (if flag
        (null (cdr flag))
        (si:get-sysprop fname 'NOTINLINE))))

(defun inline-possible (fname &optional (env *cmp-env*))
  (not (declared-notinline-p fname env)))

;;; Install inline expansion of function.
(defun maybe-install-inline-function (fname form)
  (when (declared-inline-p fname *cmp-env-root*)
    ;; The function was already PROCLAIMED inline and might be
    ;; redefined in the file we are currently compiling. Declare it as
    ;; inline in the compiler environment and remove the symbol
    ;; property so that if we can't inline the new definition (e.g.
    ;; because it is a closure) we don't accidentally inline an old
    ;; definition from the symbol property.
    (declare-inline fname *cmp-env-root*)
    (si:rem-sysprop fname 'inline)
    ;; If the function is PROCLAIMED or DECLAIMED inline, then we
    ;; install the definition as a symbol property during loading of
    ;; the compiled file. If the function was only DECLARED inline
    ;; locally we don't keep the definition.
    `(eval-when (:load-toplevel :execute)
       (si:put-sysprop ',fname 'inline ',form))))
