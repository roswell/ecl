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
;;;;
;;;; CMPTYPE-PROP -- Type propagation basic routines and database
;;;;

(in-package #-new-cmp "COMPILER" #+new-cmp "C-ENV")

(defun function-arg-types (arg-types &aux (types nil))
  (do ((al arg-types (cdr al)))
      ((or (endp al)
           (member (car al) '(&optional &rest &key)))
       (nreverse types))
      (declare (object al))
    (push (car al) types)))

;;; The valid return type declaration is:
;;;	(( VALUES {type}* )) or ( {type}* ).

(defun function-return-type (return-types)
  (cond ((endp return-types) t)
        ((and (consp (car return-types))
              (eq (caar return-types) 'VALUES))
         (cond ((not (endp (cdr return-types)))
                (warn "The function return types ~s is illegal." return-types)
                t)
               ((or (endp (cdar return-types))
                    (member (cadar return-types) '(&optional &rest &key)))
                t)
               (t (car return-types))))
        (t (car return-types))))

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
	      ((and (consp l) (null (rest l)))
	       (setf return-types (function-return-type l)))
	      (t (warn "The function proclamation ~s ~s is not valid."
		       fname decl)))
        (when (eq arg-types '())
          (setf arg-types '(&optional)))
	(if (eq arg-types '*)
	    (rem-sysprop fname 'PROCLAIMED-ARG-TYPES)
	    (put-sysprop fname 'PROCLAIMED-ARG-TYPES arg-types))
	(if (member return-types '(* (VALUES &rest t))
                    :test #'equalp)
	    (rem-sysprop fname 'PROCLAIMED-RETURN-TYPE)
	    (put-sysprop fname 'PROCLAIMED-RETURN-TYPE return-types)))
      (warn "The function proclamation ~s ~s is not valid." fname decl)))

(defun add-function-declaration (fname arg-types return-types &optional (env *cmp-env*))
  (if (si::valid-function-name-p fname)
      (let ((fun (cmp-env-search-function fname)))
	(if (functionp fun)
	    (warn "Found function declaration for local macro ~A" fname)
            (cmp-env-register-ftype fname (list arg-types return-types) env)))
      (warn "In (DECLARE (FTYPE ...)): ~s is not a valid function name" fname))
  env)

(defun get-arg-types (fname &optional (env *cmp-env*) (may-be-global t))
  (let ((x (cmp-env-search-ftype fname env)))
    (if x
        (values (first x) t)
        (when may-be-global
          (let ((fun (cmp-env-search-function fname env)))
            (when (or (null fun) (and (fun-p fun) (fun-global fun)))
              (sys:get-sysprop fname 'PROCLAIMED-ARG-TYPES)))))))

(defun get-return-type (fname &optional (env *cmp-env*))
  (let ((x (cmp-env-search-ftype fname env)))
    (if x
	(values (second x) t)
        (let ((fun (cmp-env-search-function fname env)))
          (when (or (null fun) (and (fun-p fun) (fun-global fun)))
            (sys:get-sysprop fname 'PROCLAIMED-RETURN-TYPE))))))

(defun get-local-arg-types (fun &optional (env *cmp-env*))
  (let ((x (cmp-env-search-ftype (fun-name fun))))
    (if x
        (values (first x) t)
        (values nil nil))))

(defun get-local-return-type (fun &optional (env *cmp-env*))
  (let ((x (cmp-env-search-ftype (fun-name fun))))
    (if x
        (values (second x) t)
        (values nil nil))))

(defun get-proclaimed-narg (fun &optional (env *cmp-env*))
  (multiple-value-bind (arg-list found)
      (get-arg-types fun env)
    (if found
	(loop for type in arg-list
	   with minarg = 0
	   and maxarg = 0
	   and in-optionals = nil
	   do (cond ((member type '(* &rest &key &allow-other-keys) :test #'eq)
		     (return (values minarg call-arguments-limit)))
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

(defun declare-inline (fname-list &optional (env *cmp-env*))
  (unless (every #'si::valid-function-name-p fname-list)
    (cmperr "Not a valid argument to INLINE declaration~%~4I~A"
            fname-list))
  (cmp-env-extend-declaration 'INLINE
                              (loop for name in fname-list
                                 collect (cons name t))
			      env))

(defun declare-notinline (fname-list &optional (env *cmp-env*))
  (unless (every #'si::valid-function-name-p fname-list)
    (cmperr "Not a valid argument to NOTINLINE declaration~%~4I~A"
            fname-list))
  (cmp-env-extend-declaration 'INLINE
                              (loop for name in fname-list
                                 collect (cons name nil))
			      env))

(defun proclaim-inline (fname)
  (dolist (fun fname-list)
    (unless (si::valid-function-name-p fun)
      (error "Not a valid function name ~s in INLINE proclamation" fun))
    (sys:put-sysprop fun 'INLINE t)
    (sys:rem-sysprop fun 'NOTINLINE)))

(defun proclaim-notinline (fname-list)
  (dolist (fun fname-list)
    (unless (si::valid-function-name-p fun)
      (error "Not a valid function name ~s in NOTINLINE proclamation" fun))
    (sys:rem-sysprop fun 'INLINE)
    (sys:put-sysprop fun 'NOTINLINE t)))

(defun inline-possible (fname &optional (env *cmp-env*))
  ;; This function determines whether FNAME can be inlined in one
  ;; of various forms: via compiler macros, via inline functions,
  ;; via C code, etc.
  ;;
  ;; First investigate the compiler environment looking for an INLINE
  ;; declaration or DECLAIM field
  (let* ((x (cmp-env-search-declaration 'inline env))
         (flag (assoc fname x :test #'same-fname-p)))
    (cond (flag
	   (cdr flag))
	  ;; Then look up the global environment for some NOTINLINE
	  ;; declaration.
	  ((sys:get-sysprop fname 'NOTINLINE)
	   nil)
	  ;; Finally, return any possible INLINE expansion
	  ((sys:get-sysprop fname 'INLINE))
	  ;; or default to T
	  (t))))

;;; Install inline expansion of function. If the function is DECLAIMED
;;; inline, then we only keep the definition in the compiler environment.
;;; If the function is PROCLAIMED inline, then we also keep a copy as
;;; a symbol property.
(defun maybe-install-inline-function (fname form env)
  (let* ((x (cmp-env-search-declaration 'inline env))
	 (flag (assoc fname x :test #'same-fname-p)))
    (when (and flag (cdr flag))
      (rplacd flag form))
    (when (sys:get-sysprop fname 'inline)
      (cmpnote "Storing inline form for ~a" fname)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (si::put-sysprop ',fname 'inline ',form)))))
