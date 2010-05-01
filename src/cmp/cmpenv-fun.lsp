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
      (push (type-filter (car al)) types)))

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
               (t (type-filter (car return-types) t))))
        (t (#-new-cmp type-filter #+new-cmp c-types:type-filter
            (car return-types)))))

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

(defun get-arg-types (fname &optional (env *cmp-env*))
  (let ((x (cmp-env-search-ftype fname env)))
    (if x
        (values (first x) t)
        (sys:get-sysprop fname 'PROCLAIMED-ARG-TYPES))))

(defun get-return-type (fname &optional (env *cmp-env*))
  (let ((x (cmp-env-search-ftype fname env)))
    (if x
	(values (second x) t)
	(sys:get-sysprop fname 'PROCLAIMED-RETURN-TYPE))))

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
                                 collect (cons name t))))

(defun declare-notinline (fname-list &optional (env *cmp-env*))
  (unless (every #'symbolp fname-list)
    (cmperr "Not a valid argument to NOTINLINE declaration~%~4I~A"
            fname-list))
  (cmp-env-extend-declaration 'INLINE
                              (loop for name in fname-list
                                 collect (cons name nil))))

(defun inline-possible (fname &optional (env *cmp-env*))
  (let* ((x (cmp-env-search-declaration 'inline env))
         (flag (assoc fname x :test #'same-fname-p)))
    (if flag
        (cdr flag)
        (not (or ;; (compiler-<push-events)
              ;;(>= *debug* 2) Breaks compilation of STACK-PUSH-VALUES
              (sys:get-sysprop fname 'CMP-NOTINLINE))))))

