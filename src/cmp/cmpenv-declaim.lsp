;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    See file 'LICENSE' for the copyright details.
;;;;

;;;; CMPENV-DECLAIM -- Proclamations local to the current file
;;;;
;;;; One implementation of DECLAIM that uses the compiler environment
;;;; providing a "base" set of entries that all other environments
;;;; stem from.
;;;;
 
(in-package "COMPILER")

(defun process-declaim-args (args)
  (flet ((add-variables (env types specials)
           (loop for name in specials
              unless (assoc name types)
              do (let ((v (make-global-var name :kind 'special)))
                   (setf env (cmp-env-register-var v env nil))))
           (loop for (name . type) in types
              for specialp = (or (si:specialp name) (member name specials))
              for kind = (if specialp 'SPECIAL 'GLOBAL)
              for v = (make-global-var name :type type :kind kind)
              do (setf env (cmp-env-register-var v env nil)))
           env))
    (multiple-value-bind (body specials types ignored others doc all)
        (c1body `((DECLARE ,@args)) nil)
      (declare (ignore body doc all))
      (when ignored
        (cmpwarn-style "IGNORE/IGNORABLE declarations in DECLAIM are ignored"))
      (reduce #'add-one-declaration others
              :initial-value (add-variables *cmp-env* types specials))
      (reduce #'add-one-declaration others
              :initial-value (add-variables *cmp-env-root* types specials)))))

(defmacro declaim (&rest declarations)
  `(locally (declare (notinline mapc))
     (ext:with-backend
       :c/c++ (eval-when (:compile-toplevel)
                (c::process-declaim-args ',declarations))
       :bytecodes (eval-when (:compile-toplevel)
                    (mapc 'proclaim ',declarations)))
     (eval-when (:load-toplevel :execute)
       (mapc 'proclaim ',declarations))))

(defmacro ext::c-declaim (&rest declarations)
  `(ext:with-backend
       :c/c++ (eval-when (:compile-toplevel)
                (c::process-declaim-args ',declarations))))
