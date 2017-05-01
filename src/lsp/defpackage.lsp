;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;
;;;                              THE BOEING COMPANY
;;;                           BOEING COMPUTER SERVICES
;;;                            RESEARCH AND TECHNOLOGY
;;;                               COMPUTER SCIENCE
;;;                           P.O. BOX 24346, MS 7L-64
;;;                            SEATTLE, WA 98124-0346
;;;
;;;
;;; Copyright (c) 1990, 1991 The Boeing Company, All Rights Reserved.
;;;
;;; Permission is granted to any individual or institution to use,
;;; copy, modify, and distribute this software, provided that this
;;; complete copyright and permission notice is maintained, intact, in
;;; all copies and supporting documentation and that modifications are
;;; appropriately documented with date, author and description of the
;;; change.
;;;
;;; Stephen L. Nicoud (snicoud@boeing.com) provides this software "as
;;; is" without express or implied warranty by him or The Boeing
;;; Company.
;;;
;;; This software is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor accepts
;;; responsibility to anyone for the consequences of using it or for
;;; whether it serves any particular purpose or works at all.
;;;
;;;     Author: Stephen L. Nicoud
;;;
;;; -----------------------------------------------------------------
;;;
;;;     Adapted for X3J13 by Stephen L Nicoud, 91/5/23
;;;     Adapted for ECL by Giuseppe Attardi, 6/6/1994.
;;;     Partially rewritten by Daniel Kochmański, 2017-05-01
;;; 
;;; -----------------------------------------------------------------

(in-package "SYSTEM")

(defmacro DEFPACKAGE (name &rest options)
  (declare (type (or symbol string character) name))
  "DEFPACKAGE - DEFINED-PACKAGE-NAME {OPTION}*                  [Macro]

   This creates a new package, or modifies an existing one, whose name is
   DEFINED-PACKAGE-NAME.  The DEFINED-PACKAGE-NAME may be a string or a 
   symbol; if it is a symbol, only its print name matters, and not what
   package, if any, the symbol happens to be in.  The newly created or 
   modified package is returned as the value of the DEFPACKAGE form.

   Each standard OPTION is a list of keyword (the name of the option)
   and associated arguments.  No part of a DEFPACKAGE form is
   evaluated.  Except for the :LOCK and :DOCUMENTATION options, more
   than one option of the same kind may occur within the same
   DEFPACKAGE form.

  Valid Options:
        (:documentation         string)
        (:lock                  boolean)
        (:nicknames             {package-name}*)
        (:shadow                {symbol-name}*)
        (:shadowing-import-from package-name {symbol-name}*)
        (:use                   {package-name}*)
        (:local-nicknames       (local-nickname actual-package-name)*)
        (:import-from           package-name {symbol-name}*)
        (:intern                {symbol-name}*)
        (:export                {symbol-name}*)
        (:export-from           {package-name}*)

  [Note: :EXPORT-FROM, :DOCUMENTATION, :LOCK and :LOCAL-NICKNAMES are
         extensions to DEFPACKAGE.

         If a symbol is interned in the package being created and if a
         symbol with the same print name appears as an external symbol
         of one of the packages in the :EXPORT-FROM option, then the
         symbol is exported from the package being created.]"

  (flet ((designators (values)
	   (mapcar #'string values)))
    (let ((nicknames nil)
	  (documentation nil)
	  (shadowed-symbol-names nil)
	  (interned-symbol-names nil)
	  (exported-symbol-names nil)
	  (shadowing-imported-from-symbol-names-list nil)
	  (imported-from-symbol-names-list nil)
	  (exported-from-package-names nil)
	  (use nil)
	  (use-p nil)
	  (lock nil)
	  (local-nicknames nil))
      (dolist (option options)
	(case (car option)
	  (:nicknames
	   (setf nicknames (append nicknames (designators (rest option)))))
	  (:documentation
	   (when documentation
	     (si:simple-program-error
	      "DEFPACKAGE option :DOCUMENTATION specified more than once."))
	   (setf documentation (second option)))
	  (:use
	   (setf use (append use (designators (rest option)))
		 use-p t))
	  (:shadow
	   (setf shadowed-symbol-names
		 (append shadowed-symbol-names (designators (rest option)))))
	  (:intern
	   (setf interned-symbol-names
		 (append interned-symbol-names (designators (rest option)))))
	  (:export
	   (setf exported-symbol-names
		 (append exported-symbol-names (designators (rest option)))))
	  (:shadowing-import-from
	   (destructuring-bind (package-name . names)
	       (designators (rest option))
	     (let ((assoc (assoc package-name shadowing-imported-from-symbol-names-list
				 :test #'string=)))
	       (if assoc
		   (setf (cdr assoc) (append (cdr assoc) names))
		   (setf shadowing-imported-from-symbol-names-list
			 (acons package-name names shadowing-imported-from-symbol-names-list))))))
	  (:import-from
	   (destructuring-bind (package-name . names)
	       (designators (rest option))
	     (let ((assoc (assoc package-name imported-from-symbol-names-list
				 :test #'string=)))
	       (if assoc
		   (setf (cdr assoc) (append (cdr assoc) names))
		   (setf imported-from-symbol-names-list
			 (acons package-name names imported-from-symbol-names-list))))))
	  ;; extensions
	  (:export-from
	   (setf exported-from-package-names
		 (append exported-from-package-names (designators (rest option)))))
	  (:size #+ (or) "we silently ignore `:size' option")
	  (:lock
	   (when lock
	     (si:simple-program-error
	      "DEFPACKAGE option :LOCK specified more than once.")
	     (setf lock (second option))))
	  (:local-nicknames
	   (setf local-nicknames
		 (append local-nicknames
			 (mapcar (lambda (spec)
				   (destructuring-bind (nick name) spec
				     (cons nick name)))
				 (rest option)))))
	  ;; unknown
	  (otherwise
	   (cerror "Proceed, ignoring this option."
		   "~s is not a valid DEFPACKAGE option." option))))
      (check-disjoint `(:intern ,@interned-symbol-names)
		      `(:export ,@exported-symbol-names))
      (check-disjoint `(:intern ,@interned-symbol-names)
		      `(:import-from
			,@(apply #'append (mapcar #'rest imported-from-symbol-names-list)))
		      `(:shadow ,@shadowed-symbol-names)
		      `(:shadowing-import-from
			,@(apply #'append (mapcar #'rest shadowing-imported-from-symbol-names-list))))
      `(eval-when (eval compile load)
	 (si::dodefpackage
	  ,(string name)
	  ',nicknames
	  ,documentation
	  ,(cadr (assoc ':lock options))
	  ',(if use-p use "CL")
	  ',local-nicknames
	  ',shadowed-symbol-names
	  ',interned-symbol-names
	  ',exported-symbol-names
	  ',shadowing-imported-from-symbol-names-list
	  ',imported-from-symbol-names-list
	  ',exported-from-package-names)))))

(defun check-disjoint (&rest args)
  ;; An arg is (:key . set)
  (do ((list args (cdr list)))
      ((endp list))
    (loop
      with x = (car list)
      for y in (rest list)
      for z = (remove-duplicates (intersection (cdr x)(cdr y) :test #'string=))
      when z do (error 'simple-program-error
                       :format-control "Parameters ~S and ~S must be disjoint ~
                                        but have common elements ~%   ~S"
                       :format-arguments (list (car x)(car y) z)))))

(defun dodefpackage
    (name
     nicknames
     documentation
     lock
     use
     local-nicknames
     shadowed-symbol-names
     interned-symbol-names
     exported-symbol-names
     shadowing-imported-from-symbol-names-list
     imported-from-symbol-names-list
     exported-from-package-names)
  (if (find-package name)
      (progn ; (rename-package name name)
        (when nicknames
          (rename-package name name nicknames))
        (when use
          (unuse-package (package-use-list (find-package name)) name)))
      (make-package name :use nil :nicknames nicknames :local-nicknames local-nicknames))
  (let ((*package* (find-package name)))
    (when documentation
      (setf (documentation *package* t) documentation))
    (shadow shadowed-symbol-names)
    (dolist (item shadowing-imported-from-symbol-names-list)
      (let ((package (find-package (first item))))
        (dolist (name (rest item))
          (shadowing-import (find-or-make-symbol name package)))))
    (use-package use)
    (dolist (item imported-from-symbol-names-list)
      (let ((package (find-package (first item))))
        (dolist (name (rest item))
          ;; IMPORT can accept a list as argument, hence if we want to
          ;; import symbol NIL, we have to enclose it in a list.
          (import (or (find-or-make-symbol name package) (list NIL))))))
    (mapc #'intern interned-symbol-names)
    (export (mapcar #'intern exported-symbol-names))
    (dolist (package exported-from-package-names)
      (do-external-symbols (symbol (find-package package))
        (when (nth 1 (multiple-value-list
                      (find-symbol (string symbol))))
          (export (list (intern (string symbol))))))))
  (when lock (lock-package name))
  (find-package name))

(defun find-or-make-symbol (name package)
  (declare (si::c-local))
  (multiple-value-bind (symbol found)
      (find-symbol name package)
    (unless found
      (signal-simple-error 'package-error "INTERN it."
                           "Cannot find symbol ~S in package ~S"
                           (list name package)
                           :package package)
      (setq symbol (intern name package)))
    symbol))

;;;; ------------------------------------------------------------
;;;;    End of File
;;;; ------------------------------------------------------------
