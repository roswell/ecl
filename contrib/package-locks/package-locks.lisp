;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2016 Daniel Kochmańskin
;;;;
;;;;    See file 'LICENSE' for the copyright details.

;;;; PACKAGE-LOCKS Convenient interface for package-locks mechanism.

(in-package "EXT")


;;; Package locks
(pushnew :package-locks *features*)

(defun lock-package (package &aux (package (si:coerce-to-package package)))
  (ffi:c-inline (package) (:object) :void
                "(#0)->pack.locked = 1"
                :side-effects t
                :one-liner t)
  T)

(defun unlock-package (package &aux (package (si:coerce-to-package package)))
  (ffi:c-inline (package) (:object) :void
                "(#0)->pack.locked = 0"
                :side-effects t
                :one-liner t)
  T)

(defun package-locked-p (package &aux (package (si:coerce-to-package package)))
  "Returns T when PACKAGE is locked, NIL otherwise. Signals an error
if PACKAGE doesn't designate a valid package."
  (ffi:c-inline (package) (:object) :object
                "(#0)->pack.locked ? ECL_T : ECL_NIL"
                :side-effects nil
                :one-liner t))

(defmacro without-package-locks (&body body)
  "Ignores all runtime package lock violations during the execution of
body. Body can begin with declarations."
  `(let ((si::*ignore-package-locks* t)) ,@body))

(defmacro with-unlocked-packages ((&rest packages) &body forms)
  "Unlocks PACKAGES for the dynamic scope of the body. Signals an
error if any of PACKAGES is not a valid package designator."
  (with-unique-names (unlocked-packages)
    `(let (,unlocked-packages)
      (unwind-protect
           (progn
             (dolist (p ',packages)
               (when (package-locked-p p)
                 (push p ,unlocked-packages)
                 (unlock-package p)))
             ,@forms)
        (dolist (p ,unlocked-packages)
          (when (find-package p)
            (lock-package p)))))))

(provide '#:package-locks)
