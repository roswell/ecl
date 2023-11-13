;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  CMPFFI --  Foreign functions interface.

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

;;; ----------------------------------------------------------------------
;;; C/C++ DECLARATIONS AND HEADERS
;;;
;;; All lines from CLINES statements are grouped at the beginning of the header
;;; Notice that it does not make sense to guarantee that c-lines statements
;;; are produced in-between the function definitions, because two functions
;;; might be collapsed into one, or we might not produce that function at all
;;; and rather inline it.
;;;

;;; FIXME pass1 handler defined in the pass2 module.
(defun c1clines (args)
  (unless (every #'stringp args)
    (cmperr "The argument to CLINES, ~s, is not a list of strings." args))
  (setf *clines-string-list* (nconc *clines-string-list* (copy-list args)))
  '(progn))

(defun output-clines (output-stream)
  (loop for s in *clines-string-list*
        do (terpri output-stream)
        do (if (find #\@ s)
               (cmperr "The character #\\@ is not allowed in ~s." 'FFI:CLINES)
               (write-string s output-stream)))
  (terpri output-stream)
  (setf *clines-string-list* nil))

;; ----------------------------------------------------------------------
;; C/C++ INLINE CODE
;;

(defun c2c-progn (c1form variables statements)
  (declare (ignore c1form))
  (loop with *destination* = 'TRASH
        for form in statements
        do (cond ((stringp form)
                  (wt-nl)
                  (wt-c-inline-loc :void form variables
                                   t    ; side effects
                                   nil) ; no output variables
                  )
                 (t
                  (c2expr* form)))
        finally (unwind-exit *vv-nil*)))

(defun c2c-inline (c1form arguments &rest rest)
  (declare (ignore c1form))
  (let ((*inline-blocks* 0)
        (*temp* *temp*))
    (unwind-exit (apply #'produce-inline-loc (inline-args arguments) rest))
    (close-inline-blocks)))

(defun t3-defcallback (lisp-name c-name c-name-constant return-type return-type-code
                       arg-types arg-type-constants call-type &aux (return-p t))
  (declare (ignore lisp-name))
  (when (eql return-type :void)
    (setf return-p nil))
  (let ((return-type-name (rep-type->c-name (ffi::%convert-to-arg-type return-type)))
        (fmod (case call-type
                ((:cdecl :default) "")
                (:stdcall "__stdcall ")
                (t (cmperr "DEFCALLBACK does not support ~A as calling convention"
                           call-type)))))
    (wt-nl-h "static " return-type-name " " fmod c-name "(")
    (wt-nl1  "static " return-type-name " " fmod c-name "(")
    (loop with comma = ""
          for n from 0
          for type in arg-types
          for arg-type-name = (rep-type->c-name (ffi::%convert-to-arg-type type))
          do (wt-h comma arg-type-name " var" n)
             (wt   comma arg-type-name " var" n)
             (setf comma ","))
    (wt ")")
    (wt-h ");")
    (wt-nl-open-brace)
    (when return-p
      (wt-nl return-type-name " output;"))
    (wt-nl "const cl_env_ptr cl_env_copy = ecl_process_env();")
    (wt-nl "cl_object aux;")
    (wt-nl "ECL_BUILD_STACK_FRAME(cl_env_copy, frame, helper)")
    (loop for n from 0
          and type in arg-types
          and ct in arg-type-constants
          do (wt-nl "ecl_stack_frame_push("
                    "frame,ecl_foreign_data_ref_elt(" "&var" n "," ct ")"
                    ");"))
    (wt-nl "aux = ecl_apply_from_stack_frame(frame,"
           "ecl_fdefinition(" c-name-constant "));")
    (wt-nl "ecl_stack_frame_close(frame);")
    (when return-p
      (wt-nl "ecl_foreign_data_set_elt(&output," return-type-code ",aux);")
      (wt-nl "return output;"))
    (wt-nl-close-brace)))
