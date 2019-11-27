;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  CMPCBK --  Callbacks: lisp functions that can be called from the C world

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defconstant +foreign-elt-type-codes+
  '(                (:char               . "ECL_FFI_CHAR")
                    (:unsigned-char      . "ECL_FFI_UNSIGNED_CHAR")
                    (:byte               . "ECL_FFI_BYTE")
                    (:unsigned-byte      . "ECL_FFI_UNSIGNED_BYTE")
                    (:short              . "ECL_FFI_SHORT")
                    (:unsigned-short     . "ECL_FFI_UNSIGNED_SHORT")
                    (:int                . "ECL_FFI_INT")
                    (:unsigned-int       . "ECL_FFI_UNSIGNED_INT")
                    (:long               . "ECL_FFI_LONG")
                    (:unsigned-long      . "ECL_FFI_UNSIGNED_LONG")
    #+:uint16-t     (:int16-t            . "ECL_FFI_INT16_T")
    #+:uint16-t     (:uint16-t           . "ECL_FFI_UINT16_T")
    #+:uint32-t     (:int32-t            . "ECL_FFI_INT32_T")
    #+:uint32-t     (:uint32-t           . "ECL_FFI_UINT32_T")
    #+:uint64-t     (:int64-t            . "ECL_FFI_INT64_T")
    #+:uint64-t     (:uint64-t           . "ECL_FFI_UINT64_T")
    #+:long-long    (:long-long          . "ECL_FFI_LONG_LONG")
    #+:long-long    (:unsigned-long-long . "ECL_FFI_UNSIGNED_LONG_LONG")
                    (:pointer-void       . "ECL_FFI_POINTER_VOID")
                    (:cstring            . "ECL_FFI_CSTRING")
                    (:object             . "ECL_FFI_OBJECT")
                    (:float              . "ECL_FFI_FLOAT")
                    (:double             . "ECL_FFI_DOUBLE")
                    (:long-double        . "ECL_FFI_LONG_DOUBLE")
    #+complex-float (:csfloat            . "ECL_FFI_CSFLOAT")
    #+complex-float (:cdfloat            . "ECL_FFI_CDFLOAT")
    #+complex-float (:clfloat            . "ECL_FFI_CLFLOAT")
                    (:void               . "ECL_FFI_VOID")))

(defun foreign-elt-type-code (type)
  (let ((x (assoc type +foreign-elt-type-codes+)))
    (unless x
      (cmperr "DEFCALLBACK: ~a is not a valid elementary FFI type." type))
    (cdr x)))

(defun c1-defcallback (args)
  (destructuring-bind (name return-type arg-list &rest body)
      args
    (cond ((eql return-type nil)
           (setf return-type :void))
          ((and (consp return-type)
                (member (first return-type) '(* array)))
           (setf return-type :pointer-void)))
    (let ((arg-types '())
          (arg-type-constants '())
          (arg-variables '())
          (c-name (format nil "ecl_callback_~d" (length *callbacks*)))
          (name (if (consp name) (first name) name))
          (call-type (if (consp name) (second name) :cdecl)))
      (dolist (i arg-list)
        (unless (consp i)
          (cmperr "Syntax error in CALLBACK form: C type is missing in argument ~A "i))
        (push (first i) arg-variables)
        (let ((type (second i)))
          (push type arg-types)
          (push (foreign-elt-type-code type) arg-type-constants)))
      (push (list name c-name (add-object name)
                  return-type
                  (foreign-elt-type-code return-type)
                  (reverse arg-types)
                  (reverse arg-type-constants)
                  call-type)
            *callbacks*)
      (c1expr
       `(progn
          (defun ,name ,(reverse arg-variables) ,@body)
          (si:put-sysprop ',name :callback
                          (ffi:c-inline () () :object
                                        ,(format nil "ecl_make_foreign_data(@':pointer-void,0,(void*)~a)" c-name)
                                        :one-liner t)))))))

(defun t3-defcallback (lisp-name c-name c-name-constant return-type return-type-code
                       arg-types arg-type-constants call-type &aux (return-p t))
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
