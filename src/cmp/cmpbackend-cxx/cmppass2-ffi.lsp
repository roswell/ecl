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
  (flet ((parse-one-string (s output-stream)
           (with-input-from-string (stream s)
             (loop for c = (read-char stream nil nil)
                   while c
                   do (if (eq c #\@)
                          (let ((object (handler-case (read stream)
                                          (serious-condition (c)
                                            (cmperr "Unable to parse FFI:CLINES string~& ~S"
                                                    s)))))
                            (let ((*compiler-output1* output-stream))
                              (wt (add-object object :permanent t))))
                          (write-char c output-stream))))))
    (loop for s in *clines-string-list*
          do (terpri output-stream)
          do (if (find #\@ s)
                 (parse-one-string s output-stream)
                 (write-string s output-stream)))
    (terpri output-stream)
    (setf *clines-string-list* nil)))

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
        finally (unwind-exit nil)))

(defun produce-inline-loc (inlined-arguments arg-types output-rep-type
                           c-expression side-effects one-liner)
  (let* (args-to-be-saved
         coerced-arguments)
    ;; If the expression begins with @[0-9a-z]*, this means we are
    ;; saving some variables.
    (when (and (> (length c-expression) 1)
               (eq (char c-expression 0) #\@))
      (do ((ndx 1 (1+ ndx)))
          ((>= ndx (length c-expression)))
        (let ((c (char c-expression ndx)))
          (when (eq c #\;)
            (setf c-expression (subseq c-expression (1+ ndx)))
            (return))
          (unless (alphanumericp c)
            (setf args-to-be-saved nil)
            (return))
          (push (- (char-code c) (char-code #\0))
                args-to-be-saved))))

    (setf coerced-arguments (coerce-locs inlined-arguments arg-types args-to-be-saved))
    ;;(setf output-rep-type (lisp-type->rep-type output-rep-type))

    ;; If the form does not output any data, and there are no side
    ;; effects, try to omit it.
    (when (null output-rep-type)
      (if side-effects
          (progn
            (wt-nl)
            (wt-c-inline-loc output-rep-type c-expression coerced-arguments t nil)
            (when one-liner (wt ";")))
          (cmpnote "Ignoring form ~S" c-expression))
      (wt-nl "value0 = ECL_NIL;")
      (wt-nl "cl_env_copy->nvalues = 0;")
      (return-from produce-inline-loc 'RETURN))

    ;; If the form is a one-liner, we can simply propagate this expression until the
    ;; place where the value is used.
    (when one-liner
      (return-from produce-inline-loc
        `(ffi:c-inline ,output-rep-type ,c-expression ,coerced-arguments ,side-effects
                       ,(if (equalp output-rep-type '((VALUES &REST T)))
                            'VALUES NIL))))

    ;; If the output is a in the VALUES vector, just write down the form and output
    ;; the location of the data.
    (when (equalp output-rep-type '((VALUES &REST T)))
      (wt-c-inline-loc output-rep-type c-expression coerced-arguments side-effects
                       'VALUES)
      (return-from produce-inline-loc 'VALUES))

    ;; Otherwise we have to set up variables for holding the output.
    (flet ((make-output-var (type)
             (let ((var (make-lcl-var :rep-type type)))
               (wt-nl (rep-type->c-name type) " " var ";")
               var)))
      (open-inline-block)
      (let ((output-vars (mapcar #'make-output-var output-rep-type)))
        (wt-c-inline-loc output-rep-type c-expression coerced-arguments side-effects output-vars)
        (cond ((= (length output-vars) 1)
               (first output-vars))
              (t
               (loop for v in output-vars
                     for i from 0
                     do (let ((*destination* `(VALUE ,i))) (set-loc v)))
               (wt "cl_env_copy->nvalues = " (length output-vars) ";")
               'VALUES))))))

(defun c2c-inline (c1form arguments &rest rest)
  (declare (ignore c1form))
  (let ((*inline-blocks* 0)
        (*temp* *temp*))
    (unwind-exit (apply #'produce-inline-loc (inline-args arguments) rest))
    (close-inline-blocks)))

(defun coerce-locs (inlined-args &optional types args-to-be-saved)
  ;; INLINED-ARGS is a list of (TYPE LOCATION) produced by the
  ;; inline code. ARGS-TO-BE-SAVED is a positional list created by
  ;; C-INLINE, instructing that the value should be saved in a temporary
  ;; variable. Finally, TYPES is a list of destination types, to which
  ;; the former values are coerced. The destination types can be
  ;;    - A lisp type (:OBJECT, :FINXUM, etc)
  ;;    - A machine representation type (T, INTEGER, etc)
  (loop with block-opened = nil
        for (lisp-type loc) in inlined-args
        for type in (or types '#1=(:object . #1#))
        for i from 0
        for rep-type = (lisp-type->rep-type type)
        collect
        (cond ((and args-to-be-saved
                    (member i args-to-be-saved :test #'eql)
                    (not (loc-movable-p loc)))
               (let ((lcl (make-lcl-var :rep-type rep-type)))
                 (wt-nl)
                 (unless block-opened
                   (setf block-opened t)
                   (open-inline-block))
                 (wt (rep-type->c-name rep-type) " " lcl "= ")
                 (wt-coerce-loc rep-type loc)
                 (wt ";")
                 lcl))
              ((equal rep-type (loc-representation-type loc))
               loc)
              (t
               `(COERCE-LOC ,rep-type ,loc)))))

(defun c-inline-safe-string (constant-string)
  ;; Produce a text representation of a string that can be used
  ;; in a C-INLINE form, without triggering the @ or # escape
  ;; characters
  (c-filtered-string
   (concatenate 'string
                (loop for c across constant-string
                      when (member c '(#\# #\@))
                        collect c
                      collect c))))

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
