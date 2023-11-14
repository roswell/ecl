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


(defun c2block (c1form blk body)
  (declare (ignore c1form))
  (setf (blk-exit blk) *exit*
        (blk-destination blk) *destination*)
  (if (plusp (var-ref (blk-var blk)))
      (let* ((blk-var (blk-var blk))
             (*env-lvl* *env-lvl*))
        (check-vref blk-var)
        (wt-nl-open-brace)
        (when (eq :object (var-kind blk-var))
          (setf (var-loc blk-var) (next-lcl))
          (wt-nl "cl_object " blk-var ";"))
        (when (env-grows (var-ref-ccb blk-var))
          ;; var is referenced from a closure which may escape.
          (let ((env-lvl *env-lvl*))
            (wt-nl "cl_object " *volatile* "env" (incf *env-lvl*) " = env" env-lvl ";")))
        (bind "ECL_NEW_FRAME_ID(cl_env_copy)" blk-var)
        (wt-nl-open-brace)
        (wt-nl "ecl_frs_push(cl_env_copy," blk-var ");")
        (wt-nl "if (__ecl_frs_push_result!=0) {")
        (let ((*unwind-exit* (cons 'FRAME *unwind-exit*)))
          (unwind-exit 'VALUES)
          (wt-nl "} else {")
          (c2expr body)
          (wt "}"))
        (wt-nl-close-brace)
        (when (var-ref-ccb blk-var) (decf *env*))
        (wt-nl-close-brace))
      (c2expr body)))

(defun c2return-from (c1form blk nonlocal val)
  (declare (ignore c1form))
  (if nonlocal
      (progn
        (let ((*destination* 'VALUES))
          (c2expr* val))
        (let ((name (get-object (blk-name blk))))
          (wt-nl "cl_return_from(" (blk-var blk) "," name ");")))
      (let ((*destination* (blk-destination blk))
            (*exit* (blk-exit blk)))
        (c2expr val))))


(defun c2tagbody (c1form tag-loc body)
  (declare (type var tag-loc)
           (ignore c1form))
  (if (null (var-kind tag-loc))
      ;; only local goto's
      (dolist (x body (c2tagbody-body body))
        ;; Allocate labels.
        (when (and (tag-p x) (plusp (tag-ref x)))
          (setf (tag-label x) (next-label t))
          (setf (tag-unwind-exit x) *unwind-exit*)))
      ;; some tag used non locally or inside an unwind-protect
      (let ((*unwind-exit* (cons 'FRAME *unwind-exit*))
            (*env* *env*) (*env-lvl* *env-lvl*)
            (*lex* *lex*) (*lcl* *lcl*)
            (*inline-blocks* 0)
            (env-grows (env-grows (var-ref-ccb tag-loc))))
        (when env-grows
          (let ((env-lvl *env-lvl*))
            (maybe-open-inline-block)
            (wt-nl "volatile cl_object env" (incf *env-lvl*)
                   " = env" env-lvl ";")))
        (when (eq :OBJECT (var-kind tag-loc))
          (setf (var-loc tag-loc) (next-lcl))
          (maybe-open-inline-block)
          (wt-nl "cl_object " tag-loc ";"))
        (bind "ECL_NEW_FRAME_ID(cl_env_copy)" tag-loc)
        (wt-nl-open-brace)
        (wt-nl "ecl_frs_push(cl_env_copy," tag-loc ");")
        (wt-nl "if (__ecl_frs_push_result) {")
        ;; Allocate labels.
        (dolist (tag body)
          (when (and (tag-p tag) (plusp (tag-ref tag)))
            (setf (tag-label tag) (next-label nil))
            (setf (tag-unwind-exit tag) *unwind-exit*)
            (wt-nl "if (cl_env_copy->values[0]==ecl_make_fixnum(" (tag-index tag) "))")
            (wt-go (tag-label tag))))
        (when (var-ref-ccb tag-loc)
          (wt-nl "ecl_internal_error(\"GO found an inexistent tag\");"))
        (wt-nl "}")
        (wt-nl-close-brace)
        (c2tagbody-body body)
        (close-inline-blocks))))

(defun c2tagbody-body (body)
  ;;; INV: BODY is a list of tags and forms. We have processed the body
  ;;; so that the last element is always a form producing NIL.
  (do ((l body (cdr l)))
      ((null l))
    (let* ((this-form (first l)))
      (cond ((tag-p this-form)
             (wt-label (tag-label this-form)))
            ((endp (rest l))
             ;; Last form, it is never a label!
             (c2expr this-form))
            (t
             (let* ((next-form (second l))
                    (*exit* (if (tag-p next-form)
                                (tag-label next-form)
                                (next-label nil)))
                    (*unwind-exit* (cons *exit* *unwind-exit*))
                    (*destination* 'TRASH))
               (c2expr this-form)
               (unless (tag-p next-form)
                 (wt-label *exit*))))))))

(defun c2go (c1form tag nonlocal)
  (declare (ignore c1form))
  (if nonlocal
      (let ((var (tag-var tag)))
        (wt-nl "cl_go(" var ",ecl_make_fixnum(" (tag-index tag) "));"))
      ;; local go
      (progn
        (unwind-no-exit-until (tag-unwind-exit tag))
        (wt-nl) (wt-go (tag-label tag)))))


(defun c2throw (c1form tag val &aux loc)
  (declare (ignore c1form))
  (case (c1form-name tag)
    ((VARIABLE LOCATION) (setq loc (c1form-arg 0 tag)))
    (t (setq loc (make-temp-var))
     (let ((*destination* loc)) (c2expr* tag))))
  (let ((*destination* 'VALUES)) (c2expr* val))
  (wt-nl "cl_throw(" loc ");"))

(defun c2catch (c1form tag body)
  (declare (ignore c1form))
  (let* ((new-destination (tmp-destination *destination*))
         (code (gensym "CATCH")))
    (let ((*destination* 'VALUE0))
      (c2expr* tag))
    (let* ((*destination* new-destination)
           (*unwind-exit* (cons 'FRAME *unwind-exit*)))
      (wt-nl-open-brace)
      (if (member new-destination '(TRASH VALUES))
          (progn
            (wt-nl "ecl_frs_push(cl_env_copy," 'VALUE0 ");")
            (wt-nl "if (__ecl_frs_push_result==0) {")
            (wt-comment "BEGIN CATCH ~A" code)
            (with-indentation
              (c2expr* body)))
          (progn
            (wt-nl "ecl_frs_push(cl_env_copy," 'VALUE0 ");")
            (wt-nl "if (__ecl_frs_push_result) {")
            (wt-comment "BEGIN CATCH ~A" code)
            (with-indentation
              (with-exit-label (*exit*)
                (unwind-exit 'VALUES)))
            (wt-nl "} else {")
            (with-indentation
              (c2expr* body)))))
    (wt-nl "}")
    (wt-nl "ecl_frs_pop(cl_env_copy);")
    (wt-comment "END CATCH ~A" code)
    (wt-nl-close-brace)
    (unwind-exit new-destination)))

(defun c2unwind-protect (c1form form body)
  (declare (ignore c1form))
  (let* ((sp (make-lcl-var :rep-type :cl-index))
         (nargs (make-lcl-var :rep-type :cl-index))
         (*unwind-exit* `((STACK ,sp) ,@*unwind-exit*)))
    (wt-nl-open-brace)
    (wt-nl "volatile bool unwinding = FALSE;")
    (wt-nl "cl_index " sp "=ECL_STACK_INDEX(cl_env_copy)," nargs ";")
    (wt-nl "ecl_frame_ptr next_fr;")
    ;; Here we compile the form which is protected. When this form
    ;; is aborted, it continues at the ecl_frs_pop() with unwinding=TRUE.
    (wt-nl "ecl_frs_push(cl_env_copy,ECL_PROTECT_TAG);")
    (wt-nl "if (__ecl_frs_push_result) {")
    (wt-nl "  unwinding = TRUE; next_fr=cl_env_copy->nlj_fr;")
    (wt-nl "} else {")
    (let ((*unwind-exit* (cons 'FRAME *unwind-exit*))
          (*destination* 'VALUES))
      (c2expr* form))
    (wt-nl "}")
    (wt-nl "ecl_frs_pop(cl_env_copy);")
    ;; Here we save the values of the form which might have been
    ;; aborted, and execute some cleanup code. This code may also
    ;; be aborted by some control structure, but is not protected.
    (wt-nl nargs "=ecl_stack_push_values(cl_env_copy);")
    (let ((*destination* 'TRASH))
      (c2expr* body))
    (wt-nl "ecl_stack_pop_values(cl_env_copy," nargs ");")
    ;; Finally, if the protected form was aborted, jump to the
    ;; next catch point...
    (wt-nl "if (unwinding) ecl_unwind(cl_env_copy,next_fr);")
    ;; ... or simply return the values of the protected form.
    (unwind-exit 'VALUES)
    (wt-nl-close-brace)))
