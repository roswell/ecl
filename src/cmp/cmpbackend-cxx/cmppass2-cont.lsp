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
        (with-lexical-scope ()
          (when (eq :object (var-kind blk-var))
            (setf (var-loc blk-var) (next-lcl))
            (wt-nl "cl_object " blk-var ";"))
          (when (env-grows (var-ref-ccb blk-var))
            ;; var is referenced from a closure which may escape.
            (let ((env-lvl *env-lvl*))
              (wt-nl "cl_object " *volatile* "env" (incf *env-lvl*) " = env" env-lvl ";")))
          (bind 'FRAME++ blk-var)
          (with-unwind-frame (blk-var)
                             (unwind-exit 'VALUEZ)
            (c2expr body))
          (when (var-ref-ccb blk-var)
            (decf *env*))))
      (c2expr body)))

(defun c2return-from (c1form blk nonlocal val)
  (declare (ignore c1form))
  (if nonlocal
      (progn
        (let ((*destination* 'VALUEZ))
          (c2expr* val))
        (unwind-flee blk :return-from))
      (let ((*destination* (blk-destination blk))
            (*exit* (blk-exit blk)))
        (c2expr val))))


(defun c2tagbody (c1form tag-loc body)
  (declare (type var tag-loc)
           (ignore c1form))
  (macrolet ((do-tags ((tag forms result) &body body)
               ;; Allocate labels.
               `(dolist (,tag ,forms ,result)
                  (when (and (tag-p ,tag) (plusp (tag-ref ,tag)))
                    (setf (tag-jump ,tag) (next-label t))
                    ,@body))))
    (if (null (var-kind tag-loc))
        ;; only local goto's
        (do-tags (tag body (c2tagbody-body body)))
        ;; some tag used non locally or inside an unwind-protect
        (let ((*env* *env*) (*env-lvl* *env-lvl*)
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
          (bind 'FRAME++ tag-loc)
          (with-unwind-frame (tag-loc)
            (progn
              (do-tags (tag body nil)
                (unwind-cond (tag-jump tag) :jump-eq
                             'VALUEZ  (tag-index tag)))
              (when (var-ref-ccb tag-loc)
                (wt-nl "ecl_internal_error(\"GO found an inexistent tag\");")))
            (c2tagbody-body body))
          (close-inline-blocks)))))

(defun c2tagbody-body (body)
  ;;; INV: BODY is a list of tags and forms. We have processed the body
  ;;; so that the last element is always a form producing NIL.
  (loop for (this-form next-form . rest) on body do
    (cond ((tag-p this-form)
           (wt-label (tag-jump this-form)))
          ((tag-p next-form)
           (with-exit-label (*exit* (tag-jump next-form))
             (let ((*destination* 'TRASH))
               (c2expr this-form))))
          (t
           (c2expr this-form)))))

(defun c2go (c1form tag nonlocal)
  (declare (ignore c1form))
  (if nonlocal
      (unwind-flee tag :go)
      (unwind-jump (tag-jump tag))))


(defun c2throw (c1form tag val &aux loc)
  (declare (ignore c1form))
  (case (c1form-name tag)
    ((VARIABLE LOCATION)
     (setq loc (c1form-arg 0 tag)))
    (t
     (setq loc (make-temp-var))
     (let ((*destination* loc))
       (c2expr* tag))))
  (let ((*destination* 'VALUEZ))
    (c2expr* val))
  (unwind-flee loc :throw))

(defun c2catch (c1form tag body)
  (declare (ignore c1form))
  (let* ((new-destination (tmp-destination *destination*))
         (code (gensym "CATCH")))
    (let ((*destination* 'VALUE0))
      (c2expr* tag))
    (let ((*destination* new-destination))
      (wt-comment "BEGIN CATCH ~A" code)
      (with-unwind-frame ('VALUE0)
        (unless (member new-destination '(TRASH VALUEZ))
          (with-indentation
            (with-exit-label (*exit*)
              (unwind-exit 'VALUEZ))))
        (with-indentation
          (c2expr* body)))
      (wt-nl "ecl_frs_pop(cl_env_copy);")
      (wt-comment "END CATCH ~A" code))
    (unwind-exit new-destination)))

(defun c2unwind-protect (c1form form body)
  (declare (ignore c1form))
  (with-stack-frame (frame)
    ;; Here we compile the form which is protected. When this form is aborted,
    ;; it continues with unwinding=TRUE. We call ecl_frs_pop() manually because
    ;; we use C2EXPR* in the body.
    (wt-nl "volatile bool unwinding = FALSE;")
    (wt-nl "ecl_frame_ptr next_fr;")
    (with-unwind-frame ("ECL_PROTECT_TAG")
      (wt-nl "  unwinding = TRUE; next_fr=cl_env_copy->nlj_fr;")
      (let ((*destination* 'VALUEZ))
        (c2expr* form)))
    (wt-nl "ecl_frs_pop(cl_env_copy);")
    ;; Here we save the values of the form which might have been
    ;; aborted, and execute some cleanup code. This code may also
    ;; be aborted by some control structure, but is not protected.
    (wt-nl "ecl_stack_frame_push_values(" frame ");")
    (let ((*destination* 'TRASH))
      (c2expr* body))
    (wt-nl "ecl_stack_frame_pop_values(" frame ");")
    ;; Finally, if the protected form was aborted, jump to the
    ;; next catch point...
    (wt-nl "if (unwinding) ecl_unwind(cl_env_copy,next_fr);")
    ;; ... or simply return the values of the protected form.
    (unwind-exit 'VALUEZ)))
