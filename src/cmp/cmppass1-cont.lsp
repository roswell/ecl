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


;;; A dummy variable is created to hold the block identifier.  When a reference
;;; to the block (via `return-from') is found, the `var-ref' count for that
;;; variable is incremented only if the reference appears across a boundary
;;; (`SI:FUNCTION-BOUNDARY' or `SI:UNWIND-PROTECT-BOUNDARY'), while the
;;; `blk-ref' is always incremented.  Therefore `blk-ref' represents whether the
;;; block is used at all and `var-ref' for the dummy variable represents whether
;;; a block identifier must be created and stored in such variable.

(defun c1block (args)
  (check-args-number 'BLOCK args 1)
  (let ((block-name (first args)))
    (unless (symbolp block-name)
      (cmperr "The block name ~s is not a symbol." block-name))
    (let* ((blk-var (make-var :name block-name :kind 'LEXICAL))
           (blk (make-blk :var blk-var :name block-name))
           (body (let ((*cmp-env* (cmp-env-copy)))
                   (cmp-env-register-block blk)
                   (c1progn (rest args)))))
      (when (or (var-ref-ccb blk-var) (var-ref-clb blk-var))
        (incf *setjmps*))
      (if (plusp (blk-ref blk))
          (make-c1form* 'BLOCK
                        :local-vars (list blk-var)
                        :type (values-type-or (blk-type blk) (c1form-type body))
                        :args blk body)
          body))))

(defun c1return-from (args)
  (check-args-number 'RETURN-FROM args 1 2)
  (let ((name (first args)))
    (unless (symbolp name)
      (cmperr "The block name ~s is not a symbol." name))
    (multiple-value-bind (blk cfb unw)
        (cmp-env-search-block name)
      (unless blk
        (cmperr "The block ~s is undefined." name))
      (let* ((val (c1expr (second args)))
             (var (blk-var blk))
             (type T))
        (cond (cfb (setf type 'CLB
                         (var-ref-clb var) T))
              (unw (setf type 'UNWIND-PROTECT)))
        (incf (blk-ref blk))
        (setf (blk-type blk) (values-type-or (blk-type blk) (c1form-type val)))
        (let ((output (make-c1form* 'RETURN-FROM :type 'T :args blk type val)))
          (when (or cfb unw)
            (add-to-read-nodes var output))
          output)))))


;;;  A dummy variable is created to hold the tag identifier and one tag
;;;  structure (containing reference to such variable) is created for each label
;;;  in the body.  When a reference to a tag (go instruction) is found, the
;;;  var-kind is stepped from NIL to OBJECT (if appearing inside an
;;;  unwind-protect) to LEXICAL or CLOSURE (if appearing across a boundary).
;;;  The tag-ref is also incremented.  Therefore var-ref represents whether some
;;;  tag is used at all and var-kind variable represents whether a tag
;;;  identifier must be created and the kind of the dummy variable to store it.

(defun add-loop-registers (tagbody)
  ;; Find a maximal iteration interval in TAGBODY from first to end
  ;; then increment the var-ref slot.
  (labels ((add-reg1 (form)
             ;; increase the var-ref in FORM for all vars
             (cond ((c1form-p form)
                    (dolist (v (c1form-args form))
                      (add-reg1 v)))
                   ((consp form)
                    (dolist (v form)
                      (add-reg1 v)))
                   ((var-p form)
                    (setf (var-ref form) most-positive-fixnum))))
           (jumps-to-p (clause tag-name)
             ;; Does CLAUSE have a go TAG-NAME in it?
             (cond ((c1form-p clause)
                    (and (eq (c1form-name clause) 'GO)
                         (eq (tag-name (c1form-arg 0 clause)) tag-name)))
                   ((atom clause) nil)
                   (t (or (jumps-to-p (car clause) tag-name)
                          (jumps-to-p (cdr clause) tag-name))))))
    (do ((v tagbody (cdr v))
         (end nil)
         (first nil))
        ((null v)
         (do ((ww first (cdr ww)))
             ((eq ww end) (add-reg1 (car ww)))
           (add-reg1 (car ww))))
      (when (tag-p (car v))
        (unless first (setq first v))
        (do ((w (cdr v) (cdr w))
             (name (tag-name (car v))))
            ((null w))
          (when (jumps-to-p (car w) name)
            (setq end w)))))))

;; FIXME! The variable name should not be a usable one!
(defun c1tagbody (orig-body &aux (*cmp-env* (cmp-env-copy))
                  (tag-var (make-var :name 'TAGBODY :kind NIL))
                  (tag-index 0)
                  (body nil))

  ;;; Establish tags.
  (setq body
        (loop for x in orig-body
           collect (if (consp x)
                       x
                       (let ((tag (make-tag :name x :var tag-var :index tag-index)))
                         (cmp-env-register-tag (tag-name tag) tag)
                         (incf tag-index)
                         tag))))
  ;; Split forms according to the tag they are preceded by and compile
  ;; them grouped by PROGN. This help us use the optimizations in
  ;; C1PROGN to recognize transfers of control.
  (loop for form in body
     with output = '()
     with tag-body = nil
     with this-tag = (make-var :name 'tagbody-beginnnig :kind nil)
     do (cond ((tag-p form)
               (when tag-body
                 (setf output (cons (c1progn (nreconc tag-body '(nil))) output)
                       tag-body nil))
               (push form output))
              (t
               (push form tag-body)))
     finally (setf body
                   (if tag-body
                       (cons (c1progn (nreconc tag-body '(nil))) output)
                       output)))

  ;;; Reverse the body list, deleting unused tags.
  (loop for form in body
     with output = '()
     when (or (not (tag-p form)) (plusp (tag-ref form)))
     do (push form output)
     finally (setf body output))

  ;;; Ensure that the end is not just a tag, but at least a NIL body.
  (when (null body)
    (return-from c1tagbody (c1progn nil)))
  (when (tag-p (first (last body)))
    (setf body (nconc body (list (c1expr nil)))))

  ;;; Only produce a tagbody if it was needed.
  (when (zerop (var-ref tag-var))
    (return-from c1tagbody (make-c1form* 'PROGN :args
                                         (delete-if #'tag-p body))))
  (when (var-ref-ccb tag-var)
    (incf *setjmps*))
  (add-loop-registers body)
  (make-c1form* 'TAGBODY :local-vars (list tag-var)
                         :args tag-var body))

(defun c1go (args)
  (check-args-number 'GO args 1 1)
  (let ((name (first args)))
    (unless (or (symbolp name) (integerp name))
      (cmperr "The tag name ~s is not a symbol nor an integer." name))
    (multiple-value-bind (tag cfb unw)
        (cmp-env-search-tag name)
      (unless tag
        (cmperr "Undefined tag ~A" name))
      (let ((var (tag-var tag)))
        (cond (cfb (setf (var-ref-clb var) t
                         (var-kind var) 'LEXICAL))
              (unw (unless (var-kind var)
                     (setf (var-kind var) :OBJECT))))
        (incf (tag-ref tag))
        (add-to-read-nodes var (make-c1form* 'GO :args tag (or cfb unw)))))))


(defun c1throw (args)
  (check-args-number 'THROW args 2 2)
  (make-c1form* 'THROW :args (c1expr (first args)) (c1expr (second args))))

(defun c1catch (args)
  (check-args-number 'CATCH args 1)
  (incf *setjmps*)
  (make-c1form* 'CATCH :sp-change t :type t :args (c1expr (first args))
                (c1progn (rest args))))

(defun c1unwind-protect (args)
  (check-args-number 'UNWIND-PROTECT args 1)
  (cond
    ((null (rest args))
     (cmpdebug "UNWIND-PROTECT without CLEANUP-FORMS was replaced by its FORM.")
     (c1expr (first args)))
    (T
     (incf *setjmps*)
     (let ((form (let ((*cmp-env* (cmp-env-mark 'SI:UNWIND-PROTECT-BOUNDARY)))
                   (c1expr (first args)))))
       (make-c1form* 'UNWIND-PROTECT :type (c1form-type form) :sp-change t
                                     :args form (c1progn (rest args)))))))
