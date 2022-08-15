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
;;;;  CMPDATA  Data segment.
;;;;
;;;;  Each lisp compiled file consists on code and a data section. Whenever an
;;;;  #'in-package toplevel form is found, a read-time evaluated expression is
;;;;  inserted in the data section which changes the current package for the
;;;;  rest of it. This way it is possible to save some space by writing the
;;;;  symbol's package only when it does not belong to the current package.

(in-package #:compiler)

(defun data-permanent-storage-size ()
  (length *permanent-objects*))

(defun data-temporary-storage-size ()
  (length *temporary-objects*))

(defun data-size ()
  (+ (data-permanent-storage-size)
     (data-temporary-storage-size)))

(defun data-init (&optional filename)
  (if (and filename (probe-file filename))
      (with-open-file (s filename :direction :input)
        (setf *permanent-objects* (read s)
              *temporary-objects* (read s)))
      (setf *permanent-objects* (make-array 128 :adjustable t :fill-pointer 0)
            *temporary-objects* (make-array 128 :adjustable t :fill-pointer 0))))

(defun data-get-all-objects ()
  ;; We collect all objects that are to be externalized, but filter out
  ;; those which will be created by a lisp form.
  (loop for array in (list *permanent-objects* *temporary-objects*)
        nconc (loop for vv-record across array
                    for object = (vv-value vv-record)
                    collect (cond ((gethash object *load-objects*)
                                   0)
                                  ((vv-used-p vv-record)
                                   object)
                                  (t
                                   ;; Value optimized away or not used
                                   0)))))

(defun add-load-form (object location)
  (unless (eq *compiler-phase* 't1)
    (cmperr "Unable to internalize complex object ~A in ~a phase." object *compiler-phase*))
  (multiple-value-bind (make-form init-form) (make-load-form object)
    (setf (gethash object *load-objects*) location)
    (let (deferred)
      (when make-form
        (let ((*objects-init-deferred* nil)
              (*objects-being-created* (list* object *objects-being-created*)))
          (push (make-c1form* 'MAKE-FORM :args location (c1expr make-form)) *make-forms*)
          (setf deferred (nreverse *objects-init-deferred*))))
      (flet ((maybe-init (loc init)
               (handler-case
                   (push (make-c1form* 'INIT-FORM :args loc (c1expr init)) *make-forms*)
                 (circular-dependency (c)
                   (if *objects-being-created*
                       (push (cons location init-form) *objects-init-deferred*)
                       (error c))))))
        (loop for (loc . init) in deferred
              do (maybe-init loc init)
              finally (when init-form
                        (maybe-init location init-form)))))))

(defun data-empty-loc ()
  (add-object 0 :duplicate t :permanent t))

(defun add-object (object &key
                            (duplicate nil)
                            (used-p nil)
                            (permanent (or (symbolp object)
                                           *permanent-data*))
                   &aux load-form-p)
  ;; FIXME add-static-constant is tied to the C target.
  (when-let ((vv (add-static-constant object)))
    (when used-p
      (setf (vv-used-p vv) t))
    (return-from add-object vv))
  (when (and (null *compiler-constants*)
             (si::need-to-make-load-form-p object))
    ;; All objects created with MAKE-LOAD-FORM go into the permanent storage to
    ;; prevent two non-eq instances of the same object in the permanent and
    ;; temporary storage from being created (we can't move objects from the
    ;; temporary into the permanent storage once they have been created).
    (setf load-form-p t permanent t))
  (let* ((test (if *compiler-constants* 'eq 'equal-with-circularity))
         (item (if permanent
                   (find object *permanent-objects* :test test :key #'vv-value)
                   (or (find object *permanent-objects* :test test :key #'vv-value)
                       (find object *temporary-objects* :test test :key #'vv-value))))
         (array (if permanent
                    *permanent-objects*
                    *temporary-objects*))
         (vv (cond ((and item duplicate)
                    (let* ((ndx (length array))
                           (vv (make-vv :location ndx
                                        :permanent-p permanent
                                        :value object)))
                      (vector-push-extend vv array)
                      vv))
                   (item
                    (when (member object *objects-being-created*)
                      (error 'circular-dependency :form object))
                    item)
                   ;; FIXME! all other branches return VV instance while this
                   ;; branch returns a STRING making the function return value
                   ;; inconsistent.
                   ((and (not item) (not duplicate) (symbolp object)
                         (multiple-value-bind (foundp symbol)
                             (si::mangle-name object)
                           (and foundp
                                (return-from add-object symbol)))))
                   (t
                    (let* ((ndx (length array))
                           (vv (make-vv :location ndx
                                        :permanent-p permanent
                                        :value object)))
                      (vector-push-extend vv array)
                      (when load-form-p
                        (add-load-form object vv))
                      vv)))))
    (when (or duplicate used-p)
      (setf (vv-used-p vv) t))
    vv))

(defun add-symbol (symbol)
  (add-object symbol :duplicate nil :permanent t))

(defun add-keywords (keywords)
  ;; We have to build, in the vector VV[], a sequence with all the keywords that
  ;; this function uses. It does not matter whether each keyword has appeared
  ;; separately before, because cl_parse_key() needs the whole list. However, we
  ;; can reuse keywords lists from other functions when they coincide with ours.
  ;; We search for keyword lists that are similar. However, the list *OBJECTS*
  ;; contains elements in decreasing order!!!
  (if-let ((x (search keywords *permanent-objects*
                      :test #'(lambda (k record) (eq k (vv-value record))))))
    (elt *permanent-objects* x)
    (prog1 (add-object (pop keywords) :duplicate t :permanent t)
      (dolist (k keywords)
        (add-object k :duplicate t :permanent t)))))
