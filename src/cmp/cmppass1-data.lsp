;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2021, Daniel Kochmański
;;;;
;;;;    See file 'LICENSE' for the copyright details.

;;;;
;;;;  CMPDATA  Data segment.
;;;;
;;;;  Each lisp compiled file consists on code and a data section. Whenever an
;;;;  #'in-package toplevel form is found, a read-time evaluated expression is
;;;;  inserted in the data section which changes the current package for the
;;;;  rest of it. This way it is possible to save some space by writing the
;;;;  symbol's package only when it does not belong to the current package.

(in-package #:compiler)

(defun add-load-form (object location)
  (multiple-value-bind (make-form init-form) (make-load-form object)
    (push-object-location object location)
    (let (deferred)
      (when make-form
        (let ((*objects-init-deferred* nil)
              (*objects-being-created* (list* object *objects-being-created*)))
          (push-make-load-node (make-c1form* 'MAKE-FORM :args location (c1expr make-form)))
          (setf deferred (nreverse *objects-init-deferred*))))
      (flet ((maybe-init (loc init)
               (handler-case
                   (push-make-load-node (make-c1form* 'INIT-FORM :args loc (c1expr init)))
                 (circular-dependency (c)
                   (if *objects-being-created*
                       (push (cons location init-form) *objects-init-deferred*)
                       (error c))))))
        (loop for (loc . init) in deferred
              do (maybe-init loc init)
              finally (when init-form
                        (maybe-init location init-form)))))))

(defun data-empty-loc ()
  (add-object *empty-loc* :duplicate t :permanent t))

(defun find-vv (object &key permanent (errorp t))
  (let ((test (if si:*compiler-constants* 'eq 'equal-with-circularity)))
    (flet ((find-object (item)
             (when (funcall test object (vv-value item))
               (when permanent
                 ;; Objects are put in the appropriate storage in the second
                 ;; pass and we still can move them. If we look for a permanent
                 ;; object that is a temporary one, then promote it to.
                 (setf (vv-permanent-p item) t))
               (return-from find-vv item))))
      (map-referenced-objects #'find-object))
    (when errorp
      (cmperr "Unable to find object ~s." object))
    nil))

(defun push-vv (&rest args &key permanent-p value)
  (let ((vv (apply #'make-vv args)))
    (push-referenced-object vv)
    vv))

(defun add-object (object &key
                            (duplicate nil)
                            (used-p nil)
                            (always nil)
                            (permanent (or (symbolp object)
                                           *permanent-data*))
                   &aux load-form-p)
  (unless (eq *compiler-phase* 't1)
    (cmperr "ADD-OBJECT: Unable to internalize objects in ~a phase."
            *compiler-phase*))
  (let* ((item (find-vv object :permanent permanent :errorp nil))
         (vv (cond
               ((and item duplicate)
                (push-vv :permanent-p permanent :value object))
               (item
                (when (member object *objects-being-created*)
                  (error 'circular-dependency :form object))
                item)
               (t
                (let ((vv (push-vv :permanent-p permanent :value object)))
                  (when (and (null si:*compiler-constants*)
                             (si:need-to-make-load-form-p object))
                    (add-load-form object vv))
                  vv)))))
    (when always
      (setf (vv-always vv) t))
    (when (or duplicate used-p)
      (setf (vv-used-p vv) t))
    vv))

(defun add-fname (fname)
  (check-type fname function-name)
  (when (consp fname)
    (add-object (cadr fname) :duplicate nil :permanent t))
  (add-object fname :duplicate nil :permanent t))

(defun add-symbol (symbol)
  (check-type symbol symbol)
  (add-object symbol :duplicate nil :permanent t))
