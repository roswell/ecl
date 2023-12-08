;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;; Open coding nested forms as C expressions while preserving the order of
;;;; evaluation. Resulting locations may be used inline in C expressions.

(in-package "COMPILER")

(defun maybe-open-inline-block ()
  (unless (plusp *inline-blocks*)
    (open-inline-block)))

(defun open-inline-block ()
  (wt-nl-open-brace)
  (incf *inline-blocks*))

(defun close-inline-blocks ()
  (loop for i of-type fixnum from 0 below *inline-blocks*
        do (wt-nl-close-brace)))

(defun coerce-args (inlined-args)
  (mapcar (lambda (loc)
            (if (eq (loc-host-type loc) :object)
                loc
                `(COERCE-LOC :object ,LOC)))
          inlined-args))

(defun coerce-locs (inlined-args types args-to-be-saved)
  ;; INLINED-ARGS is a list of INLINED-ARG produced by the argument inliner.
  ;; Each arg is a location, "inlined" means "evaluated in the correct order".
  ;;
  ;; ARGS-TO-BE-SAVED is a positional list created by C-INLINE, instructing that
  ;; the value should be saved in a temporary variable.
  ;;
  ;; TYPES is a list of destination types, to which the former values are
  ;; coerced. The destination type can be:
  ;;
  ;;   - A host type (:OBJECT, :FIXNUM, :INT, :CHAR, ...)
  ;;   - A lisp type (T, INTEGER, STRING, CHARACTER, ...))
  ;;
  (loop with block-opened = nil
        for loc in inlined-args
        for arg-host-type = (loc-host-type loc)
        for type in types
        for i from 0
        for host-type = (lisp-type->host-type type)
        collect
        (cond ((and (member i args-to-be-saved :test #'eql)
                    (not (loc-movable-p loc)))
               (let ((lcl (make-lcl-var :host-type host-type)))
                 (wt-nl)
                 (unless block-opened
                   (setf block-opened t)
                   (open-inline-block))
                 (wt (host-type->c-name host-type) " " lcl "= ")
                 (wt-coerce-loc host-type loc)
                 (wt ";")
                 lcl))
              ((equal host-type arg-host-type)
               loc)
              (t
               `(COERCE-LOC ,host-type ,loc)))))

;;; We could use VV to represent inlined args, but the most specific for our
;;; purposes is the location (THE LISP-TYPE LOCATION) which is created when
;;; necessary by the function PRECISE-LOC-TYPE. -- jd 2023-12-06
#+ (or)
(defun make-inlined-arg (loc lisp-type)
  (make-vv :location loc
           :value *inline-loc*
           :type lisp-type
           :host-type (loc-host-type loc)))

(defun make-inlined-temp-var (lisp-type host-type)
  (if (eq host-type :object)
      (make-temp-var lisp-type)
      (let ((var (make-lcl-var :host-type host-type
                               :type lisp-type)))
        (open-inline-block)
        (wt-nl (host-type->c-name host-type) " " var ";")
        var)))

(defun emit-inlined-temp-var (form lisp-type host-type)
  (let ((*destination* (make-inlined-temp-var lisp-type host-type)))
    (c2expr* form)
    *destination*))

(defun emit-inlined-variable (form rest-forms)
  (let ((var (c1form-arg 0 form))
        (lisp-type (c1form-primary-type form)))
    (if (var-changed-in-form-list var rest-forms)
        (emit-inlined-temp-var form lisp-type (var-host-type var))
        (precise-loc-lisp-type var lisp-type))))

(defun emit-inlined-setq (form rest-forms)
  (let ((var (c1form-arg 0 form))
        (val-form (c1form-arg 1 form))
        (lisp-type (c1form-primary-type form)))
    (let ((*destination* var))
      (c2expr* val-form))
    (cond
      ((eq (c1form-name val-form) 'LOCATION)
       (precise-loc-lisp-type (c1form-arg 0 val-form) lisp-type))
      ((not (var-changed-in-form-list var rest-forms))
       (precise-loc-lisp-type var lisp-type))
      (t
       (let ((var-form (make-c1form 'VARIABLE form var nil)))
         (emit-inlined-temp-var var-form lisp-type (var-host-type var)))))))

(defun emit-inlined-progn (form rest-forms)
  (let ((args (c1form-arg 0 form)))
    (loop with *destination* = 'TRASH
          while (rest args)
          do (c2expr* (pop args)))
    (emit-inline-form (first args) rest-forms)))

(defun emit-inlined-values (form rest-forms)
  (let ((args (c1form-arg 0 form)))
    (prog1 (emit-inline-form (or (pop args) (c1nil))
                             ;; The rest of the values args need to be added to
                             ;; the rest forms to execute side effects in the
                             ;; correct order.
                             (append args rest-forms))
      (loop with *destination* = 'TRASH
            for form in args
            do (c2expr* form)))))

(defun emit-inline-form (form forms)
  (with-c1form-env (form form)
    (precise-loc-lisp-type
     (case (c1form-name form)
       (LOCATION (c1form-arg 0 form) )
       (VARIABLE (emit-inlined-variable form forms))
       (SETQ     (emit-inlined-setq form forms))
       (PROGN    (emit-inlined-progn form forms))
       (VALUES   (emit-inlined-values form forms))
       (t        (emit-inlined-temp-var form t :object)))
     (c1form-primary-type form))))

;;;
;;; inline-args:
;;;   returns locations that contain results of evaluating forms
;;;   side effects: emits code for temporary variables
;;;
;;; Whoever calls this function must wrap the body in WITH-INLINE-BLOCKS.
;;;
(defun inline-args (forms)
  (loop for form-list on forms
        for form = (first form-list)
        collect (emit-inline-form form (rest form-list))))

;;;
;;; inline-arg0:
;;;   returns a location that contains the function
;;;   side effects: emits code for a temporary variable
;;;
;;; Whoever calls this function must wrap the body in WITH-INLINE-BLOCKS.
;;;
(defun inline-arg0 (value-form other-forms)
  (emit-inline-form value-form other-forms))
