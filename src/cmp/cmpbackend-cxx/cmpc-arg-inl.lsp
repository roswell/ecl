;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;; Open coding nested forms as C expressions while preserving the order of
;;;; evaluation. Resulting locations stored in the INLINE-ARG structure may be
;;;; used inline in C expressions (locs still must to be coerced appropriately).

(in-package "COMPILER")

(defstruct (inlined-arg (:constructor %make-inlined-arg))
  loc
  type
  rep-type)

(defun make-inlined-arg (loc lisp-type)
  (%make-inlined-arg :loc loc :type lisp-type
                     :rep-type (loc-representation-type loc)))

(defun maybe-open-inline-block ()
  (unless (plusp *inline-blocks*)
    (open-inline-block)))

(defun open-inline-block ()
  (wt-nl-open-brace)
  (incf *inline-blocks*))

(defun close-inline-blocks ()
  (loop for i of-type fixnum from 0 below *inline-blocks*
        do (wt-nl-close-brace)))

(defun coerce-locs (inlined-args &optional types args-to-be-saved)
  ;; INLINED-ARGS is a list of INLINED-ARG produced by the argument inliner.
  ;; The structure contains a location, a lisp type, and the mach rep type.
  ;;
  ;; ARGS-TO-BE-SAVED is a positional list created by C-INLINE, instructing that
  ;; the value should be saved in a temporary variable.
  ;;
  ;; TYPES is a list of destination types, to which the former values are
  ;; coerced. The destination type can be:
  ;;
  ;;   - A machine rep type (:OBJECT, :FIXNUM, :INT, ...)
  ;;   - A lisp type (T, INTEGER, STRING, CHARACTER, ...))
  ;;
  (loop with block-opened = nil
        for arg in inlined-args
        for loc = (inlined-arg-loc arg)
        for arg-rep-type = (inlined-arg-rep-type arg)
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
              ((equal rep-type arg-rep-type)
               loc)
              (t
               `(COERCE-LOC ,rep-type ,loc)))))


(defun make-inline-temp-var (value-type &optional rep-type)
  (let ((out-rep-type (or rep-type (lisp-type->rep-type value-type))))
    (if (eq out-rep-type :object)
        (make-temp-var)
        (let ((var (make-lcl-var :rep-type out-rep-type
                                 :type value-type)))
          (open-inline-block)
          (wt-nl (rep-type->c-name out-rep-type) " " var ";")
          var))))

(defun emit-inlined-variable (form rest-forms)
  (let ((var (c1form-arg 0 form))
        (lisp-type (c1form-primary-type form)))
    (if (var-changed-in-form-list var rest-forms)
        (let ((temp (make-inline-temp-var lisp-type (var-rep-type var))))
          (set-loc temp var)
          (make-inlined-arg temp lisp-type))
        (make-inlined-arg var lisp-type))))

(defun emit-inlined-setq (form rest-forms)
  (let ((vref (c1form-arg 0 form))
        (form1 (c1form-arg 1 form)))
    (let ((*destination* vref))
      (c2expr* form1))
    (if (eq (c1form-name form1) 'LOCATION)
        (make-inlined-arg (c1form-arg 0 form1) (c1form-primary-type form1))
        (emit-inlined-variable (make-c1form 'VARIABLE form vref nil) rest-forms))))

(defun emit-inlined-call-global (form expected-type)
  (let* ((fname (c1form-arg 0 form))
         (args (c1form-arg 1 form))
         (return-type (c1form-primary-type form))
         (fun (find fname *global-funs* :key #'fun-name :test #'same-fname-p))
         (loc (call-global-loc fname fun args return-type expected-type))
         (type (type-and return-type (loc-type loc)))
         (temp (make-inline-temp-var type (loc-representation-type loc))))
    (set-loc temp loc)
    (make-inlined-arg temp type)))

(defun emit-inlined-progn (form forms)
  (let ((args (c1form-arg 0 form)))
    (loop with *destination* = 'TRASH
          while (rest args)
          do (c2expr* (pop args)))
    (emit-inline-form (first args) forms)))

(defun emit-inlined-values (form forms)
  (let ((args (c1form-arg 0 form)))
    (prog1 (emit-inline-form (or (pop args) (c1nil))
                             ;; the rest of the values args need to be
                             ;; added to the rest forms to execute side
                             ;; effects in the correct order
                             (append args forms))
      (loop with *destination* = 'TRASH
            for form in args
            do (c2expr* form)))))

(defun emit-inlined-structure-ref (form rest-forms)
  (let ((type (c1form-primary-type form)))
    (if (some #'c1form-side-effects rest-forms)
        (let* ((temp (make-inline-temp-var type :object))
               (*destination* temp))
          (c2expr* form)
          (make-inlined-arg temp type))
        (make-inlined-arg (list 'SI:STRUCTURE-REF
                                (first (coerce-locs
                                        (inline-args (list (c1form-arg 0 form)))))
                                (c1form-arg 1 form)
                                (c1form-arg 2 form)
                                (c1form-arg 3 form))
                          type))))

(defun emit-inlined-instance-ref (form rest-forms)
  (let ((type (c1form-primary-type form)))
    (if (some #'c1form-side-effects rest-forms)
        (let* ((temp (make-inline-temp-var type :object))
               (*destination* temp))
          (c2expr* form)
          (make-inlined-arg temp type))
        (make-inlined-arg (list 'SI:INSTANCE-REF
                                (first (coerce-locs
                                        (inline-args (list (c1form-arg 0 form)))))
                                (c1form-arg 1 form)
                                #+ (or) (c1form-arg 2 form))
                          type))))

(defun emit-inline-form (form forms)
  (with-c1form-env (form form)
    (case (c1form-name form)
      (LOCATION
       (make-inlined-arg (c1form-arg 0 form) (c1form-primary-type form)))
      (VARIABLE
       (emit-inlined-variable form forms))
      (CALL-GLOBAL
       (emit-inlined-call-global form (c1form-primary-type form)))
      (SI:STRUCTURE-REF
       (emit-inlined-structure-ref form forms))
      (SI:INSTANCE-REF
       (emit-inlined-instance-ref form forms))
      (SETQ
       (emit-inlined-setq form forms))
      (PROGN
       (emit-inlined-progn form forms))
      (VALUES
       (emit-inlined-values form forms))
      (t (let* ((type (c1form-primary-type form))
                (temp (make-inline-temp-var type))
                (*destination* temp))
           (c2expr* form)
           (make-inlined-arg temp type))))))

;;;
;;; inline-args:
;;;   returns a list of pairs (type loc)
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
