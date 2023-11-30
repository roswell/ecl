;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;; Open coding optimizer.

(in-package "COMPILER")

(defun make-inline-temp-var (value-type &optional rep-type)
  (let ((out-rep-type (or rep-type (lisp-type->rep-type value-type))))
    (if (eq out-rep-type :object)
        (make-temp-var)
        (let ((var (make-lcl-var :rep-type out-rep-type
                                 :type value-type)))
          (open-inline-block)
          (wt-nl (rep-type->c-name out-rep-type) " " var ";")
          var))))

(defun save-inline-loc (loc)
  (let* ((rep-type (loc-representation-type (second loc)))
         (temp (make-inline-temp-var (first loc) rep-type)))
    (set-loc temp loc)
    temp))

(defun emit-inlined-variable (form rest-forms)
  (let ((var (c1form-arg 0 form))
        (value-type (c1form-primary-type form)))
    (if (var-changed-in-form-list var rest-forms)
        (let ((temp (make-inline-temp-var value-type (var-rep-type var))))
          (set-loc temp var)
          (list value-type temp))
        (list value-type var))))

(defun emit-inlined-setq (form rest-forms)
  (let ((vref (c1form-arg 0 form))
        (form1 (c1form-arg 1 form)))
    (let ((*destination* vref))
      (c2expr* form1))
    (if (eq (c1form-name form1) 'LOCATION)
        (list (c1form-primary-type form1) (c1form-arg 0 form1))
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
    (list type temp)))

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
          (list type temp))
        (list type
              (list 'SI:STRUCTURE-REF
                    (first (coerce-locs
                            (inline-args (list (c1form-arg 0 form)))))
                    (c1form-arg 1 form)
                    (c1form-arg 2 form)
                    (c1form-arg 3 form))))))

(defun emit-inlined-instance-ref (form rest-forms)
  (let ((type (c1form-primary-type form)))
    (if (some #'c1form-side-effects rest-forms)
        (let* ((temp (make-inline-temp-var type :object))
               (*destination* temp))
          (c2expr* form)
          (list type temp))
        (list type
              (list 'si:instance-ref
                    (first (coerce-locs
                            (inline-args (list (c1form-arg 0 form)))))
                    (c1form-arg 1 form)
                    #+nil (c1form-arg 2 form))))))

(defun emit-inline-form (form forms)
  (with-c1form-env (form form)
    (case (c1form-name form)
      (LOCATION
       (list (c1form-primary-type form) (c1form-arg 0 form)))
      (VARIABLE
       (emit-inlined-variable form forms))
      (CALL-GLOBAL
       (emit-inlined-call-global form (c1form-primary-type form)))
      (SI:STRUCTURE-REF
       (emit-inlined-structure-ref form forms))
      #+clos
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
           (list type temp))))))

;;;
;;; inline-args:
;;;   returns a list of pairs (type loc)
;;;   side effects: emits code for temporary variables
;;;
;;; Whoever calls inline-args must bind *inline-blocks* to 0 and afterwards
;;; call close-inline-blocks.
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
;;; Whoever calls inline-arg0 must rebind *TEMP*.
;;;
(defun inline-arg0 (value-form other-forms)
  (emit-inline-form value-form other-forms))

(defun maybe-open-inline-block ()
  (unless (plusp *inline-blocks*)
    (open-inline-block)))

(defun open-inline-block ()
  (wt-nl-open-brace)
  (incf *inline-blocks*))

(defun close-inline-blocks ()
  (loop for i of-type fixnum from 0 below *inline-blocks*
     do (wt-nl-close-brace)))
