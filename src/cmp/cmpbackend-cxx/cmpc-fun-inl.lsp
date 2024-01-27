;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;;
;;;; Open coding functions as C expressions.
;;;;

(in-package "COMPILER")

(defstruct (inline-info)
  name                  ;;; Function name
  arg-host-types        ;;; List of representation types for the arguments
  return-host-type      ;;; Representation type for the output
  arg-types             ;;; List of lisp types for the arguments
  return-type           ;;; Lisp type for the output
  exact-return-type     ;;; Only use this expansion when the output is
                        ;;; declared to have a subtype of RETURN-TYPE
  multiple-values       ;;; Works with all destinations, including VALUEZ / LEAVE
  expansion             ;;; C template containing the expansion
  one-liner             ;;; Whether the expansion spans more than one line
)

(defmacro define-c-inliner (fname lambda-list &body body)
  `(setf (gethash ',fname *cinline-dispatch-table*)
         #'(lambda ,lambda-list (block nil ,@body))))

(defun apply-inliner (fname return-type inlined-args)
  (ext:if-let ((fd (gethash fname *cinline-dispatch-table*)))
    (apply fd return-type inlined-args)
    (default-c-inliner fname return-type inlined-args)))

(defun default-c-inliner (fname return-type inlined-args)
  (let* ((arg-types (mapcar #'loc-lisp-type inlined-args))
         (ii (inline-function fname arg-types return-type)))
    (and ii (apply-inline-info ii inlined-args))))

;;;
;;; inline-function:
;;;   locs are typed locs as produced by inline-args
;;;   returns NIL if inline expansion of the function is not possible
;;;
(defun inline-function (fname arg-types return-type &optional (return-host-type 'any))
  ;; Those functions that use INLINE-FUNCTION must rebind the variable
  ;; *INLINE-BLOCKS*.
  (and (inline-possible fname)
       (not (gethash fname *c2-dispatch-table*))
       (let* (;; (dest-host-type (loc-host-type *destination*))
              (ii (get-inline-info fname arg-types return-type return-host-type)))
         ii)))

(defun apply-inline-info (ii inlined-locs)
  (let* ((arg-types (inline-info-arg-types ii))
         (out-host-type (inline-info-return-host-type ii))
         ;; (out-type (inline-info-return-type ii))
         (side-effects-p (function-may-have-side-effects (inline-info-name ii)))
         (fun (inline-info-expansion ii))
         (one-liner (inline-info-one-liner ii)))
    (produce-inline-loc inlined-locs arg-types (list out-host-type)
                        fun side-effects-p one-liner)))

(defun choose-inline-info (ia ib return-type return-host-type)
  (declare (ignore return-type))
  (cond
    ;; Only accept inliners that have the right rep type
    ((not (or (eq return-host-type 'any)
              (eq return-host-type :void)
              (let ((info-type (inline-info-return-host-type ib)))
                (or (eq return-host-type info-type)
                    ;; :bool can be coerced to any other location type
                    (eq info-type :bool)))))
     ia)
    ((null ia)
     ib)
    ;; Keep the first one, which is typically the least safe but fastest. 
    ((equal (inline-info-arg-types ia) (inline-info-arg-types ib))
     ia)
    ;; More specific?
    ((every #'type>= (inline-info-arg-types ia) (inline-info-arg-types ib))
     ib)
    ;; Keep the first one, which is typically the least safe but fastest. 
    (t
     ia)))

(defun get-inline-info (fname types return-type return-host-type)
  (declare (si::c-local))
  (let ((output nil))
    (unless (safe-compile)
      (dolist (x (inline-information fname ':INLINE-UNSAFE))
        (ext:when-let ((other (inline-type-matches x types return-type)))
          (setf output (choose-inline-info output other return-type return-host-type)))))
    (dolist (x (inline-information fname ':INLINE-ALWAYS))
      (ext:when-let ((other (inline-type-matches x types return-type)))
        (setf output (choose-inline-info output other return-type return-host-type))))
    output))

(defun to-fixnum-float-type (type)
  (dolist (i '(CL:FIXNUM CL:DOUBLE-FLOAT CL:SINGLE-FLOAT CL:LONG-FLOAT) nil)
    (when (type>= i type)
      (return i))))

(defun maximum-float-type (t1 t2)
  (macrolet ((try-type (type)
               `(and (or (eq t1 ,type) (eq t2 ,type))
                     ,type)))
    (or (and (null t1) t2)
        (try-type 'CL:LONG-FLOAT)
        (try-type 'CL:DOUBLE-FLOAT)
        (try-type 'CL:SINGLE-FLOAT)
        'CL:FIXNUM)))

(defun inline-type-matches (inline-info arg-types return-type)
  (when (and (not (inline-info-multiple-values inline-info))
             (member *destination* '(VALUEZ LEAVE)))
    (return-from inline-type-matches nil))
  (let* ((rts nil)
         (number-max nil))
    ;;
    ;; Check that the argument types match those of the inline expression
    ;;
    (do* ((arg-types arg-types (cdr arg-types))
          (types (inline-info-arg-types inline-info) (cdr types)))
         ((or (endp arg-types) (endp types))
          (when (or arg-types types)
            (return-from inline-type-matches nil)))
      (let* ((arg-type (first arg-types))
             (type (first types)))
        (cond ((eq type 'FIXNUM-FLOAT)
               (let ((new-type (to-fixnum-float-type arg-type)))
                 (unless new-type
                   (return-from inline-type-matches nil))
                 (push new-type rts)
                 (setq number-max (maximum-float-type number-max new-type))))
              #+sse2
              ;; Allow implicit casts between SSE subtypes to kick in
              ((and (type>= 'ext:sse-pack type)
                    (type>= 'ext:sse-pack arg-type))
               (push type rts))
              ((type>= type arg-type)
               (push type rts))
              (t (return-from inline-type-matches nil)))))
    ;;
    ;; Now there is an optional check of the return type. This check is
    ;; only used when enforced by the inliner.
    ;;
    (when (or (eq (inline-info-return-host-type inline-info) :bool)
              (null (inline-info-exact-return-type inline-info))
              (and (policy-assume-right-type)
                   (let ((inline-return-type (inline-info-return-type inline-info)))
                     (if number-max
                         ;; for arithmetic operators we take the maximal
                         ;; type as possible result type. Note that FIXNUM
                         ;; is not an option, because the product, addition
                         ;; or difference of fixnums may be a larger
                         ;; integer.
                         (and (setf number-max (if (eq number-max 'fixnum)
                                                   'integer
                                                   number-max))
                              (type>= inline-return-type number-max)
                              (type>= number-max return-type))
                         ;; no contravariance
                         (type>= inline-return-type return-type)))))
      (let ((inline-info (copy-structure inline-info)))
        (setf (inline-info-arg-types inline-info)
              (nreverse rts))
        inline-info))))

(defun produce-inline-loc (inlined-arguments arg-types output-host-type
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
    ;;(setf output-host-type (lisp-type->host-type output-host-type))

    ;; If the form does not output any data, and there are no side
    ;; effects, try to omit it.
    (when (null output-host-type)
      (if side-effects
          (progn
            (wt-nl)
            (wt-c-inline-loc output-host-type c-expression coerced-arguments t nil)
            (when one-liner (wt ";")))
          (cmpnote "Ignoring form ~S" c-expression))
      (wt-nl "value0 = ECL_NIL;")
      (wt-nl "cl_env_copy->nvalues = 0;")
      (return-from produce-inline-loc 'LEAVE))

    ;; If the form is a one-liner, we can simply propagate this expression until the
    ;; place where the value is used.
    (when one-liner
      (return-from produce-inline-loc
        `(ffi:c-inline ,output-host-type ,c-expression ,coerced-arguments ,side-effects
                       ,(if (equalp output-host-type '((VALUES &REST T)))
                            'VALUES NIL))))

    ;; If the output is a in the VALUES vector, just write down the form and
    ;; output the location of the data.
    (when (equalp output-host-type '((VALUES &REST T)))
      (wt-c-inline-loc output-host-type c-expression coerced-arguments side-effects
                       'VALUES)
      (return-from produce-inline-loc 'VALUEZ))

    ;; Otherwise we have to set up variables for holding the output.
    (flet ((make-output-var (type)
             (let ((var (make-lcl-var :host-type type)))
               (wt-nl (host-type->c-name type) " " var ";")
               var)))
      (open-inline-block)
      (let ((output-vars (mapcar #'make-output-var output-host-type)))
        (wt-c-inline-loc output-host-type c-expression coerced-arguments side-effects output-vars)
        (cond ((= (length output-vars) 1)
               (first output-vars))
              (t
               (loop for v in output-vars
                     for i from 0
                     do (set-loc `(VALUE ,i) v))
               (wt "cl_env_copy->nvalues = " (length output-vars) ";")
               'VALUEZ))))))

;;; Whoever calls this function must wrap the body in WITH-INLINE-BLOCKS.
(defun negate-argument (argument dest-loc)
  (let* ((inlined-arg (emit-inline-form argument nil))
         (host-type (loc-host-type inlined-arg)))
    (apply #'produce-inline-loc
           (list inlined-arg)
           (if (eq (loc-host-type dest-loc) :bool)
               (case host-type
                 (:bool '((:bool) (:bool) "(#0)==ECL_NIL" nil t))
                 (:object '((:object) (:bool) "(#0)!=ECL_NIL" nil t))
                 (otherwise (return-from negate-argument nil)))
               (case host-type
                 (:bool '((:bool) (:object) "(#0)?ECL_NIL:ECL_T" nil t))
                 (:object '((:object) (:object) "Null(#0)?ECL_T:ECL_NIL" nil t))
                 (otherwise (return-from negate-argument *vv-nil*)))))))
