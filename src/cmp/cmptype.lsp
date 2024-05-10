;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    See file 'LICENSE' for the copyright details.

;;;; CMPTYPE  Type information.

(in-package "COMPILER")

;;;
;;; and-form-type
;;;   returns a copy of form whose type is the type-and of type and the form's
;;;   type
;;;
(defun and-form-type (type form original-form &optional (mode :safe)
                      (format-string "") &rest format-args)
  (let* ((type2 (c1form-primary-type form))
         (type1 (type-and type type2)))
    ;; We only change the type if it is not NIL. Is this wise?
    (if type1
        (setf (c1form-type form) type1)
        (funcall (if (eq mode :safe) #'cmperr #'cmpwarn)
                 "~?, the type of the form ~s is ~s, not ~s." format-string
                 format-args original-form type2 type))
    form))

(defun default-init (var &optional warn)
  (declare (ignore warn))
  (let ((new-value (cdr (assoc (var-type var)
                               '((fixnum . 0)
                                 (character . #\space)
                                 (long-float   . 0.0L1)
                                 (double-float . 0.0D1)
                                 (single-float . 0.0F1)
                                 #+complex-float
                                 (si:complex-single-float . #c(0.0f0 0.0f0))
                                 #+complex-float
                                 (si:complex-double-float . #c(0.0d0 0.0d0))
                                 #+complex-float
                                 (si:complex-single-float . #c(0.0l0 0.0l0)))
                               :test #'subtypep))))
    (if new-value
        (c1constant-value new-value)
        (c1nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TYPE CHECKING
;;

(defun lambda-type-check-associate (fname requireds optionals keywords global-fun-p)
  (multiple-value-bind (arg-types found)
      (and global-fun-p (get-arg-types fname *cmp-env* global-fun-p))
    (if found
        (multiple-value-bind (req-types opt-types rest-flag key-flag
                                        key-types allow-other-keys)
            (si::process-lambda-list arg-types 'ftype)
          (declare (ignore rest-flag key-flag allow-other-keys))
          (list
           (loop for var in requireds
              for type in (rest req-types)
              collect (cons var type))
           (loop for optional in optionals by #'cdddr
              for type in (rest opt-types) by #'cdddr
              collect (cons optional type))
           (loop for key-list on keywords by #'cddddr
              for keyword = (first key-list)
              for key-var = (second key-list)
              for type = (loop for key-list on (rest key-types) by #'cddr
                            when (eq keyword (first key-list))
                            return (second key-list)
                            finally (return t))
              collect (cons key-var type))))
        (list
         (loop for var in requireds
            collect (cons var t))
         (loop for optional in optionals by #'cdddr
            collect (cons optional t))
         (loop for key-list on keywords by #'cddddr
            for key-var = (second key-list)
            collect (cons key-var t))))))

(defun lambda-type-check-precise (assoc-list ts)
  (loop for record in assoc-list
     for var = (car record)
     for type = (assoc (var-name var) ts)
     when type
     do
     ;; Instead of trusting the global proclamation, we set a check based
     ;; on the local declaration, without type merging.
       (rplacd record (cdr type))
       #+(or)
       (rplacd record (type-and (cdr record) (cdr type))))
  assoc-list)

(defun lambda-type-check-expand (type-checks policy-check-type other-decls)
  (loop with checks = '()
     with new-auxs = '()
     for (var . type) in type-checks
     for name = (var-name var)
     do (if (eq type t) ;; Non trivial types are the only ones we care about
            (setf checks (list* nil checks))
            (if (and policy-check-type
                  (loop for decl in other-decls
                     never (and (consp decl)
                                (eq (first decl)
                                    'si::no-check-type)
                                (member name (rest decl)))))
             ;; We remove assumption about types, which will be checked
             ;; later due to this assertion...
             (setf (var-type var) t
                   checks (list* `(type-assertion ,name ,type) checks)
                   new-auxs (list* `(ext:truly-the ,type ,name) name new-auxs))
             ;; Or simply enforce the variable's type.
             (setf (var-type var) (type-and (var-type var) type))))
     finally
       (return (values checks new-auxs))))

(defun extract-lambda-type-checks (fname requireds optionals keywords ts other-decls)
  ;; We generate automatic type checks for function arguments that
  ;; are declared. These checks can be deactivated by appropriate
  ;; safety settings which are checked by ASSERT-TYPE. Note
  ;; that not all type declarations can be checked (take for instance
  ;; (type (function (t t) t) foo)) We let the macro do the job.
  ;; Returns 4 values: type assertions for required, optional, keyword
  ;; arguments and &aux type declarations. Type assertions may be nil,
  ;; if no type check is necessary.
  (let* ((global-fun-p (member '(si::c-global) other-decls :test #'equal))
         (policy-check-type (policy-check-arguments-type))
         (all-type-checks (mapcar #'(lambda (checks)
                                  (lambda-type-check-precise checks ts))
                              (lambda-type-check-associate fname requireds optionals keywords global-fun-p)))
         (checks '())
         (new-auxs '()))
    (setq checks
          (loop for i in all-type-checks
             collect (multiple-value-bind (check new-aux)
                         (lambda-type-check-expand i policy-check-type
                                                   other-decls)
                       (setq new-auxs (nconc new-aux new-auxs))
                       check)))
    (when new-auxs
      (cmpnote "In ~:[an anonymous function~;function ~:*~A~], checking types of argument~@[s~]~{ ~A~}."
               (fun-name *current-function*)
               (nreverse
                (loop for i in (cdr new-auxs) by #'cddr collect i))))
    (values (nreverse (car checks)) (nreverse (cadr checks))
            (nreverse (caddr checks)) (nreverse new-auxs))))

(defmacro assert-type-if-known (value type &environment env)
  "Generates a type check on an expression, ensuring that it is satisfied."
  (multiple-value-bind (trivial valid)
      (subtypep 't type)
    (cond ((and trivial valid)
           value)
          ((multiple-value-setq (valid value) (constant-value-p value env))
           (si::maybe-quote value))
          (t
           (ext:with-clean-symbols (%value)
             `(let* ((%value ,value))
                ,(simple-type-assertion '%value (si::flatten-function-types type))
                (ext:truly-the ,type %value)))))))

(defmacro optional-type-check (value type)
  (if (policy-assume-right-type)
      value
      `(assert-type-if-known ,value ,type)))

(defmacro with-let*-type-check (triplets &body body)
  `(let* ,(loop for (var value type) in triplets
             collect `(,var (assert-type-if-known ,value ,type)))
     (declare (:read-only ,@(mapcar #'car triplets)))
     ,@body))

