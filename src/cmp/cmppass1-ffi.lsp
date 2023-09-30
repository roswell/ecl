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

;;; FIXME provide the AST form for clines. Currently c1clines is defined in
;;; cmppass2-ffi and pushes directly to a backend-specific variable.
#+ (or)
(defun c1clines (args)
  (make-c1form* 'ffi:clines :args args))

(defun c1c-inline (args)
  ;; We are on the safe side by assuming that the form has side effects
  (destructuring-bind (arguments arg-types output-type c-expression
                                 &rest rest
                                 &key (side-effects t) one-liner
                                 &aux output-rep-type)
      args
    (unless (= (length arguments) (length arg-types))
      (cmperr "In a C-INLINE form the number of declare arguments and the number of supplied ones do not match:~%~S"
              `(ffi:c-inline ,@args)))
    ;; We cannot handle :cstrings as input arguments. :cstrings are
    ;; null-terminated strings, but not all of our lisp strings will
    ;; be null terminated. In particular, those with a fill pointer
    ;; will not.
    (ext:when-let ((ndx (position :cstring arg-types)))
      (let* ((var (gensym))
             (arguments (copy-list arguments))
             (value (elt arguments ndx)))
        (setf (elt arguments ndx) var
              (elt arg-types ndx) :char*)
        (return-from c1c-inline
          (c1expr
           `(ffi::with-cstring (,var ,value)
              (ffi:c-inline ,arguments ,arg-types ,output-type ,c-expression
                            ,@rest))))))
    ;; Find out the output types of the inline form. The syntax is rather relaxed
    ;;  output-type = lisp-type | c-type | (values {lisp-type | c-type}*)
    (flet ((produce-type-pair (type)
             (if (lisp-type-p type)
                 (cons type (lisp-type->rep-type type))
                 (cons (rep-type->lisp-type type) type))))
      (cond ((eq output-type ':void)
             (setf output-rep-type '()
                   output-type 'NIL))
            ((equal output-type '(VALUES &REST t))
             (setf output-rep-type '((VALUES &REST t))))
            ((and (consp output-type) (eql (first output-type) 'VALUES))
             (let ((x (mapcar #'produce-type-pair (rest output-type))))
               (setf output-rep-type (mapcar #'cdr x)
                     output-type `(VALUES ,@(mapcar #'car x)))))
            (t
             (let ((x (produce-type-pair output-type)))
               (setf output-type (car x)
                     output-rep-type (list (cdr x)))))))
    (unless (and (listp arguments)
                 (listp arg-types)
                 (stringp c-expression))
      (cmperr "C-INLINE: syntax error in ~S"
              (list* 'ffi:c-inline args)))
    (unless (= (length arguments)
               (length arg-types))
      (cmperr "C-INLINE: wrong number of arguments in ~S"
              (list* 'ffi:c-inline args)))
    (let* ((arguments (mapcar #'c1expr arguments))
           (form (make-c1form* 'ffi:c-inline :type output-type
                               :side-effects side-effects
                               :args arguments arg-types
                               output-rep-type
                               c-expression
                               side-effects
                               one-liner)))
      (loop for form in arguments
         when (eq (c1form-name form) 'VAR)
         do (let ((var (c1form-arg 0 form)))
              (add-to-set-nodes var form)))
      form)))

(defun c1c-progn (arguments)
  (let* ((variables (mapcar #'c1vref (pop arguments)))
         (statements (loop for form in arguments
                        collect (if (stringp form)
                                    form
                                    (c1expr form))))
         (form (make-c1form* 'FFI:C-PROGN :type NIL
                             :side-effects t
                             :args variables statements)))
    (add-to-set-nodes-of-var-list variables form)
    form))

;;;;
;;;;  CMPCBK --  Callbacks: lisp functions that can be called from the C world

(defconstant +foreign-elt-type-codes+
  '(                (:char               . "ECL_FFI_CHAR")
                    (:unsigned-char      . "ECL_FFI_UNSIGNED_CHAR")
                    (:byte               . "ECL_FFI_BYTE")
                    (:unsigned-byte      . "ECL_FFI_UNSIGNED_BYTE")
                    (:short              . "ECL_FFI_SHORT")
                    (:unsigned-short     . "ECL_FFI_UNSIGNED_SHORT")
                    (:int                . "ECL_FFI_INT")
                    (:unsigned-int       . "ECL_FFI_UNSIGNED_INT")
                    (:long               . "ECL_FFI_LONG")
                    (:unsigned-long      . "ECL_FFI_UNSIGNED_LONG")
    #+:uint16-t     (:int16-t            . "ECL_FFI_INT16_T")
    #+:uint16-t     (:uint16-t           . "ECL_FFI_UINT16_T")
    #+:uint32-t     (:int32-t            . "ECL_FFI_INT32_T")
    #+:uint32-t     (:uint32-t           . "ECL_FFI_UINT32_T")
    #+:uint64-t     (:int64-t            . "ECL_FFI_INT64_T")
    #+:uint64-t     (:uint64-t           . "ECL_FFI_UINT64_T")
    #+:long-long    (:long-long          . "ECL_FFI_LONG_LONG")
    #+:long-long    (:unsigned-long-long . "ECL_FFI_UNSIGNED_LONG_LONG")
                    (:pointer-void       . "ECL_FFI_POINTER_VOID")
                    (:cstring            . "ECL_FFI_CSTRING")
                    (:object             . "ECL_FFI_OBJECT")
                    (:float              . "ECL_FFI_FLOAT")
                    (:double             . "ECL_FFI_DOUBLE")
                    (:long-double        . "ECL_FFI_LONG_DOUBLE")
    #+complex-float (:csfloat            . "ECL_FFI_CSFLOAT")
    #+complex-float (:cdfloat            . "ECL_FFI_CDFLOAT")
    #+complex-float (:clfloat            . "ECL_FFI_CLFLOAT")
                    (:void               . "ECL_FFI_VOID")))

(defun foreign-elt-type-code (type)
  (ext:if-let ((x (assoc type +foreign-elt-type-codes+)))
    (cdr x)
    (cmperr "DEFCALLBACK: ~a is not a valid elementary FFI type." type)))

;;; We could have made FFI:DEFCALLBACK to accept any ffi type defined
;;; for the current machine (see cmpc-machine.lisp), but it wouldn't
;;; be useful because it only extends FFI types with ECL-specific
;;; types like :fixnum or :sse2. Another argument against such
;;; approach is semantic equivalence between interpreted and compiled
;;; versions of the special form. -- jd 2019-11-27
(defun c1-defcallback (args)
  (destructuring-bind (name return-type arg-list &rest body)
      args
    (cond ((eql return-type nil)
           (setf return-type :void))
          ((and (consp return-type)
                (member (first return-type) '(* array)))
           (setf return-type :pointer-void)))
    (let ((arg-types '())
          (arg-type-constants '())
          (arg-variables '())
          (c-name (format nil "ecl_callback_~d" (length *callbacks*)))
          (name (if (consp name) (first name) name))
          (call-type (if (consp name) (second name) :cdecl)))
      (dolist (i arg-list)
        (unless (consp i)
          (cmperr "Syntax error in CALLBACK form: C type is missing in argument ~A "i))
        (push (first i) arg-variables)
        (let ((type (second i)))
          (push type arg-types)
          (push (foreign-elt-type-code type) arg-type-constants)))
      (push (list name c-name (add-object name)
                  return-type
                  (foreign-elt-type-code return-type)
                  (reverse arg-types)
                  (reverse arg-type-constants)
                  call-type)
            *callbacks*)
      (c1expr
       `(progn
          (defun ,name ,(reverse arg-variables) ,@body)
          (si:put-sysprop ',name :callback
                          (ffi:c-inline (:pointer-void) (:object) :object
                                        ,(format nil "ecl_make_foreign_data(#0,0,(void*)~a)" c-name)
                                        :one-liner t)))))))
