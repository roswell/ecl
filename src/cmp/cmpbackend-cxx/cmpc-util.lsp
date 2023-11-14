
;;; Global variables, flag definitions and utilities.

(in-package "COMPILER")

;;; *inline-blocks* holds the number of C blocks opened for declaring temps for
;;; intermediate results of the evaluation of inlined function calls.

(defvar *inline-blocks* 0)
(defvar *opened-c-braces* 0)

(defvar *emitted-functions* nil)
(defvar *inline-information* nil)

;;; Compiled code uses the following kinds of variables:
;;; 1. Vi, declared explicitely, either unboxed or not (*lcl*, next-lcl)
;;; 2. Ti, declared collectively, of type object, may be reused (*temp*, next-temp)
;;; 4. lexi[j], for lexical variables in local functions
;;; 5. CLVi, for lexical variables in closures

(defvar *lcl* 0)                ; number of local variables

(defvar *temp* 0)               ; number of temporary variables
(defvar *max-temp* 0)           ; maximum *temp* reached

(defvar *level* 0)              ; nesting level for local functions

(defvar *lex* 0)                ; number of lexical variables in local functions
(defvar *max-lex* 0)            ; maximum *lex* reached

(defvar *env* 0)                ; number of variables in current form
(defvar *max-env* 0)            ; maximum *env* in whole function
(defvar *env-lvl* 0)            ; number of levels of environments
(defvar *aux-closure* nil)      ; stack allocated closure needed for indirect calls
(defvar *ihs-used-p* nil)       ; function must be registered in IHS?

(defvar *next-cfun* 0)          ; holds the last cfun used.

;;; *tail-recursion-info* holds NIL, if tail recursion is impossible.
;;; If possible, *tail-recursion-info* holds
;;;     ( c1-lambda-form  required-arg .... required-arg ),
;;; where each required-arg is a var-object.
(defvar *tail-recursion-info* nil)

;;; --cmpexit.lsp--
;;;
;;; *exit* holds an 'exit', which is
;;      LABEL instance or one of RETURNs (i.e. RETURN, RETURN-FIXNUM,
;;      RETURN-CHARACTER, RETURN-LONG-FLOAT, RETURN-DOUBLE-FLOAT, RETURN-SINGLE-FLOAT,
;;      RETURN-CSFLOAT, RETURN-CDFLOAT, RETURN-CLFLOAT or RETURN-OBJECT).
;;; *unwind-exit* holds a list consisting of:
;;      LABEL instance, one of RETURNs, TAIL-RECURSION-MARK, FRAME,
;;      JUMP, BDS-BIND (each pushed for a single special binding), or a
;;      LCL (which holds the bind stack pointer used to unbind).
;;;

(defvar *exit*)
(defvar *unwind-exit*)

;;; C forms to find out (SETF fname) locations
(defvar *setf-definitions*)             ; holds { name fun-vv name-vv  }*
(defvar *global-cfuns-array*)           ; holds { fun-vv fname-loc fun }*

;;; T/NIL flag to determine whether one may generate lisp constant values as C
;;; structs.
(defvar *use-static-constants-p*
  #+ecl-min t #-ecl-min nil)

;;; Constants that can be built as C values.
(defvar *static-constants*)          ; holds { ( object c-variable constant ) }*

;;; Pairs for inlining constants.
(defvar *optimizable-constants*)        ; holds { (value . c1form) }*


;;; Permanent objects are stored in VV[] that is available for the whole module
;;; during the whole lifetime of the FASL. Temporary objects are stored in
;;; VVtemp[] that is allocated in the initialization function and released after
;;; the FASL is initialized.

(defvar *permanent-objects*)            ; holds { vv-record }*
(defvar *temporary-objects*)            ; holds { vv-record }*

;;; ----------------------------------------------------------------------
;;; CONVENIENCE FUNCTIONS / MACROS
;;;

(defmacro with-cxx-env (() &body body)
  `(let ((*inline-blocks* 0)
         (*open-c-braces* 0)
         (*temp* 0)
         (*max-temp* 0)
         (*next-cfun* 0)
         (*last-label* 0)
         (*unwind-exit* nil)
         (*inline-information*
           (ext:if-let ((r (machine-inline-information *machine*)))
             (si:copy-hash-table r)
             (make-inline-information *machine*)))
         (*setf-definitions* nil)
         (*global-cfuns-array* nil)
         (*static-constants* nil)
         (*optimizable-constants* (make-optimizable-constants *machine*))
         (*permanent-objects* (make-array 128 :adjustable t :fill-pointer 0))
         (*temporary-objects* (make-array 128 :adjustable t :fill-pointer 0)))
     ,@body))

(defun-cached env-var-name (n) eql
  (format nil "env~D" n))

(defun-cached lex-env-var-name (n) eql
  (format nil "lex~D" n))

(defun next-lcl (&optional name)
  (list 'LCL (incf *lcl*) T
        (if (and name (symbol-package name))
            (lisp-to-c-name name)
            "")))

(defun next-temp ()
  (prog1 *temp*
         (incf *temp*)
         (setq *max-temp* (max *temp* *max-temp*))))

(defun next-lex ()
  (prog1 (cons *level* *lex*)
         (incf *lex*)
         (setq *max-lex* (max *lex* *max-lex*))))

(defun next-env ()
  (prog1 *env*
    (incf *env*)
    (setq *max-env* (max *env* *max-env*))))

(defun env-grows (possibily)
  ;; if additional closure variables are introduced and this is not
  ;; last form, we must use a new env.
  (and possibily
       (plusp *env*)
       (dolist (exit *unwind-exit*)
         (case exit
           (RETURN (return NIL))
           (BDS-BIND)
           (t (return T))))))

(defun next-cfun (&optional (prefix "L~D~A") (lisp-name nil))
  (let ((code (incf *next-cfun*)))
    (format nil prefix code (lisp-to-c-name lisp-name))))


;;; *LAST-LABEL* holds the label# of the last used label. This is used by the
;;; code generator to avoid duplicated labels in the same scope.

(defvar *last-label* 0)

(defstruct (label (:predicate labelp))
  id
  used-p)

(defun next-label (used-p)
  (make-label :id (incf *last-label*) :used-p used-p))

(defun maybe-next-label ()
  (if (labelp *exit*)
      *exit*
      (next-label nil)))

(defmacro with-exit-label ((label) &body body)
  `(let* ((,label (next-label nil))
          (*unwind-exit* (cons ,label *unwind-exit*)))
     ,@body
     (wt-label ,label)))

(defmacro with-optional-exit-label ((label) &body body)
  `(let* ((,label (maybe-next-label))
          (*unwind-exit* (adjoin ,label *unwind-exit*)))
     ,@body
     (unless (eq ,label *exit*)
       (wt-label ,label))))
