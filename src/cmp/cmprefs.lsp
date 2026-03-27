
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See file 'LICENSE' for the copyright details.

(in-package #:compiler)

;;;
;;; REF OBJECT
;;;
;;; Base object for functions, variables and statements. We use it to
;;; keep track of references to objects, how many times the object is
;;; referenced, by whom, and whether the references cross some closure
;;; boundaries.
;;;

(defstruct (ref (:print-object print-ref))
  name                 ;; Identifier of reference.
  (ref 0 :type fixnum) ;; Number of references.
  ref-ccb              ;; Cross closure reference: T or NIL.
  ref-clb              ;; Cross local function reference: T or NIL.
  read-nodes           ;; Nodes (c1forms) in which the reference occurs.
  )

(defun print-ref (ref-object stream)
  (ext:if-let ((name (ref-name ref-object)))
    (format stream "#<a ~A: ~A>" (type-of ref-object) name)
    (format stream "#<a ~A>" (type-of ref-object))))

(deftype OBJECT () `(not (or fixnum character float)))

(defstruct (var (:include ref) (:constructor %make-var) (:print-object print-var))
#|
  name         ;;; Variable name.
  (ref 0 :type fixnum) ;;; Number of references to the variable (-1 means IGNORE).
  ref-ccb      ;;; Cross closure reference: T or NIL.
  ref-clb      ;;; Cross local function reference: T or NIL.
  read-nodes   ;;; Nodes (c1forms) in which the reference occurs
|#
  set-nodes     ;;; Nodes in which the variable is modified
  kind          ;;; One of LEXICAL, CLOSURE, SPECIAL, GLOBAL, :OBJECT,
                ;;; or some C representation type (:FIXNUM, :CHAR, etc)
  (function *current-function*)
                ;;; For local variables, in which function it was created.
                ;;; For global variables, it doesn't have a meaning.
  (functions-setting nil)
  (functions-reading nil)
                ;;; Functions in which the variable has been modified or read.
  (loc 'OBJECT) ;;; During Pass 1: OBJECT
                ;;; During Pass 2:
                ;;; For :FIXNUM, :CHAR, :FLOAT, :DOUBLE, :OBJECT:
                ;;;   the cvar for the C variable that holds the value.
                ;;; For LEXICAL or CLOSURE: the frame-relative address for
                ;;; the variable in the form of a cons '(lex-levl . lex-ndx)
                ;;;     lex-levl is the level of lexical environment
                ;;;     lex-ndx is the index within the array for this env.
                ;;; For SPECIAL and GLOBAL: the vv-index for variable name.
  (type t)      ;;; Type of the variable.
  (ignorable nil) ;;; Whether there was an IGNORABLE/IGNORE declaration
  )

(defun print-var (var-object stream)
  (format stream "#<a VAR: ~A KIND: ~A>" (var-name var-object) (var-kind var-object)))

;;; A function may be compiled into a CFUN, CCLOSURE or CCLOSURE+LISP_CLOSURE
;;; Here are examples of function FOO for the 3 cases:
;;; 1.  (flet ((foo () (bar))) (foo))           CFUN
;;; 2.  (flet ((foo () (bar))) #'foo)           CFUN+LISP_CFUN
;;; 3.  (flet ((foo () x)) #'(lambda () (foo))) CCLOSURE
;;; 4.  (flet ((foo () x)) #'foo)               CCLOSURE+LISP_CLOSURE

;;; A function can be referenced across a ccb without being a closure, e.g:
;;;   (flet ((foo () (bar))) #'(lambda () (foo)))
;;;   [the lambda also need not be a closure]
;;; and it can be a closure without being referenced across ccb, e.g.:
;;;   (flet ((foo () x)) #'foo)  [ is this a mistake in local-function-ref?]
;;; Here instead the lambda must be a closure, but no closure is needed for foo
;;;   (flet ((foo () x)) #'(lambda () (foo)))
;;; So we use two separate fields: ref-ccb and closure.
;;; A CCLOSURE must be created for a function when:
;;; 1. it appears within a FUNCTION construct and
;;; 2. it uses some ccb references (directly or indirectly).
;;; ref-ccb corresponds to the first condition, i.e. function is referenced
;;;   across CCB. It is computed during Pass 1. A value of 'RETURNED means
;;;   that it is immediately within FUNCTION.
;;; closure corresponds to second condition and is computed in Pass 2 by
;;;   looking at the info-referenced-vars and info-local-referenced of its body.

;;; A LISP_CFUN or LISP_CLOSURE must be created when the function is returned.
;;; The LISP funob may then be referenced locally or across a function boundary:
;;;     (flet ((foo (z) (bar z))) (list #'foo)))
;;;     (flet ((foo (z) z)) (flet ((bar () #'foo)) (bar)))
;;;     (flet ((foo (z) (bar z))) #'(lambda () #'foo)))
;;; therefore we need field funob.

(defstruct (fun (:include ref))
#|
  name                  ;;; Function name.
  (ref 0 :type fixnum)  ;;; Number of references.
                        ;;; During Pass1, T or NIL.
                        ;;; During Pass2, the vs-address for the
                        ;;; function closure, or NIL.
  ref-ccb               ;;; Cross closure reference: T or NIL.
  ref-clb               ;;; Unused.
  read-nodes            ;;; Nodes (c1forms) in which the reference occurs.
|#
  cfun                  ;;; The cfun for the function.
  (variadic-entrypoint-cfun :unknown) ;;; The cfun for an entrypoint with variadic signature.
  (level 0)             ;;; Level of lexical nesting for a function.
  (env 0)               ;;; Size of env of closure.
  (global nil)          ;;; Global lisp function.
  (exported nil)        ;;; Its C name can be seen outside the module.
  (no-entry nil)        ;;; NIL if declared as C-LOCAL. Then we create no
                        ;;; function object and the C function is called
                        ;;; directly
  closure               ;;; During Pass2, T if env is used inside the function
  var                   ;;; the variable holding the funob
  description           ;;; Text for the object, in case NAME == NIL.
  lambda                ;;; Lambda c1-form for this function.
  lambda-expression     ;;; LAMBDA or LAMBDA-BLOCK expression
  (minarg 0)            ;;; Min. number arguments that the function receives.
  (maxarg call-arguments-limit)
                        ;;; Max. number arguments that the function receives.
  (return-type '(VALUES &REST T))
  (parent *current-function*)
                        ;;; Parent function, NIL if global.
  (local-vars nil)      ;;; List of local variables created here.
  (referenced-vars nil) ;;; List of external variables referenced here.
  (referenced-funs nil) ;;; List of external functions called in this one.
                        ;;; We only register direct calls, not calls via object.
  (referencing-funs nil);;; Functions that reference this one
  (child-funs nil)      ;;; List of local functions defined here.
  (file (car ext:*source-location*))
                        ;;; Source file or NIL
  (file-position (or (cdr ext:*source-location*) *compile-file-position*))
                        ;;; Top-level form number in source file
  (cmp-env (cmp-env-copy)) ;;; Environment
  required-lcls            ;;; Names of the function arguments
  (optional-type-check-forms nil) ;;; Type check forms for optional arguments
  (keyword-type-check-forms nil)  ;;; Type check forms for keyword arguments
  )

(defstruct (blk (:include ref))
#|
  name                  ;;; Block name.
  (ref 0 :type fixnum)  ;;; Total number of block references.
  ref-ccb               ;;; Unused (see blk-var).
  ref-clb               ;;; Unused (see blk-var).
  read-nodes            ;;; Unused (see blk-var).
|#
  exit                  ;;; Where to return. A label.
  destination           ;;; Where the value of the block to go.
  var                   ;;; Variable containing the block id and its references.
  (type '(VALUES &REST T)) ;;; Estimated type.
  )

(defstruct (tag (:include ref))
#|
   name                 ;;; Tag name.
   (ref 0 :type fixnum) ;;; Number of references.
   ref-ccb              ;;; Unused (see tag-var).
   ref-clb              ;;; Unused (see tag-var).
   read-nodes           ;;; Unused (see tag-var).
|#
  jump                  ;;; Where to escape. A label.
  var                   ;;; Variable containing frame ID.
  index                 ;;; An integer denoting the label.
  )
