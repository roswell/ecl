#-ecl(in-package "CL-USER")
#+ecl(in-package "COMPILER")

(defgeneric dynamic-environment (object)
  (:method (object) object)
  (:method ((object c1form))
    (c1form-env object)))

(defun make-dynamic-environment (&optional parent)
  #-ecl(or parent (cons nil nil))
  #+ecl(or parent (cmp-env-root)))

;;; Module represents a compilation unit. Each compilation unit maintains
;;; constants, a set of functions, and a special block of code "top-level" that
;;; is executed when the module is loaded.
(defclass module ()
  ((constants :reader constants :initform (make-array 0 :adjustable t :fill-pointer t))
   (functions :reader functions :initform (make-array 0 :adjustable t :fill-pointer t))
   (top-level :reader top-level :initarg :top-level)))

(defmethod dynamic-environment ((object module))
  #-ecl(cons nil nil)
  #+ecl(cmp-env-root))


;;; IBLOCK is a block of instructions that may be executed without changing the
;;; dynamic environment and corresponds to a basic block in contemporary CFG.
;;;
;;; C1FORMs representation is essentially the AST of the program. In this pass
;;; we convert it to the control flow graph composed of iblocks (c.f cleavir).
(defclass iblock ()
  ((name
    :accessor iblock-name
    :initarg :name)
   (denv
    :reader dynamic-environment
    :initarg :denv)
   (inputs
    :accessor iblock-inputs
    :initform '())
   (outputs
    :accessor iblock-outputs
    :initform '())
   (instrucitons
    :reader iblock-instructions
    :initform (make-array 0 :adjustable t :fill-pointer t))))

(defun make-iblock (name denv)
  (make-instance 'iblock :name name :denv (dynamic-environment denv)))

(defun connect-iblocks (from to)
  (push from (iblock-inputs to))
  (push to (iblock-outputs from)))


;;; In principle we could construct CFG with IBLOCK and operators defined for
;;; them. BIR-RESULT is used to track the CFG first and current IBLOCKs. This
;;; aids the recursive construction of the graph.
;;;
;;; BIR-ENTER is the entry point and does not change.
;;; BIR-TRAIL may be modified at any time:
;;;
;;; - BIR-INSERT connects the trail to an iblock and replaces the trail
;;; - BIR-ESCAPE connects the trail to an iblock and kills the trail (NIL)
;;; - BIR-RETURN sets the trail to an iblock
;;;
;;; Dynamic environments of two connected iblocks may differ and transitioning
;;; between them may involve arbitrary operations.

(defclass bir-result ()
  ((enter :initarg :enter :accessor bir-enter)
   (trail :initarg :trail :accessor bir-trail)))

(defun make-bir-result (parent)
  (let ((enter (make-iblock :enter (dynamic-environment parent))))
    (make-instance 'bir-result :enter enter :trail enter)))

(defmethod dynamic-environment ((object bir-result))
  (dynamic-environment (bir-trail object)))

(defun bir-insert (iblock bir)
  (connect-iblocks (bir-trail bir) iblock)
  (setf (bir-trail bir) iblock))

(defun bir-escape (iblock bir)
  (connect-iblocks (bir-trail bir) iblock)
  (setf (bir-trail bir) nil))

(defun bir-return (iblock bir)
  (setf (bir-trail bir) iblock))

(defun bir-rewind (iblock bir)
  (connect-iblocks (bir-trail bir) iblock))

;;; Notice that until now we've treated the dynamic environment as an opaque
;;; object. It is not clear yet whether we should introduce an "extra"
;;; environment for analysis beyond the current *cmp-env* stored in c1form.
;;;
;;; Cleavir maintains the environment as a pair: the parent dynamic environment
;;; and a hash table that stores bindings. ECL represents the environment as an
;;; association list.


;;; Helper functions

(defun add-instruction (instruction target)
  (etypecase target
    (iblock
     (vector-push-extend instruction (iblock-instructions target)))
    (bir-result
     (vector-push-extend instruction (iblock-instructions
                                      (bir-trail target))))))

;;; poor-man's fold.
(defun add-something (something target)
  (add-instruction something target)
  ;(bir-insert something target)
  )

;;; C1FORMS have a single class and are distinguished by a name, that's why we
;;; trampoline to the handler by expanding the name. Handlers are defined with
;;; the macro DEFINE-BIR-METHOD.
(defgeneric %bir-from-c1form (name form bir-result)
  ;; This default method should ERROR before merging this pass to develop.
  (:method ((name t) (form c1form) (bir bir-result))
    (bir-insert (make-iblock `(:unknown ,name) bir) bir)
    (add-instruction form bir)))

(defun bir-from-c1form (form bir)
  (typecase form
    (c1form
     (%bir-from-c1form (c1form-name form) form bir))
    (otherwise
     (bir-insert (make-iblock :not-c1form bir) bir)
     (add-instruction `(:value ,form) bir)))
  bir)

(defmacro define-bir-method (node-name (var bir) args &body body)
  (let ((name (gensym)))
    `(defmethod %bir-from-c1form
         ((,name (eql ',node-name)) (,var c1form) (,bir bir-result))
       (destructuring-bind ,args (c1form-args ,var)
         ,@body))))

(define-bir-method ordinary (form bir) (c1form)
  (bir-from-c1form c1form bir))

(define-bir-method call-global (form bir) (fun-name arg-values)
  (declare (ignore arg-values))
  (add-something (make-iblock `(:call-global ,fun-name) bir) bir))

(define-bir-method ffi:c-inline (form bir) (args types return c-expression se-p ol-p)
  (declare (ignorable args types return c-expression se-p ol-p))
  (add-something (make-iblock `(:c-inline ,c-expression) bir) bir))

(define-bir-method progn (form bir) (body)
  (loop while (bir-trail bir)
        for form in body
        do (bir-from-c1form form bir)))

(define-bir-method if (form bir) (fmla-c1form true-c1form false-c1form)
  (bir-from-c1form fmla-c1form bir)
  (let ((entry (bir-trail bir))
        (denv (dynamic-environment bir)))
    (when entry
      (flet ((go-path (name form)
               (bir-insert (make-iblock name denv) bir)
               (bir-from-c1form form bir)
               (prog1 (bir-trail bir)
                 (bir-return entry bir))))
        (let ((a-returns (go-path :if-true true-c1form))
              (b-returns (go-path :if-false false-c1form)))
          (if (and a-returns b-returns)
              (let ((join (make-iblock :if-join bir)))
                ;; Connect "true"
                (bir-return a-returns bir)
                (bir-insert join bir)
                ;; Connect "false"
                (bir-return b-returns bir)
                (bir-insert join bir))
              (bir-return (or a-returns b-returns) bir)))))))

;;; The IR maintains nodes that are defined in `cmptables.lsp:+all-c1-forms+'.
;;; Common Lisp treats some forms specially when they are top-level. That is
;;; reflected in the internal representation by simple means of nesting.
;;;
;;; When we process a top-level form with the special meaning then the c1form of
;;; the appropriate type is created, otherwise a c1form of type `ordinary' is
;;; created and the real c1form is its argument.
;;;
;;; Some c1forms have locations as arguments. Locations are values that are
;;; expected to be available at runtime.

(defun compiler-pass/custom-pass (forms)
  (cmpprogress "~&;;; Constructing BIR.")
  (let* ((bir (make-bir-result (make-dynamic-environment)))
         (mod (make-instance 'module :top-level bir)))
    (dolist (form forms)
      (bir-from-c1form form bir))
    (when (bir-trail bir)
      (bir-insert (make-iblock :leave mod) bir))
    mod))
