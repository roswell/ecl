(in-package "COMPILER")

;;; C1FORMs representation is essentially the AST of the program. In this file
;;; we convert it to the control flow graph composed of iblocks (c.f cleavir) -
;;; basic blocks that preserve the dynamic environment of enclosed instructions.
;;; We are taking a big inspiration from the cleavir architecture.

(defvar *current-module*)               ;compilation-unit?
(defvar *current-iblock*)
(defvar *dumpster*)

;;; Module represents the compilation unit.
(defclass module ()
  ((constants :initform (make-array 0 :adjustable t :fill-pointer t)
              :reader constants)
   (functions :initform (make-array 0 :adjustable t :fill-pointer t)
              :reader functions)
   (top-level :initarg :top-level
              :reader top-level)))

(defclass iblock ()
  ((name
    :initarg :name
    :accessor name)
   (node
    :initarg :node
    :accessor node)
   (denv
    :initform '()
    :initarg :denv
    :accessor denv)
   (inputs
    :initform '()
    :accessor inputs)
   (outputs
    :initform '()
    :accessor outputs)
   (instr
    :initform (make-array 0 :adjustable t :fill-pointer t)
    :reader instructions)))

(defun make-iblock (name)
  (make-instance 'iblock :name name))

(defun add-leader (name instruction)
  (format *debug-io* "adding leader ~s~%" name)
  (let* ((iblock (make-instance 'iblock :name name))
         (*current-iblock* iblock))
    (add-normal instruction)
    iblock))

(defun add-normal (instruction)
  (when instruction
    (vector-push-extend instruction (instructions *current-iblock*)))
  *current-iblock*)

(defun connect-iblocks (from to)
  (push from (inputs to))
  (push to (outputs from)))

(defun disconnect-iblocks (from to)
  (setf (inputs to) (delete from (inputs to)))
  (setf (outputs from) (delete to (outputs from))))

(defun next-iblock (next &optional (current *current-iblock*))
  (connect-iblocks current next)
  (setf *current-iblock* next))

;;; BIR-RESULT is an auxiliary structure supporting recursive construction of
;;; the graph.
(defclass bir-result ()
  ((enter :initarg :enter :initform (make-iblock :enter) :accessor bir-enter)
   (leave :initarg :leave :initform (make-iblock :leave) :accessor bir-leave)
   ;; Dead code enters at STRAY (for debugging).
   (stray :initform (make-iblock :stray) :accessor bir-stray)))

(defun make-bir-result (&rest args)
  (apply #'make-instance 'bir-result args))

(defun bir-returns-p (bir)
  (inputs (bir-leave bir)))

(defun bir-replace-enter (bir new-enter)
  (let ((old-enter (bir-enter bir)))
    (dolist (iblock (copy-list (outputs old-enter)))
      (connect-iblocks new-enter iblock)
      (disconnect-iblocks old-enter iblock)))
  (setf (bir-enter bir) new-enter))

(defun bir-replace-leave (bir new-leave)
  (let ((old-leave (bir-leave bir)))
    (dolist (iblock (copy-list (inputs old-leave)))
      (connect-iblocks iblock new-leave)
      (disconnect-iblocks iblock old-leave)))
  (setf (bir-leave bir) new-leave))

;;; Currently c1forms have a single class and are distinguished by a name,
;;; that's why we trampoline to them by expanding the name as a second
;;; argument. DEFINE-BIR-METHOD is responsible for defining handlers.
(defgeneric %bir-from-c1form (name form bir-result))

(defun bir-from-c1form (form &optional (result (make-bir-result)))
  (typecase form
    (c1form
     (%bir-from-c1form (c1form-name form) form result))
    (otherwise
     (connect-iblocks (bir-enter result) (add-leader :not-c1form form))))
  result)

;;; The function returns either the object representing the returned values or
;;; NIL meaning that a non-local control transfer occured.
(defmacro define-bir-method (node-name (var bir) args &body body)
  (ext:with-gensyms (name)
    `(defmethod %bir-from-c1form
         ((,name (eql ',node-name)) (,var c1form) (,bir bir-result))
       (destructuring-bind ,args (c1form-args ,var)
         ,@body))))

(defmethod %bir-from-c1form ((name t) (form c1form) (bir bir-result))
  (let ((iblock (add-leader (list :unknown name) form)))
    (connect-iblocks (bir-enter bir) iblock)
    (connect-iblocks iblock (bir-leave bir))))

(define-bir-method ordinary (form bir) (c1form)
  (bir-from-c1form c1form bir))

;; (defun merge-bir-nodes (prev next)
;;   (make-bir-result))

;; (defun insert-bir-sequence (forms)
;;   (loop for form in forms
;;         for obir = nil then sbir
;;         for sbir = (bir-from-c1form form)
;;         when obir
;;              (merge-bir-nodes obir sbir)))

(define-bir-method progn (form bir) (body)
  (let ((this bir))
    (dolist (sub body)
      (let ((sub-bir (bir-from-c1form sub)))
        (bir-replace-enter sub-bir (bir-leave this))
        (setf this sub-bir))
      #+ ()
      (let ((sub-bir (bir-from-c1form sub this)))
        (if (bir-returns-p sub-bir)
            (bir-replace-enter sub-bir (bir-enter this))
            (bir-replace-enter sub-bir (bir-stray bir)))
        (setf this sub-bir)))
    (bir-replace-leave this (bir-leave bir))
    #+ (or)
    (when (bir-returns-p this)
      (bir-replace-leave this (bir-leave bir)))))

;; (define-bir-method (form if) (fmla-c1form true-c1form false-c1form)
;;   (when (bir-from-c1form fmla-c1form)

;;     (let ((split *current-iblock*)
;;           (join (add-leader :if-join nil)))

;;       (when (bir-from-c1form true-c1form)
;;         (let ((this (next-iblock (add-leader :if-true nil) split)))
;;           (connect-iblocks this join)))

;;       (when (bir-from-c1form false-c1form)
;;         (let ((this (next-iblock (add-leader :if-false nil) split)))
;;           (connect-iblocks this join)))

;;       (setf *current-iblock* join))))

;; (define-bir-method (form block) (blk-var progn-c1form)
;;   (let ((split *current-iblock*)
;;         (join (add-leader :block-join blk-var)))
;;     (setf (gethash blk-var *dumpster*) join)
;;     (bir-from-c1form progn-c1form)
;;     (next-iblock join)))

;; (define-bir-method (form return-from) (blk-var nonlocal value)
;;   (bir-from-c1form value)
;;   (let ((join (gethash blk-var *dumpster*)))
;;     (add-normal (if nonlocal :longjmp :jump))
;;     (next-iblock join)
;;     nil))

;; (define-bir-method (form let*) (vars vals body)
;;   (loop for var in vars
;;         for val in vals
;;         do (bir-from-c1form var)
;;            (bir-from-c1form val))
;;   (bir-from-c1form body))

;; ;;; tag-var is used _only_ to trace whether the tag-body is a target of a
;; ;;; non-local jump.
;; (define-bir-method (form tagbody) (tag-var tag-body)
;;   (dolist (form tag-body)
;;     (if (tag-p form)
;;         (let ((target (add-leader (list :tag (tag-label form))  nil)))
;;           (setf (gethash form *dumpster*) target)
;;           (next-iblock target))
;;         (bir-from-c1form form)))
;;   (next-iblock (add-leader :tagbody-exit nil)))

;; (define-bir-method (form go) (tag-var nonlocal)
;;   (let ((join (gethash tag-var *dumpster*))
;;         (gblock (add-normal (if nonlocal :longjmp/go :jump/go))))
;;     (next-iblock join)))

;;; The IR maintains nodes that are defined in `cmptables.lsp:+all-c1-forms+'.
;;; Common Lisp treats some forms specially when they are top-level. That is
;;; reflected in the internal representation by simple means of nesting.
;;;
;;; When we process a top-level form with the special meaning then the c1form of
;;; the appropriate type is created, otherwise a c1form of type `ordinary' is
;;; created and the real c1form is its argument.
;;;
;;; Some c1forms have locations as arugments. Locations are values that are
;;; expected to be available at runtime.

(defun compiler-pass/custom-pass (forms)
  (cmpprogress "~&;;; Constructing BIR." (data-size))
  (let* ((*current-module* (make-instance 'module :top-level (add-leader :begin nil)))
         (*current-iblock* (top-level *current-module*))
         (*dumpster* (make-hash-table))
         (bir (make-bir-result :enter (add-leader :top-level nil)
                               :leave (add-leader :terminate nil))))
    (next-iblock (bir-enter bir))
    (dolist (form forms)
      (bir-from-c1form form bir))
    (next-iblock (bir-leave bir))
    *current-module*))
