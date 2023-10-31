(in-package "COMPILER")

(ext:package-lock '#:cl nil)
(setf ext:*register-with-pde-hook* nil)

;;; Module represents a compilation unit. Each compilation unit maintains
;;; constants, a set of functions, and a special block of code "top-level" that
;;; is executed when the module is loaded.
(defclass module ()
  ((constants :reader constants :initform (make-array 0 :adjustable t :fill-pointer t))
   (top-level :accessor top-level :initarg :top-level)
   (functions :accessor functions :initarg :functions)))

(defgeneric dynamic-environment (object)
  (:method ((object null))
    nil)
  (:method ((object module))
    object)
  (:method ((object fun))
    object))

(defun make-module ()
  (let ((module (make-instance 'module)))
    (setf (top-level module) (make-bir module))
    module))


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
    :accessor dynamic-environment
    :initarg :denv)
   (inputs
    :accessor iblock-inputs
    :initform (make-array 0 :adjustable t :fill-pointer t))
   (outputs
    :accessor iblock-outputs
    :initform (make-array 0 :adjustable t :fill-pointer t))
   ;; Unwind is currently mutually exclusive with outputs. It means that we
   ;; escape a dynamic context. When the target is not known, like CL:THROW,
   ;; iblock-unwind points to a designated node BIR-ELUDE.
   (unwind
    :accessor iblock-unwind
    :initform nil)
   (instrucitons
    :reader iblock-instructions
    :initform (make-array 0 :adjustable t :fill-pointer t))))

(defun make-iblock (name)
  (make-instance 'iblock :name name :denv *unwind-exit*))

(defmethod print-object ((object iblock) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "~s" (iblock-name object))))

(defun connect-iblocks (from to)
  (assert (null (iblock-unwind from)))
  (vector-push-extend to (iblock-outputs from))
  (vector-push-extend from (iblock-inputs to))
  to)

(defun abscond-iblocks (from to)
  (assert (emptyp (iblock-outputs from)))
  (setf (iblock-unwind from) to)
  (vector-push-extend from (iblock-inputs to))
  to)

(defun map-iblocks (fun enter)
  (let ((visited (make-hash-table))
        (fcalled (make-hash-table)))
    (labels ((%fnc (iblock)
               (unless (gethash iblock fcalled)
                 (setf (gethash iblock fcalled) t)
                 (funcall fun iblock)))
             (%map (iblock)
               (unless (gethash iblock visited)
                 (setf (gethash iblock visited) t)
                 (map nil #'%fnc (iblock-outputs iblock))
                 (map nil #'%map (iblock-outputs iblock))
                 (ext:when-let ((unwind (iblock-unwind iblock)))
                   (%fnc unwind)
                   (%map unwind)))))
      (%fnc enter)
      (%map enter))))

(defun extend-iblock (this-instruction iblock)
  (setf (instruction-iblock this-instruction) iblock)
  (let* ((instructions (iblock-instructions iblock))
         (n (length instructions)))
    (unless (zerop n)
      (let ((last-instruction (aref instructions (1- n))))
        (setf (instruction-next last-instruction) this-instruction
              (instruction-prev this-instruction) last-instruction))))
  (vector-push-extend this-instruction (iblock-instructions iblock))
  iblock)


;;; The class instruction is important because it holds references to input and
;;; output values. Instructions are derived from c1forms, but there may be many
;;; instructions referencing the same node.
(defclass instruction ()
  (;; The name is used to tell aparat different operators. Alternatively we may
   ;; create a class hierarchy when each class represents the instruction type.
   (opcode :initarg :opcode :accessor instruction-opcode)
   ;; Arguments accepted by the instruction.
   (inputs :initarg :inputs :accessor instruction-inputs)
   ;; Values returned by the instruction.
   (output :initarg :output :accessor instruction-output)
   ;; Backpointer.
   (iblock :accessor instruction-iblock)
   ;; Leader has no prececessors, terminator has no successors.
   (prev :initform nil :accessor instruction-prev)
   (next :initform nil :accessor instruction-next))
  (:default-initargs :opcode (error "~s is required." :opcode)
                     :inputs (error "~s is required." :inputs)
                     :output (error "~s is required." :output)))

;;; IBLOCK, PREV and NEXT are fixed in the function EXTEND-BLOCK.
;;; INPUTS  depend on the particular operator.
;;; OUTPUTS depend on the particular caller.
(defun make-instruction (opcode inputs)
  (make-instance 'instruction :opcode opcode
                              :inputs inputs
                              :output *destination*))


;;; In principle we could construct CFG with IBLOCK and operators defined for
;;; them. BIR is used to track the CFG first and current IBLOCKs. This aids the
;;; recursive construction of the graph.
;;;
;;; BIR-ENTER is the entry point and does not change.
;;; BIR-TRAIL may be modified at any time.
;;;
;;; Extending the graph:
;;;
;;; - BIR-INSERT bir iblock :: connect to iblock, update the trail
;;; - BIR-RETURN bir iblock :: connect to iblock, delete the trail
;;; - BIR-ESCAPE bir iblock :: dynamic to iblock, delete the trail
;;; - BIR-REWIND bir iblock :: abandon the trail, update the trail
;;;
;;; Extending the trail:
;;;
;;; - BIR-EXTEND bir instruction :: add an instruction to the trail (if any)
;;;
;;; Dynamic environments of two connected iblocks may differ and transitioning
;;; between them may involve arbitrary operations.

(defclass bir ()
  (;; parent may be a module or a function.
   (parent :initarg :parent :accessor bir-parent)
   (enter :initarg :enter :accessor bir-enter)
   (leave :initarg :leave :accessor bir-leave)
   (elude :initarg :elude :accessor bir-elude)
   (trail :initarg :trail :accessor bir-trail)
   ;; backpointers are added to find destination iblocks.
   (backs :initarg :backs :accessor bir-backs))
  (:default-initargs :backs (make-hash-table)))

(defmethod dynamic-environment ((object bir))
  (or (dynamic-environment (bir-trail object))
      (dynamic-environment (bir-enter object))))

(defun get-iblock (bir iblock-key)
  (or (gethash iblock-key (bir-backs bir))
      (error "can't find ~s" iblock-key)))

(defun set-iblock (bir iblock)
  (setf (gethash (iblock-name iblock) (bir-backs bir)) iblock))

(defun make-bir (parent)
  (let* ((enter (make-iblock :enter))
         (leave (make-iblock :leave))
         (elude (make-iblock :elude)))
    (make-instance 'bir :parent parent
                   :enter enter :leave leave :elude elude :trail enter)))

(defun bir-insert (bir target)
  (ext:when-let ((trail (bir-trail bir)))
    (connect-iblocks trail target))
  ;; Even when the trail is broken the iblock may be a destination of a jump.
  (setf (bir-trail bir) target))

(defun bir-escape (bir target)
  (ext:when-let ((trail (bir-trail bir)))
    (abscond-iblocks trail target)
    (setf (bir-trail bir) nil)))

(defun bir-return (bir target)
  (when (bir-trail bir)
    (bir-insert bir target)
    (setf (bir-trail bir) nil)))

(defun bir-rewind (bir iblock)
  (setf (bir-trail bir) iblock))

(defun bir-extend (bir instruction)
  (when (bir-trail bir)
    (extend-iblock instruction (bir-trail bir))))


;;; Exit manager.

(defvar *exit* nil)
(defvar *unwind-exit* nil)
(defvar *destination* nil)

;;; The exit block denotes the continuation of the body. When the body returns
;;; (bir-trail is not null), and the dynamic environment does not change, and
;;; the exit block is not a target of a jump, then we don't need to create it.
(defun exit-iblock-necessary-p (bir iblock)
  (not (and (bir-trail bir)
            (eq (dynamic-environment iblock) (rest *unwind-exit*))
            (emptyp (iblock-inputs iblock)))))

(defmacro with-exit-iblock ((bir block destination) &body body)
  `(let* ((,block (make-iblock ',block))
          (*unwind-exit* (cons ,block *unwind-exit*))
          (*destination* ,destination))
     ,@body
     (when (exit-iblock-necessary-p ,bir ,block)
       (bir-insert ,bir ,block))))

;;; This function jumps to EXIT without setting the destination.
;; (defun unwind-jump (exit))

;;; This function jumps to EXIT and sets the destination to location.
;; (defun unwind-exit (exit location))




;;; C1FORMS have a single class and are distinguished by a name, that's why we
;;; trampoline to the handler by expanding the name. Handlers are defined with
;;; the macro DEFINE-BIR-METHOD.
(defgeneric %bir-from-c1form (bir form name)
  ;; This default method should ERROR before merging this pass to develop.
  (:method ((bir bir) (form c1form) (name t))
    (bir-insert bir (make-iblock `(:unknown ,name)))))

(defun bir-from-c1form (bir form)
  (typecase form
    (c1form
     (%bir-from-c1form bir form (c1form-name form) ))
    (otherwise
     (bir-insert bir (make-iblock (cons :not-c1form form))))))

(defun bir-from-function (bir fun)
  (bir-insert bir (make-iblock (cons :function (fun-name fun)))))

(defmacro define-bir-method ((bir var node-name) args &body body)
  (let ((name (gensym)))
    `(defmethod %bir-from-c1form
         ((,bir bir) (,var c1form) (,name (eql ',node-name)))
       (destructuring-bind ,args (c1form-args ,var)
         ,@body))))


(progn
  (define-bir-method (bir form ext:compiler-let) (symbols values body)
    (progv symbols values
      (bir-from-c1form bir body)))

  (define-bir-method (bir form cl:load-time-value) (dest-loc value-c1form)
    (with-exit-iblock (bir *exit* dest-loc)
      (bir-from-c1form bir value-c1form)))

  (define-bir-method (bir form make-form) (dest-loc value-c1form)
    (with-exit-iblock (bir *exit* dest-loc)
      (bir-from-c1form bir value-c1form)))

  (define-bir-method (bir form init-form) (vv-loc value-c1form)
    (declare (ignore vv-loc))
    (with-exit-iblock (bir *exit* nil)
      (bir-from-c1form bir value-c1form)))

  (define-bir-method (bir form ordinary) (c1form)
    (with-exit-iblock (bir *exit* nil)
      (bir-from-c1form bir c1form)))
  
  (define-bir-method (bir form cl:progn) (body)
    (with-exit-iblock (bir *exit* *destination*)
      (loop while (bir-trail bir)
            for (form . rest) on body
            do (if (null rest)
                   (let ((*destination* nil))
                     (bir-from-c1form bir form))
                   (bir-from-c1form bir form))))))

;;; Places, assignment and binding
(progn
  ;; FIXME this is a stub
  (define-bir-method (bir form location) (loc)
    (declare (ignore loc))
    (let ((instruction (make-instruction form 0)))
      (bir-extend bir instruction)))

  ;; FIXME this is a stub
  ;; VALUE is a VV location if it is known at compile time.
  (define-bir-method (bir form variable) (var value)
    (declare (ignore var value))
    (let ((instruction (make-instruction form 0)))
      (bir-extend bir instruction)))
  ;; FIXME this is a stub
  (define-bir-method (bir form cl:setq)
      (var value-c1form)
    (let ((instruction (make-instruction form 1)))
      (let ((*destination* var))
        (bir-from-c1form bir value-c1form))
      (bir-extend bir instruction)))
  ;; IMPLEMENTME
  (define-bir-method (bir form cl:psetq) (var-list value-c1form-list)
    (call-next-method))
  ;; IMPLEMENTME
  (define-bir-method (bir form cl:progv) (symbols values form)
    (call-next-method))
  ;; FIXME this is a stub
  (define-bir-method (bir form cl:let*) (vars-list var-init-c1form-list decl-body-c1form)
    (loop for var in vars-list
          for val in var-init-c1form-list
          do (let ((*destionation* var))
               (bir-from-c1form bir val)))
    (bir-from-c1form bir decl-body-c1form))
  ;; IMPLEMENTME
  (define-bir-method (bir form cl:multiple-value-setq) (vars-list values-c1form-list)
    (call-next-method))
  ;; IMPLEMENTME
  (define-bir-method (bir form cl:multiple-value-bind) (vars-list init-c1form body)
    (call-next-method)))

;;; Function namespace
(progn
  ;; FIXME this is a stub
  (define-bir-method (bir form si:fset)
      (function-object vv-loc macro-p pprint-p lambda-form)
    (declare (ignore function-object vv-loc macro-p pprint-p lambda-form))
    (let ((instruction (make-instruction form 4)))
      (bir-extend bir instruction)))

  ;; IMPLEMENTME
  (define-bir-method (bir form cl:function) (fname)
    (call-next-method))

  ;; FIXME this is a stub
  (define-bir-method (bir form locals) (local-fun-list body labels-p)
    (bir-from-c1form bir body)))

;;; Specialized accessors
(progn
  ;; IMPLEMENTME
  (define-bir-method (bir form si:structure-ref)
      (struct-c1form type-name slot-index)
    (call-next-method))
  ;; IMPLEMENTME
  (define-bir-method (bir form si:structure-set)
      (struct-c1form type-name slot-index value-c1form)
    (call-next-method)))

;;; Control structures
(progn
  (define-bir-method (bir form cl:block) (blk-var progn-c1form)
    (let ((exit (make-iblock blk-var)))
      (set-iblock bir exit)
      (bir-from-c1form bir progn-c1form)
      (bir-insert bir exit)))

  ;; FIXME value is ignored(!)
  (define-bir-method (bir form cl:return-from) (blk-var nonlocal value)
    (let ((cont (get-iblock bir blk-var)))
      (if nonlocal
          (bir-escape bir cont)
          (bir-return bir cont))))

  (define-bir-method (bir form cl:tagbody) (tag-var tag-body)
    ;; We go in two passes - first we estabilish target iblocks and only after
    ;; that we /do our thing/. This is important because cl:go requires iblocks.
    (dolist (form tag-body)
      (when (tag-p form)
        (set-iblock bir (make-iblock form))))
    (dolist (form tag-body)
      (if (tag-p form)
          (bir-insert bir (get-iblock bir form))
          (bir-from-c1form bir form))))

  (define-bir-method (bir form cl:go) (tag nonlocal)
    (let ((cont (get-iblock bir tag)))
      (if nonlocal
          (bir-escape bir cont)
          (bir-return bir cont))))

  ;; This one is symmetrical to block/return-from, but catch-value is a dynamic
  ;; variable. Generally similar, but we always escape.
  (define-bir-method (bir form cl:catch) (catch-value body)
    (let ((exit (make-iblock :catch)))
      (set-iblock bir exit)
      (bir-from-c1form bir body)
      (bir-insert bir exit)
      (bir-rewind bir (bir-elude bir))
      (bir-insert bir exit)))

  ;; This one is symmetrical to block/return-from, but catch-value is a dynamic
  ;; variable. Generally similar, but we always escape.
  (define-bir-method (bir form cl:throw) (catch-value output-value)
    (bir-escape bir (bir-elude bir)))

  (define-bir-method (bir form cl:unwind-protect) (protected-c1form body)
    (let* ((enter (make-iblock :op_protect))
           (clean (make-iblock :op_protect_normal))
           (leave (make-iblock :op_protect_exit)))
      (bir-insert bir enter)
      (bir-from-c1form bir protected-c1form)
      (bir-insert bir clean)
      (let ((*destination* nil))
        (bir-from-c1form bir body))
      ;; EXIT is responsible for reinstating values and (when due) unwinding.
      (bir-insert bir leave))))

;;; Control structures*
(progn
  (define-bir-method (bir form cl:if) (fmla-c1form true-c1form false-c1form)
    (let ((if-true (make-iblock :if-true))
          (if-false (make-iblock :if-false))
          (if-merge (make-iblock :if-merge))
          ;;
          (instruction (make-instruction form 1))
          (if-merge-list '()))
      (let ((*destination* (list instruction 0)))
        (bir-from-c1form bir fmla-c1form))
      (flet ((add-branch (iblock c1form)
               (bir-insert bir iblock)
               (bir-from-c1form bir c1form)
               (when (bir-trail bir)
                 (push (bir-trail bir) if-merge-list))))
       (ext:when-let ((if-split (bir-trail bir)))
         (bir-extend bir instruction)
         (add-branch if-true true-c1form)
         (bir-rewind bir if-split)
         (add-branch if-false false-c1form)
         (ecase (length if-merge-list)
           (0 nil)
           (1 (bir-rewind bir (first if-merge-list)))
           (2 (dolist (iblock if-merge-list)
                (bir-rewind bir iblock)
                (bir-insert bir if-merge))))))))

  ;; FIXME this is a stub
  (define-bir-method (bir form fmla-not) (c1form)
    (let ((instruction (make-instruction form 1)))
      (let ((*destination* (list instruction 0)))
        (bir-from-c1form bir c1form))
      (bir-extend bir instruction)))
  ;; IMPLEMENTME
  (define-bir-method (bir form fmla-and) (&rest c1forms)
    (call-next-method))
  ;; IMPLEMENTME
  (define-bir-method (bir form fmla-or) (&rest c1forms)
    (call-next-method)))

;;; Function calls
(progn
  (define-bir-method (bir form fcall) (fun-form args fun type)
    (declare (ignore fun-form fun type))
    (let ((instruction (make-instruction form (length args))))
      ;; FIXME each argument has a separate position destination-wise.
      (loop for arg in args
            for n from 0
            do (let ((*destination* (list instruction n)))
                 (bir-from-c1form bir arg)))
      (bir-extend bir instruction)))
  ;; IMPLEMENTME
  (define-bir-method (bir form mcall) (fun-form args fun type)
    (call-next-method)))

;;; Other operators
(progn
  ;; IMPLEMENTME
  (define-bir-method (bir form lambda) (lambda-list doc body-c1form)
    (call-next-method))
  ;; IMPLEMENTME
  (define-bir-method (bir form values) (values-c1form-list)
    (call-next-method))
  ;; IMPLEMENTME
  (define-bir-method (bir form mv-prog1) (form body)
    (call-next-method)))

;;; Extensions
(progn
  ;; IMPLEMENTME
  (define-bir-method (bir form ext:compiler-typecase) (var expressions)
    (call-next-method))
  ;; IMPLEMENTME
  (define-bir-method (bir form ext:checked-value) (type value-c1form let-form)
    (call-next-method)))

;;; Backend-specific operators
(progn
  ;; IMPLEMENTME
  (define-bir-method (bir form ffi:c-inline) (arg-forms arg-types return-type
                                              c-expression-string
                                              side-effects-p one-liner-p)
    (call-next-method))
  ;; IMPLEMENTME
  (define-bir-method (bir form ffi:c-progn) (variables forms)
    (call-next-method)))

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
(defvar *last-mod* nil)

(defun compiler-pass/custom-pass ()
  (cmpprogress "~&;;; Constructing BIR.")
  (let* ((module (make-module))
         (top-level (top-level module))
         (*destination* nil))
    (dolist (form *make-forms*)
      (bir-from-c1form top-level form))
    (dolist (form *top-level-forms*)
      (bir-from-c1form top-level form))
    (bir-return top-level (bir-leave top-level))
    (setf (functions module)
          (loop for fun in *functions*
                for bir = (make-bir fun)
                do (bir-from-function bir fun)
                collect bir))
    (setf *last-mod* module)
    (compiler-pass/generate-cxx*
     module
     "/tmp/foo.c" "/tmp/foo.h" "/tmp/foo.data"
     "dummy_init_name" "dummy_source.lsp")))
