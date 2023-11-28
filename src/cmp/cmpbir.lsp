;;;;
;;;;  Copyright (c) 2023, Daniel Kochmański.
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;; CMPBIR Internal representation (CFG, basic blocks).

(in-package "COMPILER")

;;; Module represents a compilation unit. Each compilation unit maintains
;;; constants, a set of functions, and a special block of code "top-level" that
;;; is executed when the module is loaded.
#+not-yet
(defclass module ()
  ((constants :reader constants :initform (make-array 0 :adjustable t :fill-pointer t))
   (top-level :accessor top-level :initarg :top-level)
   (functions :accessor functions :initarg :functions)))

;;; BIR represents a control flow graph. It maintains four pointers to iblocks:
;;; - bir-enter - the entry point to the program
;;; - bir-leave - the terminating block of the CFG (normal exit)
;;; - bir-elude - the terminating block of the CFG (escape exit)
;;; - bir-trail - the current node in the graph while constructing it
(defclass bir ()
  ((enter :initarg :enter :accessor bir-enter)
   (leave :initarg :leave :accessor bir-leave)
   (elude :initarg :elude :accessor bir-elude)
   (trail :initarg :trail :accessor bir-trail)
   ;; backpointers are added to find destination iblocks.
   (backs :initarg :backs :accessor bir-backs))
  (:default-initargs :backs (make-hash-table)))

;;; IBLOCK is a block of instructions that may be executed without changing the
;;; dynamic environment and corresponds to a basic block in a contemporary CFG.
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
   ;; iblock-unwind points to a designated node BIR-ELUDE. When it is known,
   ;; like CL:GO and CL:RETURN-FROM, iblock-unwind points to a target block.
   (unwind
    :accessor iblock-unwind
    :initform nil)
   (instrucitons
    :reader iblock-instructions
    :initform (make-array 0 :adjustable t :fill-pointer t))))



;;; INSTRUCTION holds references to inputs and the destination.
(defclass instruction ()
  (;; The name is used to tell aparat different operators. Alternatively we may
   ;; create a class hierarchy when each class represents the instruction type.
   (opcode :initarg :opcode :accessor instruction-opcode)
   ;; Arguments accepted by the instruction.
   (inputs :initarg :inputs :accessor instruction-inputs)
   ;; Destination of the instruction.
   (output :initarg :output :accessor instruction-output)
   ;; Backpointer.
   (iblock :accessor instruction-iblock)
   ;; Leader has no prececessors, terminator has no successors.
   (prev :initform nil :accessor instruction-prev)
   (next :initform nil :accessor instruction-next))
  (:default-initargs :opcode (error "~s is required." :opcode)
                     :inputs (error "~s is required." :inputs)
                     :output (error "~s is required." :output)))


;;; instruction operators

;;; IBLOCK, PREV and NEXT are fixed in the function EXTEND-IBLOCK.
;;; INPUTS  depend on the particular operator.
;;; OUTPUTS depend on the particular caller.
(defun make-instruction (opcode inputs)
  (make-instance 'instruction :opcode opcode
                              :inputs inputs
                              :output *destination*))

(defun dunk-instruction (instruction)
  (bir-extend *bir* instruction)
  ;; FIXME currently we are interleaving CFG building and codegen.
  (codegen :cxx (instruction-opcode instruction) instruction))

(defun push-instruction (opcode &rest inputs)
  (dunk-instruction (make-instruction opcode inputs)))


;;; iblock operators
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


;;; Bir operators

(defun make-bir ()
  (let ((enter (make-iblock :enter))
        (leave (make-iblock :leave))
        (elude (make-iblock :elude)))
    (make-instance 'bir :enter enter :leave leave :elude elude :trail enter)))

;;; Constructing the graph

(defun bir-insert (bir target)
  (ext:when-let ((trail (bir-trail bir)))
    (connect-iblocks trail target))
  ;; Even when the trail is broken the iblock may be a destination of a jump.
  (bir-rewind bir target))

(defun bir-branch (bir target)
  (ext:when-let ((trail (bir-trail bir)))
    (bir-insert bir target)
    (bir-rewind bir trail)))

(defun bir-escape (bir target)
  (ext:when-let ((trail (bir-trail bir)))
    (abscond-iblocks trail target)
    (bir-rewind bir nil)))

(defun bir-return (bir target)
  (when (bir-trail bir)
    (bir-insert bir target)
    (bir-rewind bir nil)))

(defun bir-humour (bir target)
  (when *debug-compiler*
    (bir-insert bir target)))

;;; Miscellaneous

(defun bir-rewind (bir iblock)
  (setf (bir-trail bir) iblock))

(defun bir-embark (bir)
  (bir-rewind bir (bir-enter bir))
  (bir-insert bir (make-iblock :body)))

(defun bir-finish (bir)
  (unless (eq (bir-trail bir) (bir-leave bir))
    (bir-insert bir (bir-leave bir))))

(defun bir-rename (bir name)
  (ext:when-let ((trail (bir-trail bir)))
    (setf (iblock-name trail) name)))

(defun bir-extend (bir instruction)
  (ext:when-let ((trail (bir-trail bir)))
    (extend-iblock instruction trail)))
