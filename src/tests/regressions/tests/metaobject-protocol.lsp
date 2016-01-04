;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Author:   Daniel Kochmański
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Metaobject Protocol tests

(in-package :cl-test)

(use-package :clos)


;; mop-001

(defun delete-class (&rest class-names)
  ;;; do nothing. We will figure out later what to do.
  (values))

;;; Fixed: 14/04/2006 (juanjo)
;;; Description:
;;;
;;;     The slot definitions from some classes did not get converted.
;;;     Besides, metaobject CLASS had the same list for direct and effective
;;;     slots.
;;;
(deftest mop-0001-fixup
    (block top
      (labels ((test-class (class-object)
                 (let ((x (find-if-not #'(lambda (x)
                                           (typep x 'standard-direct-slot-definition))
                                       (class-direct-slots class-object))))
                   (when x
                     (format t "Class ~a has as direct slot ~a" class-object x)
                     (return-from top (class-name class-object))))
                 (let ((x (find-if-not #'(lambda (x)
                                           (typep x 'standard-effective-slot-definition))
                                       (class-slots class-object))))
                   (when x
                     (format t "Class ~a has as effective slot ~a" class-object x)
                     (return-from top (class-name class-object))))
                 (mapc #'test-class (clos::class-direct-subclasses class-object))))
        (test-class (find-class 't))
        nil))
  nil)

;;; Date: 13/02/2006
;;; From: Dan Debertin
;;; Fixed: 24-02-2006 (juanjo)
;;; Description:
;;;
;;;     Subclasses of STANDARD-CLASS would not inherit all their slots
;;;     and thus would cause runtime errors when creating instances.
;;;

(deftest mop-0002-metaclasses
    (eval '(progn
            (defclass foo-metaclass (standard-class) ())
            (defclass faa () ((a :initform 2 :initarg :a)) (:metaclass foo-metaclass))
            (prog1 (slot-value (make-instance 'faa :a 3) 'a)
              (cl-test::delete-class 'foo-metaclass 'faa))))
  3)

;;; Date: 02/03/2006
;;; From: Pascal Costanza
;;; Fixed: 07/03/2006 (juanjo)
;;; Description:
;;;
;;;     CLOS should export the symbols from the AMOP.
;;;


(defconstant +mop-symbols+ '("DIRECT-SLOT-DEFINITION"
"EFFECTIVE-SLOT-DEFINITION" "EQL-SPECIALIZER" "FORWARD-REFERENCED-CLASS"
"FUNCALLABLE-STANDARD-CLASS" "FUNCALLABLE-STANDARD-OBJECT" "METAOBJECT"
"SLOT-DEFINITION" "SPECIALIZER" "STANDARD-ACCESSOR-METHOD"
"STANDARD-DIRECT-SLOT-DEFINITION" "STANDARD-EFFECTIVE-SLOT-DEFINITION"
"STANDARD-READER-METHOD" "STANDARD-SLOT-DEFINITION" "STANDARD-WRITER-METHOD"
"ACCESSOR-METHOD-SLOT-DEFINITION" "ADD-DEPENDENT" "ADD-DIRECT-METHOD"
"ADD-DIRECT-SUBCLASS" "CLASS-DEFAULT-INITARGS"
"CLASS-DIRECT-DEFAULT-INITARGS" "CLASS-DIRECT-SLOTS"
"CLASS-DIRECT-SUBCLASSES" "CLASS-DIRECT-SUPERCLASSES" "CLASS-FINALIZED-P"
"CLASS-PRECEDENCE-LIST" "CLASS-PROTOTYPE" "CLASS-SLOTS"
"COMPUTE-APPLICABLE-METHODS-USING-CLASSES" "COMPUTE-CLASS-PRECEDENCE-LIST"
"COMPUTE-DEFAULT-INITARGS" "COMPUTE-DISCRIMINATING-FUNCTION"
"COMPUTE-EFFECTIVE-METHOD" "COMPUTE-EFFECTIVE-SLOT-DEFINITION"
"COMPUTE-SLOTS" "DIRECT-SLOT-DEFINITION-CLASS"
"EFFECTIVE-SLOT-DEFINITION-CLASS" "ENSURE-CLASS" "ENSURE-CLASS-USING-CLASS"
"ENSURE-GENERIC-FUNCTION-USING-CLASS" "EQL-SPECIALIZER-OBJECT"
"EXTRACT-LAMBDA-LIST" "EXTRACT-SPECIALIZER-NAMES" "FINALIZE-INHERITANCE"
"FIND-METHOD-COMBINATION" "FUNCALLABLE-STANDARD-INSTANCE-ACCESS"
"GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER"
"GENERIC-FUNCTION-DECLARATIONS" "GENERIC-FUNCTION-LAMBDA-LIST"
"GENERIC-FUNCTION-METHOD-CLASS" "GENERIC-FUNCTION-METHOD-COMBINATION"
"GENERIC-FUNCTION-METHODS" "GENERIC-FUNCTION-NAME" "INTERN-EQL-SPECIALIZER"
"MAKE-METHOD-LAMBDA" "MAP-DEPENDENTS" "METHOD-FUNCTION"
"METHOD-GENERIC-FUNCTION" "METHOD-LAMBDA-LIST" "METHOD-SPECIALIZERS"
"READER-METHOD-CLASS" "REMOVE-DEPENDENT" "REMOVE-DIRECT-METHOD"
"REMOVE-DIRECT-SUBCLASS" "SET-FUNCALLABLE-INSTANCE-FUNCTION"
"SLOT-BOUNDP-USING-CLASS" "SLOT-DEFINITION-ALLOCATION"
"SLOT-DEFINITION-INITARGS" "SLOT-DEFINITION-INITFORM"
"SLOT-DEFINITION-INITFUNCTION" "SLOT-DEFINITION-LOCATION"
"SLOT-DEFINITION-NAME" "SLOT-DEFINITION-READERS" "SLOT-DEFINITION-WRITERS"
"SLOT-DEFINITION-TYPE" "SLOT-MAKUNBOUND-USING-CLASS"
"SLOT-VALUE-USING-CLASS" "SPECIALIZER-DIRECT-GENERIC-FUNCTIONS"
"SPECIALIZER-DIRECT-METHODS" "STANDARD-INSTANCE-ACCESS" "UPDATE-DEPENDENT"
"VALIDATE-SUPERCLASS" "WRITER-METHOD-CLASS"))

(deftest mop-0003-symbols
    (let ((*package* (find-package "CLOS")))
      (and (remove-if #'(lambda (x)
                          (multiple-value-bind (s t)
                              (find-symbol x *package*)
                            (and s (eq t :external))))
                      +mop-symbols+)
           t))
  nil)

;;; Date: 02/03/2006
;;; From: Dank Corkill
;;; Fixed: 02-03-2006 (Dan Corkill)
;;; Description:
;;;
;;;     DEFCLASS allows additional options which should be handled by the
;;;     metaclass.
;;;

(deftest mop-0004-defclass-options
    (eval '(let ((*aux* 5))
            (declare (special *aux*))
            (defclass foo-metaclass (standard-class) ())
            (defmethod shared-initialize ((class foo-metaclass) slot-names
                                           &rest initargs &key option)
              (prog1 (call-next-method)
                (setf *aux* option)))
            (defclass faa ()
              ((a :initform *aux* :initarg :a))
              (:metaclass foo-metaclass)
              (:option t))
            (prog1 (slot-value (make-instance 'faa) 'a)
              (cl-test::delete-class 'foo-metaclass 'faa))))
  (T))

;;; Date: 02/03/2006
;;; From: Dank Corkill
;;; Fixed: 02-03-2006 (Dan Corkill)
;;; Description:
;;;
;;;     Readers and writers for slot documentation.
;;;

(deftest mop-0004b-slot-documentation
    (eval '(progn
            (defclass fee ()
              ((a :initform *aux* :initarg :a)))
            (setf (documentation (first (clos:class-slots (find-class 'fee))) t)
             #1="hola")
            (documentation (first (clos:class-slots (find-class 'fee))) t)))
  #1#)

;;; Date: 25/03/2006
;;; From: Pascal Costanza
;;; Fixed: 03/04/2006 (juanjo)
;;; Description:
;;;
;;;     The default slot setter methods had the first argument
;;;     (i.e. the new value) specialized to NIL. This makes it
;;;     impossible to write further specializations.
;;;

(deftest mop-0005-setf-specializer
    (progn
      (defclass fee ()
        ((a :accessor fee-a)))
      (prog1
          (list
           (mapcar #'class-name
                   (method-specializers (first (generic-function-methods #'(setf fee-a)))))
           (mapcar #'class-name
                   (method-specializers (first (generic-function-methods #'fee-a)))))
        (delete-class 'fee)))
  ((t fee) (fee)))

;;; Date: 06/04/2006
;;; From: Pascal Costanza
;;; Fixed: ---
;;; Description:
;;;
;;;     When a required argument in a method is not explicitely given
;;;     an specializer, the specializer should be T. Thus
;;;             (defmethod foo (a))
;;;     is equivalent to
;;;             (defmethod foo ((a t)))
;;;

(deftest mop-0006-method-specializer
    (progn
      (defmethod mop-0006-foo (a))
      (prog1
          (method-specializers (first (generic-function-methods #'mop-0006-foo)))
        (fmakunbound 'mop-0006-foo)))
  (#.(find-class t)))

;;; Date: 22/04/2006
;;; From: M. Goffioul
;;; Fixed: 23/04/2006 (juanjo)
;;; Description:
;;;
;;;     When a class inherits from two other classes which have a slot
;;;     with the same name, the new class should inherit the accessors
;;;     from both classes.
;;;

(deftest mop-0007-slot-inheritance
    (progn
      (defclass fee-1 ()
        ((slot-0 :initform 0 :reader slot-0)
         (slot-1 :initform 1 :reader slot-1)))
      (defclass fee-2 ()
        ((slot-0 :initform 2 :reader slot-2)))
      (defclass fee-3 (fee-1 fee-2)
        ((slot-0 :initform 3 :accessor c-slot-0)))
      (flet ((accessors (class)
               (list (class-name class)
                     (mapcar #'slot-definition-readers (class-slots class))
                     (mapcar #'slot-definition-readers (class-slots class)))))
        (prog1
            (list (accessors (find-class 'fee-1))
                  (accessors (find-class 'fee-2))
                  (accessors (find-class 'fee-3))
                  (mapcar #'(lambda (o)
                              (mapcar #'(lambda (method)
                                          (handler-case (funcall method o)
                                            (error (c) nil)))
                                      '(slot-0 slot-2 c-slot-0)))
                          (mapcar #'make-instance '(fee-1 fee-2 fee-3))))
          (delete-class 'fee-1 'fee-2 'fee-3))))
  ((fee-1 ((slot-0) (slot-1)) ((slot-0) (slot-1)))
   (fee-2 ((slot-2)) ((slot-2)))
   (fee-3 ((c-slot-0 slot-0 slot-2) (slot-1))
          ((c-slot-0 slot-0 slot-2) (slot-1)))
   ((0 nil nil)
    (nil 2 nil)
    (3 3 3))))


;;; Date: 28/04/2006
;;; From: P. Costanza
;;; Fixed: 05/05/2006 (P. Costanza)
;;; Description:
;;;
;;;     Option names from classes and generic functions which are not
;;;     in the keyword package should be quoted. This test is
;;;     essentially like mop-0004-... because our DEFGENERIC does not
;;;     support non-keyword options.
;;;

(deftest mop-0008-defclass-option-quote
    (eval '(let ((*aux* 5))
            (declare (special *aux*))
            (defclass foo-metaclass (standard-class) ())
            (defmethod shared-initialize ((class foo-metaclass) slot-names
                                          &rest initargs &key ((cl-user::option option)))
              (prog1 (call-next-method)
                (setf *aux* option)))
            (defclass faa ()
              ((a :initform *aux* :initarg :a))
              (:metaclass foo-metaclass)
              (cl-user::option t))
            (prog1 (slot-value (make-instance 'faa) 'a)
              (cl-test::delete-class 'foo-metaclass 'faa))))
  (t))


;;; Date: 05/10/2006
;;; From: Rick Taube
;;; Fixed: 10/10/2006 (juanjo)
;;; Description:
;;;
;;;     :INITFORM arguments do not get properly expanded when the form
;;;     is a constant variable.
;;;
;;;     (defclass a () ((a :initform most-positive-fixnum)))
;;;     (slot-value (make-instance a) 'a) => most-positive-fixnum
;;;

(deftest mop-0009-defclass-initform
    (loop for quoting in '(nil t)
          collect
          (loop for f in '(most-positive-fixnum #1=#.(lambda () 1) 12 "hola" :a t nil)
                collect (prog1 (eval `(progn
                                       (defclass foo () ((a :initform ,(if quoting (list 'quote f) f))))
                                       (slot-value (make-instance 'foo) 'a)))
                          (cl-test::delete-class 'foo))))
  ((#.most-positive-fixnum #1# 12 "hola" :a t nil)
   (most-positive-fixnum #1# 12 "hola" :a t nil)))


;; Test MOP dependents
(defclass mop-dependent-object ()
  ((log :initform nil :initarg :log :accessor mop-dependent-object-log)))

(defmethod update-dependent ((object t) (dep mop-dependent-object) &rest initargs)
  (push (list* object initargs) (mop-dependent-object-log dep)))

;;; Date: 23/04/2012
;;; Description:
;;;
;;;     ADD-DEPENDENT uses pushnew
;;;
(deftest mop-gf-add-non-redundant
    (let* ((dep (make-instance 'mop-dependent-object))
           l1 l2)
      (fmakunbound 'mop-gf-add/remove-dependent)
      (defgeneric mop-gf-add/remove-dependent (a))
      (let ((f #'mop-gf-add/remove-dependent))
        (clos:add-dependent f dep)
        (setf l1 (clos::generic-function-dependents f))
        (clos:add-dependent f dep)
        (setf l2 (clos::generic-function-dependents f))
        (and (eq l1 l2)
             (equalp l1 (list dep))
             t)))
  t)

;;; Date: 23/04/2012
;;; Description:
;;;
;;;     Generic functions have dependents and are activated
;;;
(deftest mop-gf-add/remove-dependent
    (let* ((dep (make-instance 'mop-dependent-object))
           l1 l2 l3 l4 l5 l6)
      (fmakunbound 'mop-gf-add/remove-dependent)
      (defgeneric mop-gf-add/remove-dependent (a))
      (let ((f #'mop-gf-add/remove-dependent)
            m1 m2)
        ;;
        ;; * ADD-DEPENDENT registers the object with the function
        ;;
        (clos:add-dependent f dep)
        (setf l1 (clos::generic-function-dependents f))
        ;;
        ;; * ADD-METHOD invokes UPDATE-DEPENDENT
        ;;
        (defmethod mop-gf-add/remove-dependent ((a number)) (cos a))
        (setf l2 (mop-dependent-object-log dep))
        ;;
        ;; * REMOVE-METHOD invokes UPDATE-DEPENDENT
        ;;
        (setf m1 (first (compute-applicable-methods f (list 1.0))))
        (remove-method f m1)
        (setf l3 (mop-dependent-object-log dep))
        ;;
        ;; * REMOVE-DEPENDENT eliminates all dependencies
        ;;
        (clos:remove-dependent f dep)
        (setf l4 (clos::generic-function-dependents f))
        ;;
        ;; * ADD-METHOD invokes UPDATE-DEPENDENT but has no effect
        ;;
        (defmethod mop-gf-add/remove-dependent ((a symbol)) a)
        (setf l5 (mop-dependent-object-log dep))
        ;;
        ;; * REMOVE-METHOD invokes UPDATE-DEPENDENT but has no effect
        ;;
        (setf m2 (first (compute-applicable-methods f (list 'a))))
        (setf l6 (mop-dependent-object-log dep))
        ;; the first call to defmethod adds two entries: one for the
        ;; add-method and another one for a reinitialize-instance with
        ;; the name of the function
        (values (equalp l1 (list dep))
                (eq l2 (rest l3))
                (equalp l3
                        (list (list f 'remove-method m1)
                              (list f 'add-method m1)
                              (list f)))
                (null l4)
                (eq l5 l3)
                (eq l6 l3)
                t)))
  t t t t t t t)

;;; Date: 23/04/2012
;;; Description:
;;;
;;;     ADD-DEPENDENT does not duplicate elements
;;;
(deftest mop-class-add/remove-dependent
    (let* ((dep (make-instance 'mop-dependent-object))
           l1 l2)
      (when (find-class 'mop-class-add/remove-dependent nil)
        (setf (class-name (find-class 'mop-class-add/remove-dependent)) nil))
      (defclass mop-class-add/remove-dependent () ())
      (let ((f (find-class 'mop-class-add/remove-dependent)))
        (clos:add-dependent f dep)
        (setf l1 (clos::class-dependents f))
        (clos:add-dependent f dep)
        (setf l2 (clos::class-dependents f))
        (and (eq l1 l2)
             (equalp l1 (list dep))
             t)))
  t)

;;; Date: 23/04/2012
;;; Description:
;;;
;;;     Standard classes have dependents and are activated
;;;
(deftest mop-class-add/remove-dependent
    (let* ((dep (make-instance 'mop-dependent-object))
           l1 l2 l3 l4 l5)
      (when (find-class 'mop-class-add/remove-dependent nil)
        (setf (class-name (find-class 'mop-class-add/remove-dependent)) nil))
      (defclass mop-class-add/remove-dependent () ())
      (let ((f (find-class 'mop-class-add/remove-dependent)))
        ;;
        ;; * ADD-DEPENDENT registers the object with the class
        ;;
        (clos:add-dependent f dep)
        (setf l1 (clos::class-dependents f))
        ;;
        ;; * SHARED-INITIALIZE invokes UPDATE-DEPENDENT
        ;;
        (defclass mop-class-add/remove-dependent () (a))
        (setf l2 (clos::class-dependents f))
        (setf l3 (mop-dependent-object-log dep))
        ;;
        ;; * REMOVE-DEPENDENT eliminates object from list
        ;;
        (clos:remove-dependent f dep)
        (setf l4 (clos::class-dependents f))
        ;;
        ;; * SHARED-INITIALIZE invokes UPDATE-DEPENDENT without effect
        ;;
        (defclass mop-class-add/remove-dependent () ())
        (setf l5 (mop-dependent-object-log dep))
        ;;
        ;; the first call to defclass adds one entry with the reinitialization
        ;; of the class both in name and list of slots
        (and (equalp l1 (list dep))
              (eq l1 l2)
              (equalp l3
                      (list (list f :name 'mop-class-add/remove-dependent
                                  :direct-superclasses nil
                                  :direct-slots '((:name a)))))
              (null l4)
              (eq l5 l3)
              t)))
  t)


;; Test MOP dispatch

;;; Date: 23/04/2012
;;; Description:
;;;
;;;     COMPUTE-APPLICABLE-METHODS-USING-CLASSES works with one and
;;;     two methods and no EQL.
;;;
(deftest mop-c-a-m-u-c-two-methods
    (progn
      (fmakunbound 'mop-fn)
      (defgeneric mop-fn (a)
        (:method ((a number)) (cos a))
        (:method ((a symbol)) a))
      (let ((m1 (compute-applicable-methods #'mop-fn (list 1.0)))
            (m2 (compute-applicable-methods #'mop-fn (list 'a))))
        (flet ((f (class)
                 (multiple-value-list (clos:compute-applicable-methods-using-classes
                                       #'mop-fn (list (find-class class))))))
          (and (equalp (f 'number) (list m1 t))
               (equalp (f 'real) (list m1 t))
               (equalp (f 'symbol) (list m2 t))
               (equalp (f 'cons) '(nil t))
               t))))
  t)

;;; Date: 23/04/2012
;;; Description:
;;;
;;;     COMPUTE-APPLICABLE-METHODS-USING-CLASSES fails with EQL
;;;     specializers when one of the specializers is covered by the
;;;     classes.
;;;
(deftest mop-c-a-m-u-c-fails-with-eql
    (progn
      (fmakunbound 'mop-fn)
      (defgeneric mop-fn (a)
        (:method ((a (eql 1))) 1)
        (:method ((a (eql 'a))) 2)
        (:method ((a float)) 3))
      (let ((m1 (compute-applicable-methods #'mop-fn (list 1)))
            (m2 (compute-applicable-methods #'mop-fn (list 'a)))
            (m3 (compute-applicable-methods #'mop-fn (list 1.0))))
        (flet ((f (class)
                 (multiple-value-list (clos:compute-applicable-methods-using-classes
                                       #'mop-fn (list (find-class class))))))
          (and (equalp (f 'integer) (list nil nil))
               (equalp (f 'number) (list nil nil))
               (equalp (f 'symbol) (list nil nil))
               (equalp (f 'float) (list m3 t))
               (= (length m1) 1)
               (= (length m2) 1)
               (= (length m3) 1)
               t))))
  t)

;;; Date: 24/04/2012
;;; Description:
;;;
;;;     COMPUTE-DISCRIMINATING-FUNCTION is invoked and honored by ECL.
;;;
(deftest mop-discriminator
    (progn
      (fmakunbound 'foo)
      (defclass my-generic-function (standard-generic-function)
        ())
      (defmethod clos:compute-discriminating-function ((gf my-generic-function))
        ;; We compute the invocaions of c-d-f. Note that it is invoked
        ;; quite often -- we could probably optimize this.
        #'(lambda (&rest args)
            args))
      (defgeneric foo (a)
        (:generic-function-class my-generic-function))
      (unwind-protect
           (foo 2)
        (fmakunbound 'foo)))
  (2))

;;; Date: 24/04/2012
;;; Description:
;;;
;;;     COMPUTE-DISCRIMINATING-FUNCTION is invoked on ADD-METHOD, REMOVE-METHOD,
;;;     DEFGENERIC, INITIALIZE-INSTANCE and REINITIALIZE-INSTANCE acting on
;;;     generic functions.
;;;
(deftest mop-discriminator-recomputation
    (progn
      (defparameter *mop-discriminator-recomputation* 0)
      (fmakunbound 'foo)
      (defclass my-generic-function (standard-generic-function)
        ())
      (defmethod clos:compute-discriminating-function ((gf my-generic-function))
        ;; We compute the invocaions of c-d-f. Note that it is invoked
        ;; quite often -- we could probably optimize this.
        (incf *mop-discriminator-recomputation*)
        (call-next-method))
      (and (progn
             (setf *mop-discriminator-recomputation* 0)
             (eval '(defgeneric foo (a)
                     (:generic-function-class my-generic-function)))
             (plusp *mop-discriminator-recomputation* ))
           (typep #'foo 'my-generic-function)
           (progn
             (setf *mop-discriminator-recomputation* 0)
             (eval '(defmethod foo ((a number)) (print a)))
             (plusp *mop-discriminator-recomputation*))
           (progn
             (setf *mop-discriminator-recomputation* 0)
             (eval '(remove-method #'foo (first (compute-applicable-methods
                                                 #'foo
                                                 (list 1.0)))))
             (plusp *mop-discriminator-recomputation*))
           t))
  t)

;;; Date: 24/04/2012
;;; Description:
;;;
;;;     Verify ECL calls COMPUTE-APPLICABLE-METHODS-USING-CLASSES for
;;;     user-defined generic function classes.
;;;
(deftest mop-compute-applicable-methods-using-classes-is-honored
    (progn
      (defparameter *mop-dispatch-used* 0)
      (fmakunbound 'foo)
      (defclass my-generic-function (standard-generic-function)
        ())
      (defmethod clos:compute-applicable-methods-using-classes
          ((gf my-generic-function) classes)
        (incf *mop-dispatch-used*)
        (call-next-method))
      (defgeneric foo (a)
        (:generic-function-class my-generic-function)
        (:method ((a number)) (cos 1.0)))
      (and (zerop *mop-dispatch-used*)
           (progn (foo 1.0) (plusp *mop-dispatch-used*))))
  t)

;;; Date: 24/04/2012
;;; Description:
;;;
;;;     Verify ECL calls COMPUTE-APPLICABLE-METHODS for
;;;     user-defined generic function classes.
;;;
(deftest mop-compute-applicable-methods-is-honored
    (progn
      (defparameter *mop-dispatch-used* 0)
      (fmakunbound 'foo)
      (defclass my-generic-function (standard-generic-function)
        ())
      (defmethod clos:compute-applicable-methods-using-classes
          ((gf my-generic-function) classes)
        (incf *mop-dispatch-used*)
        (values nil nil))
      (defmethod compute-applicable-methods
          ((gf my-generic-function) args)
        (incf *mop-dispatch-used*)
        (call-next-method))
      (defgeneric foo (a)
        (:generic-function-class my-generic-function)
        (:method ((a number)) (cos 1.0)))
      (and (zerop *mop-dispatch-used*)
           (progn (foo 1.0) (= *mop-dispatch-used* 2))))
  t)

;;; From: Pascal Costanza
;;; Description:
;;;
;;;     sort-applicable-methods is invoked by two methods and one
;;;     invocation triggers a disambiguation error:
;;;
;;;       Condition of type: SIMPLE-ERROR
;;;       The type specifiers #<The STANDARD-CLASS COMMON-LISP-USER::B> and #<The STANDARD-CLASS COMMON-LISP-USER::A> can not be disambiguated with respect to the argument specializer: #<The STANDARD-CLASS STANDARD-CLASS>
(deftest mop-compute-applicable-methods-disambiguation.0001
    (ext:with-clean-symbols (a b c f)
      (defclass a () ())
      (defclass b () ())
      (defclass c (a b) ())
      (defmethod f ((o a)))
      (defmethod f ((o b)))
      (compute-applicable-methods-using-classes
       #'f (list (find-class 'c)))
      T)
  T)


