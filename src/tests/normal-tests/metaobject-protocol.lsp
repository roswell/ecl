;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Author:   Daniel Kochmański
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Metaobject Protocol tests

(in-package #:cl-test)

(suite 'mop)


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
(test mop.0001.fixup
  (is-false
   (block top
     (labels ((test-class (class-object)
                (let ((x (find-if-not #'(lambda (x)
                                          (typep x 'clos:standard-direct-slot-definition))
                                      (clos:class-direct-slots class-object))))
                  (when x
                    (format t "Class ~a has as direct slot ~a" class-object x)
                    (return-from top (class-name class-object))))
                (let ((x (find-if-not #'(lambda (x)
                                          (typep x 'clos:standard-effective-slot-definition))
                                      (clos:class-slots class-object))))
                  (when x
                    (format t "Class ~a has as effective slot ~a" class-object x)
                    (return-from top (class-name class-object))))
                (mapc #'test-class (clos:class-direct-subclasses class-object))))
       (test-class (find-class 't))
       nil))))

;;; Date: 13/02/2006
;;; From: Dan Debertin
;;; Fixed: 24-02-2006 (juanjo)
;;; Description:
;;;
;;;     Subclasses of STANDARD-CLASS would not inherit all their slots
;;;     and thus would cause runtime errors when creating instances.
;;;

(test mop.0002.metaclasses
  (is
   (= 3
      (eval '(progn
              (defclass foo-metaclass (standard-class) ())
              (defclass faa () ((a :initform 2 :initarg :a)) (:metaclass foo-metaclass))
              (prog1 (slot-value (make-instance 'faa :a 3) 'a)
                (cl-test::delete-class 'foo-metaclass 'faa)))))))

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

(test mop.0003.amop-symbols
  (is-false
   (let ((*package* (find-package "CLOS")))
     (remove-if #'(lambda (x)
                    (multiple-value-bind (s to)
                        (find-symbol x *package*)
                      (and s (eq to :external))))
                +mop-symbols+))))

;;; Date: 02/03/2006
;;; From: Dank Corkill
;;; Fixed: 02-03-2006 (Dan Corkill)
;;; Description:
;;;
;;;     DEFCLASS allows additional options which should be handled by the
;;;     metaclass.
;;;
;;; Description:
;;;
;;;     Readers and writers for slot documentation.
;;;

(test mop.0004.defclass-options
  (is
   (equal
    '(T)
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
              (cl-test::delete-class 'foo-metaclass 'faa)))))
   "DEFCLASS allows additional options which should be handled by ~
the metaclass")
  (is
   (equal (eval '(progn
                  (defclass fee ()
                    ((a :initform *aux* :initarg :a)))
                  (setf (documentation (first (clos:class-slots (find-class 'fee))) t)
                   #1="hola")
                  (documentation (first (clos:class-slots (find-class 'fee))) t)))
          #1#)
   "Readers and writers for slot documentation"))

;;; Date: 25/03/2006
;;; From: Pascal Costanza
;;; Fixed: 03/04/2006 (juanjo)
;;; Description:
;;;
;;;     The default slot setter methods had the first argument
;;;     (i.e. the new value) specialized to NIL. This makes it
;;;     impossible to write further specializations.
;;;
(test mop.0005.setf-specializer
  (defclass fee ()
    ((a :accessor fee-a)))
  (is
   (equal '(t fee)
          (mapcar #'class-name
                  (clos:method-specializers
                   (first (clos:generic-function-methods #'(setf fee-a)))))))
  (is
   (equal '(fee)
          (mapcar #'class-name
                  (clos:method-specializers
                   (first (clos:generic-function-methods #'fee-a))))))
  (delete-class 'fee))

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
(ext:with-clean-symbols (test-method)
 (test mop.0006.method-specializer
   (defmethod test-method (a))
   (is (equal
        (mop:method-specializers
         (first (mop:generic-function-methods #'test-method)))
        (list (find-class t))))
   (fmakunbound 'test-method)))

;;; Date: 22/04/2006
;;; From: M. Goffioul
;;; Fixed: 23/04/2006 (juanjo)
;;; Description:
;;;
;;;     When a class inherits from two other classes which have a slot
;;;     with the same name, the new class should inherit the accessors
;;;     from both classes.
;;;

(ext:with-clean-symbols (fee-1 fee-2 fee-3 c-slot-0)
  (test mop.0007.slot-inheritance
    (defclass fee-1 ()
      ((slot-0 :initform 0 :reader slot-0)
       (slot-1 :initform 1 :reader slot-1)))
    (defclass fee-2 ()
      ((slot-0 :initform 2 :reader slot-2)))
    (defclass fee-3 (fee-1 fee-2)
      ((slot-0 :initform 3 :accessor c-slot-0)))
    (flet ((accessors (class)
             (list (class-name class)
                   (mapcar #'clos:slot-definition-readers (clos:class-slots class))
                   (mapcar #'clos:slot-definition-readers (clos:class-slots class)))))
      (is (equal (accessors (find-class 'fee-1))
                 '(fee-1 ((slot-0) (slot-1)) ((slot-0) (slot-1)))))
      (is (equal (accessors (find-class 'fee-2))
                 '(fee-2 ((slot-2)) ((slot-2)))))
      (is (equal (accessors (find-class 'fee-3))
                 '(fee-3 ((c-slot-0 slot-0 slot-2) (slot-1))
                   ((c-slot-0 slot-0 slot-2) (slot-1)))))
      (is (equal (mapcar #'(lambda (o)
                             (mapcar #'(lambda (method)
                                         (handler-case (funcall method o)
                                           (error (c) nil)))
                                     '(slot-0 slot-2 c-slot-0)))
                         (mapcar #'make-instance '(fee-1 fee-2 fee-3)))
                 '((0 nil nil)
                   (nil 2 nil)
                   (3 3 3))))
      (delete-class 'fee-1 'fee-2 'fee-3))))


;;; Date: 28/04/2006
;;; From: P. Costanza
;;; Fixed: 05/05/2006 (P. Costanza)
;;; Description:
;;;
;;;     Option names from classes and generic functions which are not
;;;     in the keyword package should be quoted. This test is
;;;     essentially like mop.0004... because our DEFGENERIC does not
;;;     support non-keyword options.
;;;
(test mop.0008.defclass-option-quote
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
          (is (equal '(t)
               (slot-value (make-instance 'faa) 'a)))
          (cl-test::delete-class 'foo-metaclass 'faa))))


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
(test mop.0009.defclass-initform
  (is (equal
       (loop for quoting in '(nil t)
          collect
            (loop for f in '(most-positive-fixnum #1=#.(lambda () 1) 12 "hola" :a t nil)
               collect (prog1 (eval `(progn
                                       (defclass foo ()
                                         ((a :initform ,(if quoting (list 'quote f) f))))
                                       (slot-value (make-instance 'foo) 'a)))
                         (cl-test::delete-class 'foo))))
       '((#.most-positive-fixnum #1# 12 "hola" :a t nil)
         (most-positive-fixnum #1# 12 "hola" :a t nil)))))


;; Test MOP dependents
(defclass mop-dependent-object ()
  ((log :initform nil :initarg :log :accessor mop-dependent-object-log)))

(defmethod clos:update-dependent ((object t) (dep mop-dependent-object) &rest initargs)
  (push (list* object initargs) (mop-dependent-object-log dep)))

;;; Date: 23/04/2012
;;; Description:
;;;
;;;     ADD-DEPENDENT uses pushnew
;;;
(test mop.0010.gf-add/non-redundant
  (is-true
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
            (equalp l1 (list dep)))))))

;;; Date: 23/04/2012
;;; Description:
;;;
;;;     Generic functions have dependents and are activated
;;;
(test mop.0011.gf-add/remove-dependent
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
      (is-true (equalp l1 (list dep)))
      (is-true (eq l2 (rest l3)))
      (is-true (equalp l3
                       (list (list f 'remove-method m1)
                             (list f 'add-method m1)
                             (list f))))
      (is-true (null l4))
      (is-true (eq l5 l3))
      (is-true (eq l6 l3)))))

;;; Date: 23/04/2012
;;; Description:
;;;
;;;     ADD-DEPENDENT does not duplicate elements
;;;
(test mop.0012.class-add/remove-dependent
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
      (is-true
       (and (eq l1 l2)
            (equalp l1 (list dep)))))))

;;; Date: 23/04/2012
;;; Description:
;;;
;;;     Standard classes have dependents and are activated
;;;
(test mop.0013.class-add/remove-dependent
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
      (is-true
       (and (equalp l1 (list dep))
            (eq l1 l2)
            (equalp l3
                    (list (list f :name 'mop-class-add/remove-dependent
                                :direct-superclasses nil
                                :direct-slots '((:name a)))))
            (null l4)
            (eq l5 l3))))))


;; Test MOP dispatch

;;; Date: 23/04/2012
;;; Description:
;;;
;;;     COMPUTE-APPLICABLE-METHODS-USING-CLASSES works with one and
;;;     two methods and no EQL.
;;;
(test mop.0014.c-a-m-u-c-two-methods
  (fmakunbound 'mop-fn)
  (defgeneric mop-fn (a)
    (:method ((a number)) (cos a))
    (:method ((a symbol)) a))
  (let ((m1 (compute-applicable-methods #'mop-fn (list 1.0)))
        (m2 (compute-applicable-methods #'mop-fn (list 'a))))
    (flet ((f (class)
             (multiple-value-list (clos:compute-applicable-methods-using-classes
                                   #'mop-fn (list (find-class class))))))
      (is-true
       (and (equalp (f 'number) (list m1 t))
            (equalp (f 'real) (list m1 t))
            (equalp (f 'symbol) (list m2 t))
            (equalp (f 'cons) '(nil t)))))))

;;; Date: 23/04/2012
;;; Description:
;;;
;;;     COMPUTE-APPLICABLE-METHODS-USING-CLASSES fails with EQL
;;;     specializers when one of the specializers is covered by the
;;;     classes.
;;;
(test mop.0015.-c-a-m-u-c-fails-with-eql
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
      (is-true
       (and (equalp (f 'integer) (list nil nil))
            (equalp (f 'number) (list nil nil))
            (equalp (f 'symbol) (list nil nil))
            (equalp (f 'float) (list m3 t))
            (= (length m1) 1)
            (= (length m2) 1)
            (= (length m3) 1))))))

;;; Date: 24/04/2012
;;; Description:
;;;
;;;     COMPUTE-DISCRIMINATING-FUNCTION is invoked and honored by ECL.
;;;
(test mop.0016.discriminator
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
  (is (equal '(2)
             (unwind-protect
                  (foo 2)
               (fmakunbound 'foo)))))

;;; Date: 24/04/2012
;;; Description:
;;;
;;;     COMPUTE-DISCRIMINATING-FUNCTION is invoked on ADD-METHOD, REMOVE-METHOD,
;;;     DEFGENERIC, INITIALIZE-INSTANCE and REINITIALIZE-INSTANCE acting on
;;;     generic functions.
;;;
(ext:with-clean-symbols (*mop-discriminator-recomputation* foo my-generic-function)
 (test mop.0017.discriminator-recomputation
   (defparameter *mop-discriminator-recomputation* 0)
   (defclass my-generic-function (standard-generic-function) ())
   (defmethod clos:compute-discriminating-function ((gf my-generic-function))
    ;; We compute the invocaions of c-d-f. Note that it is invoked
    ;; quite often -- we could probably optimize this.
    (incf *mop-discriminator-recomputation*)
    (call-next-method))
  (is-true
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
          (plusp *mop-discriminator-recomputation*))))))

;;; Date: 24/04/2012
;;; Description:
;;;
;;;     Verify ECL calls COMPUTE-APPLICABLE-METHODS-USING-CLASSES for
;;;     user-defined generic function classes.
;;;
(ext:with-clean-symbols (*mop-dispatch-used* my-generic-function foo)
  (test mop.0018.c-a-m-u-c-is-honored
    (defparameter *mop-dispatch-used* 0)
    (defclass my-generic-function (standard-generic-function) ())
    (defmethod clos:compute-applicable-methods-using-classes
        ((gf my-generic-function) classes)
      (incf *mop-dispatch-used*)
      (call-next-method))
    (defgeneric foo (a)
      (:generic-function-class my-generic-function)
      (:method ((a number)) (cos 1.0)))
    (is-true
     (and (zerop *mop-dispatch-used*)
          (progn (foo 1.0) (plusp *mop-dispatch-used*))))))

;;; Date: 24/04/2012
;;; Description:
;;;
;;;     Verify ECL calls COMPUTE-APPLICABLE-METHODS for
;;;     user-defined generic function classes.
;;;
(ext:with-clean-symbols (*mop-dispatch-used* my-generic-function foo)
  (test mop.0019.compute-applicable-methods-is-honored
    (defparameter *mop-dispatch-used* 0)
    (defclass my-generic-function (standard-generic-function) ())
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
    (is-true
     (and (zerop *mop-dispatch-used*)
          (progn (foo 1.0) (= *mop-dispatch-used* 2))))))

;;; From: Pascal Costanza
;;; Description:
;;;
;;;  sort-applicable-methods is invoked by two methods and one
;;;  invocation triggers a disambiguation error:
;;;
;;;   Condition of type: SIMPLE-ERROR
;;;
;;;    The type specifiers #<The STANDARD-CLASS COMMON-LISP-USER::B>
;;;    and #<The STANDARD-CLASS COMMON-LISP-USER::A> can not be
;;;    disambiguated with respect to the argument specializer:
;;;    #<The STANDARD-CLASS STANDARD-CLASS>
(ext:with-clean-symbols (a b c f)
  (defclass a () ())
  (defclass b () ())
  (defclass c (a b) ())
  (defmethod f ((o a)))
  (defmethod f ((o b)))
  (test mop.0020.c-a-m-disambiguation
    (finishes
      (clos:compute-applicable-methods-using-classes
       #'f (list (find-class 'c))))))

;;; Bug #46
;;;
;;; Reported 2016-05-30
;;;
;;; Description: DEFGENERIC doesn't create methods on same pass as creating
;;; generics.
(test mop.0021.ensure-generic
  (is (progn (fmakunbound 'mop.0021.ensure-generic.fun)
             (defun mop.0021.ensure-generic.fun () 'hi)
             (with-temporary-file (input-file "
(fmakunbound 'mop.0021.ensure-generic.fun)
(defmethod mop.0021.ensure-generic.fun () (print 'bye))
")
               (compile-file input-file)))))

;;; Bug #426
;;;
;;; CLOS::*CLOS-BOOTED* wasn't set correctly because of gray-streams bootstrap
;;; (fundamental-stream subclasses stream).
(ext:with-clean-symbols (foo)
  (test mop.0022.subclass-builtin
    (signals error (defclass foo (stream) ()))))

;;; Bug #425
;;;
;;; update-dependent didn't invalidate initarg caches when new methods were
;;; defined.
(ext:with-clean-symbols (foo)
  (test mop.0023.update-dependent
    (defclass foo () ())
    (finishes (make-instance 'foo))
    (signals error (make-instance 'foo :test "hi"))
    (defmethod initialize-instance :after ((obj foo) &key test) test)
    (finishes (make-instance 'foo :test "hi"))
    (signals error (make-instance 'foo :test "hi" :bam "bye"))))

;;; Ensure that forward-referenced classes work as expected.
(ext:with-clean-symbols (foo1 foo2)
  (test mop.0024.frc
    (finishes (defclass foo1 (foo2) ()))
    (signals error (make-instance 'foo1))
    (finishes (defclass foo2 () ()))
    (finishes (make-instance 'foo1))))
