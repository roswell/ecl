;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: CLOS -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    See file 'LICENSE' for the copyright details.

(in-package "CLOS")

;;; ----------------------------------------------------------------------
;;; INSTANCE UPDATE PROTOCOL
;;;
;;;
;;; PART 1: CHANGING THE CLASS OF AN INSTANCE
;;;
;;; The method CHANGE-CLASS performs most of the work.
;;;
;;;     a) The structure of the instance is changed to match the new
;;;        number of local slots.
;;;     b) The new local slots are filled with the value of the old
;;;        slots. Only the name is used, so that a new local slot may
;;;        get the value of old slots that were either local or shared.
;;;     c) Finally, UPDATE-INSTANCE-FOR-DIFFERENT-CLASS is invoked
;;;        with a copy of the instance as it looked before the change,
;;;        the changed instance and enough information to perform any
;;;        extra processing.
;;;

(defmethod update-instance-for-different-class
    ((old-data standard-object) (new-data standard-object) &rest initargs)
  (let ((old-local-slotds (si::instance-slotds old-data))
        (new-local-slotds (remove :instance (si::instance-slotds new-data)
                                  :test-not #'eq :key #'slot-definition-allocation))
        added-slots)
    (setf added-slots (set-difference (mapcar #'slot-definition-name new-local-slotds)
                                      (mapcar #'slot-definition-name old-local-slotds)))
    (check-initargs (class-of new-data) initargs
                    (valid-keywords-from-methods
                     (compute-applicable-methods
                      #'update-instance-for-different-class
                      (list old-data new-data))
                     (compute-applicable-methods
                      #'shared-initialize (list new-data added-slots))))
    (apply #'shared-initialize new-data added-slots initargs)))

(defmethod change-class ((instance standard-object) (new-class std-class)
                         &rest initargs)
  (let* ((old-instance (si::copy-instance instance))
         (new-size (class-size new-class))
         (instance (si::allocate-raw-instance instance new-class new-size)))
    (si::instance-sig-set instance)
    ;; "The values of local slots specified by both the class Cto and
    ;; Cfrom are retained.  If such a local slot was unbound, it remains
    ;; unbound."
    ;; "The values of slots specified as shared in the class Cfrom and
    ;; as local in the class Cto are retained."
    (let* ((new-local-slotds (class-slots (class-of instance))))
      (dolist (new-slot new-local-slotds)
        ;; CHANGE-CLASS can only operate on the value of local slots.
        (when (eq (slot-definition-allocation new-slot) :INSTANCE)
          (let ((name (slot-definition-name new-slot)))
            (if (and (slot-exists-p old-instance name)
                     (slot-boundp old-instance name))
                (setf (slot-value instance name) (slot-value old-instance name))
                (slot-makunbound instance name))))))
    (apply #'update-instance-for-different-class old-instance instance
           initargs)
    instance))

(defmethod change-class ((instance class) new-class &rest initargs)
  (declare (ignore new-class initargs))
  (if (forward-referenced-class-p instance)
      (call-next-method)
      (error "The metaclass of a class metaobject cannot be changed.")))

;;;
;;; PART 2: UPDATING AN INSTANCE THAT BECAME OBSOLETE
;;;
;;; Each instance has a field (readable with SI::INSTANCE-SLOTDS),
;;; which contains the list of slot definitions from its class which
;;; are needed to update it when it is obsolete. Generally
;;;
;;;     (SI::INSTANCE-OBSOLETE-P x)
;;;
;;; returns T whenever the instance became obsolete. We track that by
;;; comparing the class stamp (which is updated by M-I-O) with a stamp
;;; embedded in the instance. When the instance is being updated its
;;; stamp is corrected to match its class stamp.
;;;
;;; There are two circumstances under which a instance may become
;;; obsolete: either the class has been modified so the list of slots
;;; has changed in an incompatible way (see FINALIZE-INHERITANCE), or
;;; MAKE-INSTANCES-OBSOLETE has been called explicitly.
;;;
;;; The function UPDATE-INSTANCE (hidden to the user) does the job of
;;; updating an instance that has become obsolete.
;;;
;;;     a) A copy of the instance is saved to check the old values.
;;;     b) The structure of the instance is changed to match the new
;;;        number of local slots.
;;;     c) The new local slots are filled with the value of the old
;;;        local slots.
;;;     d) Finally, UPDATE-INSTANCE-FOR-REDEFINED-CLASS is invoked
;;;        with enough information to perform any extra initialization,
;;;        for instance of new slots.
;;;
;;; It is not clear when the function UPDATE-INSTANCE is invoked. At least
;;; this will happen whenever the functions SLOT-VALUE, (SETF SLOT-VALUE),
;;; SLOT-BOUNDP or SLOT-EXISTS-P are used.
;;;

(defmethod update-instance-for-redefined-class
    ((instance standard-object) added-slots discarded-slots property-list
     &rest initargs)
  (check-initargs (class-of instance) initargs
                  (valid-keywords-from-methods
                   (compute-applicable-methods
                    #'update-instance-for-redefined-class
                    (list instance added-slots discarded-slots property-list))
                   (compute-applicable-methods
                    #'shared-initialize
                    (list instance added-slots))))
  (apply #'shared-initialize instance added-slots initargs))

(defmethod update-instance-for-redefined-class
    ((instance std-class) added-slots discarded-slots property-list
     &rest initargs)
  (declare (ignore initargs))
  ;; If the metaclass of this class changed, so did probably that of its
  ;; subclasses. We need those subclasses to be up-to-date. This prevents
  ;; errors when loading twice the following
  ;;   (defclass metaclas ...)
  ;;   (defclass x () ... (:metaclas metaclas))
  ;;   (defclass y (y) ...)
  ;; because X might be redefined with Y not being up-to-date on the second
  ;; pass.
  (prog1 (call-next-method)
    (dolist (class (class-direct-subclasses instance))
      (ensure-up-to-date-instance class))))

(defun update-instance (instance)
  (let* ((class (class-of instance))
         (old-slotds (si::instance-slotds instance))
         (new-slotds (class-slots class))
         (old-instance (si::copy-instance instance))
         (discarded-slots '())
         (added-slots '())
         (property-list '()))
    (setf instance (si::allocate-raw-instance instance class (class-size class)))
    (si::instance-sig-set instance)
    (let* ((new-i 0)
           (old-local-slotds (remove :instance old-slotds :test-not #'eq
                                     :key #'slot-definition-allocation))
           (new-local-slotds (remove :instance new-slotds :test-not #'eq
                                     :key #'slot-definition-allocation)))
      (declare (fixnum new-i))
      (setq discarded-slots
            (set-difference (mapcar #'slot-definition-name old-local-slotds)
                            (mapcar #'slot-definition-name new-local-slotds)))
      (dolist (slot-name discarded-slots)
        (let* ((ndx (position slot-name old-local-slotds :key #'slot-definition-name)))
          (setf property-list
                (list* slot-name (si::instance-ref old-instance ndx) property-list))))
      (dolist (new-slot new-local-slotds)
        (let* ((name (slot-definition-name new-slot))
               (old-i (position name old-local-slotds :key #'slot-definition-name)))
          (if old-i
              (si::instance-set instance new-i
                                (si::instance-ref old-instance old-i))
              (push name added-slots))
          (incf new-i))))
    (update-instance-for-redefined-class instance added-slots
                                         discarded-slots property-list)))

;;; ----------------------------------------------------------------------
;;; CLASS REDEFINITION PROTOCOL

(ensure-generic-function 'reinitialize-instance
                         :lambda-list '(class &rest initargs))

(defmethod reinitialize-instance ((class class) &rest initargs
                                  &key (direct-superclasses () direct-superclasses-p)
                                       (direct-slots nil direct-slots-p))
  (declare (ignore initargs))
  (let ((name (class-name class)))
    (when (member name '(CLASS BUILT-IN-CLASS) :test #'eq)
      (error "The kernel CLOS class ~S cannot be changed." name)))
  ;; remove previous defined accessor methods
  (when (class-finalized-p class)
    (remove-optional-slot-accessors class))
  (call-next-method)
  ;; the list of direct slots is converted to direct-slot-definitions
  (when direct-slots-p
    (setf (class-direct-slots class)
          (loop for s in direct-slots
                collect (canonical-slot-to-direct-slot class s))))
  ;; set up inheritance checking that it makes sense
  (when direct-superclasses-p
    (setf direct-superclasses 
          (check-direct-superclasses class direct-superclasses))
    (dolist (l (class-direct-superclasses class))
      (unless (member l direct-superclasses)
        (remove-direct-subclass l class)))
    (dolist (l (setf (class-direct-superclasses class)
                     direct-superclasses))
      (add-direct-subclass l class)))
  ;; Per "Reinitialization of Class Metaobjects" we must finalize the
  ;; inheritance here. Note that this means that already finalized class can't
  ;; be reinitialized to have a forward-referenced-class as a superclass.
  (if (class-finalized-p class)
      (finalize-inheritance class)
      (finalize-unless-forward class))
  class)

(defun slot-definitions-compatible-p (old-slotds new-slotds)
  (loop for o = (pop old-slotds)
        for n = (pop new-slotds)
        while (and o n)
        do (let ((old-alloc (slot-definition-allocation o))
                 (new-alloc (slot-definition-allocation n))
                 (old-class (class-of o))
                 (new-class (class-of n)))
             (unless (and (eq old-alloc new-alloc)
                          (eq (slot-definition-name o)
                              (slot-definition-name n))
                          (or (not (eq old-alloc :instance))
                              (= (slot-definition-location o)
                                 (slot-definition-location n)))
                          (eq old-class new-class)
                          (or (eq new-class
                                  (find-class 'standard-direct-slot-definition))
                              (eq new-class
                                  (find-class 'standard-effective-slot-definition))))
               (return-from slot-definitions-compatible-p nil)))
        finally
           (return (and (null o)
                        (null n)
                        (null old-slotds)
                        (null new-slotds)))))

(defmethod make-instances-obsolete ((class class))
  (si::instance-new-stamp class)
  class)

;;; Structures can't be redefined in an incompatible way.
(defmethod make-instances-obsolete ((class structure-class))
  class)

(defun remove-optional-slot-accessors (class)
  (declare (class class)
           (optimize (safety 0))
           (si::c-local))
  (dolist (slotd (class-slots class))
    ;; remove previous defined reader methods
    (dolist (reader (slot-definition-readers slotd))
      (let* ((gf-object (fdefinition reader))
             found)
        ;; primary method
        (when (setq found
                    (find-method gf-object nil (list class) nil))
          (remove-method gf-object found))
        ;; before method
        (when (setq found
                    (find-method gf-object ':before (list class) nil))
          (remove-method gf-object found))
        ;; after method
        (when (setq found
                    (find-method gf-object ':after (list class) nil))
          (remove-method gf-object found))))

    ;; remove previous defined writer methods
    (dolist (writer (slot-definition-writers slotd))
      (let* ((gf-object (fdefinition writer))
             found)
        ;; primary method
        (when (setq found
                    (find-method gf-object nil (list 'T class) nil))
          (remove-method gf-object found))
        ;; before method
        (when (setq found
                    (find-method gf-object ':before (list 'T class) nil))
          (remove-method gf-object found))
        ;; after method
        (when (setq found
                    (find-method gf-object ':after (list 'T class) nil))
          (remove-method gf-object found))))))
