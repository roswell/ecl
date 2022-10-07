;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;  Copyright (c) 2015-2021, Daniel Kochmański
;;;;
;;;;  See file 'LICENSE' for the copyright details.
;;;;

;;; Bootstrapping CLOS is prone to metastability issues. To avoid them some
;;; operators have different definitions during the bootstrap and the runtime.
;;; This file "fixes" these operators to have their final form.

(in-package "CLOS")

;;; ---------------------------------------------------------------------
;;; Fixup

;;; Early version of the stack handler.
(defun sys::stack-error-handler (continue-string datum args)
  (declare (ignore continue-string))
  (apply #'error datum args))

(defun register-method-with-specializers (method)
  (declare (si::c-local))
  (with-early-accessors (+standard-method-slots+ +specializer-slots+)
    (loop for spec in (method-specializers method)
          ;; Inlined "early" ADD-DIRECT-METHODS.
          do (let ((cell (specializer-method-holder spec)))
               (setf (cdr cell) nil
                     (car cell) (adjoin method (car cell) :test #'eq))))))

(defun fixup-early-methods ()
  (dolist (method-info *early-methods* (makunbound '*EARLY-METHODS*))
    (let* ((method-name (car method-info))
           (gfun (fdefinition method-name))
           (standard-method-class (find-class 'standard-method)))
      (with-early-accessors (+class-slots+)
        (when (eq 'T (class-id (si:instance-class gfun)))
          ;; complete the generic function object
          (si:instance-class-set gfun (find-class 'STANDARD-GENERIC-FUNCTION))
          (si::instance-sig-set gfun)
          (setf (slot-value gfun 'method-class) standard-method-class)
          (setf (slot-value gfun 'docstring) nil)))
      (dolist (method (cdr method-info))
        ;; complete the method object
        (let ((old-class (si::instance-class method)))
          (si::instance-class-set method
                                  (cond ((null old-class)
                                         (find-class 'standard-method))
                                        ((symbolp old-class)
                                         (find-class (truly-the
                                                      symbol
                                                      old-class)))
                                        (t
                                         old-class))))
        (si::instance-sig-set gfun)
        (register-method-with-specializers method)))))

(fixup-early-methods)

;;; Now we protect classes from redefinition:
(eval-when (:compile-toplevel :load-toplevel)
  (defun setf-find-class (new-value name &optional errorp env)
    (declare (ignore errorp))
    (let ((old-class (find-class name nil env)))
      (cond
        ((typep old-class 'built-in-class)
         (error "The class associated to the CL specifier ~S cannot be changed."
                name))
        ((member name '(CLASS BUILT-IN-CLASS) :test #'eq)
         (error "The kernel CLOS class ~S cannot be changed." name))
        ((classp new-value)
         (setf (gethash name si:*class-name-hash-table*) new-value))
        ((null new-value) (remhash name si:*class-name-hash-table*))
        (t (error "~A is not a class." new-value))))
    new-value))


;;; ---------------------------------------------------------------------
;;; Redefined functions

(defun method-p (method) (typep method 'METHOD))

(defun make-method (method-class qualifiers specializers arglist
                    function options)
  (apply #'make-instance
         method-class
         :generic-function nil
         :qualifiers qualifiers
         :lambda-list arglist
         :specializers specializers
         :function function
         :allow-other-keys t
         options))

(defun all-keywords (l)
  (declare (si::c-local))
  (let ((all-keys '()))
    (do ((l (rest l) (cddddr l)))
        ((null l)
         all-keys)
      (push (first l) all-keys))))

(defun congruent-lambda-p (l1 l2)
  (multiple-value-bind (r1 opts1 rest1 key-flag1 keywords1 a-o-k1)
      (si::process-lambda-list l1 'FUNCTION)
    (multiple-value-bind (r2 opts2 rest2 key-flag2 keywords2 a-o-k2)
        (si::process-lambda-list l2 'FUNCTION)
      (and (= (length r2) (length r1))
           (= (length opts1) (length opts2))
           (eq (and (null rest1) (null key-flag1))
               (and (null rest2) (null key-flag2)))
           ;; All keywords mentioned in the generic function must be
           ;; accepted by the method.
           (or (null key-flag1)
               (null key-flag2)
               ;; Testing for a-o-k1 here may not be conformant when
               ;; the fourth point of 7.6.4 is read literally, but it
               ;; is more consistent with the generic function calling
               ;; specification. Also it is compatible with popular
               ;; implementations like SBCL and CCL. -- jd 2020-04-07
               a-o-k1
               a-o-k2
               (null (set-difference (all-keywords keywords1)
                                     (all-keywords keywords2))))
           t))))

(defun add-method (gf method)
  ;; during boot it's a structure accessor
  (declare (notinline method-qualifiers remove-method))
  ;;
  ;; 1) The method must not be already installed in another generic
  ;; function.
  ;;
  (let ((other-gf (method-generic-function method)))
    (unless (or (null other-gf) (eq other-gf gf))
      (error "The method ~A belongs to the generic function ~A ~
and cannot be added to ~A." method other-gf gf)))
  ;;
  ;; 2) The method and the generic function should have congruent
  ;;    lambda lists. That is, it should accept the same number of
  ;;    required and optional arguments, and only accept keyword
  ;;    arguments when the generic function does.
  ;;
  (let ((new-lambda-list (method-lambda-list method)))
    (if (slot-boundp gf 'lambda-list)
        (let ((old-lambda-list (generic-function-lambda-list gf)))
          (unless (congruent-lambda-p old-lambda-list new-lambda-list)
            (error "Cannot add the method ~A to the generic function ~A because ~
their lambda lists ~A and ~A are not congruent."
                   method gf old-lambda-list new-lambda-list)))
        (reinitialize-instance
         gf :lambda-list (implicit-generic-lambda new-lambda-list))))
  ;;
  ;; 3) Finally, it is inserted in the list of methods, and the method is
  ;;    marked as belonging to a generic function.
  ;;
  (when (generic-function-methods gf)
    (let* ((method-qualifiers (method-qualifiers method)) 
           (specializers (method-specializers method))
           (found (find-method gf method-qualifiers specializers nil)))
      (when found
        (remove-method gf found))))
  ;;
  ;; We install the method by:
  ;;  i) Adding it to the list of methods
  (push method (generic-function-methods gf))
  (setf (method-generic-function method) gf)
  ;;  ii) Updating the specializers list of the generic function.
  (dolist (spec (method-specializers method))
    (add-direct-method spec method))
  ;;  iii) Computing a new discriminating function... Well, since the core ECL
  ;;  does not need the discriminating function because we always use the same
  ;;  one, we just update the spec-how list of the generic function..
  (compute-g-f-spec-list gf)
  (set-generic-function-dispatch gf)
  ;;  iv) Update dependents.
  (update-dependents gf (list 'add-method method))
  gf)

(defun remove-method (gf method)
  (setf (generic-function-methods gf)
        (delete method (generic-function-methods gf))
        (method-generic-function method) nil)
  (si:clear-gfun-hash gf)
  (loop for spec in (method-specializers method)
     do (remove-direct-method spec method))
  (compute-g-f-spec-list gf)
  (set-generic-function-dispatch gf)
  (update-dependents gf (list 'remove-method method))
  gf)


;;; ----------------------------------------------------------------------------
;;; Converting functions to methods
;;;
;;; To avoid metastability issues we install the old function directly as the
;;; discriminator (with necessary checks).  That works because there are no
;;; other methods. When a new method is added then the discriminating function
;;; will be recomputed.

(macrolet
    ((do-function-to-method (name signature)
       (multiple-value-bind (lambda-list type-checks ignores)
           (loop for argument in signature
                 if (consp argument)
                   do (assert (atom (second argument)))
                   and collect (first argument)
                         into lambda-list
                         and collect `(typep ,(first argument)
                                             ',(second argument))
                               into type-checks
                 else
                   collect argument into lambda-list
                   and unless (member argument lambda-list-keywords)
                         collect argument into ignores
                 finally
                    (return (values lambda-list type-checks ignores)))
         `(let* ((aux-name 'temp-method)
                 (old-function (fdefinition ',name))
                 (method (eval `(defmethod ,aux-name ,',signature)))
                 (generic-function (fdefinition aux-name)))
            (with-early-accessors (+standard-method-slots+)
              (setf (method-function method)
                    (wrapped-method-function old-function)))
            (setf (fdefinition ',name) generic-function)
            (setf (slot-value generic-function 'name) ',name)
            (set-funcallable-instance-function
             generic-function
             #'(lambda (&rest args)
                 (destructuring-bind ,lambda-list args
                   (declare (ignore ,@ignores))
                   (unless (or (null *clos-booted*)
                               (and ,@type-checks))
                     (apply #'no-applicable-method generic-function args)))
                 (apply old-function args)))
            (fmakunbound aux-name)))))
  (do-function-to-method add-method
    ((gf standard-generic-function) (method standard-method)))
  (do-function-to-method remove-method
    ((gf standard-generic-function) (method standard-method)))
  (do-function-to-method find-method
    ((gf standard-generic-function) qualifiers specializers &optional error))
  (do-function-to-method compute-discriminating-function
    ((gf standard-generic-function)))
  (do-function-to-method generic-function-method-class
    ((gf standard-generic-function)))
  (do-function-to-method (setf generic-function-name)
    (name (gf generic-function)))
  (do-function-to-method find-method-combination
    ((gf standard-generic-function)
     method-combination-type-name
     method-combination-options))
  (do-function-to-method make-method-lambda
    ((gf standard-generic-function) (method standard-method)
     lambda-form environment))
  (do-function-to-method compute-applicable-methods-using-classes
    ((gf standard-generic-function) classes))
  (do-function-to-method compute-applicable-methods
    ((gf standard-generic-function) arguments))
  (do-function-to-method compute-effective-method
    ((gf standard-generic-function) method-combination applicable-methods)))


;;; ----------------------------------------------------------------------------
;;; Missing methods

(defmethod reader-method-class ((class std-class)
                                (direct-slot direct-slot-definition)
                                &rest initargs)
  (declare (ignore class direct-slot initargs))
  (find-class (if (member (class-name (class-of class))
                          '(standard-class
                            funcallable-standard-class
                            structure-class))
                  'standard-optimized-reader-method
                  'standard-reader-method)))

(defmethod writer-method-class ((class std-class)
                                (direct-slot direct-slot-definition)
                                &rest initargs)
  (declare (ignore class direct-slot initargs))
  (find-class (if (member (class-name (class-of class))
                          '(standard-class
                            funcallable-standard-class
                            structure-class))
                  'standard-optimized-writer-method
                  'standard-reader-method)))

;;; Error messages
(defmethod no-applicable-method (gf &rest args)
  (error "No applicable method for ~S with ~
          ~:[no arguments~;arguments of types ~:*~{~& ~A~}~]."
         (generic-function-name gf)
         (mapcar #'type-of args)))

(defmethod no-next-method (gf method &rest args)
  (declare (ignore gf))
  (error "In method ~A~%No next method given arguments ~A" method args))

(defun no-primary-method (gf &rest args)
  (error "Generic function: ~A. No primary method given arguments: ~S"
         (generic-function-name gf) args))



;;; ----------------------------------------------------------------------------
;;; Dependent maintenance protocol

(defmethod add-dependent ((c class) dep)
  (pushnew dep (class-dependents c)))

(defmethod add-dependent ((c generic-function) dependent)
  (pushnew dependent (generic-function-dependents c)))

(defmethod remove-dependent ((c class) dep)
  (setf (class-dependents c)
        (remove dep (class-dependents c))))

(defmethod remove-dependent ((c standard-generic-function) dep)
  (setf (generic-function-dependents c)
        (remove dep (generic-function-dependents c))))

(defmethod map-dependents ((c class) function)
  (dolist (d (class-dependents c))
    (funcall function d)))

(defmethod map-dependents ((c standard-generic-function) function)
  (dolist (d (generic-function-dependents c))
    (funcall function d)))

(ensure-generic-function 'update-dependent
                         :lambda-list '(object dependent &rest initargs))

;; After this, update-dependents will work
(setf *clos-booted* 'map-dependents)

(defclass initargs-updater () ())

(defun recursively-update-classes (a-class)
  (slot-makunbound a-class 'valid-initargs)
  (mapc #'recursively-update-classes
        (class-direct-subclasses a-class)))

(defmethod update-dependent ((object generic-function) (dep initargs-updater)
                             &rest initargs
                             &key
                               ((add-method added-method) nil am-p)
                               ((remove-method removed-method) nil rm-p)
                             &allow-other-keys)
  (declare (ignore object dep initargs))
  (when-let ((method (cond (am-p added-method)
                           (rm-p removed-method))))
    ;; update-dependent is also called when the gf itself is reinitialized,
    ;; so make sure we actually have a method that's added or removed
    (let ((spec (first (method-specializers method)))) ; the class being initialized or allocated
      (when (classp spec)            ; sanity check against eql specialization
        (recursively-update-classes spec)))))

(let ((x (make-instance 'initargs-updater)))
  (add-dependent #'shared-initialize x)
  (add-dependent #'initialize-instance x)
  (add-dependent #'allocate-instance x))
