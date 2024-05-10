;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: CLOS -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    See file 'LICENSE' for the copyright details.

(in-package "CLOS")

(defparameter *clos-booted* nil)

;;; ----------------------------------------------------------------------
;;;
;;; FIND-CLASS  naming classes.
;;;
;;;
;;; (FIND-CLASS <name>) returns the class named <name>.  setf can be used
;;; with find-class to set the class named <name>.  These are "extrinsic"
;;; names.  Neither find-class nor setf of find-class do anything with the
;;; name slot of the class, they only lookup and change the association from
;;; name to class.
;;; 
;;; This is only used during boot. The real one is in built-in.
(eval-when (:compile-toplevel)
  (defun setf-find-class (new-value class &optional errorp env)
    (warn "Ignoring class definition for ~S" class)))

(defun setf-find-class (new-value name &optional errorp env)
  (declare (ignore errorp env))
  (let ((old-class (find-class name nil)))
    (cond
      ((and old-class
            (or (typep old-class 'built-in-class)
                (member name '(class built-in-class) :test #'eq)))
       (unless (eq new-value old-class)
         (error "The class associated to the CL specifier ~S cannot be changed."
                name)))
      ((classp new-value)
       (setf (gethash name si:*class-name-hash-table*) new-value))
      ((null new-value) (remhash name si:*class-name-hash-table*))
      (t (error "~A is not a class." new-value))))
  new-value)

(defsetf find-class (&rest x) (v) `(setf-find-class ,v ,@x))

(defun classp (obj)
  (and (si:instancep obj)
       (let ((topmost (find-class 'CLASS nil)))
         ;; All instances can be classes until the class CLASS has
         ;; been installed. Otherwise, we check the parents.
         ;(print (list (class-id (class-of obj))topmost (and topmost (class-precedence-list topmost))))
         (or (null topmost)
             (si::subclassp (si::instance-class obj) topmost)))
       t))

;;; ----------------------------------------------------------------------
;;; Methods

(defun install-method (name qualifiers specializers lambda-list fun &rest options)
  (declare (notinline ensure-generic-function))
  ;;(record-definition 'method `(method ,name ,@qualifiers ,specializers))
  (let* ((gf (ensure-generic-function name))
         (fun (wrapped-method-function fun))
         (specializers (mapcar #'(lambda (x)
                                   (cond ((consp x) (intern-eql-specializer (second x)))
                                         ((typep x 'specializer) x)
                                         ((find-class x nil))
                                         (t
                                          (error "In method definition for ~A, found an invalid specializer ~A" name specializers))))
                               specializers))
         (method (make-method (generic-function-method-class gf)
                              qualifiers specializers lambda-list
                              fun options)))
    (add-method gf method)
    method))

(defun wrapped-method-function (method-function)
  #'(lambda (.combined-method-args. *next-methods*)
      (declare (special .combined-method-args. *next-methods*))
      (apply method-function .combined-method-args.)))

;;; ----------------------------------------------------------------------
;;;                                                         early versions

;;; early version used during bootstrap
(defun ensure-generic-function (name &key (lambda-list (si::unbound) l-l-p))
  (if (and (fboundp name) (si::instancep (fdefinition name)))
      (fdefinition name)
      ;; create a fake standard-generic-function object:
      (with-early-make-instance +standard-generic-function-slots+
        (gfun (find-class 'standard-generic-function)
              :name name
              :spec-list nil
              :method-combination (find-method-combination nil 'standard nil)
              :lambda-list lambda-list
              :argument-precedence-order
              (and l-l-p (rest (si::process-lambda-list lambda-list t)))
              :method-class (find-class 'standard-method)
              :docstring nil
              :methods nil
              :a-p-o-function nil
              :declarations nil
              :dependents nil)
        ;; create a new gfun
        (set-funcallable-instance-function gfun 'standard-generic-function)
        (setf (fdefinition name) gfun)
        gfun)))

(defun (setf generic-function-name) (new-name gf)
  (if *clos-booted*
      (reinitialize-instance gf :name new-name)
      (setf (slot-value gf 'name) new-name)))

(defun std-compute-discriminating-function (generic-function)
  (values (unoptimized-discriminator generic-function) t))

(defun compute-discriminating-function (gf)
  (declare (notinline std-compute-discriminating-function))
  (std-compute-discriminating-function gf))

(defun set-generic-function-dispatch (gf)
  ;; We have to decide which discriminating function to install:
  ;;   1. One supplied by the user (or not possible to optimize in C)
  ;;   2. One coded in C that follows the MOP
  ;;   3. One in C specialized for slot accessors
  ;;   4. One in C specialized for standard-generic-function
  ;; Respectively:
  ;;   1. The user supplies the discriminating function[1]
  ;;   2. The function is not STANDARD-GENERIC-FUNCTION
  ;;   3. The class of all function methods is STANDARD-OPTIMIZED-*-METHOD
  ;;   4. The function is a STANDARD-GENERIC-FUNCTION
  ;;
  ;; This chain of reasoning uses the fact that the user is not allowed to
  ;; override standard methods specialized on standard classes.
  ;;
  ;; [1] We recognize that the user didn't overwrite our function by examining
  ;;     the second value returned from C-D-F; It is also possible that C-D-F
  ;;     disables the C optimization because it has more efficient strategy.
  ;;
  (declare (notinline compute-discriminating-function))
  (multiple-value-bind (computed-discriminator optimizable)
      (compute-discriminating-function gf)
    (unless (not (> (length (slot-value gf 'spec-list)) 63))
      ;; Code in C caches effective methods; the cache key length is 64 and
      ;; can't handle functions with more specializable arguments. The first
      ;; argument is the generic function itself.
      (setf optimizable nil))
    (let ((methods  (slot-value gf 'methods))
          (standard-generic-function-p
            (eq (slot-value (class-of gf) 'name) 'standard-generic-function)))
      (flet ((only-slot-accessors-p (class-name)
               (and methods
                    (loop with class = (find-class class-name nil)
                          for m in methods
                          always (eq class (class-of m)))
                    class-name)))
        (set-funcallable-instance-function
         gf
         (cond
           ((not optimizable)                 computed-discriminator)
           ((not standard-generic-function-p)                     't)
           ((only-slot-accessors-p 'standard-optimized-reader-method))
           ((only-slot-accessors-p 'standard-optimized-writer-method))
           (t                             'standard-generic-function)))))))

;;; ----------------------------------------------------------------------
;;; COMPUTE-APPLICABLE-METHODS
;;;
;;; This part is a source of problems because we have to access slots of
;;; various objects, which could potentially lead to infinite recursion as
;;; those accessors require also some dispatch. The solution is to avoid
;;; calling then generic function that implement the accessors.
;;; This is possible because:
;;;   1. The user can only extend compute-applicable-methods if it
;;;      defines a method with a subclass of standard-generic-function
;;;   2. The user cannot extend slot-value and friends on standard-classes
;;;      due to the restriction "Any method defined by a portable program
;;;      on a specified generic function must have at least one specializer
;;;      that is neither a specified class nor an eql specializer whose
;;;      associated value is an instance of a specified class."
;;;   3. Subclasses of specified classes preserve the slot order in ECL.
;;;
(defun std-compute-applicable-methods (gf args)
  (declare (optimize (speed 3)))
  (with-early-accessors (+standard-method-slots+
                         +standard-generic-function-slots+
                         +eql-specializer-slots+
                         +standard-class-slots+)
    (flet ((applicable-method-p (method args)
             (loop for spec in (method-specializers method)
                   for arg in args
                   always (if (eql-specializer-flag spec)
                              (eql arg (eql-specializer-object spec))
                              (si::of-class-p arg spec)))))
      (let ((methods (loop for method in (generic-function-methods gf)
                           when (applicable-method-p method args)
                             collect method)))
        (sort-applicable-methods gf methods (mapcar #'class-of args))))))

(defun std-compute-applicable-methods-using-classes (gf classes)
  (declare (optimize (speed 3)))
  (with-early-accessors (+standard-method-slots+
                         +eql-specializer-slots+
                         +standard-generic-function-slots+)
    (flet ((applicable-method-p (method classes)
             ;; EQL specializer invalidates the computation. We still check
             ;; other classes because maybe later specializer will make this
             ;; method not applicable. -- jd 2022-01-05
             (loop with eql-spec-p = nil
                   for spec in (method-specializers method)
                   for class in classes
                   do (if (eql-specializer-flag spec)
                          (let ((object (eql-specializer-object spec)))
                            (if (si::of-class-p object class)
                                (setf eql-spec-p t)
                                (return-from applicable-method-p nil)))
                          (unless (si::subclassp class spec)
                            (return-from applicable-method-p nil)))
                   finally
                      (if eql-spec-p
                          (return-from std-compute-applicable-methods-using-classes
                            (values nil nil))
                          (return t)))))
      (let* ((methods (loop for method in (generic-function-methods gf)
                            when (applicable-method-p method classes)
                              collect method))
             (sorted-methods (sort-applicable-methods gf methods classes)))
        (values sorted-methods t)))))

(setf (fdefinition 'compute-applicable-methods)
      #'std-compute-applicable-methods)

(setf (fdefinition 'compute-applicable-methods-using-classes)
      #'std-compute-applicable-methods-using-classes)

(defun sort-applicable-methods (gf applicable-list args-specializers)
  (declare (optimize (safety 0) (speed 3)))
  (with-early-accessors (+standard-method-slots+
                         +standard-generic-function-slots+
                         +eql-specializer-slots+
                         +standard-class-slots+)
    (let ((f (generic-function-a-p-o-function gf))
          (args-classes
            (loop for arg-spec in args-specializers
                  for req-args in (generic-function-argument-precedence-order gf)
                  collect (if (eql-specializer-flag arg-spec)
                              (class-of (eql-specializer-object arg-spec))
                              arg-spec))))
      ;; reorder args to match the precedence order
      (when f
        (setf args-classes (funcall f args-classes)))
      ;; then order the list
      (do* ((scan applicable-list)
            (most-specific (first scan) (first scan))
            (ordered-list '()))
           ((null (cdr scan))
            (when most-specific
              ;; at least one method
              (nreverse
               (push most-specific ordered-list))))
        (dolist (method (cdr scan))
          (when (eq (compare-methods most-specific method args-classes f)
                    2)
            (setq most-specific method)))
        (setq scan (delete most-specific scan))
        (push most-specific ordered-list)))))

(defun compare-methods (method-1 method-2 args-classes f)
  (declare (si::c-local))
  (with-early-accessors (+standard-method-slots+)
    (let ((specializers-list-1 (method-specializers method-1))
          (specializers-list-2 (method-specializers method-2)))
      (when f
        (setf specializers-list-1 (funcall f specializers-list-1))
        (setf specializers-list-2 (funcall f specializers-list-2)))
      (compare-specializers-lists specializers-list-1
                                  specializers-list-2
                                  args-classes))))

(defun compare-specializers-lists (spec-list-1 spec-list-2 args-classes)
  (declare (si::c-local))
  (when (or spec-list-1 spec-list-2)
    (ecase (compare-specializers (first spec-list-1)
                                 (first spec-list-2)
                                 (first args-classes))
      (1 '1)
      (2 '2)
      (= 
       (compare-specializers-lists (cdr spec-list-1)
                                   (cdr spec-list-2)
                                   (cdr args-classes)))
      ((nil)
       (error "The type specifiers ~S and ~S can not be disambiguated~
                  with respect to the argument specializer: ~S"
              (or (car spec-list-1) t)
              (or (car spec-list-2) t)
              (car args-classes))))))

(defun fast-subtypep (spec1 spec2)
  (declare (si::c-local))
  ;; Specialized version of subtypep which uses the fact that spec1
  ;; and spec2 are either classes or of the form (EQL x)
  (with-early-accessors (+eql-specializer-slots+ +standard-class-slots+)
    (if (eql-specializer-flag spec1)
        (if (eql-specializer-flag spec2)
            (eql (eql-specializer-object spec1)
                 (eql-specializer-object spec2))
            (si::of-class-p (eql-specializer-object spec1) spec2))
        (if (eql-specializer-flag spec2)
            ;; There is only one class with a single element, which
            ;; is NULL = (MEMBER NIL).
            (and (null (eql-specializer-object spec2))
                 (eq (class-name spec1) 'null))
            (si::subclassp spec1 spec2)))))

(defun compare-specializers (spec-1 spec-2 arg-class)
  (declare (si::c-local))
  (with-early-accessors (+standard-class-slots+ +standard-class-slots+)
    (let ((cpl (class-precedence-list arg-class)))
      (cond ((eq spec-1 spec-2) '=)
            ((fast-subtypep spec-1 spec-2) '1)
            ((fast-subtypep spec-2 spec-1) '2)
            ((eql-specializer-flag spec-1) '1) ; is this enough?
            ((eql-specializer-flag spec-2) '2) ; Beppe
            ((member spec-1 (member spec-2 cpl)) '2)
            ((member spec-2 (member spec-1 cpl)) '1)
            ;; This will force an error in the caller
            (t nil)))))

;;; The specialization profile is a sequence with as many elements as the
;;; generic function has required arguments. Each element is a cons:
;;;
;;; (CLASS-SPECIALIZED-P . EQL-SPECIALIZERS)
;;;
;;; CLASS-SPECIALIZED-P is used to determine whether the argument has a
;;; specializer for any class other than the builtin-class T.
;;;
;;; EQL-SPECIALIZERS is a list of all EQL-SPECIALIZER-OBJECTs
(defun compute-g-f-spec-list (gf)
  (with-early-accessors (+standard-generic-function-slots+
                         +eql-specializer-slots+
                         +standard-method-slots+)
    (flet ((nupdate-profile (spec-how-list specializers)
             (if (null spec-how-list)
                 (loop for spec in specializers
                       collect (if (eql-specializer-flag spec)
                                   (list nil (eql-specializer-object spec))
                                   (if (eq spec (find-class t))
                                       (cons nil nil)
                                       (cons   t nil))))
                 (loop for spec in specializers
                       for (arg-profile) on spec-how-list
                       do (if (eql-specializer-flag spec)
                              (push (eql-specializer-object spec)
                                    (cdr arg-profile))
                              (when (and (not (car arg-profile))
                                         (not (eq spec (find-class t))))
                                (setf (car arg-profile) t)))
                       finally (return spec-how-list)))))
      (setf (generic-function-spec-list gf)
            (reduce #'nupdate-profile (generic-function-methods gf)
                    :key #'method-specializers :initial-value nil))
      (let ((function nil)
            (a-p-o (generic-function-argument-precedence-order gf))
            (gf-ll (generic-function-lambda-list gf)))
        (when (consp gf-ll)
          (let ((required-arguments (rest (si::process-lambda-list gf-ll t))))
            (unless (equal a-p-o required-arguments)
              (setf function
                    (coerce `(lambda (%list)
                               (destructuring-bind ,required-arguments %list
                                 (list ,@a-p-o)))
                            'function)))))
        (setf (generic-function-a-p-o-function gf) function))
      (si:clear-gfun-hash gf))))

(defun print-object (object stream)
  (print-unreadable-object (object stream)))
