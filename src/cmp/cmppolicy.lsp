;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;;
;;;; CMPPOLICY -- Code generation choices
;;;;

(in-package "COMPILER")

;;;
;;; ECL encodes the compiler policy an integer. Each bit represents a single
;;; optimization choice. Lowest twenty bits encode the standard optimization
;;; qualities DEBUG, SAFETY, SPEED, SPACE and COMPILATION-SPEED - four bits for
;;; each level. Levels are mutually exclusive for a single quality. Then each
;;; defined policy occupies one bit. For example:
;;;
;;;     X Y Z COMPILATION-SPEED SPACE SPEED SAFETY DEBUG
;;;     0 1 0              0010  0010  1000   0001  0010
;;;
;;; Represents the following optimization settings:
;;;
;;;     (OPTIMIZE (DEBUG 1) (SAFETY 0) (SPEED 3) (COMPILATION-SPEED 2) Y)
;;;
;;; New optimization qualities are defined with DEFINE-POLICY. Such definition
;;; adds one more bit tot he compilation policy and defines a function to test
;;; whether the quality is applicable under the compilation policy of the env.
;;; This functions first checks whether the quality bit is "1" and then may
;;; perform additional tests defined with clauses :REQUIRES.
;;;
;;; Each optimization quality (level) has associated two numbers. When it is
;;; declared in the environment the first number added to the compilation policy
;;; with LOGIOR and the second number is removed from the compilation policy
;;; with LOGANDC2.  Thanks to that it is possible for declaration of one policy
;;; to enable other policies associated with it. For example (DEBUG 1) may be:
;;;
;;;     X Y Z COMPILATION-SPEED SPACE SPEED SAFETY DEBUG
;;;     1 1 0              0000  0000  0000   0000  0010  "on"
;;;     0 0 1              0000  0000  0000   0000  1101  "off"
;;;
;;; When (DEBUG 1) is declared then bits representing X, Y and (DEBUG 1) are set
;;; to 1 and bits representing Z and other DEBUG levels are set to 0. Everything
;;; else remains unchanged. These pairs are "optimization quality switches".
;;;
;;; When a new policy is defined it may contain multiple :ON and :OFF clauses
;;; with an optional parameter representing the "cut off" level. For example:
;;;
;;;   (define-policy W
;;;      ; (SAFETY 0) and (SAFETY 1) "off" flags for W = 1
;;;      ; (SAFETY 2) and (SAFETY 3) "on"  flags for W = 1
;;;     (:on safety 2)
;;;      ; (DEBUG 0)  and (DEBUG 1)  "on"  flags for W = 1
;;;      ; (DEBUG 2)  and (DEBUG 3)  "off" flags for W = 1
;;;     (:off debug 2))
;;;
;;; With this example declaring (SAFETY 2) will enable the policy W and
;;; declaring (SAFETY 1) will disable it. Consider the following example:
;;;
;;;     (locally (declare (safety 2) (debug 2))
;;;       (do-something))
;;;
;;; The optimization (SAFETY 2) enables the policy W while the optimization
;;; (DEBUG 2) disables it. It is apparent from this example that the order in
;;; which we apply quality switches to the compilation policy is important.
;;; COMPUTE-POLICY prioritizes "off" flags over "on" flags so in this case the
;;; policy W will be disabled.
;;;
;;; Only standard optimization qualities have levels. User defined policies may
;;; be also references but the level must not be specified, i.e (:ON CHECK-FOO).
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant *standard-optimization-quality-names*
    '(debug safety speed space compilation-speed)))

(defun standard-optimization-quality-p (name)
  (member name *standard-optimization-quality-names* :test #'eq))

(eval-when (:compile-toplevel :execute)
  (defvar *last-optimization-bit* 20)
  (defvar *optimization-quality-switches*
    (loop with hash = (make-hash-table :size 64 :test #'eq)
          for name in *standard-optimization-quality-names*
          for i from 0 by 4
          for list = (loop with mask = (ash #b1111 i)
                           for level from 0 to 3
                           for bits = (ash 1 (+ level i))
                           collect (cons bits (logxor bits mask)))
          do (setf (gethash name hash) list)
          finally (return hash)))
  ;; For the standard qualities we encode the lowest bit position.
  (defvar *optimization-bits*
    (loop with hash = (make-hash-table :size 64 :test #'eq)
          for name in *standard-optimization-quality-names*
          for i from 0 by 4
          do (setf (gethash name hash) i)
          finally (return hash))))

(eval-when (:load-toplevel :execute)
  (defvar *last-optimization-bit* #.*last-optimization-bit*)
  (defvar *optimization-quality-switches* #.*optimization-quality-switches*)
  (defvar *optimization-bits* #.*optimization-bits*))

(defun take-optimization-bit (name)
  (or (gethash name *optimization-bits*)
      (setf (gethash name *optimization-bits*)
            (incf *last-optimization-bit*))))

(defun optimization-quality-switches (type index)
  (nth index (gethash type *optimization-quality-switches*)))

(defun compute-policy (arguments old-bits &aux (on 0) (off 0))
  (flet ((get-flags (x)
           (if (atom x)
               (if (standard-optimization-quality-p x)
                   (optimization-quality-switches x 3)
                   (optimization-quality-switches x 1))
               (destructuring-bind (name value) x
                 (when (typep value '(integer 0 3))
                   (optimization-quality-switches name value))))))
    (dolist (x arguments)
      (ext:if-let ((flags (get-flags x)))
        (setf on  (logior on  (car flags))
              off (logior off (cdr flags)))
        (cmpwarn "Illegal or unknown OPTIMIZE proclamation ~s." x))))
  (logandc2 (logior old-bits on) off))

(defun augment-policy-switch (on-off switches flag)
  (ecase on-off
    (:on  (rplaca switches (logior (car switches) flag)))
    (:off (rplacd switches (logior (cdr switches) flag)))))

(defun augment-standard-policy (quality level on-off flag)
  (loop for i from 0 to 3
        for bits = (optimization-quality-switches quality i)
        do (if (< i level)
               (ecase on-off
                 (:on  (augment-policy-switch :off bits flag))
                 (:off (augment-policy-switch :on  bits flag)))
               (ecase on-off
                 (:on  (augment-policy-switch :on  bits flag))
                 (:off (augment-policy-switch :off bits flag))))))

(defun augment-extended-policy (quality on-off flag)
  (let ((bits (optimization-quality-switches quality 1)))
    (ecase on-off
      (:only-on  (augment-policy-switch :on  bits flag))
      (:only-off (augment-policy-switch :off bits flag)))))

(defun policy-function-name (base)
  (intern (concatenate 'string "POLICY-" (symbol-name base))
          (find-package "C")))

(defmacro define-policy (&whole whole name &rest conditions)
  (let ((doc (and (stringp (car conditions)) (pop conditions)))
        (test (ash 1 (take-optimization-bit name)))
        (function-name (policy-function-name name)))
    ;; Register as an optimization quality with its own flags.
    (setf (gethash name *optimization-quality-switches*)
          ;;    switched off   switched on    | two levels
          (list (cons 0 test) (cons test 0)))
    ;; Scan the definition and propagate flags of dependent policies.
    (loop with extra = '()
          for case in conditions
          do (case (car case)
               ((:on :off)
                (destructuring-bind (op quality level) case
                  (augment-standard-policy quality level op test)))
               ((:only-on :only-off)
                (destructuring-bind (op quality) case
                  (augment-extended-policy quality op test)))
               (:requires
                (destructuring-bind (op form) case
                  (declare (ignore op))
                  (push form extra)))
               (otherwise
                (error "Syntax error in macro~%  ~A" `(define-policy ,@whole))))
          finally
             (return
               `(defun ,function-name (&optional (env *cmp-env*))
                  ,@(and doc (list doc))
                  (let ((bits (cmp-env-policy env)))
                    (and (logtest bits ,test)
                         ,@extra)))))))

(defmacro define-policy-alias (name doc (op alias))
  (let ((bits (gethash alias *optimization-quality-switches*)))
    (ecase op
      (:alias
       (setf (gethash name *optimization-quality-switches*) bits)
       `(defun ,(policy-function-name name) (&optional (env *cmp-env*))
          ,doc
          (,(policy-function-name alias) env)))
      (:anti-alias
       (setf (gethash name *optimization-quality-switches*) (reverse bits))
       `(defun ,(policy-function-name name) (&optional (env *cmp-env*))
          ,doc
          (not (,(policy-function-name alias) env)))))))

(macrolet ((define-function (fun-name offset)
             `(defun ,fun-name (policy)
                (declare (ext:assume-right-type))
                (loop for level from 0 to 3
                      when (logbitp (+ level ,offset) policy)
                        return level))))
  (define-function policy-to-debug-level  0)
  (define-function policy-to-safety-level 4)
  (define-function policy-to-speed-level  8)
  (define-function policy-to-space-level 12)
  (define-function policy-to-compilation-speed-level 16))
