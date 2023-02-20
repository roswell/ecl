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


(defconstant *standard-optimization-quality-names*
  '(debug safety speed space compilation-speed))

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
               (optimization-quality-switches x 3)
               (destructuring-bind (name value) x
                 (when (typep value '(integer 0 3))
                   (optimization-quality-switches name value))))))
    (dolist (x arguments)
      (ext:if-let ((flags (get-flags x)))
        (setf on  (logior on  (car flags))
              off (logior off (cdr flags)))
        (cmpwarn "Illegal or unknown OPTIMIZE proclamation ~s." x))))
  (logandc2 (logior old-bits on) off))

;;; for example        debug   2     :on    #x10
(defun augment-policy (quality level on-off flag)
  (flet ((flip (on-off switches flag)
           (ecase on-off
             (:on  (rplaca switches (logior (car switches) flag)))
             (:off (rplacd switches (logior (cdr switches) flag))))))
    (loop for i from 0 to 3
          for bits = (optimization-quality-switches quality i)
          do (if (< i level)
                 (ecase on-off
                   (:on       (flip :off bits flag))
                   (:off      (flip :on  bits flag))
                   (:only-on  nil)
                   (:only-off nil))
                 (ecase on-off
                   (:on       (flip :on  bits flag))
                   (:off      (flip :off bits flag))
                   (:only-on  (flip :on  bits flag))
                   (:only-off (flip :off bits flag)))))))

(defun policy-function-name (base)
  (intern (concatenate 'string "POLICY-" (symbol-name base))
          (find-package "C")))

(defmacro define-policy (&whole whole name &rest conditions)
  (let ((doc (and (stringp (car conditions)) (pop conditions)))
        (test (ash 1 (take-optimization-bit name)))
        (function-name (policy-function-name name)))
    ;; Register as an optimization quality with its own flags.
    (let* ((circular-list (list (cons test 0)))
           (flags-list (list* (cons 0 test) circular-list)))
      (rplacd circular-list circular-list)
      (setf (gethash name *optimization-quality-switches*) flags-list))
    ;; Scan the definition and propagate flags of dependent policies.
    (loop with extra = '()
          for case in conditions
          do (case (car case)
               ((:on :off)
                (destructuring-bind (op quality level) case
                  (augment-policy quality level op test)))
               ((:only-on :only-off)
                (destructuring-bind (op quality) case
                  (augment-policy quality 1 op test)))
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
       (rotatef (first bits) (second bits))
       (rplacd (cdr bits) (cdr bits))
       (setf (gethash name *optimization-quality-switches*) bits)
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
