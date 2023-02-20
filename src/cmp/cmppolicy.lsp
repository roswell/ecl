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
  (defvar *optimization-quality-switches*
    (loop with hash = (make-hash-table :size 64 :test #'eq)
          for name in '(debug safety speed space compilation-speed)
          for i from 0 by 4
          for list = (loop with mask = (ash #b1111 i)
                           for level from 0 to 3
                           for bits = (ash 1 (+ level i))
                           collect (cons bits (logxor bits mask)))
          do (setf (gethash name hash) list)
          finally (return hash)))
  (defvar *last-optimization-bit* 20)
  (defvar *optimization-bits* (make-hash-table)))

(eval-when (:load-toplevel :execute)
  (defvar *optimization-quality-switches* #.*optimization-quality-switches*)
  (defvar *last-optimization-bit* #.*last-optimization-bit*)
  (defvar *optimization-bits* #.*optimization-bits*))

(defun take-optimization-bit (name)
  (or (gethash name *optimization-bits*)
      (setf (gethash name *optimization-bits*)
            (incf *last-optimization-bit*))))

(defun optimization-quality-switches (type index)
  (nth index (gethash type *optimization-quality-switches*)))

(defun compute-policy (arguments old-bits)
  (let* ((bits old-bits)
         (on 0)
         (off 0))
    (dolist (x arguments)
      (let (flags name value)
        (cond ((symbolp x)
               (setq name x
                     value 3
                     flags (optimization-quality-switches name value)))
              ((or (not (consp x))
                   (not (consp (cdr x)))
                   (not (numberp (second x)))
                   (not (<= 0 (second x) 3))))
              (t
               (setf name (first x)
                     value (second x)
                     flags (optimization-quality-switches name value))))
        (if (null flags)
            (cmpwarn "Illegal or unknown OPTIMIZE proclamation ~s." x)
            (setf on (logior on (car flags))
                  off (logior off (cdr flags))))))
    ;;(format t "~%*~64b" bits)
    ;;(format t "~% ~64b" on)
    ;;(format t "~% ~64b" off)
    (logandc2 (logior bits on) off)))

;;; for example        debug   2     :on    #x10
(defun augment-policy (quality level on-off flag)
  (loop for i from 0 to 3
        for bits = (optimization-quality-switches quality i)
        do (if (< i level)
               (case on-off
                 (:on (rplacd bits (logior (cdr bits) flag)))
                 (:off (rplaca bits (logior (car bits) flag))))
               (case on-off
                 ((:only-on :on) (rplaca bits (logior (car bits) flag)))
                 ((:only-off :off) (rplacd bits (logior (cdr bits) flag)))))))

(defun policy-function-name (base)
  (intern (concatenate 'string "POLICY-" (symbol-name base))
          (find-package "C")))

(defmacro define-policy (&whole whole name &rest conditions)
  (let* ((test (ash 1 (take-optimization-bit name)))
         (function-name (policy-function-name name))
         (doc (find-if #'stringp conditions))
         (emit-function t))
    ;; If it is an alias, just copy the bits
    ;; Register as an optimization quality with its own flags
    (let* ((circular-list (list (cons test 0)))
           (flags-list (list* (cons 0 test) circular-list)))
      (rplacd circular-list circular-list)
      (setf (gethash name *optimization-quality-switches*) flags-list))
    ;; Scan the definition and correct the flags
    (loop with extra = '()
          with conditions = (remove doc conditions)
          for case = (pop conditions)
          while case
          do (case case
               (:no-function
                (setf emit-function nil))
               (:alias
                (let* ((alias (first conditions)))
                  (setf (gethash name  *optimization-quality-switches*)
                        (gethash alias *optimization-quality-switches*))
                  (return `(defun ,function-name (&optional (env *cmp-env*))
                             ,@(and doc (list doc))
                             (,(policy-function-name alias) env)))))
               (:anti-alias
                (let* ((alias (first conditions))
                       (bits (gethash alias *optimization-quality-switches*)))

                  (setf bits (list (second bits)
                                   (first bits)))
                  (rplacd (cdr bits) (cdr bits))
                  (setf (gethash name *optimization-quality-switches*) bits)
                  (return `(defun ,function-name (&optional (env *cmp-env*))
                             ,@(and doc (list doc))
                             (not (,(policy-function-name alias) env))))))
               ((:only-on :on)
                (augment-policy (pop conditions) (pop conditions) case test))
               ((:only-off :off)
                (augment-policy (pop conditions) (pop conditions) case test))
               (:requires
                (push (pop conditions) extra))
               (otherwise
                (error "Syntax error in macro~%  ~A"
                       `(define-policy ,@whole))))
          finally
             (return
               (and emit-function
                    `(defun ,function-name (&optional (env *cmp-env*))
                       ,@(and doc (list doc))
                       (let ((bits (cmp-env-policy env)))
                         (and (logtest bits ,test)
                              ,@extra))))))))

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
