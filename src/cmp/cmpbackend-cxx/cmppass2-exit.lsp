;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2023, Daniel Kochmański.
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;; CMPEXIT  Exit manager.
;;;;
;;;; The exit manager has two main operators that unwind the dynamic context:
;;;;
;;;;     (UNWIND-EXIT value) carries VALUE to *DESTINATION* and unwinds to *EXIT*
;;;;     (UNWIND-JUMP label) unwinds to LABEL
;;;;     (UNWIND-COND label) unwinds to LABEL (conditionally)
;;;;     (UNWIND-FLEE label) escapes to LABEL (runtime unwind)
;;;;

(in-package "COMPILER")

(defun unwind-exit (loc)
  (flet ((unwind-cond-p ()
           (and (consp *destination*)
                (member (si:cons-car *destination*) '(JUMP-FALSE JUMP-TRUE))))
         (unwind-jump-p ()
           (labelp *exit*))
         (unwind-exit-p ()
           (eq *exit* 'LEAVE)))
    (cond ((unwind-cond-p) (unwind-cjump loc))
          ((unwind-jump-p) (unwind-label loc))
          ((unwind-exit-p) (unwind-leave loc))
          (t (baboon-exit-invalid *exit*)))))

(defun unwind-jump (exit)
  (push-instruction :unwind (label-denv exit) *unwind-exit*)
  (push-instruction :goto exit)
  (bir-return *bir* (exit-iblock exit)))

(defun unwind-cont (exit)
  (push-instruction :unwind (label-denv exit) *unwind-exit*)
  (push-instruction :goto exit)
  (bir-insert *bir* (exit-iblock exit)))

(defun unwind-flee (exit kind)
  (push-instruction :escape exit kind)
  (bir-escape *bir* (exit-iblock exit)))

(defun unwind-cond (exit kind &rest args)
  (push-instruction :branch exit *unwind-exit* kind args)
  (bir-branch *bir* (exit-iblock exit))
  (bir-insert *bir* (make-iblock :cont)))

;;;

(defun baboon-exit-not-found (exit)
  (baboon :format-control "The value of exit~%~A~%is not found in *UNWIND-EXIT*~%~A"
          :format-arguments (list exit *unwind-exit*)))

(defun baboon-exit-invalid (exit)
  (baboon :format-control "The value of exit~%~A~%is not valid."
          :format-arguments (list exit)))

(defun baboon-unwind-invalid (unwind-to unwind-from)
  (baboon :format-control "The unwind value~%~A~%is not a tail of the unwind value~%~A"
          :format-arguments (list unwind-to unwind-from)))

(defun baboon-unwind-exit (exit)
  (baboon :format-control "The value of exit~%~A~%found in *UNWIND-EXIT*~%~A~%is not valid."
          :format-arguments (list exit *unwind-exit*)))

(defun destination-value-matters-p (dest)
  (declare (si::c-local))
  (if (atom dest)
      (not (eq dest 'TRASH))
      (not (member (car dest) '(JUMP-FALSE JUMP-TRUE)))))

;;; UNWIND-EXIT TAGS       PURPOSE
;;;
;;; FRAME               -> ecl_frs_push()
;;; (STACK frame)       -> ecl_stack_frame_open(env, frame, initial_size)
;;; IHS                 -> ihs push
;;; IHS-ENV             -> ihs push
;;; BDS-BIND            -> binding of 1 special variable
;;; (LCL n)             -> binding stack pointer to Nth local variable
;;; LEAVE               -> outermost location
;;; #<label id used-p>  -> label (basic block leader)

(defun compute-unwind (unwind-to unwind-from)
  ;; (declare (si::c-local))
  (unless (tailp unwind-to unwind-from)
    (baboon-unwind-invalid unwind-to unwind-from))
  (loop with bds-lcl = nil
        with bds-bind = 0
        with stack-frame = nil
        with ihs-p = nil
        with frs-bind = 0
        with jump-p = nil
        with exit-p = nil
        for unwind-exit on unwind-from
        for ue = (car unwind-exit)
        until (eq unwind-exit unwind-to)
        do (cond
             ((consp ue)
              (case (first ue)
                (STACK (setq stack-frame (second ue)))
                (LCL (setq bds-lcl ue bds-bind 0))
                (otherwise (baboon-unwind-exit ue))))
             ((labelp ue)
              (setf jump-p t))
             ((eq ue 'BDS-BIND)
              (incf bds-bind))
             ((eq ue 'FRAME)
              (incf frs-bind))
             ((eq ue 'IHS)
              (setf ihs-p ue))
             ((eq ue 'IHS-ENV)
              (setf ihs-p (or ihs-p ue)))
             ((eq ue 'LEAVE)
              (setf exit-p t))
             (t (baboon-unwind-exit ue)))
        finally (return (values frs-bind bds-lcl bds-bind stack-frame ihs-p jump-p exit-p))))

(defun unwind-leave (loc)
  (declare (si::c-local))
  ;; *destination* must be either LEAVE or TRASH.
  (unless (member loc '(VALUEZ LEAVE))
    (push-instruction :move 'LEAVE loc)
    (setf loc 'LEAVE))
  (push-instruction :unwind nil *unwind-exit*)
  (push-instruction :exit loc)
  (bir-return *bir* (bir-leave *bir*)))

(defun unwind-label (loc)
  (declare (si::c-local))
  (let* ((exit *exit*)
         (dest *destination*)
         (from *unwind-exit*)
         (exit-denv (member exit from :test #'eq)))
    (unless exit-denv
      (baboon-exit-not-found exit))
    (if (and (destination-value-matters-p dest)
             (loc-refers-to-special-p dest))
        ;; Save the value if destination may possibly refer to a special
        ;; binding. Otherwise we set the destination /before/ the unwind.
        (let* ((*temp* *temp*)
               (temp (make-temp-var)))
          (push-instruction :move temp loc)
          (push-instruction :unwind exit-denv from)
          (push-instruction :move dest temp))
        (progn
          (push-instruction :move dest loc)
          (push-instruction :unwind exit-denv from)))
    (push-instruction :jump exit from)
    (bir-insert *bir* (exit-iblock exit))))

;;; Conditional JUMP based on the value of *DESTINATION*. This allows FMLA to
;;; jump over *EXIT* to skip the dead part of the computation. -- jd 2023-11-16
(defun unwind-cjump (loc)
  (declare (si::c-local))
  (multiple-value-bind (constantp value) (loc-immediate-value-p loc)
    (destructuring-bind (target label) *destination*
      (ecase target
        (JUMP-TRUE
         (cond ((not constantp)
                (unwind-cond label :jump-t loc))
               ((not (null value))
                (unwind-cont label)))
         (unless (and constantp (not (null value)))
           (let ((*destination* 'TRASH))
             (if (labelp *exit*)
                 (unwind-label *vv-nil*)
                 (unwind-leave *vv-nil*)))))
        (JUMP-FALSE
         (cond ((not constantp)
                (unwind-cond label :jump-f loc))
               ((null value)
                (unwind-cont label)))
         (unless (and constantp (null value))
           (let ((*destination* 'TRASH))
             (if (labelp *exit*)
                 (unwind-label *vv-t*)
                 (unwind-leave *vv-t*)))))))))

