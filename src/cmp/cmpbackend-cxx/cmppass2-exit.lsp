;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPEXIT  Exit manager.

(in-package "COMPILER")

(defun baboon-exit-not-found (exit)
  (baboon :format-control "The value of exit~%~A~%is not found in *UNWIND-EXIT*~%~A"
          :format-arguments (list exit *unwind-exit*)))

(defun baboon-unwind-invalid (unwind-exit)
  (baboon :format-control "The value~%~A~%is not a tail of *UNWIND-EXIT*~%~A"
          :format-arguments (list unwind-exit *unwind-exit*)))

(defun baboon-unwind-exit (exit)
  (baboon :format-control "The value of exit~%~A~%found in *UNWIND-EXIT*~%~A~%is not valid."
          :format-arguments (list exit *unwind-exit*)))

;;; UNWIND-EXIT TAGS       PURPOSE
;;;
;;; FRAME               -> ecl_frs_push()
;;; IHS                 -> ihs push
;;; IHS-ENV             -> ihs push
;;; BDS-BIND            -> binding of 1 special variable
;;; #<label id used-p>  -> label (basic block leader)
;;; (LCL n)             -> n local variables
;;; (STACK n)           -> n elements pushed in stack
;;; LEAVE               -> outermost location

(defun unwind-stacks (frs-bind bds-lcl bds-bind stack-frame ihs-p)
  (declare (fixnum frs-bind bds-bind))
  (when (plusp frs-bind)
    (wt-nl "ecl_frs_pop_n(cl_env_copy, " frs-bind ");"))
  (when stack-frame
    (if (stringp stack-frame)
        (wt-nl "ecl_stack_frame_close(" stack-frame ");")
        (wt-nl "ECL_STACK_SET_INDEX(cl_env_copy," stack-frame ");")))
  (when bds-lcl
    (wt-nl "ecl_bds_unwind(cl_env_copy," bds-lcl ");"))
  (if (< bds-bind 4)
      (dotimes (n bds-bind)
        (declare (ignorable n))
        (wt-nl "ecl_bds_unwind1(cl_env_copy);"))
      (wt-nl "ecl_bds_unwind_n(cl_env_copy," bds-bind ");"))
  (case ihs-p
    (IHS     (wt-nl "ecl_ihs_pop(cl_env_copy);"))
    (IHS-ENV (wt-nl "ihs.lex_env = _ecl_debug_env;"))))

(defun unwind-delta (last-cons)
  (loop with bds-lcl = nil
        with bds-bind = 0
        with stack-frame = nil
        with ihs-p = nil
        with frs-bind = 0
        with jump-p = nil
        with exit-p = nil
        for unwind-exit on *unwind-exit*
        for ue = (car unwind-exit)
        until (eq unwind-exit last-cons)
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

(defun unwind-delta* (exit)
  (unwind-delta (or (member exit *unwind-exit* :test #'eq)
                    (baboon-exit-not-found exit))))

(defun unwind-exit (loc)
  (when (consp *destination*)
    (case (car *destination*)
      (JUMP-TRUE
       (set-jump-true loc (second *destination*))
       (when (eq loc *vv-t*)
         (return-from unwind-exit)))
      (JUMP-FALSE
       (set-jump-false loc (second *destination*))
       (when (eq loc *vv-nil*)
         (return-from unwind-exit)))))
  (multiple-value-bind (frs-bind bds-lcl bds-bind stack-frame ihs-p jump-p exit-p)
      (unwind-delta* *exit*)
    (declare (fixnum frs-bind bds-bind))
    (assert (null exit-p)) ; this operator does not cross the function boundary.
    (when (eq *exit* 'LEAVE)
      ;; *destination* must be either LEAVE or TRASH.
      (cond ((eq loc 'VALUEZ)
             ;; from multiple-value-prog1 or values
             (unwind-stacks frs-bind bds-lcl bds-bind stack-frame ihs-p)
             (wt-nl "return cl_env_copy->values[0];"))
            ((eq loc 'LEAVE)
             ;; from multiple-value-prog1 or values
             (unwind-stacks frs-bind bds-lcl bds-bind stack-frame ihs-p)
             (wt-nl "return value0;"))
            (t
             (let* ((*destination* 'LEAVE))
               (set-loc loc))
             (unwind-stacks frs-bind bds-lcl bds-bind stack-frame ihs-p)
             (wt-nl "return value0;")))
      (return-from unwind-exit))
    ;; All body forms except the last (returning) are dealt here
    (cond ((and (consp *destination*)
                (or (eq (car *destination*) 'JUMP-TRUE)
                    (eq (car *destination*) 'JUMP-FALSE)))
           (unwind-stacks frs-bind bds-lcl bds-bind stack-frame ihs-p))
          ((not (or (plusp frs-bind) bds-lcl (plusp bds-bind) stack-frame))
           (set-loc loc))
          ;; Save the value if LOC may possibly refer to special binding.
          ((or (loc-refers-to-special-p loc)
               (loc-refers-to-special-p *destination*))
           (let* ((*temp* *temp*)
                  (temp (make-temp-var)))
             (let ((*destination* temp))
               (set-loc loc)) ; temp <- loc
             (unwind-stacks frs-bind bds-lcl bds-bind stack-frame ihs-p)
             (set-loc temp))) ; *destination* <- temp
          (t
           (set-loc loc)
           (unwind-stacks frs-bind bds-lcl bds-bind stack-frame ihs-p)))
    ;; When JUMP-P is NULL then we "fall through" onto the exit block.
    (when jump-p
      (wt-nl-go *exit*))))

(defun unwind-jump (exit)
  (multiple-value-bind (frs-bind bds-lcl bds-bind stack-frame ihs-p)
      (unwind-delta (label-denv exit))
    (unwind-stacks frs-bind bds-lcl bds-bind stack-frame ihs-p)
    (wt-nl-go exit)))
