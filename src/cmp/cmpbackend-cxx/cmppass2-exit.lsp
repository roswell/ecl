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
  (multiple-value-bind (frs-bind bds-lcl bds-bind stack-frame ihs-p)
      (compute-unwind (label-denv exit))
    (perform-unwind frs-bind bds-lcl bds-bind stack-frame ihs-p)
    (wt-nl-go exit)))

;;; A conditional jump that is meant to be used as the IF statement body.
;;; FIXME we want UNWIND-JEQL and UNWIND-JNOT and open-code the test too.
(defun unwind-cond (exit)
  (multiple-value-bind (frs-bind bds-lcl bds-bind stack-frame ihs-p)
      (compute-unwind (label-denv exit))
    (with-lexical-scope ()
      (perform-unwind frs-bind bds-lcl bds-bind stack-frame ihs-p)
      (wt-nl-go exit))))

(defun unwind-flee (exit kind)
  ;; All these boil down to calling ecl_unwind which unwinds stacks dynamically.
  ;; If we want to implement call/cc, then this is the place where we dispatch.
  #+ (or) (wt-nl "ecl_unwind(cl_env_copy," frs-id ");")
  (ecase kind
    (:go
     ;; The second argument is passed as a value (index for jump).
     (wt-nl "cl_go(" (tag-var exit) ",ecl_make_fixnum(" (tag-index exit) "));"))
    (:throw
     ;; Unlike GO and RETURN-FROM, the destination is not known at compile time.
     ;; TODO in some cases it is possible to prove the destination CATCH form.
     (wt-nl "cl_throw(" exit ");"))
    (:return-from
     ;; The second argument is used only to signal the error.
     (wt-nl "cl_return_from(" (blk-var exit) "," (get-object (blk-name exit)) ");"))))

;;;

(defun baboon-exit-not-found (exit)
  (baboon :format-control "The value of exit~%~A~%is not found in *UNWIND-EXIT*~%~A"
          :format-arguments (list exit *unwind-exit*)))

(defun baboon-exit-invalid (exit)
  (baboon :format-control "The value of exit~%~A~%is not valid."
          :format-arguments (list exit)))

(defun baboon-unwind-invalid (unwind-exit)
  (baboon :format-control "The value~%~A~%is not a tail of *UNWIND-EXIT*~%~A"
          :format-arguments (list unwind-exit *unwind-exit*)))

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

(defun perform-unwind (frs-bind bds-lcl bds-bind stack-frame ihs-p)
  (declare (si::c-local)
           (fixnum frs-bind bds-bind))
  (when (plusp frs-bind)
    (wt-nl "ecl_frs_pop_n(cl_env_copy, " frs-bind ");"))
  (when stack-frame
    (wt-nl "ecl_stack_frame_close(" stack-frame ");"))
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

(defun compute-unwind (last-cons)
  (declare (si::c-local))
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

(defun unwind-leave (loc)
  (declare (si::c-local))
  (multiple-value-bind (frs-bind bds-lcl bds-bind stack-frame ihs-p)
      (compute-unwind nil)
    (declare (fixnum frs-bind bds-bind))
    ;; *destination* must be either LEAVE or TRASH.
    (cond ((eq loc 'VALUEZ)
           ;; from multiple-value-prog1 or values
           (perform-unwind frs-bind bds-lcl bds-bind stack-frame ihs-p)
           (wt-nl "return cl_env_copy->values[0];"))
          ((eq loc 'LEAVE)
           ;; from multiple-value-prog1 or values
           (perform-unwind frs-bind bds-lcl bds-bind stack-frame ihs-p)
           (wt-nl "return value0;"))
          (t
           (set-loc 'LEAVE loc)
           (perform-unwind frs-bind bds-lcl bds-bind stack-frame ihs-p)
           (wt-nl "return value0;")))))

(defun unwind-label (loc)
  (declare (si::c-local))
  (multiple-value-bind (frs-bind bds-lcl bds-bind stack-frame ihs-p jump-p exit-p)
      (compute-unwind (or (member *exit* *unwind-exit* :test #'eq)
                          (baboon-exit-not-found *exit*)))
    (declare (fixnum frs-bind bds-bind))
    ;; This operator does not cross the function boundary.
    (assert (null exit-p))
    (cond ((and (destination-value-matters-p *destination*)
                (loc-refers-to-special-p *destination*))
           ;; Save the value if *DESTINATION* may possibly refer to special
           ;; binding. Otherwise we may set *DESTINATION* /before/ the unwind.
           (let* ((*temp* *temp*)
                  (temp (make-temp-var)))
             (set-loc temp loc)
             (perform-unwind frs-bind bds-lcl bds-bind stack-frame ihs-p)
             (set-loc *destination* temp)))
          (t
           (set-loc *destination* loc)
           (perform-unwind frs-bind bds-lcl bds-bind stack-frame ihs-p)))
    ;; When JUMP-P is NULL then we "fall through" onto the exit block.
    (when jump-p
      (wt-nl-go *exit*))))

;;; Conditional JUMP based on the value of *DESTINATION*. This allows FMLA to
;;; jump over *EXIT* to skip the dead part of the computation. -- jd 2023-11-16
(defun unwind-cjump (loc)
  (declare (si::c-local))
  (multiple-value-bind (constantp value) (loc-immediate-value-p loc)
    (destructuring-bind (target label) *destination*
      (ecase target
        (JUMP-TRUE
         (cond ((not constantp)
                (case (loc-representation-type loc)
                  (:bool     (wt-nl "if (" loc ")"))
                  (:object   (wt-nl "if (" loc "!=ECL_NIL)"))
                  (otherwise (wt-nl "if ((") (wt-coerce-loc :object loc) (wt ")!=ECL_NIL)")))
                (unwind-cond label))
               ((not (null value))
                (unwind-jump label)))
         (unless (and constantp (not (null value)))
           (let ((*destination* 'TRASH))
             (if (labelp *exit*)
                 (unwind-label *vv-nil*)
                 (unwind-leave *vv-nil*)))))
        (JUMP-FALSE
         (cond ((not constantp)
                (case (loc-representation-type loc)
                  (:bool     (wt-nl "if (!(" loc "))"))
                  (:object   (wt-nl "if (Null(" loc "))"))
                  (otherwise (wt-nl "if (Null(") (wt-coerce-loc :object loc) (wt "))")))
                (unwind-cond label))
               ((null value)
                (unwind-jump label)))
         (unless (and constantp (null value))
           (let ((*destination* 'TRASH))
             (if (labelp *exit*)
                 (unwind-label *vv-t*)
                 (unwind-leave *vv-t*)))))))))
