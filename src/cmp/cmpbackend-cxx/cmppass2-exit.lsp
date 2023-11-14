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

;;; UNWIND-EXIT TAGS       PURPOSE
;;;
;;; FRAME               -> ecl_frs_push()
;;; IHS                 -> ihs push
;;; IHS-ENV             -> ihs push
;;; BDS-BIND            -> binding of 1 special variable
;;; #<label id used-p>  -> label (basic block leader)
;;; (LCL n)             -> n local variables
;;; (STACK n)           -> n elements pushed in stack
;;; TAIL-RECURSION-MARK -> TTL: label created
;;; RETURN              -> outermost location (*)
;;;
;;; (*) also RETURN-{FIXNUM,CHARACTER,OBJECT}
;;;          RETURN-{SINGLE-FLOAT,DOUBLE-FLOAT,LONG-FLOAT}
;;;          RETURN-{CSFLOAT,CDFLOAT,CLFLOAT}
;;;
(defun unwind-bds (bds-lcl bds-bind stack-frame ihs-p)
  (declare (fixnum bds-bind))
  (let ((some nil))
    (when stack-frame
      (setf some t)
      (if (stringp stack-frame)
          (wt-nl "ecl_stack_frame_close(" stack-frame ");")
          (wt-nl "ECL_STACK_SET_INDEX(cl_env_copy," stack-frame ");")))
    (when bds-lcl
      (setf some t)
      (wt-nl "ecl_bds_unwind(cl_env_copy," bds-lcl ");"))
    (cond ((< bds-bind 4)
           (dotimes (n bds-bind)
             (declare (fixnum n))
             (setf some t)
             (wt-nl "ecl_bds_unwind1(cl_env_copy);")))
          (t
           (setf some t)
           (wt-nl "ecl_bds_unwind_n(cl_env_copy," bds-bind ");")))
    (case ihs-p
      (IHS
       (setf some t)
       (wt-nl "ecl_ihs_pop(cl_env_copy);"))
      (IHS-ENV
       (setf some t)
       (wt-nl "ihs.lex_env = _ecl_debug_env;")))
    some))

(defun unwind-exit (loc &aux (jump-p nil) (bds-lcl nil) (bds-bind 0) (stack-frame nil) (ihs-p nil))
  (declare (fixnum bds-bind))
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
  (dolist (ue *unwind-exit* (baboon-improper-*exit*))
    ;; perform all unwind-exit's which precede *exit*
    (cond
      ((consp ue)                   ; (STACK n) | (LCL n)
       (cond ((eq (car ue) 'STACK)
              (setf stack-frame (second ue)))
             ((eq (car ue) 'LCL)
              (setq bds-lcl ue bds-bind 0))
             (t (baboon-unwind-exit ue))))
      ((labelp ue)
       (when (eq ue *exit*)
         ;; all body forms except the last (returning) are dealt here
         (cond ((and (consp *destination*)
                     (or (eq (car *destination*) 'JUMP-TRUE)
                         (eq (car *destination*) 'JUMP-FALSE)))
                (unwind-bds bds-lcl bds-bind stack-frame ihs-p))
               ((not (or bds-lcl (plusp bds-bind) stack-frame))
                (set-loc loc))
               ;; Save the value if LOC may possibly refer to special binding.
               ((or (loc-refers-to-special-p loc)
                    (loc-refers-to-special-p *destination*))
                (let* ((*temp* *temp*)
                       (temp (make-temp-var)))
                  (let ((*destination* temp))
                    (set-loc loc)) ; temp <- loc
                  (unwind-bds bds-lcl bds-bind stack-frame ihs-p)
                  (set-loc temp))) ; *destination* <- temp
               (t
                (set-loc loc)
                (unwind-bds bds-lcl bds-bind stack-frame ihs-p)))
         (when jump-p
           (wt-nl)
           (wt-go *exit*))
         (return))
       ;; We are crossing the block boundary.
       (setq jump-p t))
      (t (case ue
           (IHS (setf ihs-p ue))
           (IHS-ENV (setf ihs-p (or ihs-p ue)))
           (BDS-BIND (incf bds-bind))
           (RETURN
             (unless (eq *exit* 'RETURN)
               (baboon-unwind-exit ue))
             ;; *destination* must be either RETURN or TRASH.
             (cond ((eq loc 'VALUES)
                    ;; from multiple-value-prog1 or values
                    (unwind-bds bds-lcl bds-bind stack-frame ihs-p)
                    (wt-nl "return cl_env_copy->values[0];"))
                   ((eq loc 'RETURN)
                    ;; from multiple-value-prog1 or values
                    (unwind-bds bds-lcl bds-bind stack-frame ihs-p)
                    (wt-nl "return value0;"))      
                   (t
                    (let* ((*destination* 'RETURN))
                      (set-loc loc))
                    (unwind-bds bds-lcl bds-bind stack-frame ihs-p)
                    (wt-nl "return value0;")))
             (return))
           ((RETURN-FIXNUM RETURN-CHARACTER RETURN-OBJECT
             RETURN-DOUBLE-FLOAT RETURN-SINGLE-FLOAT RETURN-LONG-FLOAT
             RETURN-CSFLOAT RETURN-CDFLOAT RETURN-CLFLOAT)
            (when (eq *exit* ue)
              ;; *destination* must be RETURN-FIXNUM
              (setq loc (list 'COERCE-LOC
                              (getf '(RETURN-FIXNUM :fixnum
                                      RETURN-CHARACTER :char
                                      RETURN-SINGLE-FLOAT :float
                                      RETURN-DOUBLE-FLOAT :double
                                      RETURN-CSFLOAT :csfloat
                                      RETURN-CDFLOAT :cdfloat
                                      RETURN-CLFLOAT :clfloat
                                      RETURN-OBJECT :object)
                                    ue)
                              loc))
              (if (or bds-lcl (plusp bds-bind))
                  (let ((lcl (make-lcl-var :type (second loc))))
                    (wt-nl-open-brace)
                    (wt-nl "cl_fixnum " lcl "= " loc ";")
                    (unwind-bds bds-lcl bds-bind stack-frame ihs-p)
                    (wt-nl "return(" lcl ");")
                    (wt-nl-close-brace))
                  (progn
                    (wt-nl "return(" loc ");")))
              (return)))
           (FRAME
            (let ((*destination* (tmp-destination *destination*)))
              (set-loc loc)
              (setq loc *destination*))
            (wt-nl "ecl_frs_pop(cl_env_copy);"))
           (TAIL-RECURSION-MARK)
           (t (baboon-unwind-exit ue))))))
  ;;; Never reached
  )

(defun baboon-improper-*exit* ()
  (baboon :format-control "The value of *EXIT*~%~A~%is not found in *UNWIND-EXIT*~%~A"
          :format-arguments (list *exit* *unwind-exit*)))

(defun baboon-unwind-exit (ue)
  (baboon :format-control "The value of unwind exit~%~A~%found in *UNWIND-EXIT*~%~A~%is not valid."
          :format-arguments (list ue *unwind-exit*)))

(defun unwind-no-exit-until (last-cons)
  (loop with bds-lcl = nil
        with bds-bind = 0
        with stack-frame = nil
        with ihs-p = nil
        for unwind-exit on *unwind-exit*
        for ue = (car unwind-exit)
        until (eq unwind-exit last-cons)
        do (cond
             ((consp ue)
              (when (eq (first ue) 'STACK)
                (setf stack-frame (second ue))))
             ((labelp ue))
             ((eq ue 'BDS-BIND)
              (incf bds-bind))
             ((eq ue 'FRAME)
              (wt-nl "ecl_frs_pop(cl_env_copy);"))
             ((eq ue 'IHS-ENV)
              (setf ihs-p ue))
             (t (baboon-unwind-exit ue)))
        finally (return (unwind-bds bds-lcl bds-bind stack-frame ihs-p))))

(defun unwind-no-exit (exit)
  (let ((where (member exit *unwind-exit* :test #'eq)))
    (unless where
      (baboon :format-control "Unwind-exit label ~A not found"
              :format-arguments (list exit)))
    (unwind-no-exit-until where)))
