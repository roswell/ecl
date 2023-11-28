;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

(in-package #:compiler)

;;; Functions that use MAYBE-SAVE-VALUE should rebind *TEMP*.
(defun maybe-save-value (value &optional (other-forms nil other-forms-flag))
  (declare (si::c-local))
  (let ((name (c1form-name value)))
    (cond ((eq name 'LOCATION)
           (c1form-arg 0 value))
          ((and (eq name 'VARIABLE)
                other-forms-flag
                (not (var-changed-in-form-list (c1form-arg 0 value) other-forms)))
           (c1form-arg 0 value))
          (t
           (let* ((temp (make-temp-var))
                  (*destination* temp))
             (c2expr* value)
             temp)))))

;;; FIXME functions declared as SI::C-LOCAL can't be called from the stack
;;; because they are not installed in the environment. That means that if we
;;; have such function and call it with too many arguments it will be
;;; "undefined". This is a defect (but not a regression). -- jd 2023-06-16

;;;
;;; c2fcall:
;;;
;;;   FUN the function to be called
;;;   ARGS is the list of arguments
;;;   FUN-VAL depends on the particular call type
;;;       :LOCAL   structure FUN [see cmprefs.lsp]
;;;       :GLOBAL  function name [symbol or (SETF symbol)]
;;;       :UNKNOWN the value NIL
;;;   CALL-TYPE is (member :LOCAL :GLOBAL :UKNOWN)
;;;
(defun c2fcall (c1form fun args fun-val call-type)
  (if (> (length args) si:c-arguments-limit)
      (c2call-stack c1form fun args nil)
      (ecase call-type
        (:local (c2call-local c1form fun-val args))
        (:global (c2call-global c1form fun-val args))
        (:unknown (c2call-unknown c1form fun args)))))

(defun c2mcall (c1form form args fun-val call-type)
  (c2call-stack c1form form args t))

;;;
;;; c2call-global:
;;;
;;;   ARGS is the list of arguments
;;;   LOC is either NIL or the location of the function object
;;;
(defun c2call-global (c1form fname args)
  (let ((fun (find fname *global-funs* :key #'fun-name :test #'same-fname-p)))
    (when (and fun (c2try-tail-recursive-call fun args))
      (return-from c2call-global))
    (let* ((*inline-blocks* 0)
           (*temp* *temp*))
      (unwind-exit (call-global-loc fname fun args (c1form-primary-type c1form)
                                    (loc-type *destination*)))
      (close-inline-blocks))))

;;; Tail-recursion optimization for a function F is possible only if
;;;     1. F receives only required parameters, and
;;;     2. no required parameter of F is enclosed in a closure.
;;;
;;; A recursive call (F e1 ... en) may be replaced by a loop only if
;;;     1. F is not declared as NOTINLINE,
;;;     2. n is equal to the number of required parameters of F,
;;;     3. the form is a normal function call (i.e. args are not ARGS-PUSHED),
;;;     4. (F e1 ... en) is not surrounded by a form that causes dynamic
;;;        binding (such as LET, LET*, PROGV),
;;;     5. (F e1 ... en) is not surrounded by a form that that pushes a frame
;;;        onto the frame-stack (such as BLOCK and TAGBODY whose tags are
;;;        enclosed in a closure, and CATCH),

(defun tail-recursion-possible ()
  (dolist (ue *unwind-exit*
              (baboon :format-control "tail-recursion-possible: should never return."))
    (cond ((eq ue *tail-recursion-mark*)
           (return t))
          ((or (eq ue 'BDS-BIND) (eq ue 'FRAME))
           (return nil))
          ((or (consp ue) (labelp ue) (eq ue 'IHS-ENV)))
          (t (baboon :format-control "tail-recursion-possible: unexpected situation.")))))

(defun last-call-p ()
  (eq *exit* 'LEAVE))

(defun c2try-tail-recursive-call (fun args)
  (when (and *tail-recursion-info*
             (eq fun (first *tail-recursion-info*))
             (last-call-p)
             (tail-recursion-possible)
             (inline-possible (fun-name fun))
             (= (length args) (length (rest *tail-recursion-info*))))
    (with-exit-label (*exit*)
      (let ((*destination* 'TRASH))
        ;; We do not provide any C2FORM.
        (c2psetq nil (cdr *tail-recursion-info*) args)))
    (unwind-jump *tail-recursion-mark*)
    (cmpdebug "Tail-recursive call of ~s was replaced by iteration." (fun-name fun))
    t))

(defun c2call-local (c1form fun args)
  (declare (type fun fun))
  (unless (c2try-tail-recursive-call fun args)
    (let ((*inline-blocks* 0)
          (*temp* *temp*))
      (unwind-exit (call-loc (fun-name fun) fun (inline-args args)
                             (c1form-primary-type c1form)))
      (close-inline-blocks))))

(defun c2call-unknown (c1form form args)
  (declare (ignore c1form))
  (let* ((*inline-blocks* 0)
         (*temp* *temp*)
         (form-type (c1form-primary-type form))
         (function-p (and (subtypep form-type 'function)
                          (policy-assume-right-type)))
         (loc (maybe-save-value form args)))
    (unwind-exit (call-unknown-global-loc loc (inline-args args) function-p))
    (close-inline-blocks)))

(defun c2call-stack (c1form form args values-p)
  (declare (ignore c1form))
  (let* ((*temp* *temp*)
         (loc (maybe-save-value form args)))
    (with-stack-frame (frame)
      (let ((*destination* (if values-p 'VALUEZ 'LEAVE)))
        (dolist (arg args)
          (c2expr* arg)
          (if values-p
              (wt-nl "ecl_stack_frame_push_values(" frame ");")
              (wt-nl "ecl_stack_frame_push(" frame ",value0);"))))
      (unwind-exit (call-stack-loc nil loc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALL LOCATIONS
;;;
;;;
;;; call-global:
;;;   FNAME: the name of the function
;;;   LOC: either a function object or NIL
;;;   ARGS: a list of typed locs with arguments
;;;   RETURN-TYPE: the type to which the output is coerced
;;;
(defun call-global-loc (fname fun args return-type expected-type)
  ;; Check whether it is a global function that we cannot call directly.
  (when (and (or (null fun) (fun-global fun)) (not (inline-possible fname)))
    (return-from call-global-loc
      (call-unknown-global-fun fname (inline-args args))))

  (setf args (inline-args args))

  ;; Try with a function that has a C-INLINE expansion
  (let ((inline-loc (apply-inliner fname
                                   (type-and return-type expected-type)
                                   args)))
    (when inline-loc
      (return-from call-global-loc inline-loc)))

  ;; Call to a function defined in the same file. Direct calls are
  ;; only emitted for low or neutral values of DEBUG is >= 2.
  (when (and (policy-use-direct-C-call)
             (or (fun-p fun)
                 (and (null fun)
                      (setf fun (find fname *global-funs* :test #'same-fname-p
                                                          :key #'fun-name)))))
    (return-from call-global-loc (call-loc fname fun args return-type)))

  ;; Call to a global (SETF ...) function
  (when (not (symbolp fname))
    (return-from call-global-loc
      (call-unknown-global-fun fname args)))

  ;; Call to a function whose C language function name is known,
  ;; either because it has been proclaimed so, or because it belongs
  ;; to the runtime.
  (multiple-value-bind (found fd minarg maxarg)
      (si:mangle-name fname t)
    (when found
      (return-from call-global-loc
        (call-exported-function-loc fname args fd minarg maxarg t return-type))))

  (when (policy-use-direct-C-call)
    (ext:when-let ((fd (si:get-sysprop fname 'Lfun)))
      (multiple-value-bind (minarg maxarg found) (get-proclaimed-narg fname)
        (unless found
          ;; Without knowing the number of arguments we cannot call the C
          ;; function. When compiling ECL itself, we get this information
          ;; through si::mangle-name from symbols_list.h for core functions
          ;; defined in Lisp code.
          #+ecl-min
          (let (ignored)
            (multiple-value-setq (found ignored minarg maxarg)
              (si:mangle-name fname)))
          (unless found
            (cmperr "Can not call the function ~A using its exported C name ~A because its function type has not been proclaimed."
                    fname fd)))
        (return-from call-global-loc
          (call-exported-function-loc fname args fd minarg maxarg
                                      (si:mangle-name fname) return-type)))))

  (call-unknown-global-fun fname args))

(defun call-loc (fname fun args type)
  (declare (ignore fname))
  `(CALL-NORMAL ,fun ,(coerce-locs args) ,type))

(defun call-exported-function-loc (fname args fun-c-name minarg maxarg in-core
                                   return-type)
  (unless in-core
    ;; We only write declarations for functions which are not in lisp_external.h
    (multiple-value-bind (val declared)
        (gethash fun-c-name *compiler-declared-globals*)
      (declare (ignore val))
      (unless declared
        (if (= maxarg minarg)
            (progn
              (wt-nl-h "extern cl_object " fun-c-name "(")
              (dotimes (i maxarg)
                (when (> i 0) (wt-h1 ","))
                (wt-h1 "cl_object"))
              (wt-h1 ");"))
            (progn
              (wt-nl-h "extern cl_object " fun-c-name "(cl_narg")
              (dotimes (i (min minarg si:c-arguments-limit))
                (wt-h1 ",cl_object"))
              (wt-h1 ",...);")))
        (setf (gethash fun-c-name *compiler-declared-globals*) 1))))
  (let ((fun (make-fun :name fname :global t :cfun fun-c-name :lambda 'NIL
                       :minarg minarg :maxarg maxarg)))
    (call-loc fname fun args return-type)))

;;;
;;; call-unknown-global-loc
;;;   LOC is NIL or location containing function
;;;   ARGS is the list of typed locations for arguments
;;;
(defun call-unknown-global-loc (loc args &optional function-p)
  `(CALL-INDIRECT ,loc ,(coerce-locs args) nil ,function-p))

;;;
;;; call-unknown-global-fun
;;;   FNAME is the name of the global function
;;;   ARGS is the list of typed locations for arguments
;;;
(defun call-unknown-global-fun (fname args)
  `(CALL-INDIRECT (FDEFINITION ,fname) ,(coerce-locs args) ,fname t))

#+ (or)
;;; This version is correct but unnecessarily slow - it goes through
;;; ecl_function_dispatch. wt-fdefinition handles all proper names.
(defun call-unknown-global-fun (fname args)
  `(CALL-INDIRECT ,(get-object fname) ,(coerce-locs args) ,fname nil))

;;;
;;; call-stack-loc
;;;   LOC is NIL or location containing function
;;;   ARGS is the list of typed locations for arguments
;;;
(defun call-stack-loc (fname loc)
  `(CALL-STACK ,loc ,fname))
