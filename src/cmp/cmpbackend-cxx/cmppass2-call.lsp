;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

(in-package #:compiler)

;;; FIXME functions declared as SI::C-LOCAL can't be called from the stack
;;; because they are not installed in the environment. That means that if we
;;; have such function and call it with too many arguments it will be
;;; "undefined". This is a defect (but not a regression). -- jd 2023-06-16

;;;
;;; c2fcall:
;;;
;;;   FUN: the function to be called
;;;   ARGS: the list of arguments
;;;   FUN-VAL depends on the particular call type
;;;       :LOCAL   structure FUN [see cmprefs.lsp]
;;;       :GLOBAL  function name [symbol or (SETF symbol)]
;;;       :UNKNOWN the value NIL
;;;   CALL-TYPE: (member :LOCAL :GLOBAL :UKNOWN)
;;;
(defun c2fcall (c1form fun args fun-val call-type)
  (if (> (length args) si:c-arguments-limit)
      (c2call-stack c1form fun args nil)
      (with-inline-blocks ()
        (ecase call-type
          (:local (c2call-local c1form fun-val args))
          (:global (c2call-global c1form fun-val args))
          (:unknown (c2call-unknown c1form fun args))))))

(defun c2mcall (c1form form args fun-val call-type)
  (declare (ignore fun-val call-type))
  (c2call-stack c1form form args t))

;;;
;;; c2call-stack:
;;;
;;;   This is the most generic way of calling functions. First we push them on
;;;   the stack, and then we apply from the stack frame. Other variants call
;;;   inline-args and put results directly in the function call.
;;;
(defun c2call-stack (c1form form args values-p)
  (declare (ignore c1form))
  (with-stack-frame (frame)
    (let ((loc (emit-inline-form form args)))
      (let ((*destination* (if values-p 'VALUEZ 'LEAVE)))
        (dolist (arg args)
          (c2expr* arg)
          (if values-p
              (wt-nl "ecl_stack_frame_push_values(" frame ");")
              (wt-nl "ecl_stack_frame_push(" frame ",value0);"))))
      (unwind-exit (call-stack-loc nil loc)))))

;;;
;;; c2call-global:
;;;
;;;   LOC: the location of the function object or NIL
;;;   ARGS: the list of arguments
;;;
(defun c2call-global (c1form fname args)
  (let ((fun (find fname *global-funs* :key #'fun-name :test #'same-fname-p)))
    (when (and fun (c2try-tail-recursive-call fun args))
      (return-from c2call-global))
    (unwind-exit (call-global-loc fname fun
                                  (inline-args args)
                                  (type-and (c1form-primary-type c1form)
                                            (loc-lisp-type *destination*))))))

;;;
;;; c2call-local:
;;;
;;;   FUN: the function object
;;;   ARGS: the list of arguments
;;;
(defun c2call-local (c1form fun args)
  (declare (type fun fun))
  (unless (c2try-tail-recursive-call fun args)
    (unwind-exit (call-loc (fun-name fun) fun
                           (inline-args args)
                           (c1form-primary-type c1form)))))

(defun c2call-unknown (c1form form args)
  (declare (ignore c1form))
  (let* ((form-type (c1form-primary-type form))
         (function-p (and (subtypep form-type 'function)
                          (policy-assume-right-type)))
         (loc (emit-inline-form form args))
         (args (inline-args args)))
    (unwind-exit (call-unknown-global-loc loc args function-p))))

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

(defun c2try-tail-recursive-call (fun args)
  (when (and *tail-recursion-info*
             (eq fun (first *tail-recursion-info*))
             (eq *exit* 'LEAVE)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALL LOCATIONS
;;;

;;;
;;; call-stack-loc
;;;
;;;   FNAME: the name of the function or NIL
;;;   LOC: the location containing function
;;;
(defun call-stack-loc (fname loc)
  `(CALL-STACK ,loc ,fname))

;;;
;;; call-loc
;;;
;;;   FNAME: the name of the function
;;;   FUN: a function object
;;;   ARGS: a list of INLINED-ARGs
;;;   TYPE: the type to which the output is coerced
;;;
(defun call-loc (fname fun args type)
  (declare (ignore fname))
  `(CALL-NORMAL ,fun ,(coerce-args args) ,type))

;;;
;;; call-global:
;;;   FNAME: the name of the function
;;;   FUN: either a function object or NIL
;;;   ARGS: a list of typed locs with arguments
;;;   TYPE: the type to which the output is coerced
;;;
(defun call-global-loc (fname fun args type)

  ;; Check whether it is a global function that we cannot call directly.
  (when (not (inline-possible fname))
    (return-from call-global-loc
      (call-unknown-global-fun fname args)))

  ;; Try with a function that has a C-INLINE expansion
  (ext:when-let ((inline-loc (apply-inliner fname type args)))
    (return-from call-global-loc inline-loc))

  ;; Call to a function defined in the same file. Direct calls are only emitted
  ;; for low or neutral values of DEBUG, that is DEBUG < 2.
  (when (and fun (policy-use-direct-C-call))
    (return-from call-global-loc
      (call-loc fname fun args type)))

  ;; Call to a global (SETF ...) function
  (when (not (symbolp fname))
    (return-from call-global-loc
      (call-unknown-global-fun fname args)))

  ;; Call to a function whose C language function name is known because it
  ;; belongs to the runtime.
  (multiple-value-bind (found fd minarg maxarg)
      (si:mangle-name fname t)
    (when found
      (return-from call-global-loc
        (call-exported-function-loc fname args fd minarg maxarg t type))))

  ;; Call to a function whose C language function name is known because it has
  ;; been proclaimed so.
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
            (cmperr "Can not call the function ~A using its exported C name ~A ~
                     because its function type has not been proclaimed."
                    fname fd)))
        (return-from call-global-loc
          (call-exported-function-loc fname args fd minarg maxarg
                                      (si:mangle-name fname) type)))))

  (call-unknown-global-fun fname args))

(defun call-exported-function-loc (fname args fun-c-name minarg maxarg in-core type)
  (unless in-core
    ;; We only write declarations for functions which are not in lisp_external.h
    (push-instruction :declare-c-fun fun-c-name minarg maxarg))
  (let ((fun (make-fun :name fname :global t :cfun fun-c-name :lambda 'NIL
                       :minarg minarg :maxarg maxarg)))
    (call-loc fname fun args type)))

;;;
;;; call-unknown-global-loc
;;;
;;;   LOC: the location containing the function or NIL
;;;   ARGS: a list of INLINED-ARGs
;;;   FUNCTION-P: true when we can assume that LOC is the function
;;;
(defun call-unknown-global-loc (loc args function-p)
  `(CALL-INDIRECT ,loc ,(coerce-args args) nil ,function-p))

;;;
;;; call-unknown-global-fun
;;;
;;;   FNAME: the name of the global function
;;;   ARGS: a list of INLINED-ARGs
;;;
(defun call-unknown-global-fun (fname args)
  `(CALL-INDIRECT (FDEFINITION ,fname) ,(coerce-args args) ,fname t))

#+ (or)
;;; This version is correct but unnecessarily slow - it goes through
;;; ecl_function_dispatch. wt-fdefinition handles all proper names.
(defun call-unknown-global-fun (fname args)
  `(CALL-INDIRECT ,(get-object fname) ,(coerce-args args) ,fname nil))
