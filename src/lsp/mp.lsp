;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  MP.LSP  -- Multiprocessing capabilities.

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

#-threads
(defpackage "MP"
  (:use "CL" "SI")
  (:export "WITH-LOCK"))

(in-package "MP")

;;;
;;; Interrupt disabling and enabling
;;;
(defmacro without-interrupts (&body body)
  #!+sb-doc
  "Executes BODY with all deferrable interrupts disabled. Deferrable
interrupts arriving during execution of the BODY take effect after BODY has
been executed.

Deferrable interrupts include most blockable POSIX signals, and
MP:INTERRUPT-THREAD. Does not interfere with garbage collection, and
unlike in many traditional Lisps using userspace threads, in ECL
WITHOUT-INTERRUPTS does not inhibit scheduling of other threads.

Binds ALLOW-WITH-INTERRUPTS, WITH-LOCAL-INTERRUPTS and WITH-RESTORED-INTERRUPTS
as a local macros.

WITH-RESTORED-INTERRUPTS executes the body with interrupts enabled if and only
if the WITHOUT-INTERRUPTS was in an environment in which interrupts were allowed.

ALLOW-WITH-INTERRUPTS allows the WITH-INTERRUPTS to take effect during the
dynamic scope of its body, unless there is an outer WITHOUT-INTERRUPTS without
a corresponding ALLOW-WITH-INTERRUPTS.

WITH-LOCAL-INTERRUPTS executes its body with interrupts enabled provided that
there is an ALLOW-WITH-INTERRUPTS for every WITHOUT-INTERRUPTS surrounding
the current one. WITH-LOCAL-INTERRUPTS is equivalent to:

  (allow-with-interrupts (with-interrupts ...))

Care must be taken not to let either ALLOW-WITH-INTERRUPTS or
WITH-LOCAL-INTERRUPTS appear in a function that escapes from inside the
WITHOUT-INTERRUPTS in:

  (without-interrupts
    ;; The body of the lambda would be executed with WITH-INTERRUPTS allowed
    ;; regardless of the interrupt policy in effect when it is called.
    (lambda () (allow-with-interrupts ...)))

  (without-interrupts
    ;; The body of the lambda would be executed with interrupts enabled
    ;; regardless of the interrupt policy in effect when it is called.
    (lambda () (with-local-interrupts ...)))
"
  (ext:with-unique-names (outer-allow-with-interrupts outer-interrupts-enabled)
    `(multiple-value-prog1
         (macrolet ((allow-with-interrupts (&body allow-forms)
                      `(let ((si:*allow-with-interrupts* ,',outer-allow-with-interrupts))
                         ,@allow-forms))
                    (with-restored-interrupts (&body with-forms)
                      `(let ((si:*interrupts-enabled* ,',outer-interrupts-enabled))
                         ,@with-forms))
                    (with-local-interrupts (&body with-forms)
                      `(let* ((si:*allow-with-interrupts* ,',outer-allow-with-interrupts)
                              (si:*interrupts-enabled* ,',outer-allow-with-interrupts))
                         (when ,',outer-allow-with-interrupts
                           (si::check-pending-interrupts))
                         (locally ,@with-forms))))
           (let* ((,outer-interrupts-enabled si:*interrupts-enabled*)
                  (si:*interrupts-enabled* nil)
                  (,outer-allow-with-interrupts si:*allow-with-interrupts*)
                  (si:*allow-with-interrupts* nil))
             (declare (ignorable ,outer-allow-with-interrupts
                                 ,outer-interrupts-enabled))
             ,@body))
       (when si:*interrupts-enabled*
         (si::check-pending-interrupts)))))

(defmacro with-interrupts (&body body)
  "Executes BODY with deferrable interrupts conditionally enabled. If there
are pending interrupts they take effect prior to executing BODY.

As interrupts are normally allowed WITH-INTERRUPTS only makes sense if there
is an outer WITHOUT-INTERRUPTS with a corresponding ALLOW-WITH-INTERRUPTS:
interrupts are not enabled if any outer WITHOUT-INTERRUPTS is not accompanied
by ALLOW-WITH-INTERRUPTS."
  (ext:with-unique-names (allowp enablep)
    ;; We could manage without ENABLEP here, but that would require
    ;; taking extra care not to ever have *ALLOW-WITH-INTERRUPTS* NIL
    ;; and *INTERRUPTS-ENABLED* T -- instead of risking future breakage
    ;; we take the tiny hit here.
    `(let* ((,allowp si:*allow-with-interrupts*)
            (,enablep si:*interrupts-enabled*)
            (si:*interrupts-enabled* (or ,enablep ,allowp)))
       (when (and ,allowp (not ,enablep))
         (si::check-pending-interrupts))
       (locally ,@body))))


;;;
;;; Convenience macros for locks
;;;
(defmacro with-lock ((lock-form &rest options) &body body)
  #-threads
  `(progn ,@body)
  ;; Why do we need %count? Even if get-lock succeeeds, an interrupt may
  ;; happen between the end of get-lock and when we save the output of
  ;; the function. That means we lose the information and ignore that
  ;; the lock was actually acquired. Furthermore, a lock can be recursive
  ;; and mp:lock-holder is also not reliable.
  ;;
  ;; Next notice how we need to disable interrupts around the body and
  ;; the get-lock statement, to ensure that the unlocking is done with
  ;; interrupts disabled.
  #+threads
  (ext:with-unique-names (lock owner count process)
    `(let* ((,lock ,lock-form)
            (,owner (mp:lock-owner ,lock))
            (,count (mp:lock-count ,lock)))
       (declare (type fixnum ,count))
       (without-interrupts
           (unwind-protect
                (with-restored-interrupts
                    (mp::get-lock ,lock)
                  (locally ,@body))
             (let ((,process mp:*current-process*))
               (declare (optimize (speed 3) (safety 0) (debug 0)))
               (when (and (eq ,process (mp:lock-owner ,lock))
                          (or (not (eq ,owner ,process))
                              (> (the fixnum (mp:lock-count ,lock))
                                 (the fixnum ,count))))
                 (mp::giveup-lock ,lock))))))))

#+ecl-read-write-lock
(defmacro with-rwlock ((lock op) &body body)
  "Acquire rwlock for the dynamic scope of BODY for operation OP,
which is executed with the lock held by current thread, and
WITH-RWLOCK returns the values of body.

Valid values of argument OP are :READ or :WRITE
\(for reader and writer access accordingly)."
  (assert (member op '(:read :write) :test #'eq))
  (let ((s-lock (gensym)))
    `(let ((,s-lock ,lock))
       (,(if (eq :read op)
             'mp:get-rwlock-read
             'mp:get-rwlock-write)
         ,s-lock t)
       (mp:without-interrupts
           (unwind-protect
                (mp:with-restored-interrupts
                    ,@body)
             (,(if (eq :read op)
                   'mp:giveup-rwlock-read
                   'mp:giveup-rwlock-write)
               ,s-lock))))))

;;;
;;; Atomic operations
;;;
#+threads
(defmacro define-cas-expander (accessor lambda-list &body body)
  "Define a COMPARE-AND-SWAP expander similar to DEFINE-SETF-EXPANDER.
Syntax: (define-cas-expander accessor lambda-list {decl | doc}*
          {form}*)
Defines the COMPARE-AND-SWAP-expander for generalized-variables (ACCESSOR ...).
When a form (compare-and-swap (ACCESSOR arg1 ... argn) old new) is evaluated, the FORMs
given in the DEFINE-CAS-EXPANDER are evaluated in order with the parameters in
LAMBDA-LIST bound to ARG1 ... ARGn.  The last FORM must return six
values
        (var1 ... vark)
        (form1 ... formk)
        old-var
        new-var
        compare-and-swap-form
        volatile-access-form
in order (Note that old-var and new-var are single variables, unlike
in DEFINE-SETF-EXPANDER). The whole COMPARE-AND-SWAP form is then
expanded into
        (let* ((var1 from1) ... (vark formk)
               (old-var old-form)
               (new-var new-form))
          compare-and-swap-form).
Note that it is up to the user of this macro to ensure atomicity for
the resulting COMPARE-AND-SWAP expansions."
  (let ((env (member '&environment lambda-list :test #'eq)))
    (if env
        (setq lambda-list (cons (second env)
                         (nconc (ldiff lambda-list env) (cddr env))))
        (progn
          (setq env (gensym))
          (setq lambda-list (cons env lambda-list))
          (push `(declare (ignore ,env)) body))))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (si:put-sysprop ',accessor 'CAS-EXPANDER #'(ext::lambda-block ,accessor ,lambda-list ,@body))
     ',accessor))

#+threads
(defmacro defcas (accessor cas-fun &optional documentation)
  "Define a COMPARE-AND-SWAP expansion similar to the short form of DEFSETF.
Syntax: (defsetf symbol cas-fun)
Defines an expansion
        (compare-and-swap (SYMBOL arg1 ... argn) old new)
        => (CAS-FUN arg1 ... argn old new)
Note that it is up to the user of this macro to ensure atomicity for
the resulting COMPARE-AND-SWAP expansions."
  (declare (ignore documentation))
  `(define-cas-expander ,accessor (&rest args)
     (let ((old (gensym)) (new (gensym)))
       (values nil nil old new
               `(,',cas-fun ,@args ,old ,new)
               `(,',accessor ,@args)))))

#+threads
(defun get-cas-expansion (place &optional environment &aux f)
  "Args: (form)
Returns the COMPARE-AND-SWAP expansion forms and variables as defined
in DEFINE-CAS-EXPANDER for PLACE as six values."
  (cond ((setq f (si:get-sysprop (first place) 'CAS-EXPANDER))
         (apply f environment (rest place)))
        ;; We try macro expansion with MACROEXPAND-1 as in SETF
        ((and (setq f (macroexpand-1 place environment)) (not (equal f place)))
         (get-cas-expansion f environment))
        ;; Functions like (cas foo) analogous to (setf foo) are
        ;; currently not supported, hence we throw an error here
        (t (error "Cannot get the compare-and-swap expansion of ~S." place))))

(defmacro compare-and-swap (&environment env place old new)
  "Atomically stores NEW in PLACE if OLD is eq to the current value of
PLACE. Returns the previous value of PLACE: if the returned value is
eq to OLD, the swap was carried out.

Currently, the following places are supported:

car, cdr, first, rest, svref, symbol-plist, symbol-value, slot-value,
clos:standard-instance-access, clos:funcallable-standard-instance-access,
a struct accessor defined by defstruct with the :atomic-accessors
option enabled or any other place for which a compare-and-swap
expansion was defined by defcas or define-cas-expander.

For slot-value, slot-unbound is called if the slot is unbound unless
OLD is eq to si:unbound, in which case OLD is returned and NEW is
assigned to the slot. Additionally, the object should have no
applicable methods defined for slot-value-using-class or (setf
slot-value-using-class)."
  #+threads
  (multiple-value-bind (vars vals old-var new-var cas-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar #'list vars vals)
            (,old-var ,old)
            (,new-var ,new))
       ,cas-form))
  #-threads
  (let ((old-value (gensym)))
    `(let ((,old-value ,place))
       (when (eq ,old ,old-value)
         (setf ,place ,new))
       ,old-value)))

#+threads
(progn
  (defcas car mp:compare-and-swap-car)
  (defcas first mp:compare-and-swap-car)
  (defcas cdr mp:compare-and-swap-cdr)
  (defcas rest mp:compare-and-swap-cdr)
  (defcas symbol-value mp:compare-and-swap-symbol-value)
  (defcas symbol-plist mp:compare-and-swap-symbol-plist)
  (defcas svref mp:compare-and-swap-svref)
  (defcas slot-value mp::compare-and-swap-slot-value)
  (defcas clos:standard-instance-access mp::compare-and-swap-standard-instance)
  (defcas clos:funcallable-standard-instance-access mp::compare-and-swap-standard-instance))

(defmacro atomic-update (place update-fn &rest arguments)
  "Atomically updates the CAS-able PLACE to the value returned by calling UPDATE-FN with ARGUMENTS and the old value of PLACE. UPDATE-FN must be a function accepting (1+ (length ARGUMENTS)) arguments. Returns the new value which was stored in PLACE.

PLACE may be read and UPDATE-FN may be called more than once if multiple threads are trying to write to PLACE at the same time."
  #+threads
  (let ((new (gensym))
        (old (gensym)))
    `(let ((,old ,place))
       (loop for ,new = (funcall ,update-fn ,@arguments ,old)
          until (eq ,old (setf ,old (compare-and-swap ,place ,old ,new)))
          finally (return ,new))))
  #-threads
  `(setf ,place (funcall ,update-fn ,@arguments ,place)))

(defmacro atomic-push (obj place)
  "Like PUSH, but atomic. PLACE must be CAS-able and may be read
multiple times before the update succeeds."
  #+threads
  (let ((new (gensym))
        (old (gensym)))
    `(let* ((,old ,place)
            (,new (cons ,obj ,old)))
       (loop until (eq ,old (setf ,old (compare-and-swap ,place ,old ,new)))
          do (setf (cdr ,new) ,old)
          finally (return ,new))))
  #-threads
  `(push ,obj ,place))

(defmacro atomic-pop (place)
  "Like POP, but atomic. PLACE must be CAS-able and may be read
multiple times before the update succeeds."
  #+threads
  (let ((new (gensym))
        (old (gensym)))
    `(let* ((,old ,place))
       (loop for ,new = (cdr ,old)
          until (eq ,old (setf ,old (compare-and-swap ,place ,old ,new)))
          finally (return (car (ext:truly-the list ,old))))))
  #-threads
  `(pop ,place))

(defmacro atomic-incf (place &optional (increment 1))
  "Atomically increments the fixnum stored in PLACE by the given
INCREMENT and returns the value of PLACE before the increment.
Incrementing is done using modular arithmetic, so that atomic-incf of
a place whose value is most-positive-fixnum by 1 results in
most-negative-fixnum stored in place.

Currently the following places are supported:
car, cdr, first, rest, svref, symbol-value, slot-value,
clos:standard-instance-access, clos:funcallable-standard-instance-access.

For slot-value, the object should have no applicable methods defined
for slot-value-using-class or (setf slot-value-using-class).

The consequences are undefined if the value of PLACE is not of type
fixnum."
  #+threads
  (let* ((place (macroexpand place))
         (fun (case (first place)
               ((car first) 'mp:atomic-incf-car)
               ((cdr rest) 'mp:atomic-incf-cdr)
               (symbol-value 'mp:atomic-incf-symbol-value)
               (svref 'mp:atomic-incf-svref)
               (slot-value 'mp::atomic-incf-slot-value)
               ((clos:standard-instance-access clos:funcallable-standard-instance-access) 'mp::atomic-incf-standard-instance)
               (t (error "No ATOMIC-INCF expansion defined for place ~S." place)))))
    `(,fun ,@(rest place) ,increment))
  #-threads
  (let ((value (gensym))
        (incr (gensym)))
    `(let ((,value ,place)
           (,incr ,increment))
       (declare (fixnum ,value ,incr))
       (setf ,place (+ (mod (+ ,value ,incr most-negative-fixnum)
                            (* -2 most-negative-fixnum))
                       most-negative-fixnum))
       ,value)))

(defmacro atomic-decf (place &optional (decrement 1))
  "Atomically decrements the fixnum stored in PLACE by the given
DECREMENT and returns the value of PLACE before the decrement.
Decrementing is done using modular arithmetic, so that atomic-decf of
a place whose value is most-negative-fixnum by 1 results in
most-positive-fixnum stored in place.

Currently the following places are supported:
car, cdr, first, rest, svref, symbol-value, slot-value,
clos:standard-instance-access, clos:funcallable-standard-instance-access.

For slot-value, the object should have no applicable methods defined
for slot-value-using-class or (setf slot-value-using-class).

The consequences are undefined if the value of PLACE is not of type
fixnum."
  `(atomic-incf ,place (- ,decrement)))
