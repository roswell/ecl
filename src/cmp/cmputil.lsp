;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;; CMPUTIL  --  Miscellaneous Functions.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

#+cmu-format
(progn
  (defconstant +note-format+ "~&~@<  ~;~?~;~:@>")
  (defconstant +warn-format+ "~&~@<  ! ~;~?~;~:@>")
  (defconstant +error-format+ "~&~@<  * ~;~?~;~:@>")
  (defconstant +fatal-format+ "~&~@<  ** ~;~?~;~:@>"))
#-cmu-format
(progn
  (defconstant +note-format+ "~&  ~?")
  (defconstant +warn-format+ "~&  ! ~?")
  (defconstant +error-format+ "~&  * ~?")
  (defconstant +fatal-format+ "~&  ** ~?"))

;; Return a namestring for a path that is sufficiently
;; unambiguous (hopefully) for the C compiler (and associates)
;; to decipher.
(defun brief-namestring (path)
  ;; In Windows we cannot use enough-namestring in the compiler
  ;; because it breaks down when using paths such as
  ;; c:/docume~1/juanjo/locals~1/temp/foo.tmp. enough-namestring would
  ;; return /docume~1/juanjo/locals~1/temp/foo.tmp which is not found
  #+windows
  (namestring (si::coerce-to-filename path))
  #-windows
  (enough-namestring (si::coerce-to-filename path)))

(defun normalize-build-target-name (target)
  (ecase target
    ((:shared-library :dll :standalone-shared-library :standalone-dll) :shared-library)
    ((:static-library :lib :standalone-static-library :standalone-lib) :static-library)
    ((:fasl :fasb) :fasl)
    (:program :program)))

(defun innermost-non-expanded-form (form)
  (when (listp form)
    (loop with output = nil
       for f in form
       do (cond ((eq f 'macroexpand)
                 (setf output nil))
                ((null output)
                 (setf output f)))
       finally (return output))))

;; For indirect use in :REPORT functions
(defun compiler-message-report (stream c format-control &rest format-arguments)
    (let ((position (compiler-message-file-position c))
        (prefix (compiler-message-prefix c))
        (file (compiler-message-file c))
        (form (innermost-non-expanded-form (compiler-message-toplevel-form c))))
    (if (and form
             position
             (not (minusp position))
             (not (equalp form '|compiler preprocess|)))
        (let* ((*print-length* 2)
               (*print-level* 2))
          (format stream
                  "~A:~%  in file ~A, position ~D~&  at ~A"
                  prefix
                  (make-pathname :name (pathname-name file)
                                 :type (pathname-type file)
                                 :version (pathname-version file))
                  position
                  form))
        (format stream "~A:" prefix))
    (format stream (compiler-message-format c)
            format-control
            format-arguments)))

(define-condition compiler-message (simple-condition)
  ((prefix :initform "Note" :accessor compiler-message-prefix)
   (format :initform +note-format+ :accessor compiler-message-format)
   (file :initarg :file :initform *compile-file-pathname*
         :accessor compiler-message-file)
   (position :initarg :file :initform *compile-file-position*
             :accessor compiler-message-file-position)
   (toplevel-form :initarg :form :initform *current-toplevel-form*
                  :accessor compiler-message-toplevel-form)
   (form :initarg :form :initform *current-form*
         :accessor compiler-message-form))
  (:report (lambda (c stream)
             (apply #'compiler-message-report stream c
                    (simple-condition-format-control c)
                    (simple-condition-format-arguments c)))))

(define-condition compiler-note (compiler-message) ())

(define-condition compiler-debug-note (compiler-note) ())

(define-condition compiler-warning (compiler-message style-warning)
  ((prefix :initform "Warning")
   (format :initform +warn-format+)))

(define-condition compiler-macro-expansion-failed (compiler-warning)
  ())

(define-condition compiler-error (compiler-message)
  ((prefix :initform "Error")
   (format :initform +error-format+)))

(define-condition compiler-fatal-error (compiler-error)
  ((format :initform +fatal-format+)))

(define-condition compiler-internal-error (compiler-fatal-error)
  ((prefix :initform "Internal error")))

(define-condition compiler-style-warning (compiler-message style-warning)
  ((prefix :initform "Style warning")
   (format :initform +warn-format+)))

(define-condition compiler-undefined-variable (compiler-style-warning)
  ((variable :initarg :name :initform nil))
  (:report
   (lambda (c stream)
     (compiler-message-report stream c
                              "Variable ~A was undefined. ~
                               Compiler assumes it is a global."
                              (slot-value c 'variable)))))

(define-condition circular-dependency (compiler-error)
  ()
  (:report
   (lambda (c stream)
     (compiler-message-report stream c
                              "Circular references in creation form for ~S."
                              (compiler-message-form c)))))

(defun print-compiler-message (c stream)
  (unless (typep c *suppress-compiler-messages*)
    #+cmu-format
    (format stream "~&~@<;;; ~@;~A~:>" c)
    #-cmu-format
    (format stream "~&;;; ~A" c)))

;;; A few notes about the following handlers. We want the user to be
;;; able to capture, collect and perhaps abort on the different
;;; conditions signaled by the compiler. Since the compiler uses
;;; HANDLER-BIND, the only way to let this happen is either let the
;;; handler return or use SIGNAL at the beginning of the handler and
;;; let the outer handler intercept.
;;;
;;; In neither case do we want to enter the the debugger. That means
;;; we can not derive the compiler conditions from SERIOUS-CONDITION.
;;;
(defun handle-compiler-note (c)
  (declare (ignore c))
  nil)

(defun handle-compiler-warning (c)
  (push c *compiler-conditions*)
  nil)

(defun handle-compiler-error (c)
  (signal c)
  (push c *compiler-conditions*)
  (print-compiler-message c t)
  (abort))

(defun handle-compiler-internal-error (c)
  (when *compiler-break-enable*
    (invoke-debugger c))
  (setf c (make-condition 'compiler-internal-error
                          :format-control "~A"
                          :format-arguments (list c)))
  (push c *compiler-conditions*)
  (signal c)
  (print-compiler-message c t)
  (abort))

(defun do-compilation-unit (closure &key override)
  (cond (override
         (let* ((*active-protection* nil))
           (do-compilation-unit closure)))
        ((null *active-protection*)
         (let* ((*active-protection* t)
                (*pending-actions* nil))
           (unwind-protect (do-compilation-unit closure)
             (loop for action in *pending-actions*
                do (funcall action)))))
        (t
         (funcall closure))))

(defmacro with-compilation-unit ((&rest options) &body body)
 `(do-compilation-unit #'(lambda () ,@body) ,@options))

(defmacro with-compiler-env ((compiler-conditions) &body body)
  `(let ((*compiler-conditions* nil))
     (declare (special *compiler-conditions*))
     (restart-case
         (handler-bind ((compiler-note #'handle-compiler-note)
                        (warning #'handle-compiler-warning)
                        (compiler-error #'handle-compiler-error)
                        (compiler-internal-error #'handle-compiler-internal-error)
                        (serious-condition #'handle-compiler-internal-error))
           (mp:with-lock (+load-compile-lock+)
             (let ,+init-env-form+
               (with-compilation-unit ()
                 ,@body))))
       (abort ()))
     (setf ,compiler-conditions *compiler-conditions*)))

(defvar *c1form-level* 0)
(defun print-c1forms (form)
  (cond ((consp form)
         (let ((*c1form-level* (1+ *c1form-level*)))
           (mapc #'print-c1forms form)))
        ((c1form-p form)
         (format t "~% ~D > ~A, parent ~A" *c1form-level* form (c1form-parent form))
         (print-c1forms (c1form-args form))
         form
         )))

(defun print-ref (ref-object stream)
  (let ((name (ref-name ref-object)))
    (if name
        (format stream "#<a ~A: ~A>" (type-of ref-object) name)
        (format stream "#<a ~A>" (type-of ref-object)))))

(defun print-var (var-object stream)
  (format stream "#<a VAR: ~A KIND: ~A>" (var-name var-object) (var-kind var-object)))

(defun cmpprogress (&rest args)
  (when *compile-verbose*
    (apply #'format t args)))

(defmacro cmpck (condition string &rest args)
  `(if ,condition (cmperr ,string ,@args)))

(defmacro cmpassert (condition string &rest args)
  `(unless ,condition (cmperr ,string ,@args)))

(defun cmperr (string &rest args)
  (let ((c (make-condition 'compiler-error
                           :format-control string
                           :format-arguments args)))
    (signal c)
    (print-compiler-message c t)
    (abort)))

(defun safe-list-length (l)
  ;; Computes the length of a proper list or returns NIL if it
  ;; is a circular list or terminates with a non-NIL atom.
  (declare (optimize (speed 3) (safety 0)))
  (loop with slow = l
        with fast = l
        with flag = t
        for l of-type fixnum from 0
        do (cond ((null fast)
                  (return l))
                 ((not (consp fast))
                  (return nil))
                 (flag
                  (setf flag nil
                        fast (cdr (truly-the cons fast))))
                 ((eq slow fast)
                  (return nil))
                 (t
                  (setf flag t
                        slow (cdr (truly-the cons slow))
                        fast (cdr (truly-the cons fast)))))
        finally (return l)))

(defun check-args-number (operator args &optional (min 0) (max most-positive-fixnum))

  (let ((l (safe-list-length args)))
    (when (null l)
      (let ((*print-circle* t))
        (cmperr "Improper or circular list passed to ~A~%~A" operator args)))
    (when (< l min)
      (too-few-args operator min l))
    (when (and max (> l max))
      (too-many-args operator max l))))

(defun too-many-args (name upper-bound n &aux (*print-case* :upcase))
  (cmperr "~S requires at most ~R argument~:p, but ~R ~:*~[were~;was~:;were~] supplied.~%"
          name
          upper-bound
          n))

(defun too-few-args (name lower-bound n)
  (cmperr "~S requires at least ~R argument~:p, but only ~R ~:*~[were~;was~:;were~] supplied.~%"
          name
          lower-bound
          n))

(defun do-cmpwarn (&rest args)
  (declare (si::c-local))
  (let ((condition (apply #'make-condition args)))
    (restart-case (signal condition)
      (muffle-warning ()
        :REPORT "Skip warning"
        (return-from do-cmpwarn nil)))
    (print-compiler-message condition t)))

(defun cmpwarn-style (string &rest args)
  (do-cmpwarn 'compiler-style-warning :format-control string :format-arguments args))

(defun cmpwarn (string &rest args)
  (do-cmpwarn 'compiler-warning :format-control string :format-arguments args))

(defun cmpnote (string &rest args)
  (do-cmpwarn 'compiler-note :format-control string :format-arguments args))

(defun cmpdebug (string &rest args)
  (do-cmpwarn 'compiler-debug-note :format-control string :format-arguments args))

(defun print-current-form ()
  (when *compile-print*
    (let ((*print-length* 2)
          (*print-level* 2))
      (format t "~&;;; Compiling ~s.~%"
              (innermost-non-expanded-form *current-toplevel-form*))))
  nil)

(defun print-emitting (f)
  (when *compile-print*
    (let* ((name (or (fun-name f) (fun-description f))))
      (when name
        (format t "~&;;; Emitting code for ~s.~%" name)))))

(defun undefined-variable (sym)
  (do-cmpwarn 'compiler-undefined-variable :name sym))
  
(defun baboon (&key (format-control "A bug was found in the compiler")
               format-arguments)
  (signal 'compiler-internal-error
          :format-control format-control
          :format-arguments format-arguments))
  
(defmacro with-cmp-protection (main-form error-form)
  `(let* ((si::*break-enable* *compiler-break-enable*)
          (throw-flag t))
     (unwind-protect
         (multiple-value-prog1
             (if *compiler-break-enable*
                 (handler-bind ((error #'invoke-debugger))
                   ,main-form)
                 ,main-form)
           (setf throw-flag nil))
       (when throw-flag ,error-form))))

(defun cmp-eval (form &optional (env *cmp-env*))
  (handler-case (si::eval-with-env form env nil t :execute)
    (serious-condition (c)
      (when *compiler-break-enable*
        (invoke-debugger c))
      (cmperr "The form ~s was not evaluated successfully.~%Error detected:~%~A"
              form c)
      nil)))

;;; Like macro-function except it searches the lexical environment,
;;; to determine if the macro is shadowed by a function or a macro.
(defun cmp-macro-function (name)
  (or (cmp-env-search-macro name)
      (macro-function name)))

(defun cmp-expand-macro (fd form &optional (env *cmp-env*))
  (handler-case
      (let ((new-form (funcall *macroexpand-hook* fd form env)))
        (values new-form (not (eql new-form form))))
    (serious-condition (c)
      (when *compiler-break-enable*
        (invoke-debugger c))
      (cmperr "The macro form ~s was not expanded successfully.~%Error detected:~%~A"
              form c)
      (values nil nil))))
  
(defun cmp-expand-compiler-macro (fd fname args &optional (env *cmp-env*))
  (handler-case
      (cmp-expand-macro fd (list* fname args) env)
    (serious-condition (c)
      (do-cmpwarn 'compiler-macro-expansion-failed
        :format-control "The expansion of the compiler macro~%~T~A~%was aborted because of a serious condition~%~A" :format-arguments (list fname c))
      (values nil nil))))

(defun si::compiler-clear-compiler-properties (symbol)
  (si:rem-sysprop symbol 't1)
  (si:rem-sysprop symbol 't2)
  (si:rem-sysprop symbol 't3)
  (si:rem-sysprop symbol 'lfun))

(defun lisp-to-c-name (obj)
  "Translate Lisp object prin1 representation to valid C identifier name"
  (and obj 
       (map 'string 
            #'(lambda (c)
                (let ((cc (char-code c)))
                  (if (or (<= #.(char-code #\a) cc #.(char-code #\z))
                          (<= #.(char-code #\0) cc #.(char-code #\9)))
                      c #\_)))
            (string-downcase (prin1-to-string obj)))))

(defun collect-lines (stream)
  (loop for line = (read-line stream nil nil)
     while line
     collect line))

(defmacro define-compiler-macro* (name lambda-list &body body)
  "A version of define-compiler-macro that helps avoiding incorrect
evaluation order and double evaluation.

Example: A naive (simplified) definition of the compiler macro for
make-array could look as follows

(define-compiler-macro make-array (&whole form dimensions &key (element-type t) ...)
  (let ((dimensions-type (guess-array-dimensions-type dimensions)))
    (if (and (listp dimensions-type)
             (null (rest dimensions-type))
             (integerp (first dimensions-type)))
        `(si::make-vector ,element-type ,(first dimensions-type) ...)
        `(si::make-pure-array ,element-type ,dimensions ...))))

However this has several problems: first element-type and dimensions
are evaluated in the wrong order and second the compiler macro will
not accept perfectly valid forms like

(let ((etype :element-type))
  (make-array 10 etype 'character))

A correct version could be implemented using define-compiler-macro* as
follows

(define-compiler-macro make-array (&whole form dimensions &key (element-type t) ...)
  (let ((dimensions-type (guess-array-dimensions-type dimensions)))
    (if (and (listp dimensions-type)
             (null (rest dimensions-type))
             (integerp (first dimensions-type)))
        `(si::make-vector ,%element-type ,(first dimensions-type) ...)
        `(si::make-pure-array ,%element-type ,%dimensions ...))))

which would expand (make-array dim :element-type 'character) to

(let ((#:dimensions dim)
      (#:element-type 'character))
  (si::make-pure-array #:element-type #:dimensions))

Note that dimensions and element-type are evaluated in the correct
order. In the case of (let ((etype :element-type))
                        (make-array 10 etype 'character))
the corrected version using define-compiler-macro* will simply
decline to produce an expansion. On the other hand, for
(make-array 10 :element-type 'character), the corrected version would
expand to

(let ((#:dimensions 10)
      (#:element-type 'character))
  (si::make-vector #:element-type 10))


How it works:

For each argument two let bindings are established: one for the
variable holding the actual argument and one for a symbol (prefixed
with %) which will be bound to the value that the argument evaluates
to. In the compiler macro expansion, the % prefixed symbols are
evaluated in the order in which the corresponding arguments are given
to the compiler macro.

This allows the compiler-macro to look at the actual form provided for
the arguments and also use these values produced by these forms in the
final expansion.

Keyword arguments are handled specially: we try to match keywords in
the list of arguments to the compiler macro to the corresponding &key
arguments, but if we a non-keyword form in place where we expect a
keyword argument, the compiler-macro declines to provide an expansion.
"
  ;; General info: parsing happens in three steps. In the first pass,
  ;; the keyword arguments given to the compiler-macro are matched.
  ;; Then the body of the compiler macro is evaluated. In the second
  ;; pass, let bindings are constructed in the correct order for all
  ;; arguments given to the compiler macro. In the third pass, let
  ;; bindings for default initforms of arguments not given to the
  ;; compiler-macro are constructed.
  (ext:with-unique-names (;; local vars
                          given-keyword given-arg some-keyword-found output
                          all-found-keywords
                          ;; bindings-for-expansion: these bindings
                          ;; are active around the expansion returned
                          ;; by the compiler macro
                          bindings-for-expansion)
    (let* ((whole (gensym))              ; symbol for &whole compiler-macro argument
           (new-lambda-list (list whole '&whole)) ; lambda-list after we have stripped out &key, collected in reverse order
           aux-setf-forms                ; forms for &aux vars
           parse-forms-pass1 parse-forms-pass2 parse-forms-pass3 ; lists of parse-forms generated below
           keyword-parse-forms-pass2
           ;; bindings-for-body: these bindings are active around the
           ;; body of the compiler macro
           (bindings-for-body (list all-found-keywords bindings-for-expansion)))
      ;; lambda-list handling:
      ;; 1. extract &whole and &environment
      (when (eq (first lambda-list) '&whole)
        (push `(,(second lambda-list) ,whole) bindings-for-body)
        (setf lambda-list (cddr lambda-list)))
      (when-let ((env (member '&environment lambda-list)))
        (push '&environment new-lambda-list)
        (push (second env) new-lambda-list)
        (setq lambda-list (nconc (ldiff lambda-list env) (cddr env))))
      ;; 2. parse the remaining lambda-list
      (multiple-value-bind (reqs opts rest key-flag keywords allow-other-keys auxs)
          (si::process-lambda-list lambda-list 'si::macro)
        (when (and rest (or key-flag allow-other-keys))
          (error "define-compiler-macro* can't deal with lambda-lists with both &key and &rest arguments"))
        ;; utility functions
        (labels ((prefix-symbol (s)
                   (intern (concatenate 'string "%" (symbol-name s))))
                 (pass1-parse ()
                   (when key-flag
                     `(progn
                        (unless (zerop (mod (length ,rest) 2))
                          (return-from ,name ,whole))
                        (loop for ,given-keyword in ,rest by #'cddr
                              for ,given-arg in (rest ,rest) by #'cddr
                              for ,some-keyword-found = nil
                              do (when (or (not (keywordp ,given-keyword))
                                           (eq ,given-keyword :allow-other-keys))
                                   (return-from ,name ,whole))
                              ,@parse-forms-pass1
                              (when (not ,some-keyword-found)
                                (if ,allow-other-keys
                                    (push
                                     ;; &allow-other-keys: we still
                                     ;; have to evaluate the argument
                                     `(list ,(gensym) ,,given-arg)
                                     ,bindings-for-expansion)
                                    (return-from ,name ,whole)))))))
                 (handle-required-parameter (symbol)
                   (let ((%symbol (prefix-symbol symbol)))
                     (push `(,%symbol (gensym)) bindings-for-body)
                     (push symbol new-lambda-list)
                     (push `(push (list ,%symbol ,symbol) ,bindings-for-expansion)
                           parse-forms-pass2)))
                 (handle-optional-parameter (opt-spec)
                   (let* ((symbol (first opt-spec))
                          (init (second opt-spec))
                          (supplied-p (or (third opt-spec) (gensym)))
                          (%symbol (prefix-symbol symbol)))
                     (push `(,%symbol (gensym)) bindings-for-body)
                     (push `(,symbol ,init ,supplied-p) new-lambda-list)
                     (push `(when ,supplied-p
                              (push (list ,%symbol ,symbol) ,bindings-for-expansion))
                           parse-forms-pass2)
                     (push `(unless ,supplied-p
                              (push (list ,%symbol ,symbol) ,bindings-for-expansion))
                           parse-forms-pass3)))
                 (handle-keyword-parameter (key-spec)
                   (let* ((keyword (first key-spec))
                          (symbol (second key-spec))
                          (init (third key-spec))
                          (supplied-p (or (fourth key-spec) (gensym)))
                          (%symbol (prefix-symbol symbol)))
                     (push `(,%symbol (gensym)) bindings-for-body)
                     (push `(,symbol ,init) bindings-for-body)
                     (push `(,supplied-p nil) bindings-for-body)
                     (push `(when (and (eq ,given-keyword ,keyword)
                                       (not ,supplied-p))
                              (setf ,some-keyword-found t)
                              (setf ,symbol ,given-arg)
                              (setf ,supplied-p t)
                              (push ,keyword ,all-found-keywords))
                           parse-forms-pass1)
                     (push `(when (eq ,given-keyword ,keyword)
                              (push (list ,%symbol ,symbol) ,bindings-for-expansion))
                           keyword-parse-forms-pass2)
                     (push `(unless ,supplied-p
                              (push (list ,%symbol ,symbol) ,bindings-for-expansion))
                           parse-forms-pass3))))
          ;; 3. required, optional and rest parameters are simply
          ;; copied to the new lambda-list
          (mapcar #'handle-required-parameter (rest reqs))
          (when (> (first opts) 0)
            (push '&optional new-lambda-list)
            (loop for o on (rest opts) by #'cdddr
                  do (handle-optional-parameter o)))
          (when rest
            (let ((%rest (prefix-symbol rest)))
              (push `(,%rest (gensym)) bindings-for-body)
              (push `(push (list ,%rest ,rest) ,bindings-for-expansion)
                    parse-forms-pass2)
              (push '&rest new-lambda-list)
              (push rest new-lambda-list)))
          ;; 4. keyword parameters: first put all remaining parameters
          ;; in a rest argument, then parse keywords from this rest
          ;; argument
          (when (or key-flag allow-other-keys)
            (unless rest
              (setf rest (gensym))
              (push '&rest new-lambda-list)
              (push rest new-lambda-list))
            (loop for key-spec on (rest keywords) by #'cddddr
                  do (handle-keyword-parameter key-spec))
            (push `(loop for ,given-keyword in (nreverse ,all-found-keywords)
                         do ,@keyword-parse-forms-pass2)
                  parse-forms-pass2))
          ;; 5. &aux vars: these are simply set to their initforms after
          ;; parsing of keywords has finished
          (loop for a on auxs
                do (push (first auxs) bindings-for-body)
                   (push `(setf ,(first auxs) ,(second auxs)) aux-setf-forms))
          ;; 6. Finally, we are ready to create the compiler-macro definition
          `(define-compiler-macro ,name ,(nreverse new-lambda-list)
             (let* ,(nreverse bindings-for-body)
               ;; parse arguments
               ,(pass1-parse)
               ,@aux-setf-forms
               ;; evaluate the body of the compiler-macro
               (let ((,output (block ,name (locally ,@body))))
                 (if (eq ,output ,whole)
                     ,whole
                     (progn
                       ,@(nreverse parse-forms-pass2)
                       ,@(nreverse parse-forms-pass3)
                       ;; create bindings for the arguments passed to the compiler-macro
                       `(let ,(nreverse ,bindings-for-expansion)
                          ,,output)))))))))))
