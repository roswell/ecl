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

;; Return a namestring for a path that is sufficiently
;; unambiguous (hopefully) for the C compiler (and associates)
;; to decipher.
(defun brief-namestring (path)
  ;; In Windows we cannot use enough-namestring in the compiler
  ;; because it breaks down when using paths such as
  ;; c:/docume~1/juanjo/locals~1/temp/foo.tmp. enough-namestring would
  ;; return /docume~1/juanjo/locals~1/temp/foo.tmp which is not found
  (when (wild-pathname-p path)
    (error "Cannot coerce ~A to a physical filename~%" path))
  #+windows
  (namestring (si:coerce-to-file-pathname path))
  #-windows
  (enough-namestring (si:coerce-to-file-pathname path)))

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
           (mp:with-lock (mp:+load-compile-lock+)
             (let ,+init-env-form+
               (with-compilation-unit ()
                 ,@body))))
       (abort ()))
     (setf ,compiler-conditions *compiler-conditions*)))

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
                        fast (cdr (ext:truly-the cons fast))))
                 ((eq slow fast)
                  (return nil))
                 (t
                  (setf flag t
                        slow (cdr (ext:truly-the cons slow))
                        fast (cdr (ext:truly-the cons fast)))))
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

(defun print-current-form ()
  (when *compile-print*
    (let ((*print-length* 2)
          (*print-level* 2))
      (format t "~&;;; Compiling ~s.~%"
              (innermost-non-expanded-form *current-toplevel-form*))))
  nil)

(defun cmpprogress (&rest args)
  (when *compile-verbose*
    (apply #'format t args)))

(defun cmp-eval (form &optional (env *cmp-env*))
  (handler-case (si:eval-with-env form env nil t :execute)
    (serious-condition (c)
      (when *compiler-break-enable*
        (invoke-debugger c))
      (cmperr "The form ~s was not evaluated successfully.~%Error detected:~%~A"
              form c)
      nil)))

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
      (ext:when-let ((env (member '&environment lambda-list)))
        (push '&environment new-lambda-list)
        (push (second env) new-lambda-list)
        (setq lambda-list (nconc (ldiff lambda-list env) (cddr env))))
      ;; 2. parse the remaining lambda-list
      (multiple-value-bind (reqs opts rest key-flag keywords allow-other-keys auxs)
          (si:process-lambda-list lambda-list 'si:macro)
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
          (loop for a on (rest auxs) by #'cddr
                do (push (first a) bindings-for-body)
                   (push `(setf ,(first a) ,(second a)) aux-setf-forms))
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

(defun equal-with-circularity (x y)
  "A version of equal which does not run into an infinite loop when
comparing circular objects."
  ;; x0 and y0 hold pointers to previous list elements. By stepping
  ;; through x0 and y0 only at every other invocation of
  ;; equal-recursive, we will eventually call equal-recursive with all
  ;; possible combinations of x and x0 respectively y and y0. The
  ;; integer path-spec records whether we have recursively descended
  ;; into car or cdr slots of the list structure depending on whether
  ;; its bits are zero or one. The integer n tells us which bit of
  ;; path-spec we have to check in order to determine whether we have
  ;; to follow a car or cdr slot in x0 respectively y0 to follow the
  ;; same path that lead us to x respectively y.
  ;;
  ;; Optimization note: the algorithm is optimized for common cases:
  ;; it runs in linear time for non-circular lists, does not overflow
  ;; the stack for large lists (due to the cdr cases descending
  ;; recursively in the tail position) and does not create large
  ;; path-spec integers for circular lists of the form #1=(a1 ... an .
  ;; #1#) (due to the zero-bit being used to record a descent into the
  ;; cdr slot).
  (labels ((equal-recursive (x y x0 y0 switch path-spec n)
             (declare (optimize (safety 0)) (ext:check-stack-overflow)
                      (fixnum n)        ; INV: the number of elements is a fixnum
                      (type integer path-spec))
             (cond ((or (not (consp x)) (not (consp y)))
                    (equal x y))
                   ((eq x y)
                    t)
                   ((and (eq x x0) (eq y y0))
                    t)
                   ((or (eq x x0) (eq y y0))
                    nil)
                   ;; INV: The compiler will perform tail call elimination here
                   ((= n -1)
                    (and (equal-recursive (car x) (car y) x y nil 1 0)
                         (equal-recursive (cdr x) (cdr y) x y nil 0 0)))
                   (switch
                    (let* ((mask (ash 1 n))
                           (x0 (if (zerop (logand path-spec mask)) (cdr x0) (car x0)))
                           (y0 (if (zerop (logand path-spec mask)) (cdr y0) (car y0))))
                      (and (equal-recursive (car x) (car y) x0 y0 nil (logior (ash (logandc2 path-spec mask) 1) 1) n)
                           (equal-recursive (cdr x) (cdr y) x0 y0 nil (ash (logandc2 path-spec mask) 1) n))))
                   (t
                    (and (equal-recursive (car x) (car y) x0 y0 t (logior (ash path-spec 1) 1) (the fixnum (1+ n)))
                         (equal-recursive (cdr x) (cdr y) x0 y0 t (ash path-spec 1) (the fixnum (1+ n))))))))
    (equal-recursive x y nil nil t 0 -1)))

(defun type-specifier= (x y)
  "Compares two type specifiers for syntactic equality."
  ;; This function only checks if the arguments have the same name
  ;; (and arguments in case of compound type specifiers) but not if
  ;; they are aliases of each other. For example (OR REAL COMPLEX) and
  ;; NUMBER are considered different by this function but are of
  ;; course semantically equivalent.
  ;;
  ;; Note that type specifiers cannot be compared with EQUAL since in
  ;; eql and member types the arguments have to compared using EQL.
  (if (and (consp x) (consp y))
      (if (and (member (first x) '(eql member))
               (member (first y) '(eql member)))
          (every #'eql x y)
          (and (type-specifier= (car x) (car y))
               (type-specifier= (cdr x) (cdr y))))
      (eql x y)))

;; ----------------------------------------------------------------------
;; CACHED FUNCTIONS
;;
(defmacro defun-cached (name lambda-list test &body body)
  (let* ((cache-name (intern (concatenate 'string "*" (string name) "-CACHE*")
                             (symbol-package name)))
         (reset-name (intern (concatenate 'string (string name) "-EMPTY-CACHE")
                             (symbol-package name)))
         (hash-function (case test
                          (EQ 'SI::HASH-EQ)
                          (EQL 'SI::HASH-EQL)
                          ((EQUAL EQUAL-WITH-CIRCULARITY TYPE-SPECIFIER=) 'SI::HASH-EQUAL)
                          (t (setf test 'EQUALP) 'SI::HASH-EQUALP))))
    `(progn
       (defvar ,cache-name
         (make-array 1024 :element-type t :adjustable nil))
       (defun ,reset-name ()
         (setf ,cache-name
               (make-array 1024 :element-type t :adjustable nil)))
       (defun ,name ,lambda-list
         (flet ((,name ,lambda-list ,@body))
           (let* ((hash (logand (,hash-function ,@lambda-list) 1023))
                  (cache ,cache-name)
                  (elt (aref cache hash)))
             (declare (type (integer 0 1023) hash)
                      (type (array t (*)) cache))
             (if (and elt ,@(loop for arg in lambda-list
                                  collect `(,test (pop (ext:truly-the cons elt)) ,arg)))
                 (first (ext:truly-the cons elt))
                 (let ((output (,name ,@lambda-list)))
                   (setf (aref ,cache-name hash) (list ,@lambda-list output))
                   output))))))))

(defun same-fname-p (name1 name2)
  (equal name1 name2))

(defun emptyp (item)
  (etypecase item
    (list (null item))
    (vector (zerop (length item)))
    (hash-table (zerop (hash-table-count item)))))
