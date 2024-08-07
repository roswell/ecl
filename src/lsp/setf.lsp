;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;  Copyright (c) 2015, Daniel Kochmański.
;;;;
;;;;    See file 'LICENSE' for the copyright details.

;;;;                                setf routines

(in-package "SYSTEM")

(defun do-setf-method-expansion (name lambda args &optional (stores-no 1))
  (declare (si::c-local))
  (let* ((vars '())
         (inits '())
         (all '())
         (stores '()))
    (dolist (item args)
      (unless (or (fixnump item) (keywordp item))
        (push item inits)
        (setq item (gensym))
        (push item vars))
      (push item all))
    (dotimes (i stores-no)
      (push (gensym) stores))
    (let* ((all (nreverse all)))
      (values (nreverse vars)
              (nreverse inits)
              stores
              (if lambda
                  (apply lambda (append stores all))
                  `(funcall #'(setf ,name) ,@stores ,@all))
              (cons name all)))))

(defun do-defsetf (access-fn function &optional (stores-no 1))
  (declare (type-assertions nil))
  (if (symbolp function)
      (do-defsetf access-fn
        #'(lambda (store &rest args)
            `(,function ,@args ,store))
        stores-no)
      (do-define-setf-method access-fn
        #'(lambda (args env)
            (declare (ignore env))
            (do-setf-method-expansion access-fn function (cdr args) stores-no)))))

(defun do-define-setf-method (access-fn function)
  (declare (type-assertions nil))
  (put-sysprop access-fn 'SETF-METHOD function))

;;; DEFSETF macro.
(defmacro defsetf (&whole whole access-fn &rest rest)
  "Syntax: (defsetf symbol update-fun [doc])
        or
        (defsetf symbol lambda-list (store-var*) {decl | doc}* {form}*)
Defines an expansion
        (setf (SYMBOL arg1 ... argn) value)
        => (UPDATE-FUN arg1 ... argn value)
           or
           (let* ((temp ARG)*)
             (multiple-value-bind (temp-s*)
                 values-form
               rest)
where REST is the value of the last FORM with parameters in
LAMBDA-LIST bound to the symbols TEMP* and with STORE-VAR* bound to
the symbols TEMP-S*.  The doc-string DOC, if supplied, is saved as a
SETF doc and can be retrieved by (documentation 'SYMBOL 'setf)."
  (let (function documentation stores)
    (if (and (car rest) (or (symbolp (car rest)) (functionp (car rest))))
        (setq function `',(car rest)
              documentation (cadr rest)
              stores `(,(gensym)))
        (let* ((args (first rest))
               (body (cddr rest))
               decls)
          (multiple-value-setq (decls body documentation)
            (find-declarations body t))
          (setq stores (second rest)
                function `#'(lambda (,@stores ,@args)
                              ,@decls
                              (block ,access-fn ,@body)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,(ext:register-with-pde whole `(do-defsetf ',access-fn ,function ,(length stores)))
       ,@(si::expand-set-documentation access-fn 'setf documentation)
       ',access-fn)))


;;; DEFINE-SETF-METHOD macro.
(defmacro define-setf-expander (access-fn args &rest body)
  "Syntax: (define-setf-expander symbol defmacro-lambda-list {decl | doc}*
          {form}*)
Defines the SETF-method for generalized-variables (SYMBOL ...).
When a form (setf (SYMBOL arg1 ... argn) value-form) is evaluated, the FORMs
given in the DEFINE-SETF-EXPANDER are evaluated in order with the parameters in
DEFMACRO-LAMBDA-LIST bound to ARG1 ... ARGn.  The last FORM must return five
values
        (var1 ... vark)
        (form1 ... formk)
        (value-var)
        storing-form
        access-form
in order.  These values are collectively called the five gangs of the
generalized variable (SYMBOL arg1 ... argn).  The whole SETF form is then
expanded into
        (let* ((var1 from1) ... (vark formk)
               (value-var value-form))
          storing-form)
The doc-string DOC, if supplied, is saved as a SETF doc and can be retrieved
by (DOCUMENTATION 'SYMBOL 'SETF)."
  (multiple-value-bind (function pprint documentation)
      (sys::expand-defmacro access-fn args body 'define-setf-expander)
    (declare (ignore pprint))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (do-define-setf-method ',access-fn #',function)
       ,@(si::expand-set-documentation access-fn 'setf documentation)
       ',access-fn)))


;;;; get-setf-expansion.

(defun get-setf-expansion (form &optional env &aux f)
  "Args: (form)
Returns the 'five gangs' (see DEFINE-SETF-EXPANDER) for PLACE as five values.
Does not check if the third gang is a single-element list."
  (declare (check-arguments-type nil))
  ;; Note that macroexpansion of SETF arguments can only be done via
  ;; MACROEXPAND-1 [ANSI 5.1.2.7]
  (cond ((symbolp form)
         (if (and (setq f (macroexpand-1 form env)) (not (equal f form)))
             (get-setf-expansion f env)
             (let ((store (gensym)))
               (values nil nil (list store) `(setq ,form ,store) form))))
        ((or (not (consp form)) (not (symbolp (car form))))
         (error "Cannot get the setf-method of ~S." form))
        ((setq f (get-sysprop (car form) 'SETF-METHOD))
         (funcall f form env))
        ((and (setq f (macroexpand-1 form env)) (not (equal f form)))
         (get-setf-expansion f env))
        (t
         (do-setf-method-expansion (car form) nil (cdr form)))))

;;;; SETF definitions.

(defsetf car (x) (y) `(progn (rplaca ,x ,y) ,y))
(defsetf cdr (x) (y) `(progn (rplacd ,x ,y), y))
(defsetf caar (x) (y) `(progn (rplaca (car ,x) ,y) ,y))
(defsetf cdar (x) (y) `(progn (rplacd (car ,x) ,y) ,y))
(defsetf cadr (x) (y) `(progn (rplaca (cdr ,x) ,y) ,y))
(defsetf cddr (x) (y) `(progn (rplacd (cdr ,x) ,y) ,y))
(defsetf caaar (x) (y) `(progn (rplaca (caar ,x) ,y) ,y))
(defsetf cdaar (x) (y) `(progn (rplacd (caar ,x) ,y) ,y))
(defsetf cadar (x) (y) `(progn (rplaca (cdar ,x) ,y) ,y))
(defsetf cddar (x) (y) `(progn (rplacd (cdar ,x) ,y) ,y))
(defsetf caadr (x) (y) `(progn (rplaca (cadr ,x) ,y) ,y))
(defsetf cdadr (x) (y) `(progn (rplacd (cadr ,x) ,y) ,y))
(defsetf caddr (x) (y) `(progn (rplaca (cddr ,x) ,y) ,y))
(defsetf cdddr (x) (y) `(progn (rplacd (cddr ,x) ,y) ,y))
(defsetf caaaar (x) (y) `(progn (rplaca (caaar ,x) ,y) ,y))
(defsetf cdaaar (x) (y) `(progn (rplacd (caaar ,x) ,y) ,y))
(defsetf cadaar (x) (y) `(progn (rplaca (cdaar ,x) ,y) ,y))
(defsetf cddaar (x) (y) `(progn (rplacd (cdaar ,x) ,y) ,y))
(defsetf caadar (x) (y) `(progn (rplaca (cadar ,x) ,y) ,y))
(defsetf cdadar (x) (y) `(progn (rplacd (cadar ,x) ,y) ,y))
(defsetf caddar (x) (y) `(progn (rplaca (cddar ,x) ,y) ,y))
(defsetf cdddar (x) (y) `(progn (rplacd (cddar ,x) ,y) ,y))
(defsetf caaadr (x) (y) `(progn (rplaca (caadr ,x) ,y) ,y))
(defsetf cdaadr (x) (y) `(progn (rplacd (caadr ,x) ,y) ,y))
(defsetf cadadr (x) (y) `(progn (rplaca (cdadr ,x) ,y) ,y))
(defsetf cddadr (x) (y) `(progn (rplacd (cdadr ,x) ,y) ,y))
(defsetf caaddr (x) (y) `(progn (rplaca (caddr ,x) ,y) ,y))
(defsetf cdaddr (x) (y) `(progn (rplacd (caddr ,x) ,y) ,y))
(defsetf cadddr (x) (y) `(progn (rplaca (cdddr ,x) ,y) ,y))
(defsetf cddddr (x) (y) `(progn (rplacd (cdddr ,x) ,y) ,y))
(defsetf first (x) (y) `(progn (rplaca ,x ,y) ,y))
(defsetf second (x) (y) `(progn (rplaca (cdr ,x) ,y) ,y))
(defsetf third (x) (y) `(progn (rplaca (cddr ,x) ,y) ,y))
(defsetf fourth (x) (y) `(progn (rplaca (cdddr ,x) ,y) ,y))
(defsetf fifth (x) (y) `(progn (rplaca (cddddr ,x) ,y) ,y))
(defsetf sixth (x) (y) `(progn (rplaca (nthcdr 5 ,x) ,y) ,y))
(defsetf seventh (x) (y) `(progn (rplaca (nthcdr 6 ,x) ,y) ,y))
(defsetf eighth (x) (y) `(progn (rplaca (nthcdr 7 ,x) ,y) ,y))
(defsetf ninth (x) (y) `(progn (rplaca (nthcdr 8 ,x) ,y) ,y))
(defsetf tenth (x) (y) `(progn (rplaca (nthcdr 9 ,x) ,y) ,y))
(defsetf rest (x) (y) `(progn (rplacd ,x ,y) ,y))
(defsetf svref sys:svset)
(defsetf elt sys:elt-set)
(defsetf symbol-value set)
(defsetf symbol-function sys:fset)
(defsetf fdefinition sys:fset)
(defsetf macro-function (s &optional env) (v) (declare (ignore env)) `(sys:fset ,s ,v t))
(defsetf aref si::aset)
(defsetf row-major-aref sys:row-major-aset)
(defsetf get (s p &optional d) (v)
  (if d `(progn ,d (sys:putprop ,s ,v ,p)) `(sys:putprop ,s ,v ,p)))
(defsetf get-sysprop put-sysprop)
(defsetf nth (n l) (v) `(progn (rplaca (nthcdr ,n ,l) ,v) ,v))
(defsetf char sys:char-set)
(defsetf schar sys:schar-set)
(defsetf bit sys:aset)
(defsetf sbit sys:aset)
(defsetf fill-pointer sys:fill-pointer-set)
(defsetf symbol-plist sys:set-symbol-plist)
(defsetf gethash (k h &optional d) (v) (declare (ignore d)) `(sys:hash-set ,k ,h ,v))
#-clos
(defsetf documentation sys::set-documentation)
#+clos
(defsetf sys:instance-ref sys:instance-set)
(defsetf compiler-macro-function (fname) (function)
  `(sys::put-sysprop ,fname 'sys::compiler-macro ,function))
(defsetf readtable-case sys:readtable-case-set)
(defsetf stream-external-format sys::stream-external-format-set)

(define-setf-expander getf (&environment env place indicator
                            &optional (default nil default-p))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (let* ((itemp (gensym)) (store (gensym)) (def (gensym)))
      (values `(,@vars ,itemp ,@(if default-p (list def) nil))
              `(,@vals ,indicator ,@(and default-p (list default)))
              `(,store)
              `(let ((,(car stores) (sys:put-f ,access-form ,store ,itemp)))
                 ,store-form
                 ,store)
              `(getf ,access-form ,itemp ,default)))))

(defsetf subseq (sequence1 start1 &optional end1)
                (sequence2)
  `(PROGN (REPLACE ,sequence1 ,sequence2 :START1 ,start1 :END1 ,end1)
    ,sequence2))

(define-setf-expander THE (&environment env type place)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (values vars vals stores
            (subst `(THE ,type ,(first stores)) (first stores) store-form)
            `(THE ,type ,access-form))))

#|
(define-setf-expander apply (&environment env fn &rest rest)
  (unless (and (consp fn) (eq (car fn) 'FUNCTION) (symbolp (cadr fn))
               (null (cddr fn)))
          (error "Can't get the setf-method of ~S." fn))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion (cons (cadr fn) rest) env)
    (unless (eq (car (last store-form)) (car (last vars)))
            (error "Can't get the setf-method of ~S." fn))
    (values vars vals stores
            `(apply #',(car store-form) ,@(cdr store-form))
            `(apply #',(cadr fn) ,@(cdr access-form)))))
|#

(define-setf-expander apply (&environment env fn &rest rest)
  (unless (and (consp fn)
               (or (eq (car fn) 'FUNCTION) (eq (car fn) 'QUOTE))
               (symbolp (cadr fn))
               (null (cddr fn)))
    (error "Can't get the setf-method of ~S." fn))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion (cons (cadr fn) rest) env)
    (cond ((eq (car (last store-form)) (car (last vars)))
           (values vars vals stores
                   `(apply #',(car store-form) ,@(cdr store-form))
                   `(apply #',(cadr fn) ,@(cdr access-form))))
          ((eq (car (last (butlast store-form))) (car (last vars)))
           (values vars vals stores
                   `(apply #',(car store-form)
                           ,@(cdr (butlast store-form 2))
                           (append ,(car (last (butlast store-form)))
                                   (list ,(car (last store-form)))))
                   `(apply #',(cadr fn) ,@(cdr access-form))))
          (t (error "Can't get the setf-method of ~S." fn)))))

(define-setf-expander ldb (&environment env bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion int env)
    (let* ((btemp (gensym))
           (store (gensym))
           (stemp (first stores)))
      (values `(,btemp ,@temps)
              `(,bytespec ,@vals)
              `(,store)
              `(let ((,stemp (dpb ,store ,btemp ,access-form)))
                 ,store-form ,store)
              `(ldb ,btemp ,access-form)))))

(define-setf-expander mask-field (&environment env bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion int env)
    (let* ((btemp (gensym))
           (store (gensym))
           (stemp (first stores)))
      (values `(,btemp ,@temps)
              `(,bytespec ,@vals)
              `(,store)
              `(let ((,stemp (deposit-field ,store ,btemp ,access-form)))
                 ,store-form ,store)
              `(mask-field ,btemp ,access-form)))))

(defun trivial-setf-form (place vars stores store-form access-form)
  (declare (si::c-local)
           (optimize (speed 3) (safety 0)))
  (and (atom place)
       (null vars)
       (eq access-form place)
       (= (length stores) 1)
       (listp store-form)
       (= (length store-form) 3)
       (member (first store-form) '(setq setf))
       (eq (second store-form) place)
       (eq (third store-form) (first stores))
       ))

(defun try-simpler-expansion (place vars vals stores newvalue store-form)
  ;; When the store form contains all the original arguments in order
  ;; followed by a single stored value, we can produce an expansion
  ;; without LET forms.
  (declare (si::c-local)
           (optimize (speed 3) (safety 0)))
  (when (and (consp place)
             (consp store-form)
             (= (length place) (truly-the fixnum (1- (length store-form)))))
    (let ((function (pop store-form))
          (output '())
          v)
      (dolist (i (rest place)
               (when (eq (first stores) (first store-form))
                 (list* function
                        (nreverse (cons newvalue output)))))
        (unless (consp store-form)
          (return nil))
        (setq v (car (truly-the cons store-form))
              store-form (cdr (truly-the cons store-form)))
        ;; This checks that the argument at this position coincides with
        ;; the corresponding value in the original list. Note that the
        ;; variable list need not be in order.
        (unless (or (eq v i)
                    (and (eq v (pop vars))
                         (eq (pop vals) i)))
          (return nil))
        (push i output)))))

;;; The expansion function for SETF.
(defun setf-expand-1 (place newvalue env)
  (declare (si::c-local)
           (notinline mapcar))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (cond ((trivial-setf-form place vars stores store-form access-form)
           (list 'setq place newvalue))
          ((try-simpler-expansion place vars vals stores newvalue store-form))
          (t
           `(let* ,(mapcar #'list vars vals)
              ;; Unfortunately we cannot do this because there is code out
              ;; there that changes the variables and values
              ;; (declare (:read-only ,@vars))
              (multiple-value-bind ,stores ,newvalue
                ;; Same here
                ;; (declare (:read-only ,@stores))
                ,store-form))))))

(defun setf-expand (l env)
  (declare (si::c-local))
  (cond ((endp l) nil)
        ((endp (cdr l)) (error "~S is an illegal SETF form." l))
        (t
         (cons (setf-expand-1 (car l) (cadr l) env)
               (setf-expand (cddr l) env)))))

;;; SETF macro.
(defmacro setf (&environment env &rest rest)
  "Syntax: (setf {place form}*)
Evaluates each FORM and assigns the value to the corresponding PLACE in order.
Returns the value of the last FORM.
Each PLACE may be any one of the following:
  * A symbol that names a variable.
  * A function call form whose first element is the name of the following
    functions:
        nth     elt     subseq  rest    first ... tenth
        c?r     c??r    c???r   c????r
        aref    svref   char    schar   bit     sbit    fill-pointer
        get     getf    documentation   symbol-value    symbol-function
        symbol-plist    macro-function  gethash         fdefinition
        char-bit        ldb     mask-field
        apply   slot-value
    where '?' stands for either 'a' or 'd'.
  * A function call form whose first element is:
        1. an access function for a structure slot
        2. an accessor method for a CLOS object
  * the form (THE type place) with PLACE being a place recognized by SETF.
  * a macro call which expands to a place recognized by SETF.
  * any form for which a DEFSETF or DEFINE-SETF-EXPANDER declaration has been
    made."
  (cond ((endp rest) nil)
        ((endp (cdr rest)) (error "~S is an illegal SETF form." rest))
        ((endp (cddr rest)) (setf-expand-1 (car rest) (cadr rest) env))
        (t (cons 'progn (setf-expand rest env)))))

;;; PSTEF/SHIFTF/ROTATEF base macros

;; WITH-SETF-EXPANSIONS
(defmacro with-setf-expansions
    ((pairs stores store-forms access-forms)
     (args &optional env) &body body)
  "Syntax: (with-setf-expansions (pairs stores store-forms access-forms)
            ({place}* &optional env) {form}*)
Map setf-expansions of places into list of bindings in PAIRS, list of
stores in STORES, list of store-form in STORE-FORMS and list of
access-form in ACCESS-FORMS.  Creates new lexical scope composed of
these variables."
  (declare (notinline mapcar))
  `(destructuring-bind (,pairs ,stores ,store-forms ,access-forms)
       (reduce (lambda (x y)
                 (list
                  (cons (mapcar #'list (first y) (second y))
                        (first x))
                  (cons (third  y) (second x))
                  (cons (fourth y) (third  x))
                  (cons (fifth  y) (fourth x))))
               (mapcar (lambda (place)
                         (multiple-value-list
                          (get-setf-expansion place ,env)))
                       (reverse ,args))
               :initial-value '(nil nil nil nil))
     ,@body))

;;; PSETF macro.
(defmacro psetf (&environment env &rest args)
    "Syntax: (psetf {place form}*)
Similar to SETF, but evaluates all FORMs first, and then assigns each value to
the corresponding PLACE.  Returns NIL."
    (multiple-value-bind (places forms)
        (do* ((args args (cddr args))
              places forms)
             ((endp args)
              (values (nreverse places)
                      (nreverse forms)))
          (when (endp (cdr args))
            (error "~S is an illegal PSETF form (odd number of arguments)." args))
          (push (car args) places)
          (push (cadr args) forms))
      (with-setf-expansions (pairs stores store-forms access-forms) (places env)
        (declare (ignore access-forms))
        (labels ((thunk (pairs stores access-forms)
                   (if stores
                       `((let ,(car pairs)
                           (multiple-value-bind ,(car stores) ,(car access-forms)
                             ,@(thunk (cdr pairs) (cdr stores) (cdr access-forms)))))
                       store-forms)))
          `(prog1 nil
             ,@(thunk pairs stores forms))))))

;; WITH-EXPANSION-SETTER
(defmacro with-expansion-setter ((name store-forms) &body body)
  "Syntax: (with-expansion-setter (name store-forms) {form}*)
Macro creates lexically-scoped function NAME, which takes two lists as
arguments, and expands into nested MULTIPLE-VALUE-BINDs of ACCESS-FORMS
into STORES.  Finally inside bindings it expands STORE-FORMS."
  `(labels ((,name (stores access-forms)
              (if stores
                  `((multiple-value-bind ,(car stores) ,(car access-forms)
                      ,@(,name (cdr stores) (cdr access-forms))))
                  `(,@,store-forms))))
     ,@body))

;;; SHIFTF macro.
(defmacro shiftf (&environment env &rest args)
    "Syntax: (shiftf {place}+ form)
Saves the values of PLACE and FORM, and then assigns the value of each PLACE
to the PLACE on its left.  The rightmost PLACE gets the value of FORM.
Returns the original value of the leftmost PLACE."
  (with-setf-expansions (pairs stores store-forms access-forms)
      ((butlast args) env)
    (with-expansion-setter (thunk store-forms)
      `(let* ,(reduce #'append pairs)
         (multiple-value-prog1 ,(car access-forms)
           ,@(thunk stores
                    (append (cdr access-forms)
                            (last args))))))))

;;; ROTATEF macro.
(defmacro rotatef (&environment env &rest args)
  "Syntax: (rotatef {place}*)
Saves the values of PLACEs, and then assigns to each PLACE the saved value of
the PLACE to its right.  The rightmost PLACE gets the value of the leftmost
PLACE.  Returns NIL."
  (with-setf-expansions (pairs stores store-forms access-forms)
      (args env)
    (with-expansion-setter (thunk store-forms)
      `(let* ,(reduce #'append pairs)
         ,@(thunk stores
                  (append (cdr access-forms)
                          (list (car access-forms))))
         nil))))

;;; DEFINE-MODIFY-MACRO macro, by Bruno Haible.
(defmacro define-modify-macro (name lambdalist function &optional docstring)
  "Syntax: (define-modify-macro symbol lambda-list function-name [doc])
Defines a read-modify-write macro like INCF.  The defined macro will expand
a form (SYMBOL place form1 ... formn) into a form that in effect SETFs the
value of (FUNCTION-NAME place arg1 ... argm) into PLACE, where ARG1 ... ARGm
are parameters in LAMBDA-LIST which are bound to FORM1 ... FORMn.  For
example, INCF could be defined as
        (define-modify-macro incf (&optional (x 1)) +)
The doc-string DOC, if supplied, is saved as a FUNCTION doc and can be
retrieved by (DOCUMENTATION 'SYMBOL 'FUNCTION)."
  (let* ((varlist nil)
         (restvar nil))
    (do* ((lambdalistr lambdalist (cdr lambdalistr))
          (next))
         ((null lambdalistr))
      (setq next (first lambdalistr))
      (cond ((eq next '&OPTIONAL))
            ((eq next '&REST)
             (if (symbolp (second lambdalistr))
                 (setq restvar (second lambdalistr))
                 (error "In the definition of ~S: &REST variable ~S should be a symbol."
                        name (second lambdalistr)))
             (if (null (cddr lambdalistr))
                 (return)
                 (error "Only one variable is allowed after &REST, not ~S"
                        lambdalistr)))
            ((or (eq next '&KEY) (eq next '&ALLOW-OTHER-KEYS) (eq next '&AUX))
             (error "Illegal in a DEFINE-MODIFY-MACRO lambda list: ~S"
                    next))
            ((symbolp next) (push next varlist))
            ((and (listp next) (symbolp (first next)))
             (push (first next) varlist))
            (t (error "lambda list may only contain symbols and lists, not ~S"
                      next))))
    (setq varlist (nreverse varlist))
    `(DEFMACRO ,name (&ENVIRONMENT ENV %REFERENCE ,@lambdalist)
       ,@(and docstring (list docstring))
       (DECLARE (NOTINLINE MAPCAR))
       (MULTIPLE-VALUE-BIND (VARS VALS STORES SETTER GETTER)
           (GET-SETF-EXPANSION %REFERENCE ENV)
         (LET ((ALL-VARS (MAPCAR #'(LAMBDA (V) (LIST (GENSYM) V)) (LIST* ,@varlist ,restvar))))
           (IF (SYMBOLP GETTER)
               (SUBST (LIST* (QUOTE ,function) GETTER (MAPCAR #'CAR ALL-VARS))
                      (CAR STORES)
                      `(LET* ,ALL-VARS
                         (DECLARE (:READ-ONLY ,@(mapcar #'first all-vars)))
                         ,SETTER))
               (DO ((D VARS (CDR D))
                    (V VALS (CDR V))
                    (LET-LIST NIL (CONS (LIST (CAR D) (CAR V)) LET-LIST)))
                   ((NULL D)
                    (SETQ LET-LIST
                          (LIST*
                           (LIST
                            (CAR STORES)
                            (LIST* (QUOTE ,function) GETTER (MAPCAR #'CAR ALL-VARS)))
                           (APPEND ALL-VARS LET-LIST)))
                    `(LET* ,(NREVERSE LET-LIST)
                       (DECLARE (:READ-ONLY ,@(mapcar #'first all-vars)
                                            ,@vars))
                       ,SETTER)))))))))

#+(or)
(defmacro define-modify-macro (name lambda-list function &optional doc-string)
  (let ((update-form
         (do ((l lambda-list (cdr l))
              (vs nil))
             ((null l) `(list ',function access-form ,@(nreverse vs)))
           (unless (eq (car l) '&optional)
                   (if (eq (car l) '&rest)
                       (return `(list* ',function
                                       access-form
                                       ,@(nreverse vs)
                                       ,(cadr l))))
                   (if (symbolp (car l))
                       (setq vs (cons (car l) vs))
                       (setq vs (cons (caar l) vs)))))))
    `(defmacro ,name (&environment env reference . ,lambda-list)
       ,@(if doc-string (list doc-string))
       (when (symbolp reference)
             (return-from ,name
               (let ((access-form reference))
                 (list 'setq reference ,update-form))))
       (multiple-value-bind (vars vals stores store-form access-form)
           (get-setf-expansion reference env)
         `(let* ,(mapcar #'list
                  (append vars stores)
                  (append vals (list ,update-form)))
           (declare (:read-only ,@stores)) ; Beppe
           ,store-form)))))

;;; Some macro definitions.

(defmacro remf (&environment env place indicator)
  "Syntax: (remf place form)
Removes the property specified by FORM from the property list stored in PLACE.
Returns T if the property list had the specified property; NIL otherwise."
  (declare (notinline mapcar))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (let ((s (gensym)))
      `(let* (,@(mapcar #'list vars vals) (,s ,indicator))
         (declare (:read-only ,@vars)) ; Beppe
         (multiple-value-bind (,(car stores) flag)
             (sys:rem-f ,access-form ,s)
           ,store-form
           flag)))))

(define-modify-macro incf (&optional (delta 1)) +
  "Syntax: (incf place [form])
Increments the value of PLACE by the value of FORM.  FORM defaults to 1.")

(define-modify-macro decf (&optional (delta 1)) -
  "Syntax: (decf place [form])
Decrements the value of PLACE by the value of FORM.  FORM defaults to 1.")

(defmacro push (&environment env item place)
  "Syntax: (push form place)
Evaluates FORM, conses the value of FORM to the value stored in PLACE, and
makes it the new value of PLACE.  Returns the new value of PLACE."
  (declare (notinline mapcar))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (when (trivial-setf-form place vars stores store-form access-form)
      (return-from push `(setq ,place (cons ,item ,place))))
    ;; The item to be pushed has to be evaluated before the destination
    (unless (constantp item env)
      (setq vals (cons item vals)
            item (gensym)
            vars (cons item vars)))
    `(let* ,(mapcar #'list
                    (append vars stores)
                    (append vals (list (list 'cons item access-form))))
       (declare (:read-only ,@vars)) ; Beppe
       ,store-form)))

(defmacro pushnew (&environment env item place &rest rest)
  "Syntax: (pushnew form place {keyword-form value-form}*)
Evaluates FORM first.  If the value is already in the list stored in PLACE,
does nothing.  Else, conses the value onto the list and makes the result the
new value of PLACE.  Returns NIL.  KEYWORD-FORMs and VALUE-FORMs are used to
check if the value of FORM is already in PLACE as if their values are passed
to MEMBER."
  (declare (notinline mapcar))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (when (trivial-setf-form place vars stores store-form access-form)
      (return-from pushnew `(setq ,place (adjoin ,item ,place ,@rest))))
    ;; The item to be pushed has to be evaluated before the destination
    (unless (constantp item env)
      (setq vals (cons item vals)
            item (gensym)
            vars (cons item vars)))
    `(let* ,(mapcar #'list
                    (append vars stores)
                    (append vals
                            (list (list* 'adjoin item access-form rest))))
       (declare (:read-only ,@vars)) ; Beppe
       ,store-form)))

(defmacro pop (&environment env place)
  "Syntax: (pop place)
Gets the cdr of the value stored in PLACE and makes it the new value of PLACE.
Returns the car of the old value in PLACE."
  (declare (notinline mapcar))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (let ((store-var (first stores)))
      `(let* ,(mapcar #'list
                      (append vars stores)
                      (append vals (list access-form)))
         (declare (:read-only ,@vars)) ; Beppe
         (prog1 (car ,store-var)
           (setq ,store-var (cdr (truly-the list ,store-var)))
           ,store-form)))))

(define-setf-expander values (&rest values &environment env)
  (let ((all-vars '())
        (all-vals '())
        (all-stores '())
        (all-storing-forms '())
        (all-get-forms '()))
    (dolist (item (reverse values))
      (multiple-value-bind (vars vals stores storing-form get-form)
          (get-setf-expansion item env)
        ;; If a place has more than one store variable, the other ones
        ;; are set to nil.
        (let ((extra (rest stores)))
          (unless (endp extra)
            (setf vars (append extra vars)
                  vals (append (make-list (length extra)) vals)
                  stores (list (first stores)))))
        (setf all-vars (append vars all-vars)
              all-vals (append vals all-vals)
              all-stores (append stores all-stores)
              all-storing-forms (cons storing-form all-storing-forms)
              all-get-forms (cons get-form all-get-forms))))
    (values all-vars all-vals all-stores `(values ,@all-storing-forms)
            `(values ,@all-get-forms))))
#|
;;; Proposed extension:
; Expansion of (SETF (VALUES place1 ... placek) form)
; --> (MULTIPLE-VALUE-BIND (dummy1 ... dummyk) form
;       (SETF place1 dummy1 ... placek dummyk)
;       (VALUES dummy1 ... dummyk))
(define-setf-expander VALUES (&environment env &rest subplaces)
  (do ((temps) (vals) (stores)
       (storeforms) (accessforms)
       (placesr subplaces))
      ((atom placesr)
       (setq temps (nreverse temps)
             vals (nreverse vals)
             stores (nreverse stores)
             storeforms (nreverse storeforms)
             accessforms (nreverse accessforms))
       (values temps
            vals
            stores
            `(VALUES ,@storeforms)
            `(VALUES ,@accessforms)))
    (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
        (get-setf-expansion (pop placesr) env)
      (setq temps (revappend SM1 temps)
            vals (revappend SM2 vals)
            stores (revappend SM3 stores)
            storeforms (cons SM4 storeforms)
            accessforms (cons SM5 accessforms)))))
|#
