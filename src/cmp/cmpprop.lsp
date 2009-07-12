;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
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

;;;; CMPPROP Type propagation.

(in-package "COMPILER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TYPE PROPAGATION LOOP
;;;
;;;
;;; ALL C1FORMS: Intermediate language used by the compiler
;;;
;;;	(LOCATION	loc)
;;;	(VAR		var)
;;;	(SETQ		var value-c1form)
;;;	(PSETQ		var-list value-c1form-list)
;;;	(BLOCK		blk-var progn-c1form)
;;;	(PROGN		body)
;;;	(TAGBODY	tag-var tag-body)
;;;	(DECL-BODY	declaration-list progn-c1form)
;;;	(RETURN-FROM	blk-var return-type value)
;;;	(FUNCALL	fun-value (arg-value*))
;;;	(CALL-LOCAL	obj-fun (arg-value*))
;;;	(CALL-GLOBAL	fun-name (arg-value*))
;;;	(CATCH		catch-value body)
;;;	(UNWIND-PROTECT	protected-c1form body)
;;;	(THROW		catch-value output-value)
;;;	(GO		tag-var return-type)
;;;	(C-INLINE	(arg-c1form*)
;;;			(arg-type-symbol*)
;;;			output-rep-type
;;;			c-expression-string
;;;			side-effects-p
;;;			one-liner-p)
;;;	(LOCALS		local-fun-list body labels-p)
;;;	(IF		fmla-c1form true-c1form false-c1form)
;;;	(FMLA-NOT	fmla-c1form)
;;;	(LAMBDA		lambda-list doc body-c1form)
;;;	(LET		vars-list var-init-c1form-list decl-body-c1form)
;;;	(LET*		vars-list var-init-c1form-list decl-body-c1form)
;;;	(VALUES		values-c1form-list)
;;;	(MULTIPLE-VALUE-SETQ vars-list values-c1form-list)
;;;	(MULTIPLE-VALUE-BIND vars-list init-c1form body)
;;;	(COMPILER-LET	symbols values body)
;;;	(FUNCTION	{GLOBAL|CLOSURE} lambda-form fun-object)
;;;
;;;	(C2PRINC	object-string-or-char stream-var stream-c1form)
;;;	(RPLACA		dest-c1form value-c1form)
;;;	(RPLACD		dest-c1form value-c1form)
;;;	(MEMBER!2	fun-symbol args-c1form-list)
;;;	(ASSOC!2	fun-symbol args-c1form-list)
;;;
;;;	(SI:STRUCTURE-REF struct-c1form type-name slot-index {:UNSAFE|NIL})
;;;	(SI:STRUCTURE-SET struct-c1form type-name slot-index value-c1form)
;;;
;;;	(WITH-STACK	body)
;;;	(STACK-PUSH-VALUES value-c1form push-statement-c1form)
;;;
;;;	(ORDINARY	c1form)
;;;	(LOAD-TIME-VALUE dest-loc value-c1form)
;;;	(FSET		function-object vv-loc, macro-p pprint-p lambda-form)
;v;;	(MAKE-FORM	vv-loc value-c1form)
;;;	(INIT-FORM	vv-loc value-c1form)
;;;
;;;	body =		(c1form*)
;;;	tag-body =	({c1form | tag}*)
;;;	return-type =	{CLB | CCB | UNWIND-PROTECT}
;;;	*value =	c1form
;;;	lambda-list = 	(requireds optionals rest key-flag keywords allow-other-keys)
;;;
;;;

(defvar *type-propagation-messages* t)

(eval-when (eval compile)
  (defmacro prop-message (&rest args)
    `(when *type-propagation-messages*
       (format *standard-output* ,@args))))

(defun p1propagate (form assumptions)
  (let* ((name (c1form-name form))
         (type (c1form-type form))
         propagator)
    (cond ((eq name 'VAR)
           (let* ((var (c1form-arg 0 form))
                  (record (assoc var assumptions)))
             (when record
               (setf type (type-and (cdr record) (values-type-primary-type type))))
             (prop-message "~&;;; Querying variable ~A gives ~A" (var-name var) type)
             (values (setf (c1form-type form) type) assumptions)))
          ((setf propagator (get-sysprop name 'p1propagate))
           (prop-message "~&;;; Entering type propagation for ~A" name)
           (multiple-value-bind (type assumptions)
               (apply propagator form assumptions (c1form-args form))
             (prop-message "~&;;; Propagating ~A gives type ~A" name type)
             (values (setf (c1form-type form) (values-type-and (c1form-type form) type))
                     assumptions)))
          (t
           (prop-message "~&;;; Refusing to propagate ~A" name type)
           (values (c1form-type form) assumptions)))))

(defun p1propagate-list (list assumptions)
  (loop with final-type = t
     for f in list
     do (multiple-value-setq (final-type assumptions) (p1propagate f assumptions))
     finally (return (values final-type assumptions))))

(defun print-assumptions (message assumptions &optional (always-p t))
  (when (and always-p (null assumptions))
    (prop-message "~&;;; ~A: NIL" message))
  (when assumptions
    (prop-message "~&;;; ~A:" message))
    (dolist (record assumptions)
      (prop-message "~&;;; ~A : ~A" (var-name (car record)) (cdr record))))

(defun p1merge-branches (root chains)
  "ROOT is a list of assumptions, while CHAINS is list of extended versions of
ROOT. This function takes all those extensions and makes a final list in which
type assumptions have been merged, giving the variables the OR type of each
of the occurrences in those lists."
  ;; First the simple case in which we only have one list.
  (when (null (rest chains))
    (setf root (first chains))
    (print-assumptions "Only one branch" root)
    (return-from p1merge-branches root))
  ;; When we have to merge more than one list, we use a hash table in which
  ;; we push all possible assumptions, merging the types with TYPE-OR.
  (let* ((all-new-variables (make-hash-table))
         (scanned (make-hash-table)))
    (print-assumptions "Root branch" root t)
    (dolist (l chains)
      (print-assumptions "Extra branch" (ldiff l root)))
    ;; The first pass is filling the hash with unequal assumptions
    ;; mergin the types
    (loop for c in chains
       do (clrhash scanned)
       do (loop for list on c
             for record = (first list)
             until (eq list root)
             do (let* ((var (car record))
                       (type (cdr record)))
                  (unless (gethash var scanned)
                    (setf (gethash var scanned) type)
                    (let ((other-type (gethash var all-new-variables :missing)))
                      (unless (eq other-type :missing)
                        (setf type (type-or type other-type)))
                      (setf (gethash var all-new-variables) type))))))
    ;; While the last pass is extending the list of assumptions with
    ;; the merged ones.
    (loop with new-root = root
       for var being the hash-key in all-new-variables
       using (hash-value type)
       do (setf new-root (acons var type new-root))
       finally (progn
                 (print-assumptions "Output branch" new-root)
                 (return new-root)))))

(defun revise-var-type (variable assumptions where-to-stop)
  (unless (member (var-kind variable)
                  '(LEXICAL CLOSURE SPECIAL GLOBAL REPLACED) :test #'eql)
    (do* ((l assumptions (cdr l))
          (variable-type nil))
         ((or (null l) (eq l where-to-stop))
          (prop-message "~&;;; Changing type of variable ~A to ~A"
                        (var-name variable) variable-type)
          (unless variable-type
            (error "Variable ~A not found" (var-name variable)))
          (setf (var-type variable) variable-type
                (var-kind variable) (lisp-type->rep-type variable-type)))
      (let ((record (first l)))
        (print (list record (eql (car record) variable)))
        (when (eql (car record) variable)
          (let ((one-type (cdr record)))
            (setf variable-type (if variable-type
                                    (type-or variable-type one-type)
                                    one-type))))))))

(defun p1expand-assumptions (var type assumptions)
  (unless (member (var-kind var) '(LEXICAL CLOSURE SPECIAL GLOBAL REPLACED))
    (prop-message "~&;;; Adding variable ~A with type ~A" (var-name var) type)
    (unless (or (var-set-nodes var) (var-functions-setting var))
      (prop-message "~&;;; Changing type of read-only variable ~A" (var-name var))
      (setf (var-type var) type (var-kind var) (lisp-type->rep-type type)))
    (setf assumptions (acons var type assumptions))))

(defun p1expand-many (var type assumptions)
  (loop for v in var
     for v-t in type
     do (setf assumptions (p1expand-assumptions v v-t assumptions)))
  assumptions)

#+nil
(trace c::p1propagate  c::p1progate-list c::p1expand-assumptions
       c::p1call-global)

(defun p1block (c1form assumptions blk body)
  (multiple-value-bind (normal-type assumptions)
      (p1propagate body assumptions)
    (values (type-or (blk-type blk) normal-type)
            assumptions)))

(defun p1call-global (c1form assumptions fname args &optional (return-type t))
  (loop for v in args
     do (multiple-value-bind (arg-type local-ass)
            (p1propagate v assumptions)
          (setf assumptions local-ass))
     finally (let ((type (propagate-types fname args nil)))
               (prop-message "~&;;; Computing output of function ~A with args~&;;;  ~{ ~A~}~&;;; gives ~A, while before ~A"
                       fname (mapcar #'c1form-type args) type (c1form-type c1form))
               (return (values type assumptions)))))

(defun p1catch (c1form assumptions tag body)
  (multiple-value-bind (tag-type assumptions)
      (p1propagate tag assumptions)
    (p1propagate-list body assumptions))
  (values t assumptions))

(defun p1decl-body (c1form assumptions decls body)
  (p1propagate body assumptions))

(defun p1if (c1form assumptions fmla true-branch false-branch)
  (multiple-value-bind (fmla-type base-assumptions)
      (p1propagate fmla assumptions)
    (multiple-value-bind (t1 a1)
        (p1propagate true-branch base-assumptions)
      (multiple-value-bind (t2 a2)
          (p1propagate false-branch base-assumptions)
        (values (type-or t1 t2) (p1merge-branches base-assumptions (list a1 a2)))))))

(defun p1lambda (c1form assumptions lambda-list doc body &rest not-used)
  (prop-message "~&;;;~&;;; Propagating function~&;;;")
  (let ((type (p1propagate body assumptions)))
    (values type assumptions)))

(defun p1let (c1form base-assumptions vars forms body)
  (let ((new-assumptions base-assumptions))
    (loop for v in vars
       for f in forms
       do (multiple-value-bind (type ass)
              (p1propagate f base-assumptions)
            (setf new-assumptions (p1expand-assumptions v type new-assumptions))))
    (multiple-value-bind (type assumptions)
        (p1propagate body new-assumptions)
      (loop for v in vars
         do (revise-var-type v assumptions base-assumptions))
      (values (setf (c1form-type c1form) type)
              assumptions))))

(defun p1let* (c1form base-assumptions vars forms body)
  (let ((assumptions base-assumptions))
    (loop for v in vars
       for f in forms
       do (multiple-value-bind (type ass)
              (p1propagate f assumptions)
            (setf assumptions (p1expand-assumptions v type assumptions))))
    (multiple-value-bind (type assumptions)
        (p1propagate body assumptions)
      (loop for v in vars
         do (revise-var-type v assumptions base-assumptions))
      (values (setf (c1form-type c1form) type)
              assumptions))))

(defun p1locals (c1form assumptions funs body labels)
  (loop for f in funs
     do (p1propagate funs assumptions))
  (p1propagate-list body assumptions))

(defun p1multiple-value-bind (c1form assumptions vars-list init-c1form body)
  (multiple-value-bind (init-form-type assumptions)
      (p1propagate init-c1form assumptions)
    (let ((new-types (values-type-to-n-types init-form-type (length vars-list))))
      (p1propagate body (p1expand-many vars-list new-types assumptions)))))

(defun p1multiple-value-setq (c1form assumptions vars-list value-c1form)
  (multiple-value-bind (init-form-type assumptions)
      (p1propagate value-c1form assumptions)
    (let ((new-types (values-type-to-n-types init-form-type (length vars-list))))
      (values init-form-type (p1expand-many vars-list new-types assumptions)))))

(defun p1progn (c1form assumptions forms)
  (p1propagate-list forms assumptions))

(defun p1setq (c1form assumptions var c1form)
  (multiple-value-bind (value-type assumptions)
      (p1propagate c1form assumptions)
    (let ((type (type-and (var-type var) (values-type-primary-type value-type))))
      (values type (p1expand-assumptions var type assumptions)))))

(defvar *tagbody-depth* -1
  "If n > 0, limit the number of passes to converge tagbody forms. If
-1, let the compiler do as many passes as it wishes. Complexity grows
as 2^*tagbody-limit* in the worst cases.")

(defun p1tagbody (c1form assumptions tag-loc body)
  (let ((*tagbody-depth* *tagbody-depth*))
    (cond ((zerop *tagbody-depth*)
           (p1tagbody-simple c1form assumptions tag-loc body))
          (t
           (setf *tagbody-depth* (1- *tagbody-depth*))
           (p1tagbody-many-passes c1form assumptions tag-loc body)))))

(defun filter-only-declarations (assumptions)
  nil)

(defun p1tagbody-one-pass (c1form assumptions tag-loc body)
  (loop with local-ass = assumptions
     with ass-list = '()
     for f in body
     do (if (tag-p f)
            (let ((diff (ldiff local-ass assumptions)))
              (when diff
                (push diff ass-list))
              (prop-message "~&;;; Label ~A found" (tag-name f))
              (setf local-ass assumptions))
            (multiple-value-setq (aux local-ass) (p1propagate f local-ass)))
     finally (return
               (let ((diff (ldiff local-ass assumptions)))
                 (if diff
                     (cons diff ass-list)
                     ass-list)))))

(defun p1tagbody-simple (c1form orig-assumptions tag-loc body)
  (prop-message "~&;;; P1TAGBODY-SIMPLE pass")
  (print-assumptions "Orig assumptions:" orig-assumptions)
  (let* ((assumptions (filter-only-declarations orig-assumptions))
         (ass-list (p1tagbody-one-pass c1form assumptions tag-loc body)))
    (values 'null (append (p1merge-branches nil ass-list) orig-assumptions))))

(defun p1tagbody-many-passes (c1form orig-assumptions tag-loc body)
  (loop with orig-ass-list = '()
     with assumptions = orig-assumptions
     for i from 0 below 3
     for foo = (prop-message "~&;;; P1TAGBODY-MANY-PASSES pass ~D" i)
     for ass-list = (p1tagbody-one-pass c1form assumptions tag-loc body)
     for faa = (progn
                 (print-assumptions "Old tagbody assumptions" assumptions)
                 (pprint ass-list))
     for new-assumptions = (nconc (p1merge-branches nil ass-list) orig-assumptions)
     for fee = (print-assumptions "New tagbody assumptions" new-assumptions)
     for end = (equalp assumptions (setf assumptions new-assumptions))
     until end
     finally (cond (end
                    (prop-message "~&;;; P1TAGBODY-MANY-PASSES exists at ~D" i)
                    (return (values 'null assumptions)))
                   (t
                    (prop-message "~&;;; P1TAGBODY-MANY-PASSES refuses at ~D" i)
                    (p1tagbody-simple c1form orig-assumptions tag-loc body)))))

(defun p1unwind-protect (c1form assumptions form body)
  (multiple-value-bind (output-type assumptions)
      (p1propagate form assumptions)
    (p1propagate-list body assumptions)
    (values output-type assumptions)))

(put-sysprop 'BLOCK 'P1PROPAGATE 'p1block)
(put-sysprop 'call-global 'p1propagate 'p1call-global)
(put-sysprop 'CATCH 'P1PROPAGATE 'p1catch)
(put-sysprop 'decl-body 'p1propagate 'p1decl-body)
(put-sysprop 'if 'p1propagate #'p1if)
(put-sysprop 'LAMBDA 'P1PROPAGATE 'p1lambda)
(put-sysprop 'LET 'P1PROPAGATE 'p1let)
(put-sysprop 'LET* 'P1PROPAGATE 'p1let*)
(put-sysprop 'LOCALS 'p1propagate 'p1locals)
(put-sysprop 'MULTIPLE-VALUE-BIND 'p1propagate 'p1multiple-value-bind)
(put-sysprop 'MULTIPLE-VALUE-SETQ 'p1propagate 'p1multiple-value-setq)
(put-sysprop 'PROGN 'P1PROPAGATE 'p1progn)
(put-sysprop 'SETQ 'p1propagate 'p1setq)
(put-sysprop 'tagbody 'p1propagate 'p1tagbody)
(put-sysprop 'UNWIND-PROTECT 'P1PROPAGATE 'p1unwind-protect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun type-from-array-elt (array)
  "Input is a lisp type representing a valid subtype of ARRAY. Output is
either the array element type or NIL, denoting that we are not able to
compute it. This version only handles the simplest cases."
  (values (cond ((eq array 'string)
                 'character)
                ((eq array 'base-string)
                 'base-char)
                ((member array '(array vector simple-vector simple-array))
                 t)
                ((atom array)
                 (setf array 'array))
                ((not (member (first array) 
                              '(array vector simple-vector simple-array)))
                 (setf array 'array))
                ((null (rest array))
                 t)
                (t
                 (second array)))
          array))

(defun get-constant-value (form default)
  (if (constantp form)
      (cmp-eval form)
      default))

(def-type-propagator si::aset (fname obj array-type &rest indices)
  (multiple-value-bind (elt-type array-type)
      (type-from-array-elt array-type)
    (values (list* elt-type array-type
                   (make-list (length indices) :initial-element 'si::index))
            elt-type)))

(def-type-propagator aref (fname array-type &rest indices)
  (multiple-value-bind (elt-type array-type)
      (type-from-array-elt array-type)
    (values (list* array-type (make-list (length indices)
                                         :initial-element 'si::index))
            elt-type)))

(define-compiler-macro make-array (&whole form dimensions
                                          &key (element-type t)
                                          (initial-element nil initial-element-supplied-p)
                                          (initial-contents nil initial-contents-supplied-p)
                                          adjustable fill-pointer
                                          displaced-to (displaced-index-offset 0))
  (let* ((type (if (or (get-constant-value adjustable t)
                       (get-constant-value fill-pointer t)
                       (get-constant-value displaced-to t))
                   'array
                   'simple-array))
         (upgraded-type (get-constant-value element-type '*))
         (guess-dims (get-constant-value dimensions '*))
         (form (list 'si::make-pure-array element-type dimensions adjustable
                     fill-pointer displaced-to displaced-index-offset)))
    (unless (eq upgraded-type '*)
      ;; Known type?
      (if (nth-value 1 (subtypep t upgraded-type))
          (setf upgraded-type (upgraded-array-element-type upgraded-type))
          (cmpnote "Unknown element type ~A passed to MAKE-ARRAY" upgraded-type)))
    (unless (eq guess-dims '*)
      (if (listp guess-dims)
          (setf guess-dims (make-list (length guess-dims) :initial-element '*))
          (setf guess-dims '(*))))
    (setf type (list type upgraded-type guess-dims))
    (cond (initial-element-supplied-p
           (when initial-contents-supplied-p
             (cmpwarn "In MAKE-ARRAY, both :INITIAL-ELEMENT and :INITIAL-CONTENTS were supplied."))
           (setf form `(si::fill-array-with-elt ,form ,initial-element 0 nil)))
          (initial-contents-supplied-p
           (setf form `(si::fill-array-with-seq ,form ,initial-contents))))
    `(the ,type ,form)))

