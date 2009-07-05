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

(defun p1propagate (form assumptions)
  (let* ((name (c1form-name form))
         (type (c1form-type form))
         propagator)
    (cond ((eq name 'VAR)
           (let* ((var (c1form-arg 0 form))
                  (record (assoc var assumptions)))
             (when record
               (setf type (type-and (cdr record) type)))
             (format t "~&;;; Querying variable ~A gives ~A" (var-name var) type)
             (values (setf (c1form-type form) type) assumptions)))
          ((setf propagator (get-sysprop name 'p1propagate))
           (multiple-value-bind (type assumptions)
               (apply propagator form assumptions (c1form-args form))
             (format t "~&;;; Propagating ~A gives type ~A" name type)
             (values (setf (c1form-type form) (values-type-and (c1form-type form) type))
                     assumptions)))
          (t
           (format t "~&;;; Refusing to propagate ~A" name type)
           (values (c1form-type form) assumptions)))))

(defun p1propagate-list (list assumptions)
  (loop with final-type = t
     for f in list
     do (multiple-value-setq (final-type assumptions) (p1propagate f assumptions))
     finally (return (values final-type assumptions))))

(defun print-assumptions (message assumptions &optional always-p)
  (when (and always-p (null assumptions))
    (format t "~&;;; ~A: NIL" message))
  (when assumptions
    (format t "~&;;; ~A:" message))
    (dolist (record assumptions)
      (format t "~&;;; ~A : ~A" (var-name (car record)) (cdr record))))

(defun p1merge-branches (root chains)
  (let* ((all-new-variables (make-hash-table))
         (scanned (make-hash-table)))
    (print-assumptions "Root branch" root t)
    (dolist (l chains)
      (print-assumptions "Extra branch" (ldiff l root)))
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
    (loop with new-root = root
       for var being the hash-key in all-new-variables
       using (hash-value type)
       do (setf new-root (acons var type new-root))
       finally (progn
                 (print-assumptions "Output branch" (ldiff new-root root) t)
                 (return new-root)))))

(defun p1expand-assumptions (var type assumptions)
  (unless (member (var-kind var) '(LEXICAL CLOSURE SPECIAL GLOBAL REPLACED))
    (format t "~&;;; Adding variable ~A with type ~A" (var-name var) type)
    (unless (var-functions-setting var)
      (format t "~&;;; Changing type of read-only variable ~A" (var-name var))
      (setf (var-type var) type (var-kind var) (lisp-type->rep-type type)))
    (setf assumptions (acons var type assumptions))))

#+nil
(trace c::p1propagate  c::p1progate-list c::p1expand-assumptions
       c::p1call-global)

(defun p1block (c1form assumptions blk body)
  (multiple-value-bind (normal-type assumptions)
      (p1propagate body assumptions)
    (values (type-or (blk-type blk) normal-type)
            assumptions)))

(defun p1call-global (c1form assumptions fname args &optional (return-type t))
  (print args)
  (loop for v in args
     do (multiple-value-bind (arg-type local-ass)
            (p1propagate v assumptions)
          (setf assumptions local-ass))
     finally (let ((type (propagate-types fname args nil)))
               (format t "~&;;; Computing output of function ~A with args~&;;;  ~{ ~A~}~&;;; gives ~A, while before ~A"
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
  (multiple-value-bind (fmla-type assumptions)
      (p1propagate fmla assumptions)
    (multiple-value-bind (t1 a1)
        (p1propagate true-branch assumptions)
      (multiple-value-bind (t2 a2)
          (p1propagate false-branch assumptions)
        (values (type-or t1 t2) (p1merge-branches assumptions (list a1 a2)))))))

(defun p1lambda (c1form assumptions lambda-list doc body &rest not-used)
  (format t "~&;;;~&;;; Propagating function~&;;;")
  (let ((type (p1propagate body assumptions)))
    (values type assumptions)))

(defun p1let (c1form assumptions vars forms body)
  (let ((new-assumptions assumptions))
    (loop for v in vars
       for f in forms
       do (multiple-value-bind (type ass)
              (p1propagate f assumptions)
            (setf new-assumptions (p1expand-assumptions v type new-assumptions))))
    (multiple-value-bind (type assumptions)
        (p1propagate body new-assumptions)
      (values (setf (c1form-type c1form) type)
              assumptions))))

(defun p1let* (c1form assumptions vars forms body)
  (loop for v in vars
     for f in forms
     do (multiple-value-bind (type ass)
            (p1propagate f assumptions)
          (setf assumptions (p1expand-assumptions v type assumptions))))
  (multiple-value-bind (type assumptions)
      (p1propagate body assumptions)
    (values (setf (c1form-type c1form) type)
            assumptions)))

(defun p1locals (c1form assumptions funs body labels)
  (loop for f in funs
     do (p1propagate funs assumptions))
  (p1propagate-list body assumptions))

(defun p1progn (c1form assumptions forms)
  (p1propagate-list forms assumptions))

(defun p1setq (c1form assumptions var c1form)
  (multiple-value-bind (value-type assumptions)
      (p1propagate c1form assumptions)
    (let ((type (type-and (var-type var) value-type)))
      (values type (p1expand-assumptions var type assumptions)))))

(defun p1tagbody (c1form assumptions tag-loc body)
  (loop with local-ass = assumptions
     with ass-list = '()
     for f in body
     do (if (tag-p f)
            (setf ass-list (cons local-ass ass-list)
                  local-ass assumptions)
            (multiple-value-setq (aux local-ass) (p1propagate f local-ass)))
     finally (return (values 'null (p1merge-branches assumptions ass-list)))))

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
(put-sysprop 'PROGN 'P1PROPAGATE 'p1progn)
(put-sysprop 'SETQ 'p1propagate 'p1setq)
(put-sysprop 'tagbody 'p1propagate 'p1tagbody)
(put-sysprop 'UNWIND-PROTECT 'P1PROPAGATE 'p1unwind-protect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun type-from-array-elt (array)
  "Input is a lisp type representing a valid subtype of ARRAY. Output is
either the array element type or NIL, denoting that we are not able to
compute it. This version only handles the simplest cases."
  (cond ((eq array 'string)
         'character)
        ((eq array 'base-string)
         'base-char)
        ((member array '(array vector simple-vector simple-array))
         t)
        ((atom array)
         nil)
        ((not (member (first array) '(array vector simple-vector simple-array)))
         nil)
        ((null (rest array))
         t)
        (t
         (second array))))

(defun get-constant-value (form default)
  (if (constantp form)
      (cmp-eval form)
      default))

(def-type-propagator si::aset (fname obj array &rest indices)
  (let* ((array-type (c1form-primary-type array))
         (elt-type (or (type-from-array-elt array-type) t)))
    (values (list* elt-type array-type (make-list (length indices) :initial-element 'si::index))
            elt-type)))

(def-type-propagator aref (fname array &rest indices)
  (let* ((array-type (c1form-primary-type array))
         (elt-type (or (type-from-array-elt array-type) t)))
    (values (list* array-type (make-list (length indices) :initial-element 'si::index))
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

