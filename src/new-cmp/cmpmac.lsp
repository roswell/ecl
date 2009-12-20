;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;; ----------------------------------------------------------------------
;;; Macros only used in the code of the compiler itself:

(in-package "COMPILER")
(import 'sys::arglist "COMPILER")

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
                          (EQUAL 'SI::HASH-EQUAL)
                          (t (setf test 'EQUALP) 'SI::HASH-EQUALP)))
         (hash (gensym "HASH")))
    `(progn
       (defparameter ,cache-name (make-array 1024 :element-type t :adjustable nil))
       (defun ,reset-name ()
         (make-array 1024 :element-type t :adjustable nil))
       (defun ,name ,lambda-list
         (flet ((,name ,lambda-list ,@body))
           (let* ((hash (logand (,hash-function ,@lambda-list) 1023))
                  (elt (aref ,cache-name hash)))
             (declare (type (integer 0 1023) hash)
                      (type (array t (*)) ,cache-name))
             (if (and elt ,@(loop for arg in lambda-list
                               collect `(,test (pop (the cons elt)) ,arg)))
                 (first (the cons elt))
                 (let ((output (,name ,@lambda-list)))
                   (setf (aref ,cache-name hash) (list ,@lambda-list output))
                   output))))))))

(defmacro defun-equal-cached (name lambda-list &body body)
  `(defun-cached ,name ,lambda-list equal ,@body))

;;; ----------------------------------------------------------------------
;;; CONVENIENCE FUNCTIONS / MACROS
;;;

(defun-cached env-var-name (n) eql
  (format nil "env~D" n))

(defun-cached lex-env-var-name (n) eql
  (format nil "lex~D" n))

(defun same-fname-p (name1 name2) (equal name1 name2))

(defun next-label () (incf *last-label*))

(defmacro wt-go (label)
  `(wt "goto L" ,label ";"))

;;; from cmplam.lsp
(defmacro ck-spec (condition)
  `(unless ,condition
           (cmperr "The parameter specification ~s is illegal." spec)))

(defmacro ck-vl (condition)
  `(unless ,condition
           (cmperr "The lambda list ~s is illegal." vl)))

;;; fromcmputil.sp
(defmacro cmpck (condition string &rest args)
  `(if ,condition (cmperr ,string ,@args)))

(defmacro cmpassert (condition string &rest args)
  `(unless ,condition (cmperr ,string ,@args)))

;;; from cmpwt.lsp
(defmacro wt (&rest forms &aux (fl nil))
  (dolist (form forms `(progn ,@(nreverse (cons nil fl))))
    (if (stringp form)
        (push `(princ ,form *compiler-output1*) fl)
        (push `(wt1 ,form) fl))))

(defmacro wt-h (&rest forms &aux (fl nil))
  (dolist (form forms `(progn ,@(nreverse (cons nil fl))))
    (if (stringp form)
      (push `(princ ,form *compiler-output2*) fl)
      (push `(wt-h1 ,form) fl))))

(defmacro wt-nl-h (&rest forms)
  `(progn (terpri *compiler-output2*) (wt-h ,@forms)))

(defmacro princ-h (form) `(princ ,form *compiler-output2*))

(defmacro wt-nl (&rest forms)
  `(wt #\Newline #\Tab ,@forms))

(defmacro wt-nl1 (&rest forms)
  `(wt #\Newline ,@forms))

(defmacro safe-compile ()
  `(>= (cmp-env-optimization 'safety) 2))

(defmacro compiler-check-args ()
  `(>= (cmp-env-optimization 'safety) 1))

(defmacro compiler-push-events ()
  `(>= (cmp-env-optimization 'safety) 3))

;; ----------------------------------------------------------------------
;; C1-FORMS
;;

(defstruct (c1form (:include info)
		   (:print-object print-c1form)
		   (:constructor do-make-c1form))
  (name nil)
  (parent nil)
  (args '())
  (env (cmp-env-copy))
  (form nil)
  (toplevel-form)
  (file nil)
  (file-position 0))

(defun print-c1form (form stream)
  (format stream "#<form ~A ~X>" (c1form-name form) (ext::pointer form)))

(defmacro make-c1form* (&rest args)
  `(list (make-c1form-alone ,@args)))

(defmacro make-c1form-alone (name &rest args)
  (let ((info-args '())
	(form-args '()))
    (do ((l args (cdr l)))
	((endp l))
      (let ((key (first l)))
	(cond ((not (keywordp key))
	       (baboon))
	      ((eq key ':args)
	       (setf form-args (rest l))
	       (return))
	      (t
	       (setf info-args (list* key (second l) info-args)
		     l (cdr l))))))
    `(do-make-c1form :name ,name :args (list ,@form-args)
                     :form *current-form*
                     :file *compile-file-truename*
                     :file-position *compile-file-position*
                     ,@info-args)))

(defun make-c1form-alone (name &rest args)
  (let ((info-args '())
	(form-args '()))
    (do ((l args (cdr l)))
	((endp l))
      (let ((key (first l)))
	(cond ((not (keywordp key))
	       (baboon))
	      ((eq key ':args)
	       (setf form-args (rest l))
	       (return))
	      (t
	       (setf info-args (list* key (second l) info-args)
		     l (cdr l))))))
    (apply #'do-make-c1form :name name :args form-args
           :form *current-form*
           :file *compile-file-truename*
           :file-position *compile-file-position*
           info-args)))

(defun copy-c1form (form)
  (copy-structure form))

(defmacro c1form-arg (nth form)
  (case nth
    (0 `(first (c1form-args ,form)))
    (1 `(second (c1form-args ,form)))
    (otherwise `(nth ,nth (c1form-args ,form)))))

(defun c1form-volatile* (form)
  (if (c1form-volatile form) "volatile " ""))

(defun get-output-c1form (form)
  (cond ((null form)
         (error "Empty form list"))
        ((listp form)
         (first (last form)))
        (t
         form)))

(defun c1form-values-type (form)
  (c1form-type (get-output-c1form form)))

(defun (setf c1form-values-type) (type form)
  (setf (c1form-type (get-output-c1form form)) type))

(defun c1form-primary-type (form)
  (values-type-primary-type (c1form-values-type form)))

(defun find-node-in-list (home-node list)
  (flet ((parent-node-p (node presumed-child)
	   (loop
	    (cond ((null presumed-child) (return nil))
		  ((eq node presumed-child) (return t))
		  (t (setf presumed-child (c1form-parent presumed-child)))))))
    (member home-node list :test #'parent-node-p)))

(defun c1form-set-volatile (flag forms)
  (loop for i in forms
     do (setf (c1form-volatile i) flag))
  forms)
