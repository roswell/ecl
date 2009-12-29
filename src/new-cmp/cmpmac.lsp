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

(defmacro safe-compile ()
  `(>= (cmp-env-optimization 'safety) 2))

(defmacro compiler-check-args ()
  `(>= (cmp-env-optimization 'safety) 1))

(defmacro compiler-push-events ()
  `(>= (cmp-env-optimization 'safety) 3))
