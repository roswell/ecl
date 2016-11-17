;;;; ecl-tests.lisp

(defpackage #:cl-test
  (:use #:cl #:2am-ecl))

(in-package #:cl-test)


;;; Set pathnames
(defparameter *aux-dir*
  (merge-pathnames
   "auxiliary/"
   (make-pathname :directory (pathname-directory
                              (asdf:system-definition-pathname 'ecl-tests)))))

(defparameter *tmp-dir*
  (merge-pathnames
   "temporary/"
   (make-pathname :directory (pathname-directory *default-pathname-defaults*))))


;;;; Declare the suites
(suite 'ecl-tests
       '(eformat ieee-fp eprocess package-locks ansi+ mixed cmp emb ffi mop mp))

(suite 'make-check
       '(ieee-fp eprocess package-locks ansi+ mixed cmp emb ffi mop))


;;; Some syntactic sugar for 2am
(defmacro once-only (specs &body body)
  "Once-Only ({(Var Value-Expression)}*) Form*

  Create a Let* which evaluates each Value-Expression, binding a
  temporary variable to the result, and wrapping the Let* around the
  result of the evaluation of Body.  Within the body, each Var is
  bound to the corresponding temporary variable."
  (labels ((frob (specs body)
             (if (null specs)
                 `(progn ,@body)
                 (let ((spec (first specs)))
                   (when (/= (length spec) 2)
                     (error "Malformed Once-Only binding spec: ~S." spec))
                   (let ((name (first spec))
                         (exp-temp (gensym)))
                     `(let ((,exp-temp ,(second spec))
                            (,name (gensym "OO-")))
                        `(let ((,,name ,,exp-temp))
                           ,,(frob (rest specs) body))))))))
    (frob specs body)))

(defmacro is-true (form)
  (once-only ((result form))
    `(is (eql ,result t) "Expected T, but got ~s" ,result)))

(defmacro is-false (form)
  (once-only ((result form))
    `(is (null ,result) "Expected NIL, but got ~s" ,result)))

(defmacro is-equal (what form)
  (once-only ((what what)
              (form form))
    `(is (equal ,what ,form) "EQUAL: ~s to ~s" ',form ,what ,form)))

(defmacro is-eql (what form)
  (once-only ((what what)
              (form form))
    `(is (eql ,what ,form) "EQL: ~s to ~s" ,what ,form)))

(defmacro pass (form &rest args)
  (declare (ignore form args))
  `(passed))

(defmacro fail (form &rest args
                &aux
                  (fmt-ctrl (or (car args) ""))
                  (fmt-args (cdr args)))
  (declare (ignore form))
  `(failed (make-condition 'test-failure
                           :name *test-name*
                           :format-control ,fmt-ctrl
                           :format-arguments (list ,@fmt-args))))


;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Tools for doing tests, intercepting functions, etc.

(defmacro with-dflet (functions &body body)
  "Syntax:
        (with-dflet ((fname form*)*) body)
Evaluate BODY in an environment in which the function FNAME has been
redefined to evaluate the given forms _before_ executing the orginal
code."
  (let ((vars '()) (in-forms '()) (out-forms '()))
    (loop for (name . forms) in functions
          do (let ((var (gensym)))
               (push `(,var #',name) vars)
               (push `(setf (fdefinition ',name)
                       #'(lambda (&rest args) ,@forms (apply ,var args)))
                     in-forms)
               (push `(setf (fdefinition ',name) ,var) out-forms)))
    `(let ,vars
      (unwind-protect
           (progn ,@in-forms ,@body)
        (progn ,@out-forms)))))

(defmacro with-compiler ((filename &rest compiler-args) &body forms)
  "Create a lisp file with the given forms and compile it. The forms
are evaluated unless they are strings. Strings are simply inlined to
allow using reader macros. The output is stored in a string and output
as a second value."
  `(progn
     (with-open-file (s ,filename :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
       ,@(loop for f in forms collect (if (stringp f)
                                          `(format s "~A" ,f)
                                          `(print ,f s))))
     (let* ((compiled-file t)
            (output
             (with-output-to-string (*standard-output*)
               (let ((*error-output* *standard-output*)
                     (*compile-verbose* t)
                     (*compile-print* t))
                 (setf compiled-file (compile-file ,filename ,@compiler-args))))))
       (values compiled-file output))))
