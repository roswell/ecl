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
(suite 'make-check
       '(executable ieee-fp eprocess package-ext hash-tables ansi+ mixed
         cmp emb ffi mop run-program mp complex wscl #+unicode unicode
         #+clos clos))

(suite 'ecl-tests
       '(make-check eformat))


(defmacro is-true (form)
  (ext:once-only (form)
    `(is (eql ,form t) "Expected T, but got ~s" ,form)))

(defmacro is-false (form)
  (ext:once-only (form)
    `(is (null ,form) "Expected NIL, but got ~s" ,form)))

(defmacro is-equal (what form)
  (ext:once-only (what form)
    `(is (equal ,what ,form) "EQUAL: ~s to ~s" ,what ,form)))

(defmacro is-eql (what form)
  (ext:once-only (what form)
    `(is (eql ,what ,form) "EQL: ~s to ~a" ,what ,form)))

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
       (let ((*print-circle* t)
             (*print-readably* t))
         ,@(loop for f in forms collect (if (stringp f)
                                            `(format s "~A" ,f)
                                            `(print ,f s)))))
     (let* ((compiled-file t)
            (output
             (with-output-to-string (*standard-output*)
               (let ((*error-output* *standard-output*)
                     (*compile-verbose* t)
                     (*compile-print* t))
                 (setf compiled-file (compile-file ,filename ,@compiler-args))))))
       ;; todo: add delete-files flag
       ;; (when delete-files
       ;;   (delete-file filename)
       ;;   (delete-file compiled-file))
       (when (null compiled-file)
         (delete-file ,filename)
         (error "Compiling file ~a failed:~%~a" ,filename output))
       (values compiled-file output))))

(defmacro cmplambda (args &body body)
  `(compile nil '(lambda ,args ,@body)))

(defmacro with-temporary-file ((var string &rest args) &body body)
  (ext:with-unique-names (stream)
    `(let ((,var (ext:mkstemp "ecl-tests")))
       (with-open-file (,stream ,var :direction :output)
         (format ,stream ,string ,@args))
       (multiple-value-prog1 (progn ,@body)
         (delete-file ,var)))))


;;; Approximate equality function
(defun approx= (x y &optional (eps (epsilon x)))
  (or (= x y)
      (<= (abs (/ (- x y) (max (abs x) 1))) eps)))

(defun epsilon (number)
  (etypecase number
    (complex (* 2 (epsilon (realpart number)))) ;; crude
    (short-float short-float-epsilon)
    (single-float single-float-epsilon)
    (double-float double-float-epsilon)
    (long-float long-float-epsilon)
    (rational 0)))

(defun test-C-program (c-code &key capture-output (args '()) (environ '()))
  (ensure-directories-exist "tmp/")
  (with-open-file (s "tmp/ecl-aux.c" :direction :output :if-exists :supersede
				     :if-does-not-exist :create)
    (princ c-code s))
  (c::compiler-cc "tmp/ecl-aux.c" "tmp/ecl-aux.o")
  (c::linker-cc "tmp/ecl-aux.exe" '("tmp/ecl-aux.o"))
  (let ((environment
          (append #+windows (list (format nil "PATH=~a;~a"
                                          (ext:getenv "PATH")
                                          c::*ecl-library-directory*))
                  #+cygwin (list (format nil "PATH=~a:~a"
                                         (ext:getenv "PATH")
                                         c::*ecl-library-directory*))
                  #-(or windows cygwin) (list (format nil "LD_LIBRARY_PATH=~a:~a"
                                                      (ext:getenv "LD_LIBRARY_PATH")
                                                      c::*ecl-library-directory*))
		  environ
                  (ext:environ))))
    (ecase capture-output
      ((nil)
       (multiple-value-bind (stream return-code)
           (si::run-program "tmp/ecl-aux.exe" args
                            :output t :error t
                            :environ environment)
         (declare (ignore stream))
         (zerop return-code)))
      ((string :string)
       (multiple-value-bind (in return-code)
           (si::run-program "tmp/ecl-aux.exe" args :output :stream :error t
						   :environ environment)
	 (values return-code
		 (with-output-to-string (s)
                   (loop with line
			 do (setf line (read-line in nil))
		            (unless line (return))
		            (write-line line s))))))
      ((t forms :forms)
       (do* ((all '())
             (x t)
             (in (si::run-program "tmp/ecl-aux.exe" args :output :stream
							 :environ environment)))
            ((null in) all)
         (setf x (ignore-errors (read in nil nil)))
         (unless x (return all))
         (push x all))))))
