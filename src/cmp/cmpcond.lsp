
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See file 'LICENSE' for the copyright details.

(in-package #:compiler)

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
  (when *compiler-break-enable*
    (invoke-debugger c))
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

(defun too-many-args (name upper-bound n &aux (*print-case* :upcase))
  (cmperr "~S requires at most ~R argument~:p, but ~R ~:*~[were~;was~:;were~] supplied.~%"
          name upper-bound n))

(defun too-few-args (name lower-bound n)
  (cmperr "~S requires at least ~R argument~:p, but only ~R ~:*~[were~;was~:;were~] supplied.~%"
          name lower-bound n))

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

(defun undefined-variable (sym)
  (do-cmpwarn 'compiler-undefined-variable :name sym))

(defun baboon (&key (format-control "A bug was found in the compiler")
               format-arguments)
  (signal 'compiler-internal-error
          :format-control format-control
          :format-arguments format-arguments))

;;; This is not used (left for debugging).
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
