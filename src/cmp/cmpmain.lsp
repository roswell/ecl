;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll
;;;;  Copyright (c) 2015, Daniel Kochmański
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPMAIN  Compiler main program.

(ext:package-lock "CL" nil)

(in-package "COMPILER")

(defun compile-file-pathname (name &key
                                     (output-file T)
                                     (type nil type-supplied-p)
                                     (system-p nil)
                              &allow-other-keys)
  (let* ((format '())
         (extension '()))
    (unless type-supplied-p
      (setf type (if system-p :object :fasl)))
    (case type
      ((:shared-library :dll) (setf format +shared-library-format+))
      ((:static-library :library :lib) (setf format +static-library-format+))
      (:data (setf extension "data"))
      (:sdata (setf extension "sdat"))
      (:c (setf extension (if (member :cxx-core *features*) "cxx" "c")))
      (:h (setf extension "eclh"))
      (:object (setf extension +object-file-extension+))
      (:program (setf format +executable-file-format+))
      #+msvc
      (:import-library (setf extension "implib"))
      (:precompiled-header (setf format #-msvc "~a.h.gch" #+msvc "~a.pch"))
      ((:fasl :fas) (setf extension "fas")))
    (cond ((not (member output-file '(T NIL)))
           output-file)
          (format
           (merge-pathnames (format nil format (pathname-name name)) name))
          (t
           (make-pathname :type extension :defaults name)))))

(defun compile-file
    (input-pathname &rest args
     &key
       ((:verbose *compile-verbose*) *compile-verbose*)
       ((:print *compile-print*) *compile-print*)
       (source-truename nil)
       (source-offset 0)
       (c-file nil)
       (h-file nil)
       (data-file nil)
       (system-p nil)
       (load nil)
       (external-format :default)
       output-file
     &aux
       (*standard-output* *standard-output*)
       (*error-output* *error-output*)
       (*compiler-in-use* *compiler-in-use*)
       (*package* *package*)
       (*readtable* *readtable*)
       (*print-pretty* nil)
       (*compile-file-pathname* nil)
       (*compile-file-truename* nil)
       (ext:*source-location* (cons source-truename 0))
       (*suppress-compiler-messages* (or *suppress-compiler-messages*
                                         (not *compile-verbose*))))
  (declare (notinline compiler-cc)
           (ignorable c-file h-file data-file))
  "Compiles the file specified by INPUT-PATHNAME and generates a fasl file
specified by OUTPUT-FILE.  If the filetype is not specified in INPUT-PATHNAME,
then \".lsp\" is used as the default file type for the source file.  LOAD
specifies whether to load the generated fasl file after compilation.  The
:O-FILE, :C-FILE, :H-FILE, and :DATA-FILE keyword parameters allow you to
control the intermediate files generated by the ECL compiler.If the file was
compiled successfully, returns the pathname of the compiled file."
  #-dlopen
  (unless system-p
    (format t "~%;;;~
~%;;; This system does not support loading dynamically linked libraries.~
~%;;; Therefore, COMPILE-FILE without :SYSTEM-P T is unsupported.~
~%;;;"))
  (setq *compile-file-pathname* (pathname (merge-pathnames input-pathname)))
  (unless (probe-file *compile-file-pathname*)
    (if (pathname-type input-pathname)
        (error 'file-error :pathname input-pathname)
        (dolist (ext '("lsp" "LSP" "lisp" "LISP")
                     (error 'file-error :pathname input-pathname))
          (setq *compile-file-pathname* (make-pathname :type ext :defaults input-pathname))
          (when (probe-file *compile-file-pathname*)
            (return)))))
  (when (and system-p load)
    (error "Cannot load system files."))
  (cmpprogress "~&;;;~%;;; Compiling ~a." (namestring input-pathname))
  (let* ((input-file (truename *compile-file-pathname*))
         (*compile-file-truename* input-file)
         (*compiler-in-use* *compiler-in-use*)
         (*load-time-values* nil)       ; Load time values are compiled.
         (output-file (apply #'compile-file-pathname input-file :output-file output-file args))
         (true-output-file nil)         ; Will be set at the end.
         (compiler-conditions nil))
    (with-compiler-env (compiler-conditions)
      (print-compiler-info)
      (when (probe-file "./cmpinit.lsp")
        (load "./cmpinit.lsp" :verbose *compile-verbose*))
      (with-open-file (stream *compile-file-pathname* :external-format external-format)
        (unless source-truename
          (setf (car ext:*source-location*) *compile-file-pathname*))
        (compiler-pass1 stream source-offset))
      (compiler-pass/propagate-types)
      (apply #'compiler-pass/assemble-cxx input-file output-file args)
      (if (setf true-output-file (probe-file output-file))
          (cmpprogress "~&;;; Finished compiling ~a.~%;;;~%" (namestring input-pathname))
          (cmperr "The C compiler failed to compile the intermediate file."))
      (when load
        (load true-output-file :verbose *compile-verbose*))) ; with-compiler-env
    (compiler-output-values true-output-file compiler-conditions)))

(defun compiler-output-values (main-value conditions)
  (loop for i in conditions
        with warning-p = nil
        with failure-p = nil
        do (cond ((typep i 'style-warning)
                  (setf warning-p t))
                 ((typep i '(or compiler-error warning))
                  (setf warning-p t failure-p t)))
        finally (return (values main-value warning-p failure-p))))

#-dlopen
(defun compile (name &optional (def nil supplied-p))
  (format t "~%;;;~
~%;;; This system does not support loading dynamically linked libraries.~
~%;;; Therefore, COMPILE is unsupported.~
~%;;;"))

#+dlopen
(defvar *gazonk-counter* 0)

#+dlopen
(defun compile (name &optional (def nil supplied-p)
                &aux form data-pathname
                  (lexenv nil)
                  (*suppress-compiler-messages* (or *suppress-compiler-messages*
                                                    (not *compile-verbose*)))
                  (*compiler-in-use* *compiler-in-use*)
                  (*standard-output* *standard-output*)
                  (*error-output* *error-output*)
                  (*package* *package*)
                  (*compile-print* nil)
                  (*print-pretty* nil)
                  (si:*compiler-constants* t))
  "Args: (name &optional definition)

If DEFINITION is NIL, NAME must be the name of a not-yet-compiled function.
In this case, COMPILE compiles the function, installs the compiled function as
the global function definition of NAME, and returns NAME.  If DEFINITION is
non-NIL, it must be a lambda expression and NAME must be a symbol.  COMPILE
compiles the lambda expression, installs the compiled function as the function
definition of NAME, and returns NAME.  There is only one exception for this:
If NAME is NIL, then the compiled function is not installed but is simply
returned as the value of COMPILE.  In any case, COMPILE creates temporary
files, whose filenames begin with \"gazonk\", which are automatically deleted
after compilation."
  (unless (si:valid-function-name-p name)
    (error "~s is not a valid function name." name))

  (cond ((and supplied-p def)
         (when (functionp def)
           (unless (function-lambda-expression def)
             (return-from compile def))
           (multiple-value-setq (def lexenv)
             (function-lambda-expression def))
           (when (eq lexenv t)
             (warn "COMPILE can not compile C closures")
             (return-from compile (values def t nil))))
         (setq form (if name
                        `(setf (fdefinition ',name) #',def)
                        `(set 'GAZONK #',def))))
        ((not (fboundp name))
         (error "Symbol ~s is unbound." name))
        ((typep (setf def (fdefinition name)) 'standard-generic-function)
         (warn "COMPILE can not compile generic functions yet")
         (return-from compile (values def t nil)))
        ((null (setq form (function-lambda-expression def)))
         (warn "We have lost the original function definition for ~s. Compilation to C failed"
               name)
         (return-from compile (values def t nil)))
        (t
         (setq form `(setf (fdefinition ',name) #',form))))

  (let* ((*load-time-values* 'values) ;; Only the value is kept
         (tmp-names (safe-mkstemp (format nil "TMP:ECL~3,'0x" (incf *gazonk-counter*))))
         (so-pathname (compile-file-pathname (first tmp-names)))
         (compiler-conditions nil)
         (*permanent-data* t)        ; needed for literal objects in closures
         (*cmp-env-root* *cmp-env-root*))

    (with-compiler-env (compiler-conditions)
      (setf form (set-closure-env form lexenv *cmp-env-root*))
      (compiler-pass1 form)
      (compiler-pass/propagate-types)
      (let (#+(or mingw32 msvc cygwin)(*self-destructing-fasl* t))
        (compiler-pass/assemble-cxx nil so-pathname))
      (mapc 'cmp-delete-file tmp-names)
      (cond ((probe-file so-pathname)
             (load so-pathname :verbose nil)
             (cmp-delete-file so-pathname))
            (t
             (setf name nil)
             (set 'GAZONK nil)
             (cmperr "The C compiler failed to compile the intermediate code for ~s." name)))
      ) ; with-compiler-env
    (let ((output (or name (and (boundp 'GAZONK) (symbol-value 'GAZONK))
                      #'(lambda (&rest x)
                          (declare (ignore x))
                          ;; if compilation failed, we return this
                          ;; function which does nothing but resignal
                          ;; the compiler errors we got
                          (loop for c in compiler-conditions
                                if (typep c 'compiler-error)
                                  do (apply #'si::simple-program-error
                                            (simple-condition-format-control c)
                                            (simple-condition-format-arguments c)))))))
      ;; By unsetting GAZONK we avoid spurious references to the
      ;; loaded code.
      (set 'GAZONK nil)
      (si::gc t)
      (compiler-output-values output compiler-conditions))))

(defun disassemble (thing &key (h-file nil) (data-file nil)
                    &aux lexenv disassembled-form
                      (*compiler-in-use* *compiler-in-use*)
                      (*print-pretty* nil))
  "Compiles the form specified by THING and prints the intermediate C language
code for that form.  But does not install the result of compilation.  If THING
is NIL, then the previously DISASSEMBLEd form is re-DISASSEMBLEd.  If THING is
a symbol that names a function not yet compiled, the function definition is
disassembled.  If THING is a lambda expression, it is disassembled as a
function definition.  Otherwise, THING itself is disassembled as a top-level
form.  H-FILE and DATA-FILE specify intermediate files to build a fasl file
from the C language code.  NIL means \"do not create the file\"."
  (when (si:valid-function-name-p thing)
    (setq thing (fdefinition thing)))
  (when (and (functionp thing) (function-lambda-expression thing))
    (multiple-value-setq (thing lexenv)
      (function-lambda-expression thing))
    (when (eq lexenv t)
      (warn "DISASSEMBLE can not disassemble C closures.")
      (return-from disassemble nil)))
  (cond ((null thing))
        ((functionp thing)
         (unless (si:bc-disassemble thing)
           (warn "Cannot disassemble the binary function ~S because I do not have its source code." thing)
           (return-from disassemble nil)))
        ((atom thing)
         (error 'simple-type-error
                :datum thing
                :expected-type '(OR FUNCTION (SATISFIES SI:VALID-FUNCTION-NAME-P))
                :format-control "DISASSEMBLE cannot accept ~A."
                :format-arguments (list thing)))
        ((eq (car thing) 'LAMBDA)
         (setq disassembled-form `(defun gazonk ,@(cdr thing))))
        ((eq (car thing) 'EXT:LAMBDA-BLOCK)
         (setq disassembled-form `(defun ,@(rest thing))))
        (t
         (error 'simple-type-error
                :datum thing
                :expected-type '(OR FUNCTION (SATISFIES SI:VALID-FUNCTION-NAME-P))
                :format-control "DISASSEMBLE cannot accept ~A."
                :format-arguments (list thing))))

  (let* ((null-stream (make-broadcast-stream))
         (*compiler-output1* null-stream)
         (*compiler-output2* (if h-file
                                 (open h-file :direction :output :external-format :default)
                                 null-stream))
         (t3local-fun (symbol-function 'T3LOCAL-FUN))
         (compiler-conditions nil)
         (*cmp-env-root* *cmp-env-root*))
    (with-compiler-env (compiler-conditions)
      (with-cxx-env ()
        (setf disassembled-form (set-closure-env disassembled-form lexenv *cmp-env-root*))
        (unwind-protect
             (progn
               (setf (symbol-function 'T3LOCAL-FUN)
                     #'(lambda (&rest args)
                         (let ((*compiler-output1* *standard-output*))
                           (apply t3local-fun args))))
               (compiler-pass1 disassembled-form)
               (compiler-pass/propagate-types)
               (ctop-write (compute-init-name "foo" :kind :fasl)
                           (if h-file h-file "")
                           (if data-file data-file ""))
               (when data-file
                 (data-c-dump data-file)))
          (setf (symbol-function 'T3LOCAL-FUN) t3local-fun)
          (when h-file (close *compiler-output2*))))))
  nil)

;;; FIXME source-offset and source-truename are used by swanks string
;;; compilation. Revisit if it is truly needed. SBCL deals with that
;;; using WITH-COMPILATION-UNIT macro what seems to be a much better
;;; place to customize the source location. -- jd 2019-11-25
(defun compiler-pass1 (object &optional source-offset)
  (data-init)
  (if (streamp object)
      (do* ((eof '(NIL))
            (*compile-file-position* 0 (file-position object))
            (form (si:read-object-or-ignore object eof)
                  (si:read-object-or-ignore object eof)))
           ((eq form eof))
        (when form
          (setf (cdr ext:*source-location*)
                (+ source-offset *compile-file-position*))
          (t1expr form)))
      (t1expr object))
  (setq *top-level-forms* (nreverse *top-level-forms*))
  (setq *make-forms* (nreverse *make-forms*)))

(defun compiler-pass/propagate-types ()
  ;; Type propagation phase
  (when *do-type-propagation*
    (setq *compiler-phase* 'p1propagate)
    (dolist (form *top-level-forms*)
      (p1propagate form))))

(defun print-compiler-info ()
  (cmpprogress "~&;;; OPTIMIZE levels: Safety=~d, Space=~d, Speed=~d, Debug=~d~%;;;~%"
               *safety* *space* *speed* *debug*))

(defmacro with-compilation-unit (options &rest body)
  (declare (ignore options))
  `(progn ,@body))

(ext:package-lock "CL" t)

(setf *features* (delete :ecl-bytecmp *features*))

(let* ((compile #'compile)
       (disassemble #'disassemble)
       (compile-file #'compile-file)
       (compile-file-pathname #'compile-file-pathname))
  (defun ext:install-c-compiler ()
    (ext:package-lock (find-package :cl) nil)
    (setf *features* (delete :ecl-bytecmp *features*))
    (setf (fdefinition 'disassemble) disassemble
          (fdefinition 'compile) compile
          (fdefinition 'compile-file) compile-file
          (fdefinition 'compile-file-pathname) compile-file-pathname)
    (ext:package-lock (find-package :cl) t)))

(provide 'cmp)
