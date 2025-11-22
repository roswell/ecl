;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: CL-TEST-CROSS-COMPILE -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2025, Marius Gerbershagen
;;;;
;;;;    See file 'LICENSE' for the copyright details.

;;; Cross compilation testing framework
;;;
;;; We replace COMPILE-FILE and COMPILE in the ECL running on the
;;; target system by stubs which delegate the compilation to a
;;; remote ECL cross-compiling for the target.
;;;
;;; For now, the "remote" ECL is just running on the same computer
;;; (for instance an x86 version of ECL cross compiling to x86_64).
;;; The code below contains some preparations for true remote
;;; compilation by setting set the environment variable ECL_TO_RUN to
;;; start the remote ECL on a ssh connection (or similar) but this
;;; functionality is incomplete.
;;; 
;;; There are several limitations to the testing framework:
;;; 
;;; 1. Function, macro or variable definitions, proclamations and
;;;    other global state is not fully synchronized between the target
;;;    and remote ECL. To get around this issue, we simply load the
;;;    testsuite in the remote ECL before running it (which takes care
;;;    of most of these issues) and replace other functions like
;;;    PROCLAIM or SI::*MAKE-SPECIAL with stubs that perform their
;;;    function in both the target and remote ECL.
;;;
;;; 2. The COMPILE function is implemented by writing the input out to
;;;    a file, cross-compiling that file on the remote and then
;;;    loading the result on the target. In general, the input cannot
;;;    be written out unmodified to the file because literal objects
;;;    need not be copied during compilation. To deal with this, we
;;;    use the bytecodes code walker to collect literal objects in a
;;;    vector, replacing references to them by load time values.
;;;    Uninterned symbols are treated specially since they can appear
;;;    both as literal objects and as variable or function names,
;;;    making the approach of using load time values difficult. We
;;;    therefore temporarily intern those symbols during compilation.
;;;
;;; This approach is enough to get the testsuite running but will
;;; break in more compilated scenarios. When encountering a test
;;; failure, one should always check if this is due to a limitation of
;;; the testing framework or an actual error.

(defpackage #:cl-test-cross-compile
  (:use #:cl)
  (:export #:setup))

(in-package #:cl-test-cross-compile)

;;; Remote commands

(defvar *remote*)

(let ((last-output ""))
  (defun read-from-remote (remote read-operation)
    (handler-case (with-standard-io-syntax (funcall read-operation (ext:external-process-output remote)))
      (error (e)
        (if (eq (ext:external-process-status remote) :running)
            :unreadable
            (error "Remote ECL exited: ~a" last-output)))))

  (defun skip-forward-to-first-prompt (remote)
    (loop do (setf last-output
                   (let ((c (read-from-remote remote #'read-char)))
                     (unread-char c (ext:external-process-output remote))
                     (if (eq c #\>)
                         c
                         (read-from-remote remote #'read-line))))
          until (eq last-output #\>))))

(defun skip-forward-to-prompt (remote)
  (loop for x = (read-from-remote remote #'read)
        until (eq x :input-prompt)))

(defun send-command (remote command &optional ignore-result)
  (skip-forward-to-prompt remote)
  (with-standard-io-syntax
    (write `(let ((*standard-output* (make-string-output-stream))
                  warnings error result output)
              (handler-bind ((style-warning
                               ;; To keep things simple, just catch
                               ;; style-warnings and do the printing on
                               ;; the remote host to avoid having to
                               ;; deal with potentially unreadable
                               ;; format arguments or complex warnings
                               ;; which are difficult to translate to
                               ;; the host.
                               #'(lambda (w)
                                   (push (list 'c::compiler-style-warning
                                               :format-control (format nil "~a" w))
                                         warnings))))
                (handler-case
                    (setf result (multiple-value-list ,command)
                          output (get-output-stream-string *standard-output*))
                  (serious-condition (cl-user::e)
                    (setf error (format nil "~a" cl-user::e)))))
              (list error warnings result output))
           :stream (ext:external-process-input remote)
           :circle t))
  (terpri (ext:external-process-input remote))
  (let ((all-results (read-from-remote remote #'read))
        error warnings result output)
    (when (eq all-results :unreadable)
      (if ignore-result
          (return-from send-command (values))
          (error "Remote command ~s failed. Can't read back output." command)))
    (unless (and (listp all-results) (= (length all-results) 4))
      (format t "Unexpected output from remote: ~s " all-results)
      (loop do (write-string (read-from-remote remote #'read-line))
               (terpri)))
    (setf error (first all-results)
          warnings (second all-results)
          result (third all-results)
          output (fourth all-results))
    (when error
      (error "Remote command ~s failed. Got error: ~a" command error))
    (mapc #'(lambda (w) (apply #'warn w)) warnings)
    (format t "~a" output) ; echo remote output
    (values-list result)))

(defun copy-file-from-remote (remote file)
  (let ((bytes
         (send-command remote
                       `(with-open-file (s ,file
                                           :direction :input
                                           :element-type 'ext::byte8)
                          (let ((bytes (make-array (file-length s)
                                                   :element-type 'ext::byte8)))
                            (read-sequence bytes s)
                            bytes)))))
    (with-open-file (s file
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede
                       :element-type 'ext::byte8)
      (write-sequence bytes s))))

(defun copy-file-to-remote (remote file)
  (let ((bytes
          (with-open-file (s file
                             :direction :input
                             :element-type 'ext::byte8)
            (let ((bytes (make-array (file-length s)
                                     :element-type 'ext::byte8)))
              (read-sequence bytes s)
              bytes))))
    (send-command remote
                  `(with-open-file (s ,file
                                      :direction :output
                                      :if-does-not-exist :create
                                      :if-exists :supersede
                                      :element-type 'ext::byte8)
                     (write-sequence ,bytes s)))))

;;; Handling literal objects and uninterned symbols for COMPILE

(defvar *literal-objects* (make-array 8 :adjustable t :fill-pointer t))

(defun literal-object-p (form)
  (not (or (symbolp form) (numberp form))))

(defun collect-literal-objects (form env)
  (when (or (and (atom form) (literal-object-p form))
            (and (consp form) (eq (first form) 'quote)))
    (vector-push-extend form *literal-objects*))
  (let ((head (and (consp form) (car form))))
    (case head
      ;; The interpreter doesn't know about ffi special forms, so we
      ;; have to handle those separately
      ((ffi:clines ffi:defcbody ffi:defentry) nil)
      ((ffi:c-inline ffi:c-progn)
       (mapcar #'(lambda (x) (collect-literal-objects x env)) (second form))
       nil)
      (otherwise form))))

(defun replace-literal-objects (form)
  (when (and (consp form) (eq (first form) 'si:quasiquote))
    (return-from replace-literal-objects
      (replace-literal-objects (macroexpand-1 form))))
  (loop for obj across *literal-objects*
        for index from 0
        if (or (eq obj form)
               (and (consp form) (eq (first form) 'quote)
                    (consp obj) (eq (first obj) 'quote)
                    (eq (second form) (second obj))))
          do (return-from replace-literal-objects
               `(load-time-value ,(if (and (consp obj) (eq (first obj) 'quote))
                                      `(second (elt cl-test-cross-compile::*literal-objects* ,index))
                                      `(elt cl-test-cross-compile::*literal-objects* ,index)))))
  (if (consp form)
      (cons (replace-literal-objects (car form)) (replace-literal-objects (cdr form)))
      form))

(defun adjoin-macros (form lexenv temp-interned-sym)
  (loop for record in (cdr lexenv)
        do (when (and (eq (second record) 'si:macro)
                      (typep (third record) 'function))
             (multiple-value-bind (macro-form macro-lexenv)
                 (function-lambda-expression (third record))
               (multiple-value-bind (prepared-macro-form new-temp-interned-sym)
                   (prepare-form macro-form macro-lexenv)
                 (setf form `(macrolet ((,(car record) (&whole whole &environment env)
                                          (funcall ,prepared-macro-form whole env)))
                               ,form)
                       temp-interned-sym (append temp-interned-sym new-temp-interned-sym))))))
  (loop for record in (car lexenv)
        do (when (and (eq (second record) 'si:symbol-macro)
                      (typep (third record) 'function))
             (multiple-value-bind (prepared-macro-form new-temp-interned-sym)
                 (prepare-form (funcall (third record) (car record) nil)
                               (nth-value (function-lambda-expression (third record)) 1))
               (setf form `(symbol-macrolet ((,(car record) ,prepared-macro-form))
                             ,form)
                     temp-interned-sym (append temp-interned-sym new-temp-interned-sym)))))
  (values form temp-interned-sym))

(defun intern-symbols (form)
  ;; Import all uninterned symbols into temporary packages to allow
  ;; them to be sent to the remote. Each symbol gets its own package
  ;; to avoid name conflicts.
  (cond ((and (symbolp form) (null (symbol-package form)))
         (import form (make-package (symbol-name (gensym)) :use nil))
         (list form))
        ((consp form)
         (nconc (intern-symbols (car form))
                (intern-symbols (cdr form))))
        (t
         nil)))

(defun remote-intern-symbols (remote symbols)
  (send-command remote
                `(progn
                   (mapc #'(lambda (n)
                             (make-package n :use nil))
                         ',(mapcar (lambda (s)
                                     (package-name (symbol-package s)))
                                   symbols))
                   (values))))

(defun intern-symbols-host-and-remote (remote form)
  (remote-intern-symbols remote (intern-symbols form)))

(defun prepare-form (form lexenv)
  ;; Adjoin lexical environment
  (let ((c::*cmp-env* (copy-tree c::*cmp-env-root*))
        (temporarily-interned-symbols nil))
    (setf form (c::set-closure-env form lexenv))
    (multiple-value-setq (form temporarily-interned-symbols)
      (adjoin-macros form c::*cmp-env* temporarily-interned-symbols))
    (setf temporarily-interned-symbols
          (append temporarily-interned-symbols (intern-symbols form)))
    (let ((si::*code-walker* #'collect-literal-objects))
      (si::eval-with-env form c::*cmp-env* nil t :compile-toplevel))
    (setf form (replace-literal-objects form))
    (values form temporarily-interned-symbols)))

(defun filter-readable-objects (obj)
  (with-standard-io-syntax
    (let ((printed-representation
            (handler-case
                (write-to-string obj :circle t)
              (print-not-readable () (return-from filter-readable-objects nil)))))
      ;; If the string contains #$, we are probably dealing with a
      ;; random state object which is not portable across
      ;; architectures with different word sizes.
      (and (not (search "#$" printed-representation)) obj))))

;;; Cross compilation setup

(defun setup (target-info startup-code)
  (multiple-value-bind (ignored ignored remote)
      (ext:run-program (ext:getenv "ECL_TO_RUN")
                       '("--norc")
                       :wait nil
                       :environ (remove-if #'(lambda (x)
                                               (string= (subseq x 0 6) "ECLDIR"))
                                           (ext:environ)))
    (declare (ignore ignored))
    (let ((host-*make-special (fdefinition 'si::*make-special))
          (host-do-deftype (fdefinition 'si::do-deftype))
          (host-proclaim (fdefinition 'proclaim)))
      (labels ((mirror-*make-special (sym)
                 (funcall host-*make-special sym)
                 (intern-symbols-host-and-remote remote sym)
                 (send-command remote `(si::*make-special ',sym)))
               (mirror-do-deftype (name form function)
                 (funcall host-do-deftype name form function)
                 (intern-symbols-host-and-remote remote form)
                 (send-command remote form))
               (mirror-proclaim (&rest args)
                 (apply host-proclaim args)
                 (intern-symbols-host-and-remote remote args)
                 (send-command remote `(apply 'proclaim ',args)))
               (remote-compile-file (file &rest args)
                 (setf file (truename file))
                 (copy-file-to-remote remote file)
                 (let ((load-flag (getf args :load))
                       (output-file (getf args :output-file)))
                   (remf args :load)
                   (remf args :output-file)
                   (multiple-value-bind (compiled-file warnings error)
                       (send-command remote
                                     `(compile-file ,file ,@args :target *target*))
                     (unless error
                       (copy-file-from-remote remote compiled-file)
                       (when output-file
                         (setf compiled-file
                               (nth-value 2 (rename-file compiled-file output-file
                                                         :if-exists :supersede))))
                       (when load-flag
                         (load compiled-file)))
                     (values compiled-file warnings error))))
               (remote-compile (name &optional (def nil supplied-p))
                 (let (form temporarily-interned-symbols
                            (lexenv nil)
                            (fname (or name 'C::GAZONK))
                            (file "x.lisp"))
                   (cond ((and supplied-p def)
                          (when (functionp def)
                            (unless (function-lambda-expression def)
                              (return-from remote-compile def))
                            (multiple-value-setq (def lexenv)
                              (function-lambda-expression def))
                            (when (eq lexenv t)
                              (warn "COMPILE can not compile C closures")
                              (return-from remote-compile (values def t nil)))))
                         ((not (fboundp name))
                          (error "Symbol ~s is unbound." name))
                         ((typep (setf def (fdefinition name)) 'standard-generic-function)
                          (warn "COMPILE can not compile generic functions yet")
                          (return-from remote-compile (values def t nil)))
                         ((null (setq def (function-lambda-expression def)))
                          (warn "We have lost the original function definition for ~s. Compilation to C failed"
                                name)
                          (return-from remote-compile (values def t nil))))
                   ;; Prepare form
                   (setf (fill-pointer *literal-objects*) 0)
                   (multiple-value-setq (form temporarily-interned-symbols)
                     (prepare-form `(setf (fdefinition ',fname) #',def)
                                   lexenv))
                   (remote-intern-symbols remote temporarily-interned-symbols)
                   (send-command remote `(progn
                                           (setq *literal-objects* ,(map 'vector #'filter-readable-objects *literal-objects*))
                                           (values)))
                   ;; Write to file
                   (with-open-file (s file
                                      :direction :output
                                      :if-does-not-exist :create
                                      :if-exists :supersede
                                      :external-format :utf-8)
                     (with-standard-io-syntax
                       (print '(in-package #:cl-user) s)
                       (print form s)))
                   ;; Compile file remotely
                   (multiple-value-bind (compiled-file warnings error)
                       (remote-compile-file file)
                     ;; Load compiled fasl
                     (unless error
                       (load compiled-file)
                       (setf name (or name (fdefinition fname))))
                     ;; Clean up uninterned symbols
                     (send-command remote `(progn
                                             (mapc #'(lambda (s)
                                                       (let ((p (symbol-package s)))
                                                         (unintern s p)
                                                         (delete-package p)))
                                                   ',temporarily-interned-symbols)
                                             (values)))
                     (mapc #'(lambda (s)
                               (let ((p (symbol-package s)))
                                 (unintern s p)
                                 (delete-package p)))
                           temporarily-interned-symbols)
                     ;; Return values
                     (values name warnings error)))))
        (skip-forward-to-first-prompt remote)
        (prin1 `(setf si::*tpl-prompt-hook* " :input-prompt ") (ext:external-process-input remote))
        (terpri (ext:external-process-input remote))
        (skip-forward-to-prompt remote)
        (prin1 `(progn (cl:defpackage "CL-TEST-CROSS-COMPILE" (:use "CL")) (values))
               (ext:external-process-input remote))
        (terpri (ext:external-process-input remote))
        (send-command remote `(in-package #:cl-test-cross-compile) t)
        (send-command remote `(require :cmp) t)
        (send-command remote `(defvar *target* ',(c::read-target-info target-info)) t)
        (send-command remote startup-code t)
        (send-command remote `(defvar *literal-objects*) t)
        (let ((si::*ignore-package-locks* t))
          (setf (fdefinition 'si::*make-special) #'mirror-*make-special
                (fdefinition 'si::do-deftype) #'mirror-do-deftype
                (fdefinition 'proclaim) #'mirror-proclaim
                (fdefinition 'compile-file) #'remote-compile-file
                (fdefinition 'compile) #'remote-compile))
        (setf *remote* remote)))))
