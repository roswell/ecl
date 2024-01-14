;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia Ripoll
;;;;
;;;;    See file 'LICENSE' for the copyright details.

;;;; CMPOS-RUN  Executing auxiliary programs

(in-package "COMPILER")

#+(and cygwin (not ecl-min))
(ffi:clines "#include <stdlib.h>")

;;; this is needed by compile.lsp.in
(defun safe-system (string)
  (cmpnote "Invoking external command:~%  ~A~%" string)
  (let ((result (ext:system string)))
    (unless (zerop result)
      (cerror "Continues anyway."
              "(SYSTEM ~S) returned non-zero value ~D"
              string result))))

(defun save-directory (forms)
  (let ((directory
         (probe-file (make-pathname :name nil :type nil
                                    :defaults *default-pathname-defaults*))))
    (if directory
        (let ((*default-pathname-defaults* directory)
              (old-directory (ext:chdir directory)))
          (unwind-protect
               (funcall forms)
            (ext:chdir old-directory)))
        (funcall forms))))    

(defmacro with-current-directory (&body forms)
  `(save-directory #'(lambda () ,@forms)))

(defun safe-run-program (program args &aux result output)
  (cmpnote "Invoking external command:~%  ~A ~{~A ~}" program args)
  (let* ((*standard-output* ext:+process-standard-output+)
         (*error-output* ext:+process-error-output+)
         (program (split-program-options program))
         (args `(,@(cdr program) ,@args))
         (program (car program)))
    (with-current-directory
     ;; when compiling ECL itself, we only have low-level functions
     ;; available ...
     #+(and ecl-min (not cygwin))
     (multiple-value-bind (output-stream return-status pid)
         (si:run-program-inner program args :default nil)
       #+msvc
       (si::stream-external-format-set output-stream
                                       (list (si::windows-codepage-encoding) :crlf))
       (setf output (collect-lines output-stream))
       (multiple-value-setq (return-status result)
         (si:waitpid pid t)))
     ;; ... otherwise we can use run-program and get proper
     ;; quoting of arguments ...
     #+(and (not ecl-min) (not cygwin))
     (multiple-value-bind (output-stream return-status process-obj)
         (ext:run-program program args :wait nil
                          #+msvc :external-format
                          #+msvc (list (si::windows-codepage-encoding) :crlf))
       (setf output (collect-lines output-stream))
       (multiple-value-setq (return-status result)
         (ext:external-process-wait process-obj t)))
     ;; ... unless we're running on cygwin which has problems with
     ;; forking so we have to use si:system
     #+cygwin
     (setf result (si:system (format nil "~A~{ ~A~}" program args)))))
  (cond ((null result)
         (cerror "Continues anyway."
                 "Unable to execute:~%(EXT:RUN-PROGRAM ~S ~S)"
                 program args result))
        ((not (zerop result))
         (cerror "Continues anyway."
                 "Error code ~D when executing~%(EXT:RUN-PROGRAM ~S ~S):~%~{~A~^~%~}"
                 result program args output)))
  result)

(defun split-program-options (string)
  (labels ((maybe-push (options current)
             (if current
                 (cons current options)
                 options))
           (new-string ()
             (make-array 32 :element-type 'base-char :adjustable t
                         :fill-pointer 0))
           (push-char (c s)
             (unless s (setf s (new-string)))
             (vector-push-extend c s)
             s))
    (loop with output = '()
       with option = nil
       with status = nil
       for i from 0 below (length string)
       for c = (char string i)
       for now = (first status)
       do (cond ((eq now #\')
                 (if (eq c #\')
                     (setf status (rest status))
                     (setf option (push-char c option))))
                ((eq now #\\)
                 (setf option (push-char c option)
                       status (rest status)))
                ((eq now #\")
                 (if (eq c #\")
                     (setf status (rest status))
                     (setf option (push-char c option))))
                ((member c '(#\\ #\' #\"))
                 (push c status))
                ((member c '(#\Space #\Tab #\Return #\Newline))
                 (setf output (maybe-push output option)
                       option nil))
                (t
                 (setf option (push-char c option))))
       finally (cond (status
                      (error "In split-program-options, unterminated option list:~%~S"
                             string))
                     (t
                      (return (nreverse (maybe-push output option))))))))
