;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  PROCESS.LSP  -- External processes

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "EXT")

(defstruct (external-process (:constructor make-external-process ()))
  pid
  input
  output
  error-stream
  (%status :running)
  (%code nil))

(defun external-process-status (external-process)
  (let ((status (external-process-%status external-process)))
    (if (eq status :running)
        (ext:external-process-wait external-process nil)
        (values status (external-process-%code external-process)))))

;;;
;;; Backwards compatible SI:SYSTEM call. We avoid ANSI C system()
;;; because we are consuming the process wait status using a SIGCHLD
;;; handler -- this breaks some C libraries out there (OS X 32 bit).
;;;
(defun system (cmd-string)
  (let ((shell "/bin/sh")
        (option "-c"))
    #+windows
    (let ((comspec (getenv "ComSpec")))
      (when comspec
        (setf shell comspec
              option "/c")))
    (nth-value 1 (run-program shell (list option cmd-string)
                              :wait t :output nil :input nil
                              :error nil))))

;;;
;;; Wrapper around si_run_program call. Thanks to that C interface
;;; isn't clobbered with lispisms. Ultimately we'd want to have as
;;; little as possible in unixsys.d.
;;;
(defun run-program (command argv
                    &key
                      (input :stream)
                      (output :stream)
                      (error :output)
                      (wait t)
                      (environ nil)
                      (if-input-does-not-exist nil)
                      (if-output-exists :error)
                      (if-error-exists :error)
                      (external-format :default)
                      #+windows (escape-arguments t))

  (flet ((process-stream (which default &rest args)
           (cond ((eql which t) default)
                 ((or (stringp which) (pathnamep which))
                  (apply #'open which :external-format external-format args))
                 ;; this three cases are handled in create_descriptor (for now)
                 ((eql which nil)     which)
                 ((eql which :stream) which)
                 ((streamp which)     which)
                 ;; signal error as early as possible
                 (T (error "Invalid ~S argument to EXT:RUN-PROGRAM" which))))

         (prepare-args (args)
           #-windows
           (if (every #'simple-string-p args)
               args
               (mapcar #'si:copy-to-simple-base-string args))
           #+windows
           (with-output-to-string (str)
             (loop for (arg . rest) on args
                do (if (and escape-arguments
                            (find-if (lambda (c)
                                       (find c '(#\Space #\Tab #\")))
                                     arg))
                       (escape-arg arg str)
                       (princ arg str))
                  (when rest
                    (write-char #\Space str))))))

    (setf input (process-stream input *standard-input*
                                :direction :input
                                :if-does-not-exist if-input-does-not-exist)
          output (process-stream output *standard-output*
                                 :direction :output
                                 :if-exists if-output-exists)
          error (if (eql error :output)
                    :output
                    (process-stream error *error-output*
                                    :direction :output
                                    :if-exists if-error-exists)))

    (let* ((args (prepare-args (cons command argv)))
           (progname (car args)))
      (si:run-program-internal progname args
                               input output error
                               wait environ external-format))))



#+windows
(defun escape-arg (arg stream)
  ;; Normally, #\\ doesn't have to be escaped But if #\"
  ;; follows #\\, then they have to be escaped.  Do that by
  ;; counting the number of consequent backslashes, and
  ;; upon encoutering #\" immediately after them, output
  ;; the same number of backslashes, plus one for #\"
  (write-char #\" stream)
  (loop with slashes = 0
     for i below (length arg)
     for previous-char = #\a then char
     for char = (char arg i)
     do
       (case char
         (#\"
          (loop repeat slashes
             do (write-char #\\ stream))
          (write-string "\\\"" stream))
         (t
          (write-char char stream)))
       (case char
         (#\\
          (incf slashes))
         (t
          (setf slashes 0)))
     finally
     ;; The final #\" counts too, but doesn't need to be escaped itself
       (loop repeat slashes
          do (write-char #\\ stream)))
  (write-char #\" stream))
