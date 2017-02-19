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

;;; ---------------------------------------------------------------------------
;;; si:waitpid -> (values                                    status  code  pid)
;;; ---------------------------------------------------------------------------
;;;  nochg :: (values                                           nil   nil  nil)
;;;  error :: (values                        (member :abort :error)   nil  nil)
;;;  chang :: (values (member :exited :signalled :stopped :running)  code  pid)
;;; ---------------------------------------------------------------------------
(defun external-process-wait (process &optional wait)
  (let ((pid (external-process-pid process)))
    (when pid
      (multiple-value-bind (status code pid) (si:waitpid pid wait)
        (unless (and wait (null status) (null code) (null pid))
          (setf (external-process-pid process) pid
                (external-process-%status process) status
                (external-process-%code process) code)))))
  (values (external-process-%status process)
          (external-process-%code process)))

;;;
;;; Backwards compatible SI:SYSTEM call. We avoid ANSI C system()
;;; because we are consuming the process wait status using a SIGCHLD
;;; handler -- this breaks some C libraries out there (OS X 32 bit).
;;;
#+ (or)
(defun system (cmd-string)
  (let ((shell "/bin/sh")
        (option "-c"))
    #+windows
    (let ((comspec (getenv "ComSpec")))
      (when comspec
        (setf shell comspec
              option "/c")))
    (nth-value 1 (run-program shell (list option cmd-string)
                              :wait t :output nil :input nil :error nil
                              #+windows :escape-arguments nil))))

;;;
;;; Almighty EXT:RUN-PROGRAM. Built on top of SI:SPAWN-SUBPROCESS. For
;;; simpler alternative see SI:RUN-PROGRAM-INNER.
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
           (mapcar #'si:copy-to-simple-base-string args)
           #+windows
           (si:copy-to-simple-base-string
            (with-output-to-string (str)
              (loop for (arg . rest) on args
                 do (if (and escape-arguments
                             (find-if (lambda (c)
                                        (find c '(#\Space #\Tab #\")))
                                      arg))
                        (escape-arg arg str)
                        (princ arg str))
                   (when rest
                     (write-char #\Space str)))))))

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

    (let ((progname (si:copy-to-simple-base-string command))
          (args (prepare-args (cons command argv)))
          (process (make-external-process)))
      (multiple-value-bind (pid parent-write parent-read parent-error)
          (si:spawn-subprocess progname args environ input output error)
        (unless pid
          (when parent-write (ff-close parent-write))
          (when parent-read (ff-close parent-read))
          (when parent-error (ff-close parent-error))
          (error "Could not spawn subprocess to run ~S." progname))

        (let ((stream-write
               (when (< 0 parent-write)
                 (make-output-stream-from-fd progname parent-write external-format)))
              (stream-read
               (when (< 0 parent-read)
                 (make-input-stream-from-fd progname parent-read external-format)))
              (stream-error
               (when (< 0 parent-error)
                 (make-input-stream-from-fd progname parent-error external-format))))
          (setf (external-process-pid process) pid
                (external-process-input process)         (or stream-write (null-stream))
                (external-process-output process)        (or stream-read  (null-stream))
                (external-process-error-stream process)  (or stream-error (null-stream)))

          (values (make-two-way-stream (external-process-output process)
                                       (external-process-input process))
                  (when wait (nth-value 1 (si:external-process-wait process t)))
                  process))))))

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


;;; low level interface to descriptors
(defun make-input-stream-from-fd (name fd external-format)
  (ffi:c-inline
   (name fd external-format) (:string :int :object) :object
   "ecl_make_stream_from_fd(#0, #1, ecl_smm_input, 8, ECL_STREAM_DEFAULT_FORMAT, #2)"
   :one-liner t))

(defun make-output-stream-from-fd (name fd external-format)
  (ffi:c-inline
   (name fd external-format) (:string :int :object) :object
   "ecl_make_stream_from_fd(#0, #1, ecl_smm_output, 8, ECL_STREAM_DEFAULT_FORMAT, #2)"
   :one-liner t))

(defun null-stream ()
  (ffi:c-inline () () :object "cl_core.null_stream" :one-liner t :side-effects nil))

(ffi:defentry ff-close (:int) (:int "close") :no-interrupts t)

