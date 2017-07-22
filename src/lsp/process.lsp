;;;;  process.lsp  -- External processes.

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2017, Daniel Kochmański
;;;;
;;;;    See file 'LICENSE' for the copyright details.

(in-package "EXT")

(defmacro with-process-lock ((process) &body body)
  `(mp:with-lock ((external-process-%lock process))
     ,@body))

(defstruct (external-process (:constructor make-external-process ()))
  pid
  input
  output
  error-stream
  (%status :running)
  (%code nil)
  (%lock (mp:make-lock)))

(defun external-process-status (external-process)
  (let ((status (external-process-%status external-process)))
    (if (member status '(:stopped :resumed :running))
        (ext:external-process-wait external-process nil)
        (values status (external-process-%code external-process)))))

;;; ---------------------------------------------------------------------
;;; si:waitpid -> (values                              status  code  pid)
;;; ---------------------------------------------------------------------
;;;  no change :: (values                                 nil   nil  nil)
;;;  error     :: (values (member              :abort :error)   nil  nil)
;;;  finished  :: (values (member         :exited :signalled)  code  pid)
;;;  running   :: (values (member :stopped :resumed :running)  code  pid)
;;; ---------------------------------------------------------------------
(defun external-process-wait (process &optional wait)
  (with-process-lock (process)
    (let ((pid (external-process-pid process)))
      (when pid
        (multiple-value-bind (status code pid) (si:waitpid pid wait)
          (ecase status
            ((:exited :signaled :abort :error)
             (setf (external-process-pid process) nil
                   (external-process-%status process) status
                   (external-process-%code process) code))
            ((:stopped :resumed :running)
             (setf (external-process-%status process) status
                   (external-process-%code process) code))
            ((nil) #| wait was nil and process didn't change |#))))))
  (values (external-process-%status process)
          (external-process-%code process)))

(defun terminate-process (process &optional force)
  (with-process-lock (process)
    (let ((pid (external-process-pid process)))
      (when pid
        #+windows
        (ffi:c-inline
         (process pid) (:object :object) :void
         "HANDLE *ph = (HANDLE*)ecl_foreign_data_pointer_safe(#1);
         int ret = TerminateProcess(*ph, -1);
         if (ret == 0) FEerror(\"Cannot terminate the process ~A\", 1, #0);")
        #-windows
        (unless (zerop (si:killpid pid (if force +sigkill+ +sigterm+)))
          (error "Cannot terminate the process ~A" process))))))

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
                              #+windows :escape-arguments #+windows nil))))

;;; We don't handle `sigchld' because we don't want races with
;;; `external-process-wait'. Take care of forgotten processes.
(defun finalize-external-process (process)
  (unless (member (ext:external-process-wait process nil)
                  '(:exited :signaled :abort :error))
    (ext:set-finalizer process #'finalize-external-process)))

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

  (labels ((process-stream (which default &rest args)
             (cond ((eql which t)
                    default)
                   ((eql which nil)
                    (null-stream (getf args :direction)))
                   ((or (stringp which) (pathnamep which))
                    (apply #'open which :external-format external-format args))
                   #+(and (or) clos-streams threads)
                   ((and (streamp which)
                         (null (typep which 'ext:ansi-stream)))
                    #| Here we may want to return `:stream' and spawn
                    thread to handle data at runtime to fd. |#)
                   ((or (eql which :stream)
                        (streamp which))
                    which)
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
                       (write-char #\Space str))))))
           (null-stream (direction)
             (open #-windows "/dev/null"
                   #+windows "nul"
                   :direction direction)))

    (let ((progname (si:copy-to-simple-base-string command))
          (args (prepare-args (cons command argv)))
          (environ (mapcar #'si:copy-to-simple-base-string environ))
          (process (make-external-process))
          (process-input (process-stream input *standard-input*
                                         :direction :input
                                         :if-does-not-exist if-input-does-not-exist))
          (process-output (process-stream output *standard-output*
                                          :direction :output
                                          :if-exists if-output-exists))
          (process-error (if (eql error :output)
                             :output
                             (process-stream error *error-output*
                                             :direction :output
                                             :if-exists if-error-exists)))
          pid parent-write parent-read parent-error)

      (multiple-value-setq (pid parent-write parent-read parent-error)
        (si:spawn-subprocess progname args environ process-input process-output process-error))

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
              (external-process-input process)         (or stream-write (null-stream :output))
              (external-process-output process)        (or stream-read  (null-stream :input))
              (external-process-error-stream process)  (or stream-error (null-stream :input)))

        (values (make-two-way-stream (external-process-output process)
                                     (external-process-input process))
                (if wait
                    (nth-value 1 (si:external-process-wait process t))
                    (ext:set-finalizer process #'finalize-external-process))
                process)))))

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
