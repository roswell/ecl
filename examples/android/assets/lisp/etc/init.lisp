(in-package :cl-user)
(format t "ECL (Embeddable Common-Lisp) ~A (git:~D)~%"
	(lisp-implementation-version)
	(ext:lisp-implementation-vcs-id))

(format t "Setting environment variables~%")
(setq *default-directory*
      *default-pathname-defaults*)
(defvar *ecl-home* *default-directory*)

(format t "Loading the modules~%")
(require '#:asdf)
(require '#:sockets)
(require '#:serve-event)

(setf asdf:*user-cache* (merge-pathnames #P"../cache/" *default-pathname-defaults*))

(pushnew (namestring *default-pathname-defaults*)
	 asdf:*central-registry*)
(pushnew (namestring (merge-pathnames #P"home/slime-2.14/" *ecl-home*))
         asdf:*central-registry*)

(format t "Preparing swank~%")
(handler-case (asdf:oos 'asdf:load-op :swank :verbose t)
  (condition (c)
    (format t "condition ~A happened~%" c)
    (format t "filepath SYS:SLIME-2.14;SWANK.ASD.NEWEST: ~A~%"
            (truename #P"SYS:SLIME-2.14;SWANK.ASD.NEWEST"))
    (force-output)
    (error c)))
(swank-loader:init :load-contribs t
		   :setup t
		   :delete t
		   :quiet nil)

;; The following "patches" swank to work correctly on android/iOS
(in-package :swank/backend)
(defimplementation lisp-implementation-program ()
  "Return the argv[0] of the running Lisp process, or NIL."
  ;; "org.lisp.ecl"
  nil)

(in-package :swank)
(defun repl-input-stream-read (connection stdin)
  (loop
   (let* ((socket (connection.socket-io connection))
          (inputs (list socket #+(or) stdin))
          (ready (wait-for-input inputs)))
     (cond ((eq ready :interrupt)
            (check-slime-interrupts))
           ((member socket ready)
            ;; A Slime request from Emacs is pending; make sure to
            ;; redirect IO to the REPL buffer.
            (with-simple-restart (process-input "Continue reading input.")
              (let ((*sldb-quit-restart* (find-restart 'process-input)))
                (with-io-redirection (connection)
                  (handle-requests connection t)))))
           ((member stdin ready)
            ;; User typed something into the  *inferior-lisp* buffer,
            ;; so do not redirect.
            (return (read-non-blocking stdin)))
           (t (assert (null ready)))))))

;; (format t "Loading user.lisp~%")
;; (handler-case (cond ((probe-file #P"user.lisp")
;;                      (format t "user exists~%")
;;                      (load "user"))
;;                     (T (format t "user.lisp doesn't exist~%")))
;;   (condition (c)
;;     (format t "condition ~A happened~%" c)
;;     (error c)))

(when (probe-file #P"etc/user.lisp")
  (load "etc/user"))

(format t "Starting swank~%")
(mp:process-run-function
 "SLIME-listener"
 (lambda ()
   (let ((swank::*loopback-interface* "0.0.0.0"))
     (swank:create-server :port 4005
                          :dont-close t
                          ;; :style nil #|:spawn|#
                          ))))

(format t "Initialization done~%")
(force-output)
(sleep 3)
