(in-package :cl-user)
(format t "ECL (Embeddable Common-Lisp) ~A (git:~D)~%"
	(lisp-implementation-version)
	(ext:lisp-implementation-vcs-id))

(require :SOCKETS)
(require :ASDF)
(setq *default-directory*
      *default-pathname-defaults*)
(defvar *ecl-home* *default-directory*)
(ext:setenv "HOME" (namestring *ecl-home*))
(pushnew (namestring *default-pathname-defaults*)
	 asdf:*central-registry*)
(asdf:oos 'asdf:load-op :swank)
(swank-loader:init :load-contribs t
		   :setup t
		   :delete t
		   :quiet nil)

;; The following "patches" swank to work correctly on android/iOS
(in-package :swank/backend)
(defimplementation lisp-implementation-program ()
  "Return the argv[0] of the running Lisp process, or NIL."
  ;; (lisp-implementation-type)
  nil)

(in-package :swank)
(defun repl-input-stream-read (connection stdin)
  (loop
   (let* ((socket (connection.socket-io connection))
          (inputs (list socket #+nil stdin))
          (ready (wait-for-input inputs)))
     (describe (list 'hobaa connection stdin socket inputs ready))
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

(mp:process-run-function
 "SLIME-listener"
 (lambda ()
   (swank:create-server :port 4005
			:dont-close t)))

(cond ((probe-file #P"user.lisp")
       (load "user")))

