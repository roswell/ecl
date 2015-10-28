(in-package :cl-user)
(format t "ECL (Embeddable Common-Lisp) ~A (git:~D)~%"
	(lisp-implementation-version)
	(ext:lisp-implementation-vcs-id))

(defvar *ecl-home* *default-pathname-defaults*)

(format t "Loading the modules~%")
(require '#:asdf)
(require '#:sockets)
(require '#:serve-event)

(setf asdf:*user-cache* (merge-pathnames #P"../cache/" *default-pathname-defaults*))

(pushnew (namestring *default-pathname-defaults*)
	 asdf:*central-registry*)

(when (probe-file #P"etc/user.lisp")
  (load "etc/user"))
