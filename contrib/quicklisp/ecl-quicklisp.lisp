(defpackage :ecl-quicklisp
  (:use :cl))

(in-package :ecl-quicklisp)

(require :ecl-curl)

(defparameter *quicklisp-url* "http://beta.quicklisp.org/quicklisp.lisp")

(defparameter *quicklisp-directory* (translate-logical-pathname "SYS:QUICKLISP;"))

(defparameter *quicklisp-setup* "SYS:QUICKLISP;SETUP.LISP")

(defun safe-download (url filename)
  (ensure-directories-exist filename)
  (handler-case
      (ecl-curl:download-url-to-file url filename)
    (ecl-curl:download-error (c)
      (format t "~&;;;~%;;; Unable to download quicklisp. Aborting. ~%;;;")
      (ext:quit 1)))
  filename)

(defun install-quicklisp (target-directory)
  (let ((file (merge-pathnames "_installer.lisp" target-directory)))
    (ensure-directories-exist file)
    (ecl-curl:download-url-to-file *quicklisp-url* file)
    (load file)
    (eval (read-from-string
	   (format nil "(quicklisp-quickstart:install :path ~S)"
		   (namestring (truename target-directory))))
	  )))

(handler-case
    (progn
      (unless (probe-file *quicklisp-setup*)
	(install-quicklisp *quicklisp-directory*))
      (load *quicklisp-setup*))
  (error (c)
    (format t "~%;;; Unable to load / install quicklisp. Error message follows:~%~A"
	    c)))


