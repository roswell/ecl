;;; A factory for setting extra locations for modules
;;;
;;; bsd-2-clause, (c) 2026, Daniel Kochmański and Dima Pasechnik
;;;
;;; This allows configuring locations for 3rd-party modules, so that
;;;
;;;   (require :<module>)
;;;
;;; works for modules (e.g. .fas files) installed into <DIR> after
;;;
;;;   (pushnew (extra-modules-location <DIR>) ext:*module-provider-functions*)
;;;
;;; is executed. <DIR> is an absolute path given as a string.


(require :asdf)
(defun extra-modules-location (modules-dir-string)
    #'(lambda (module)
      (let* ((base (make-pathname :directory
          (cons :absolute (uiop:split-string modules-dir-string :separator "/"))))
	       (path1 (make-pathname :name (string module) :defaults base))
	       (path2 (make-pathname :name (string-downcase module) :defaults base)))
	  (or (load path1 :if-does-not-exist nil)
	      (load path2 :if-does-not-exist nil)))))

;; set an extra modules directory to "/usr/local/lib/ecl"
(pushnew (extra-modules-location "/usr/local/lib/ecl") ext:*module-provider-functions*)
