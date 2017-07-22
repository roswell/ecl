(defsystem #:example-with-dep
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
	       (:file "example")))
