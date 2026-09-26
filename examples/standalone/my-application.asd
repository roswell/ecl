(asdf:defsystem #:my-application
  :serial t
  ;; :depends-on (#:other-asdf-dependency)
  :components ((:file "my-application"))
  :build-pathname "libmy-application")
