;;;; ecl-tests.asd

(asdf:defsystem #:ecl-tests
  :description "Various tests for ECL"
  :author "Daniel Kochmański <daniel@turtleware.eu>"
  :license "LGPL-2.1+"
  :serial t
  :components ((:file "1am")            ; for stress tests
               (:file "2am")            ; continuous integration
               (:file "ecl-tests")
               (:file "universe")
               (:module normal-tests
                        :default-component-class asdf:cl-source-file.lsp
                        :components
                        ((:file "ansi")
                         (:file "mixed")
                         (:file "compiler")
                         (:file "executable-cli")
                         (:file "run-program")
                         (:file "multiprocessing" :if-feature :threads)
                         (:file "embedding" :if-feature (:not :ecl-bytecmp))
                         (:file "foreign-interface" :if-feature :ffi)
                         (:file "metaobject-protocol" :if-feature :clos)
                         (:file "ieee-fp" :if-feature :ieee-floating-point)
                         (:file "package-extensions")
			 (:file "hash-tables")
                         (:file "external-formats" :if-feature :unicode)
                         (:file "unicode" :if-feature :unicode)
                         (:file "complex")
                         (:file "wscl")))
               (:module stress-tests
                        :default-component-class asdf:cl-source-file.lsp
                        :components
                        ((:file "multiprocessing" :if-feature :threads)))))

(asdf:defsystem #:ecl-tests/stress
  :serial t
  :components
  (
   ))

;;; General tests
(asdf:defsystem #:ecl-tests/ansi)
(asdf:defsystem #:ecl-tests/benchmark)
