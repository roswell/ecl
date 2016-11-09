;;;; ecl-tests.asd

(asdf:defsystem #:ecl-tests
  :description "Various tests for ECL"
  :author "Daniel Kochmański <daniel@turtleware.eu>"
  :license "LGPL-2.1+"
  :serial t
  :components ((:file "2am")            ; continuous integration
               (:file "ecl-tests")
               (:file "universe")
               (:module regressions
                        :default-component-class asdf:cl-source-file.lsp
                        :components
                        ((:file "ansi")
                         (:file "mixed")
                         (:file "compiler")
                         (:file "embedding" :if-feature (:not :ecl-bytecmp))
                         (:file "foreign-interface" :if-feature :ffi)
                         (:file "metaobject-protocol" :if-feature :clos)
                         (:file "multiprocessing" :if-feature :threads)))
               (:module features
                        :default-component-class asdf:cl-source-file.lsp
                        :components
                        ((:file "external-formats" :if-feature :unicode)
                         (:file "ieee-fp" :if-feature :ieee-floating-point)
                         (:file "package-locks" :if-feature :package-locks)
                         (:file "external-process")
                         (:file "multiprocessing")))))

(asdf:defsystem #:ecl-tests/stress
  :serial t
  :components
  ((:file "1am")            ; for stress tests
   (:module stress
            :default-component-class asdf:cl-source-file.lsp
            :components
            (#+threads
             (:file "multiprocessing")))))

;;; General tests
(asdf:defsystem #:ecl-tests/ansi)
(asdf:defsystem #:ecl-tests/benchmark)
