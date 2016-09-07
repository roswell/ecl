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
                         #-ecl-bytecmp
                         (:file "embedding")
                         #+ffi
                         (:file "foreign-interface")
                         #+clos
                         (:file "metaobject-protocol")
                         #+threads
                         (:file "multiprocessing")))
               (:module features
                        :default-component-class asdf:cl-source-file.lsp
                        :components
                        (#+unicode
                         (:file "external-formats")
                         #+ieee-floating-point
                         (:file "ieee-fp")))))

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
