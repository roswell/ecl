;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;; Remove compiled files
(let* ((fn (compile-file-pathname "doit.lsp"))
       (type (pathname-type fn))
       (dir-pathname (make-pathname :name :wild :type type))
       (files (union (directory "aux*.*") (directory dir-pathname) :test #'equal)))
  (assert type)
  (assert (not (string-equal type "lsp")))
  (mapc #'delete-file files))

(si::package-lock (find-package "COMMON-LISP") nil)
(require 'rt)

#+ecl (compile nil '(lambda () nil))

(setq *load-verbose* nil
      *load-print* nil
      *compile-verbose* nil
      *compile-print* nil)

(unless (find-package :cl-test)
  (make-package :cl-test))

(in-package :cl-test)
(use-package :sb-rt)

(load "tools.lsp")
(load "universe.lsp")
(load "ansi-aux.lsp")

(load "tests/test-ansi.lsp")
(load "tests/mixed.lsp")
(load "tests/compiler.lsp")

#-ecl-bytecmp
(progn
  (load "tests/embedding.lsp")
  #+ffi (load "tests/foreign-interface.lsp"))

#+clos
(load "tests/metaobject-protocol.lsp")

#+threads
(load "tests/multiprocessing.lsp")

#+unicode
(load "tests/external-formats.lsp")
(load "tests/random-states.lsp")

(setf sb-rt::*expected-failures*
      (nconc sb-rt::*expected-failures*
             '(SEM-SIGNAL-N-PROCESSES
               SEM-SIGNAL-ONLY-N-PROCESSES
               SEM-INTERRUPTED-RESIGNALS)))

(time (sb-rt:do-tests))
