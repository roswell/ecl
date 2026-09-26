;;; build-binary.lisp -- Build "my-application" binary using ECL
;;;
;;; SPDX-License-Identifier: BSD-2-Clause
;;;
;;; Copyright (c) 2026, Thomas Fitzsimmons <fitzsim@fitzsim.org>

(require "asdf")

;; Push to asdf:*central-registry* the full pathnames of all directories
;; that contain dependency .asd files.
(push (merge-pathnames #p"" (uiop:getcwd)) asdf:*central-registry*)
;; (push (merge-pathnames #p"other-asdf-dependency" (uiop:getcwd))
;;       asdf:*central-registry*)

;; Load my-application system.
(asdf:load-system :my-application)

;; Build libmy-application.a.
(asdf:make-build :my-application
                 :type :static-library
                 :move-here (uiop:getcwd)
                 :monolithic t
                 :init-name "init_lib_MY_APPLICATION")

;; Link executable.
(uiop:run-program
 (format nil "gcc -g -I /ecl-installation/include ~
              my-application.c ~
              -o my-application ~
              libmy-application.a ~
              /ecl-installation/lib/ecl-26.5.5/libasdf.a ~
              /ecl-installation/lib/ecl-26.5.5/libsb-bsd-sockets.a ~
              /ecl-installation/lib/ecl-26.5.5/libsockets.a ~
              /ecl-installation/lib/libecl.a ~
              /ecl-installation/lib/libeclgc.a ~
              /ecl-installation/lib/libeclatomic.a ~
              -lffi -lgmp -lpthread -ldl -lm"))

;; Strip executable.
(uiop:run-program "strip my-application")

;; Quit read-eval-print-loop.
(uiop:quit)
