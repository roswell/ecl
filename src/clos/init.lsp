;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: CLOS -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
(load "../util/system")
(load "defsys")
(load "cmpinit")
(setq compiler:*cc* (concatenate 'STRING compiler:*cc* " -I../h -I../../linux/h"))
(rename-package 'clos 'old-clos)
(setq si:*gc-verbose* nil)
(sbt:build-system clos :compile :force)
;(setq *print-circle* t)
;(allocate 'cons 800 t)
(quit)
