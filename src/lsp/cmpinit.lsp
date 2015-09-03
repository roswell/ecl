;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;

(in-package "SYSTEM")

;;; Disable PDE facilities within LISP kernel:
(setq *features* (delete ':pde *features*))
;;; Disable record-source-pathname within LISP kernel:
(defmacro record-source-pathname (x y))
