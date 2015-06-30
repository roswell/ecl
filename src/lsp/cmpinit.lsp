;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; indent-tabs-mode: nil -*-
;;;;

(in-package "SYSTEM")

;;; Disable PDE facilities within LISP kernel:
(setq *features* (delete ':pde *features*))
;;; Disable record-source-pathname within LISP kernel:
(defmacro record-source-pathname (x y))
