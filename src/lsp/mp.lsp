;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  MP.LSP  -- Multiprocessing capabilities.

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

#-threads
(defpackage "MP"
  (:use "CL")
  (:export "WITH-LOCK" "WITHOUT-INTERRUPTS"))

(in-package "MP")

(defmacro with-lock ((lock) &body body)
  #-threads
  `(progn ,@body)
  ;; Why do we need %count? Even if get-lock succeeeds, an interrupt may
  ;; happen between the end of get-lock and when we save the output of
  ;; the function. That means we lose the information and ignore that
  ;; the lock was actually acquired. Furthermore, a lock can be recursive
  ;; and mp:lock-holder is also not reliable.
  #+threads
  `(let* ((%the-lock ,lock)
          (%count (mp:lock-count %the-lock)))
     (unwind-protect
          (progn
            (mp::get-lock %the-lock)
            ,@body)
       (when (> (mp:lock-count %the-lock) %count)
         (mp::giveup-lock %the-lock)))))

(defmacro without-interrupts (&body body)
  `(let ((si:*interrupt-enable* nil))
    (multiple-value-prog1
	(progn ,@body)
      (si::check-pending-interrupts))))
