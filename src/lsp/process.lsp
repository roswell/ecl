;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  PROCESS.LSP  -- External processes

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "EXT")

(defstruct (external-process (:constructor make-external-process
                                           (pid input output)))
  pid
  input
  output
  (%status :running)
  (%code nil))

(defun external-process-status (external-process)
  (let ((status (external-process-%status external-process)))
    (when (eq status :running)
      (ext:external-process-wait external-process nil)
      (values status (external-process-%code external-process)))))
