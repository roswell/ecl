;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPC-PASS  Optimization specific to each backend
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia Ripoll.
;;;;
;;;;    ECL is free software; you can redistribute it and/or modify it
;;;;    under the terms of the GNU Library General Public License as
;;;;    published by the Free Software Foundation; either version 2 of
;;;;    the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "C-BACKEND")

(defun c-backend-passes ()
  (replace-optimizable-constants)
  (execute-pass 'pass-consistency)
  (execute-pass 'pass-delete-no-side-effects)
  (execute-pass 'pass-delete-unused-bindings)
  (execute-pass 'pass-decide-var-rep-types)
  (execute-pass 'pass-assign-labels))
