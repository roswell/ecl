;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;;  CMPPACKAGE -- Package definitions and exported symbols
;;;;

(defpackage #:c
  (:nicknames #:compiler)
  (:use #:ffi #:ext #+threads #:mp #:cl)
  (:export
   ;; Flags controlling the compiler behavior.
   #:*compiler-break-enable*
   #:*compile-print*
   #:*compile-to-linking-call*
   #:*compile-verbose*
   #:*compiler-features*
   #:*cc*
   #:*cc-optimize*
   #:*user-cc-flags*
   #:*user-ld-flags*
   #:*suppress-compiler-messages*
   ;; Build targets. BUILD-ECL is not defined, preasumbly it was meant
   ;; for cross compilation.
   #:build-ecl
   #:build-program
   #:build-fasl
   #:build-static-library
   #:build-shared-library
   ;; Conditions (and their accessors).
   #:compiler-warning
   #:compiler-note
   #:compiler-message
   #:compiler-error
   #:compiler-fatal-error
   #:compiler-internal-error
   #:compiler-undefined-variable
   #:compiler-message-file
   #:compiler-message-file-position
   #:compiler-message-form
   ;; Other operators.
   #:install-c-compiler
   #:update-compiler-features)
  (:import-from #:si
                #:get-sysprop #:put-sysprop #:rem-sysprop #:macro
                #:*compiler-constants* #:register-global
                #:cmp-env-register-macrolet #:compiler-let))

(ext:package-lock '#:cl nil)
