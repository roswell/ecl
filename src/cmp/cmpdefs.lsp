;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    See file 'LICENSE' for the copyright details.

;;;; CMPDEF -- Definitions created at compile / configuration time

(in-package "COMPILER")

(defvar *config-options* '(*features*))

(defmacro defconfig (name value &optional docstring)
  "Define a configuration option. Like DEFVAR, but records the variable
for cross-compilation."
  `(progn
     ,(if docstring
          `(defvar ,name ,value ,docstring)
          `(defvar ,name ,value))
     (pushnew ',name *config-options*)))

;;; This is copied into each .h file generated, EXCEPT for system-p calls.
;;; The constant string *include-string* is the content of file "ecl.h".
;;; Here we use just a placeholder: it will be replaced with sed.
(defconfig *cmpinclude* "<ecl/ecl-cmp.h>")

(defconfig *cc* "@ECL_CC@"
  "This variable controls how the C compiler is invoked by ECL.
The default value is \"cc -I. -I/usr/local/include/\".
The second -I option names the directory where the file ECL.h has been installed.
One can set the variable appropriately adding for instance flags which the 
C compiler may need to exploit special hardware features (e.g. a floating point
coprocessor).")

(defconfig *ld* "@ECL_CC@"
  "This variable controls the linker which is used by ECL.")

(defconfig *ranlib* "@RANLIB@"
  "Name of the `ranlib' program on the hosting platform.")

(defconfig *ar* "@AR@"
  "Name of the `AR' program on the hosting platform.")

(defconfig *cc-flags* "@CPPFLAGS@ @CFLAGS@ @ECL_CFLAGS@")

(defconfig *cc-optimize* #-msvc "-O2"
           #+msvc "@CFLAGS_OPTIMIZE@")

(defconfig *ld-format* #-msvc "~A -o ~S -L~S ~{~S ~} ~@[~S~]~{ '~A'~} ~A"
           #+msvc "~A -Fe~S~* ~{~S ~} ~@[~S~]~{ '~A'~} ~A")

(defconfig *cc-format* (cond ((member :msvc *features*)
			      "~A -I. \"-I~A\" ~A ~:[~*~;~A~] -w -c \"~A\" -o \"~A\"~{ '~A'~}")
			     ((member :nacl *features*) ;; pnacl-clang doesn't support -w
			      "~A -I. \"-I~A\" ~A ~:[~*~;~A~] -c \"~A\" -o \"~A\"~{ '~A'~}")
			     (t
			      "~A -I. \"-I~A\" ~A ~:[~*~;~A~] -w -c \"~A\" -o \"~A\"~{ '~A'~}")))

(defconfig *ld-flags* "@LDFLAGS@")
#-dlopen
(defconfig *ld-libs* "-lecl @CORE_LIBS@ @FASL_LIBS@ @LIBS@")
#+dlopen
(defconfig *ld-libs* #-msvc "-lecl @FASL_LIBS@ @LIBS@" #+msvc "ecl.lib @CLIBS@")
(defconfig *ld-shared-flags* #+dlopen "@SHARED_LDFLAGS@ @LDFLAGS@" #-dlopen "")
(defconfig *ld-bundle-flags* #+dlopen "@BUNDLE_LDFLAGS@ @LDFLAGS@" #-dlopen "")
(defconfig *ld-program-flags* "@PROGRAM_LDFLAGS@ @LDFLAGS@")

(defconfig +shared-library-prefix+ "@SHAREDPREFIX@")
(defconfig +shared-library-extension+ "@SHAREDEXT@")
(defconfig +shared-library-format+ "@SHAREDPREFIX@~a.@SHAREDEXT@")
(defconfig +static-library-prefix+ "@LIBPREFIX@")
(defconfig +static-library-extension+ "@LIBEXT@")
(defconfig +static-library-format+ "@LIBPREFIX@~a.@LIBEXT@")
(defconfig +object-file-extension+ "@OBJEXT@")
(defconfig +executable-file-format+ "~a@EXEEXT@")

(defconfig *ecl-include-directory* "@includedir\@/")
(defconfig *ecl-library-directory* "@libdir\@/")
(defconfig *ecl-data-directory* "@ecldir\@/")

(defconfig *ld-rpath*
    (let ((x "@ECL_LDRPATH@"))
      (and (plusp (length x))
           (format nil x *ecl-library-directory*))))

(defconfig *target-architecture* "@ARCHITECTURE@")
(defconfig *target-software-type* "@SOFTWARE_TYPE@")
(defconfig *target-lisp-implementation-version* "@PACKAGE_VERSION@")
(defconfig *target-identifier* "@TARGET_IDENTIFIER@")
