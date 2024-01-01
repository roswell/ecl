;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;; CMPBACKEND-CXX -- backend for compiling to C99/C++ and then invoking the
;;;; external compiler.

(in-package "COMPILER")


;;; External tool wrappers
(defun safe-mkstemp (template)
  ;; We do several things here. One is to check for success in MKSTEMP,
  ;; the other one is to ensure that the output of this function _always_
  ;; carries a file type -- this solves a problem with filesystems where
  ;; mkstemp may introduce one or more dots in the name causing several
  ;; functions below to ignore parts of the name. Note that this forces
  ;; us to have two files per temp: one with and one without extension.
  (let ((base (ext:mkstemp template)))
    (unless base
      (error "Unable to create temporary file~%~
        ~AXXXXXX
Make sure you have enough free space in disk, check permissions or set~%~
the environment variable TMPDIR to a different value." template))
    (let ((output (make-pathname :name
                                 (concatenate 'string (pathname-name base)
                                              (or (pathname-type base) ""))
                                 :type "tmp"
                                 :defaults base)))
      (if (and (not (probe-file output))
               (si:copy-file base output))
          (setf base (list (truename output) (truename base)))
          (progn (delete-file base)
                 (setf base nil))))
    base))

#+msvc
(defun delete-msvc-generated-files (output-pathname)
  (loop for i in '("implib" "exp" "ilk" )
        for full = (make-pathname :type i :defaults output-pathname)
        for truename = (probe-file full)
        when truename
        do (cmp-delete-file truename)))

#+msvc
(defun embed-manifest-file (o-file &optional (type :dll))
  (let* ((real-file (probe-file o-file)))
    (when real-file
      (let* ((manifest-namestring (concatenate 'string (namestring o-file)
                                               ".manifest"))
             (resource-code (ecase type
                              ((:dll :shared-library :fasl :fas) 2)
                              ((:program) 1)))
             (resource-option (format nil "-outputresource:~A;~D"
                                      (namestring real-file)
                                      resource-code))
             (manifest (probe-file manifest-namestring)))
        (when manifest
          (safe-run-program "mt"
                            (list "-nologo"
                                  "-manifest"
                                  manifest-namestring
                                  resource-option))
          (delete-file manifest))))))

(defun cmp-delete-file (file)
  (cond ((null *delete-files*))
        ((ext:getenv "ECL_PRESERVE_FILES"))
        ((null (probe-file file)))
        (*debug-compiler*
         (cmpprogress "~%Postponing deletion of ~A" file)
         (push file *files-to-be-deleted*))
        (t
         (delete-file file))))

(push #'(lambda () (mapc #'delete-file *files-to-be-deleted*))
      si::*exit-hooks*)

#-mingw32
(defmacro fix-for-mingw (directory-namestring)
  directory-namestring)

#+mingw32
(defun fix-for-mingw (directory-namestring)
  (let ((x (string-right-trim '(#\\ #\/) directory-namestring)))
    (if (zerop (length x)) "/" x)))

(defun get-deprecated-user-ld-flags ()
  (let ((flags (split-program-options *user-ld-flags*)))
    (when flags
      (cmpwarn "The variable ~s is deprecated, please use ~s and ~s instead."
              '*user-ld-flags* '*user-linker-flags* '*user-linker-libs*))
    flags))

#+msvc
(defun linker-cc (o-pathname object-files &key
                  (type :program)
                  (ld-flags (split-program-options (if (eq type :program)
                                                       *ld-program-flags*
                                                       *ld-flags*)))
                  (ld-libs (split-program-options *ld-libs*)))
  (safe-run-program
   *ld*
   `(,(concatenate 'string "-Fe" (brief-namestring o-pathname))
     ,@(split-program-options *ld-rpath*)
     ,@(split-program-options *user-linker-flags*)
     ,@object-files
     ,@ld-flags
     ,@(split-program-options *user-linker-libs*)
     ,@(get-deprecated-user-ld-flags)
     ,@ld-libs
     ,(if (eq type :program)
          (concatenate 'string "/IMPLIB:prog" (file-namestring o-pathname) ".lib")
          "")
     ,(concatenate 'string "/LIBPATH:"
                   (ecl-library-directory))))
  (embed-manifest-file o-pathname type)
  (delete-msvc-generated-files o-pathname))

#-msvc
(defun linker-cc (o-pathname object-files &key
                  (type :program)
                  (ld-flags (split-program-options (if (eq type :program)
                                                       *ld-program-flags*
                                                       *ld-flags*)))
                  (ld-libs (split-program-options *ld-libs*)))
  (declare (ignore type))
  (safe-run-program
   *ld*
   `("-o" ,(brief-namestring o-pathname)
     ,(concatenate 'string "-L" (fix-for-mingw (ecl-library-directory)))
     ,@(split-program-options *user-linker-flags*)
     ,@ld-flags
     ,@object-files
     ,@(and *ld-rpath* (list *ld-rpath*))
     ,@(split-program-options *user-linker-libs*)
     ,@(get-deprecated-user-ld-flags)
     ,@ld-libs)))

(defun linker-ar (output-name o-name ld-libs)
  #-msvc
  (static-lib-ar (namestring output-name)
                 (list* (brief-namestring o-name) ld-libs))
  #+msvc
  (unwind-protect
       (progn
         (with-open-file (f "static_lib.tmp" :direction :output
                            :if-does-not-exist :create :if-exists :supersede)
           (format f "/OUT:~A ~A ~{~&\"~A\"~}"
                   output-name o-name ld-libs))
         (safe-run-program "link" '("-lib" "-nologo" "@static_lib.tmp")))
    (when (probe-file "static_lib.tmp")
      (cmp-delete-file "static_lib.tmp"))))

(defun static-lib-ar (lib object-files)
  (let ((lib (brief-namestring lib)))
    (when (probe-file lib)
      (delete-file lib))
    (safe-run-program *ar* (list* "cr" lib (mapcar #'brief-namestring object-files)))
    (safe-run-program *ranlib* (list lib))))

(defun compiler-cc (c-pathname o-pathname)
  (safe-run-program
   *cc*
   `("-I."
     ,@(precompiled-header-flags)
     ,(concatenate 'string "-I" (fix-for-mingw (ecl-include-directory)))
     ,@(split-program-options *cc-flags*)
     ,@(and (>= (cmp-env-optimization 'speed) 2)
            (split-program-options *cc-optimize*))
     "-c"
     ,(brief-namestring c-pathname)
     #-msvc
     ,@(list "-o" (brief-namestring o-pathname))
     #+msvc
     ,(concatenate 'string "-Fo" (brief-namestring o-pathname))
     ,@(split-program-options *user-cc-flags*))))
;;; Since the SUN4 assembler loops with big files, you might want to use this:
;;;   (format nil "~A ~@[~*-O1~] -S -I. -I~A -w ~A ; as -o ~A ~A"
;;;          *cc* (>= *speed* 2)
;;;          *include-directory*
;;;          (namestring c-pathname)
;;;          (namestring o-pathname)
;;;          (namestring s-pathname))


(defun need-to-dump-precompiled-header ()
  (let* ((config *precompiled-header-cc-config*)
         (need-to-dump (or (null config)
                           (not (eq (svref config 0) *cc*))
                           (not (eq (svref config 1) (ecl-include-directory)))
                           (not (eq (svref config 2) *cc-flags*))
                           (not (eq (svref config 3) *cc-optimize*))
                           (not (eq (svref config 4) *user-cc-flags*)))))
    (when need-to-dump
      (setf *precompiled-header-cc-config*
            (vector *cc* (ecl-include-directory) *cc-flags*
                    *cc-optimize* *user-cc-flags*)))
    need-to-dump))

(defun precompiled-header-flags ()
  (when *use-precompiled-headers*
    (when (need-to-dump-precompiled-header)
      (handler-case
          (dump-precompiled-header)
        (error (err)
          (setf *use-precompiled-headers* nil
                *precompiled-header-flags* nil
                *precompiled-header-cc-config* nil)
          (cmpnote "Disabling precompiled header files due to error:~%  ~A" err))))
    *precompiled-header-flags*))

#+msvc
(defun dump-precompiled-header ()
  ;; The way precompiled headers work on msvc is not compatible with
  ;; what we want to use them for. The msvc compiler creates a
  ;; precompiled header file out of ordinary source files by
  ;; processing them up to a certain point at which all needed headers
  ;; are included. This creates both a precompiled header and a object
  ;; file. The object file created by this compilation must be
  ;; included in all binaries which are linked together from other
  ;; source files compiled using the precompiled header. Thus, we
  ;; would need to include the first object file created in a session
  ;; in all further object files if we wanted to support that.
  (error "Precompiled headers are not supported for msvc."))

#-msvc
(defun dump-precompiled-header ()
  (let* ((input-file (make-pathname
                      :directory (append (pathname-directory (ecl-include-directory))
                                         '("ecl"))
                      :defaults (ecl-include-directory)
                      :name "ecl-cmp"
                      :type "h"))
         (output-dir (merge-pathnames
                      (format nil "ecl-include~4,'0x/" (random #xffff))
                      (translate-logical-pathname "TMP:")))
         (output-file (compile-file-pathname
                       (make-pathname :name "ecl-cmp" :defaults output-dir)
                       :type :precompiled-header)))
        (ensure-directories-exist output-dir)
        (push output-dir *files-to-be-deleted*)
        (safe-run-program
         *cc*
         `("-x" "c-header"
                ,(fix-for-mingw (namestring input-file))
                ,(concatenate 'string "-I" (fix-for-mingw (ecl-include-directory)))
                ,@(split-program-options *cc-flags*)
                ,@(split-program-options *cc-optimize*)
                "-o"
                ,(fix-for-mingw (namestring output-file))
                ,@(split-program-options *user-cc-flags*)))
        (push output-file *files-to-be-deleted*)
        (setf *precompiled-header-flags*
              (list (concatenate 'string "-I" (namestring output-dir))
                    "-include"
                    (concatenate 'string (namestring output-dir) "ecl-cmp.h")))))


;;; Collecting necessary information

(defun ecl-include-directory ()
  "Finds the directory in which the header files were installed."
  (cond ((and *ecl-include-directory*
              (probe-file (merge-pathnames "ecl/config.h" *ecl-include-directory*)))
         *ecl-include-directory*)
        ((probe-file "SYS:ecl;config.h")
         (setf *ecl-include-directory* (namestring (translate-logical-pathname "SYS:"))))
        ((error "Unable to find include directory"))))

(defun ecl-library-directory ()
  "Finds the directory in which the ECL core library was installed."
  (cond ((and *ecl-library-directory*
              (probe-file (merge-pathnames (compile-file-pathname "ecl" :type
                                            #+dlopen :shared-library
                                            #-dlopen :static-library)
                                           *ecl-library-directory*)))
         *ecl-library-directory*)
        ((probe-file "SYS:BUILD-STAMP")
         (setf *ecl-library-directory* (namestring (translate-logical-pathname "SYS:"))))
        ((error "Unable to find library directory"))))

(defun guess-kind (pathname)
  "Given a file name, guess whether it is an object file, a library, a program
or a loadable module."
  (let ((record (assoc (pathname-type pathname)
                       '((#.+object-file-extension+ :object)
                         ("o" :object)
                         ("obj" :object)
                         ("c" :c)
                         (#.+static-library-extension+ :static-library)
                         ("lib" :static-library)
                         ("a" :static-library)
                         (#.+shared-library-extension+ :shared-library)
                         ("dylib" :shared-library)
                         ("dll" :shared-library)
                         ("so" :shared-library)
                         ("fas" :fasl))
                       :test #'string-equal)))
    (if record
        (second record)
        (progn
          (warn "File ~s is of no known file type. Assuming it is an object file."
                pathname)
          :object))))

(defun guess-ld-libs (pathname &key (kind (guess-kind pathname)))
  "Given a file name, return the compiler command line argument to link this file in."
  (case kind
    ((:object :c)
     (brief-namestring pathname))
    ((:fasl :fas)
     nil)
    ((:static-library :lib)
     (brief-namestring pathname))
    ((:shared-library :dll)
     (brief-namestring pathname))
    ((:program)
     nil)
    (otherwise
     (error "C::BUILDER cannot accept files of kind ~s" kind))))

(defun system-ld-flag (library)
  "Given a symbol, try to find a library that matches it, either by looking in the
filesystem or in the database of ASDF modules."
  (let ((asdf #+asdf (find-package "ASDF"))
        system)
    (labels ((asdfsym (x) (find-symbol (string x) asdf))
             (asdfcall (fun &rest rest) (apply (asdfsym fun) rest))
             (system-output (system type)
               (let ((build (make-instance (asdfsym :build-op) :type type)))
                 (first (asdfcall :output-files build system))))
             (existing-system-output (system type)
               (let ((o (system-output system type)))
                 (and o (setf o (probe-file o)) (namestring o))))
             (find-archive (system)
                 (or (existing-system-output system :library)
                     (existing-system-output system :shared-library)))
             (fallback ()
                 (translate-logical-pathname
                  (merge-pathnames
                   "SYS:"
                   (compile-file-pathname (string-downcase library)
                                          :type :library)))))
      (or
       #-ecl-min
       (and asdf
            (setf system (asdfcall :find-system library nil))
            (find-archive system))
       (fallback)))))


;;; Target-specific invocations.

#+dlopen
(defun shared-cc (o-pathname object-files)
  (let ((ld-flags (split-program-options *ld-shared-flags*))
        (ld-libs (split-program-options *ld-libs*)))
    #+msvc
    (setf ld-flags
          (let ((implib (brief-namestring
                         (compile-file-pathname o-pathname :type :lib))))
            ;; MSVC linker options are added at the end, after the
            ;; /link flag, because they are not processed by the
            ;; compiler, but by the linker
            (append ld-flags
                    (list (concatenate 'string "/LIBPATH:"
                                       (ecl-library-directory))
                          (concatenate 'string "/IMPLIB:" implib)))))
    #+mingw32
    (setf ld-flags (list* "-shared" ld-flags))
    (linker-cc o-pathname object-files :type :dll
               :ld-flags ld-flags :ld-libs ld-libs)))

#+dlopen
(defun bundle-cc (o-pathname init-name object-files)
  (declare (ignore init-name))
  (let ((ld-flags (split-program-options *ld-bundle-flags*))
        (ld-libs (split-program-options *ld-libs*)))
    #+msvc
    (setf ld-flags
          (let ((implib (brief-namestring
                         (compile-file-pathname o-pathname :type :import-library))))
            ;; MSVC linker options are added at the end, after the
            ;; /link flag, because they are not processed by the
            ;; compiler, but by the linker
            (append ld-flags
                    (list
                     ;; Not needed because we use ECL_DLLEXPORT
                     ;; (concatenate 'string "/EXPORT:" init-name)
                     (concatenate 'string "/LIBPATH:"
                                  (ecl-library-directory))
                     (concatenate 'string "/IMPLIB:" implib)))))
    #+mingw32
    (setf ld-flags (list* "-shared" "-Wl,--export-all-symbols" ld-flags))
    (linker-cc o-pathname object-files :type :fasl
                                       :ld-flags ld-flags :ld-libs ld-libs)))

(defconstant +lisp-program-header+ "
#include <ecl/ecl.h>

#ifdef __cplusplus
#define ECL_CPP_TAG \"C\"
#else
#define ECL_CPP_TAG
#endif

~:{     extern ECL_CPP_TAG void ~A(cl_object);~%~}

")

;;
;; This format string contains the structure of the code that initializes
;; a program, a library, a module, etc. Basically, it processes a codeblock
;; just like in a normal compiled file, but then adds all the codeblocks of
;; its corresponding modules.
;;
(defconstant +lisp-program-init+ "
#ifdef __cplusplus
extern \"C\"
#endif

ECL_DLLEXPORT
void ~A(cl_object cblock)
{
        /*
         * This function is first invoked with a pointer to a Cblock
         * structure, so that the function initializes it, and then
         * it is invoked with OBJNULL, to force initialization.
         */
        static cl_object Cblock = OBJNULL;
        if (cblock != OBJNULL) {
                Cblock = cblock;
#ifndef ECL_DYNAMIC_VV
                cblock->cblock.data = NULL;
#endif
                cblock->cblock.data_size = 0;
                return;
        }
        Cblock->cblock.data_text = (const cl_object *)\"~A\";
        ~A
{
        /*
         * At this point Cblock contains the cblock of the parent.
         * Notice how the modules are linked to the parent forming a
         * circular chain. This disables the garbage collection of
         * the library until _ALL_ functions in all modules are unlinked.
         */
        cl_object current = OBJNULL, next = Cblock;
~:{
        current = ecl_make_codeblock();
        current->cblock.next = next;
        next = current;
        ecl_init_module(current, ~A);
~}
        Cblock->cblock.next = current;
}
        ~A
}
")

(defconstant +lisp-init-wrapper+ "
#ifdef __cplusplus
extern \"C\"
#endif

ECL_DLLEXPORT
void ~A(cl_object cblock)
{
        /* This is a wrapper around the randomized init function name. */
        ~A(cblock);
}
")

(defconstant +lisp-program-main+ "
extern int
main(int argc, char **argv)
{
        cl_boot(argc, argv);
        ECL_CATCH_ALL_BEGIN(ecl_process_env()) {
        ~A
        ecl_init_module(OBJNULL, ~A);
        ~A
        } ECL_CATCH_ALL_END;
        si_exit(0);
}
")

(defconstant +lisp-library-main+ "
extern int
~A(int argc, char **argv)
{
        cl_boot(argc, argv);
        ECL_CATCH_ALL_BEGIN(ecl_process_env()) {
        ~A
        ecl_init_module(OBJNULL, ~A);
        ~A
        } ECL_CATCH_ALL_END;
        return 0;
}
")

#+:win32
(defconstant +lisp-program-winmain+ "
#include <windows.h>
int
WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
        char **argv;
        int argc;
        ecl_get_commandline_args(&argc, &argv);
        cl_boot(argc, argv);
        ECL_CATCH_ALL_BEGIN(ecl_process_env()) {
        ~A
        ecl_init_module(OBJNULL, ~A);
        ~A
        } ECL_CATCH_ALL_END;
        si_exit(0);
        for (int i = 0; i < argc; i++) {
          LocalFree(argv[i]);
        }
        LocalFree(argv);
}
")


;;; Code assembly

(defun compiler-pass/assemble-cxx (input-file output-file
                                   &key
                                     (c-file nil)
                                     (h-file nil)
                                     (data-file nil)
                                     (system-p nil)
                                   &allow-other-keys)
  (let* ((cpath (compile-file-pathname output-file :output-file c-file :type :c))
         (hpath (compile-file-pathname output-file :output-file h-file :type :h))
         (dpath (compile-file-pathname output-file :output-file data-file :type :data))
         (opath (compile-file-pathname output-file :type :object))
         (to-delete (nconc (unless c-file (list cpath))
                           (unless h-file (list hpath))
                           (unless data-file (list dpath))
                           (unless system-p (list opath))))
         (init-name (compute-init-name output-file :kind (if system-p :object :fasl))))
    (compiler-pass/generate-cxx cpath hpath dpath init-name input-file)
    (if system-p
        (compiler-cc cpath opath)
        (progn
          (compiler-cc cpath opath)
          (bundle-cc (brief-namestring output-file)
                     init-name
                     (list (brief-namestring opath)))))
    (mapc 'cmp-delete-file to-delete)))


;;; The builder.

(defun builder (target output-name
                &key
                  lisp-files ld-flags ld-libs
                  (init-name nil)
                  (main-name nil)
                  (prologue-code "")
                  (epilogue-code (when (eq target :program) '(SI::TOP-LEVEL T)))
                  #+:win32 (system :console)
                  &aux
                    (*suppress-compiler-messages* (or *suppress-compiler-messages*
                                                      (not *compile-verbose*)))
                    (target (normalize-build-target-name target))
                    (output-name (if (or (symbolp output-name) (stringp output-name))
                                     (compile-file-pathname output-name :type target)
                                     output-name))
                    ;; wrap-name is the init function name defined by a programmer
                    (wrap-name init-name))
  ;; init-name should always be unique
  (setf init-name (compute-init-name output-name :kind target))
  (cond ((null wrap-name) nil)
        ((equal init-name wrap-name)    ; fixup for ASDF
         (cmpwarn "Parameter `init-name' is the same as the result of an internal function `compute-init-name'. Ignoring.")
         (setf wrap-name nil))
        ((null (member target '(:static-library :shared-library)))
         (cmpwarn "Supplying `init-name' is valid only for libraries. Ignoring.")))
  (unless main-name
    (setf main-name (compute-init-name output-name :kind target :prefix "main_")))


  ;;
  ;; The epilogue-code can be either a string made of C code, or a
  ;; lisp form.  In the latter case we add some additional C code to
  ;; clean up, and the lisp form is stored in a text representation,
  ;; to avoid using the compiler.
  ;;
  (cond ((null epilogue-code)
         (setf epilogue-code ""))
        ((stringp epilogue-code)
         nil)
        (t
         (with-standard-io-syntax
           (setq epilogue-code
                 (with-output-to-string (stream)
                   (princ "{ const char *lisp_code = " stream)
                   (wt-filtered-data (write-to-string epilogue-code) stream)
                   (princ ";
cl_object output;
si_select_package(ecl_make_constant_base_string(\"CL-USER\", 7));
output = si_safe_eval(2, ecl_read_from_cstring(lisp_code), ECL_NIL);
}" stream)
                   )))))
  (cond ((null prologue-code)
         (setf prologue-code ""))
        ((stringp prologue-code)
         )
        (t
         (with-standard-io-syntax
           (setq prologue-code
                 (with-output-to-string (stream)
                   (princ "{ const char *lisp_code = " stream)
                   (wt-filtered-data (write-to-string prologue-code) stream)
                   (princ ";
cl_object output;
si_select_package(ecl_make_constant_base_string(\"CL-USER\", 7));
output = si_safe_eval(2, ecl_read_from_cstring(lisp_code), ECL_NIL);
}" stream)
                   )))))
  ;;
  ;; When a module is built out of several object files, we have to
  ;; create an additional object file that initializes those ones.
  ;; This routine is responsible for creating this file.
  ;;
  ;; To avoid name clashes, this object file will have a temporary
  ;; file name (tmp-name).
  ;;
  (let* ((tmp-names (safe-mkstemp #P"TMP:ECLINIT"))
         (tmp-name (first tmp-names))
         (c-name (brief-namestring
                  (compile-file-pathname tmp-name :type :c)))
         (o-name (brief-namestring
                  (compile-file-pathname tmp-name :type :object)))
         submodules
         c-file)
    (dolist (item (reverse lisp-files))
      (let* ((path (etypecase item
                     (symbol (system-ld-flag item))
                     (pathname item)
                     (string (parse-namestring item))))
             (kind (guess-kind path)))

        ;; Shared and static libraries may be linked in a program or
        ;; fasl, but if we try to create a `static-library' from two
        ;; static libraries we will end with broken binary because
        ;; `ar' works fine only with object files. See #274.
        (unless (member kind `(,@(unless (eql target :static-library)
                                   '(:shared-library :static-library))
                               :object :c))
          (error "C::BUILDER does not accept a file ~s of kind ~s for target ~s" item kind target))
        (let ((init-fn (guess-init-name path kind))
              (guessed-libs (guess-ld-libs path)))
          ;; We should give a warning that we cannot link this module in
          (when guessed-libs
            (push guessed-libs ld-libs))
          (when init-fn
            (push (list init-fn path) submodules)))))
    (setq c-file (open c-name :direction :output :external-format :default))
    (format c-file +lisp-program-header+ submodules)

    (let ((init-tag (init-name-tag init-name :kind target)))
      (ecase target
        (:program
         (format c-file +lisp-program-init+ init-name init-tag "" submodules "")
         ;; we don't need wrapper in the program, we have main for that
                                        ;(format c-file +lisp-init-wrapper+ wrap-name init-name)
         (format c-file
                 #+:win32 (ecase system
                            (:console +lisp-program-main+)
                            (:windows +lisp-program-winmain+))
                 #-:win32 +lisp-program-main+
                 prologue-code init-name epilogue-code)
         (close c-file)
         (compiler-cc c-name o-name)
         (linker-cc output-name (append ld-flags (list (namestring o-name))
                                        ld-libs)))
        (:static-library
         (format c-file +lisp-program-init+
                 init-name init-tag prologue-code submodules epilogue-code)
         (when wrap-name
           (format c-file +lisp-init-wrapper+ wrap-name init-name))
         (format c-file +lisp-library-main+
                 main-name prologue-code init-name epilogue-code)
         (close c-file)
         (compiler-cc c-name o-name)
         (when (probe-file output-name) (delete-file output-name))
         (linker-ar output-name o-name ld-libs))
        #+dlopen
        (:shared-library
         (format c-file +lisp-program-init+
                 init-name init-tag prologue-code submodules epilogue-code)
         (when wrap-name
           (format c-file +lisp-init-wrapper+ wrap-name init-name))
         (format c-file +lisp-library-main+
                 main-name prologue-code init-name epilogue-code)
         (close c-file)
         (compiler-cc c-name o-name)
         (shared-cc output-name (append ld-flags (list o-name)
                                        ld-libs)))
        #+dlopen
        (:fasl
         (format c-file +lisp-program-init+ init-name init-tag prologue-code
                 submodules epilogue-code)
         ;; we don't need wrapper in the fasl, we scan for init function name
                                        ;(format c-file +lisp-init-wrapper+ wrap-name init-name)
         (close c-file)
         (compiler-cc c-name o-name)
         (bundle-cc output-name init-name (append ld-flags (list o-name)
                                                  ld-libs))))
      (mapc 'cmp-delete-file tmp-names)
      (cmp-delete-file c-name)
      (cmp-delete-file o-name)
      output-name)))

(defun build-fasl (&rest args)
  (apply #'builder :fasl args))

(defun build-program (&rest args)
  (apply #'builder :program args))

(defun build-static-library (&rest args)
  (apply #'builder :static-library args))

(defun build-shared-library (&rest args)
  #-dlopen
  (error "Dynamically loadable libraries not supported in this system.")
  #+dlopen
  (apply #'builder :shared-library args))
