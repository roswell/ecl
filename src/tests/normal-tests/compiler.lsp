;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Compiler regression tests

(in-package :cl-test)

(suite 'cmp)


;; cl-001

;;; Date: 09/05/2006
;;; From: Brian Spilsbury
;;; Fixed: 20/05/2006 (Brian Spilsbury)
;;; Description:
;;;
;;;     (DEFPACKAGE "FOO" (:USE) (:IMPORT-FROM "CL" "NIL" "T"))
;;;     fails to import symbol NIL because IMPORT is invoked as
;;;     (IMPORT NIL (find-package "CL")), which does not import
;;;     any symbol.
;;;
(test cmp.0001.import
  (defpackage "FOO" (:USE) (:IMPORT-FROM "CL" "NIL" "T"))
  (multiple-value-bind (symbol access)
      (find-symbol "NIL" (find-package "FOO"))
    (is (and (eql symbol NIL)
             (eql access :INTERNAL))))
  (delete-package "FOO"))

;;; Date: 09/05/2006
;;; From: Brian Spilsbury
;;; Fixed: 20/05/2006 (Brian Spilsbury)
;;; Description:
;;;
;;;      Compiled FLET forms failed to shadow global macro definitions, if not
;;;      for the compiler, at least for MACRO-FUNCTION and MACROEXPAND[-1]
;;;
(test cmp.0002.macro-shadow
  (with-compiler ("aux-cl-0002.lsp" :load t)
    '(defmacro foo () 2)
    '(defmacro bar (symbol &environment env)
      (and (macro-function symbol env) t))
    '(defun doit () (flet ((foo () 1)) (bar foo))))
  (delete-file "aux-cl-0002.lsp")
  (delete-file (compile-file-pathname "aux-cl-0002" :type :fas))
  (is-false (doit))
  (fmakunbound 'doit)
  (fmakunbound 'bar)
  (fmakunbound 'foo))

;;;
;;; Fixed: 14/06/2006 (juanjo)
;;; Description:
;;;
;;;     APROPOS, APROPOS-LIST and HELP* are case sensitive.
;;;
(test cmp.0003.apropos
  (is (equal (apropos-list "bin")
             (apropos-list "bin"))))

;;; Date: 08/07/2006 (Dave Roberts)
;;; Fixed: 02/08/2006 (juanjo)
;;; Description:
;;;
;;;     SLIME traps when invoking DESCRIBE. Reason is that STREAMP breaks on
;;;     Gray streams.
;;;
(test cmp.0004.streamp
  (is-true (streamp (make-instance 'gray:fundamental-stream))))

;;; Date: 02/08/2006 (juanjo)
;;; Description:
;;;
;;;     There is a problem with SUBTYPEP and type STREAM
;;;
(defclass gray-stream-test (gray:fundamental-character-output-stream) ())
(test cmp.0005.subtypep-stream
  (is (equal (multiple-value-list
              (subtypep (find-class 'gray:fundamental-stream) 'stream))
             (list t t)))
  (is (equal (multiple-value-list
              (subtypep (find-class 'gray-stream-test) 'stream))
             (list t t))))

;;; Date: 09/07/2006 (Tim S)
;;; Fixed: 09/07/2006 (Tim S)
;;; Description:
;;;
;;;     ENOUGH-NAMESTRING provided too large pathnames even when the
;;;     pathname was a subdirectory of the default pathname.
;;;
;;; Date: 31/12/2006 (Richard M. Kreuter)
;;; Fixed: 5/1/2007 (Juanjo)
;;; Description:
;;;     ENOUGH-NAMESTRING does not simplify the pathname when the
;;;     directory matches completely that of the default path.
;;;


(ext:with-clean-symbols (*enough-namestring_tests*)
  (defvar *enough-namestring_tests*
    `(("/A/b/C/"
       ("/A/b/C/drink-up.sot"
        "/A/b/C/loozer/whiskey.sot"
        "/A/b/C/loozer/whiskey"
        "/A/b/whiskey.sot"
        "/A/"
        "whiskey.sot"
        "loozer/whiskey.sot"
        "C/loozer/whisky.sot"
        ""))
      ("A/b/C" ("A/b/C" "A/b/C/loozer" "b/C" "/A/b/C" "/A/" ""))
      ("/" ("/A/b/C/drink-up.sot" "/A/b/C/" "/A/" ""))
      ("" ("/A/b/C/drink-up.sot" "/A/b/C/loozer/whiskey.sot"
                                 "/A/b/C/loozer/whiskey" "/A/b/whiskey.sot"
                                 "/A/" "whiskey.sot" "loozer/whiskey.sot" "C/loozer/whisky.sot"))
      ("/A/*/C/drink-up.sot"
       ("/A/*/C/drink-up.sot" "/A/b/C/drink-up.sot" "/A/b/C/loozer/whiskey.*"
                              "/A/b/C/loozer/*.sot" "/A/**/whiskey.sot" ""))
      ("/A/b/../c/d.sot" ("/A/b/../c/d.sot" "/A/b/../c/D/e.sot"
                                            "/A/c/d.sot" "../c/d.sot"
                                            "c/e/d.sot"))))

  (test cmp.0006.enough-namestring
    (labels ((test-path (path defaults)
               (let* ((e-ns (enough-namestring path defaults))
                      (d1 (pathname-directory path))
                      (d2 (pathname-directory defaults))
                      (d3 (pathname-directory e-ns)))
                 (and (equalp (merge-pathnames e-ns defaults)
                              (merge-pathnames (parse-namestring path nil defaults)
                                               defaults))
                      ;; If directories concide, the "enough-namestring"
                      ;; removes the directory. But only if the pathname is
                      ;; absolute.
                      (not (and (equal (first d1) ':absolute)
                                (equalp d1 d2)
                                d3)))))
             (test-default+paths (default+paths)
               (let ((defaults (first default+paths))
                     (paths (second default+paths)))
                 (every (lambda (path)
                          (handler-case (test-path path defaults)
                            (error (error) 'NIL)))
                        paths))))
      (is-true
       (every #'test-default+paths *enough-namestring_tests*)))))

;;; Date: 10/08/2006 (Lars Brinkhoff)
;;; Fixed: 1/09/2006 (juanjo)
;;; Details:
;;;
;;;     ADJUST-ARRAY must signal a type error when the value of :FILL-POINTER is
;;;     not NIL and the adjustable array does not have a fill pointer
;;;
(test cmp.0007.adjustable-array
  (is (equal
       (loop for fp in '(nil t) collect
            (loop for i in '(t nil 0 1 2 3) collect
                 (and
                  (handler-case (adjust-array (make-array 3 :adjustable t :fill-pointer fp) 4
                                              :fill-pointer i)
                    (type-error (c) nil)
                    (error (c) t))
                  t)))
       '((nil t nil nil nil nil) (t t t t t t)))))

;;; Date: 09/10/2006 (Dustin Long)
;;; Fixed: 10/10/2006
;;; Description:
;;;
;;;     The namestring "." is improperly parsed, getting a file type of ""
;;;     Additionally we found it more convenient to have the _last_ dot mark
;;;     the file type, so that (pathname-type "foo.mpq.txt") => "txt"
;;;

(test cmp.0008.parse-namestring
  (is-false
   (loop for (namestring name type) in
        '(("." "." NIL) (".." "." "") (".foo" ".foo" NIL) (".foo.mpq.txt" ".foo.mpq" "txt")
          ("foo.txt" "foo" "txt") ("foo.mpq.txt" "foo.mpq" "txt"))
      unless (let ((x (parse-namestring namestring)))
               (and (equal name (pathname-name x))
                    (equal type (pathname-type x))
                    (equal '() (pathname-directory x))))
      collect namestring)))

;;; Date: 28/09/2006
;;; Fixed: 10/10/2006
;;; Description:
;;;
;;;     Nested calls to queue_finalizer trashed the value of ecl_core.to_be_finalized
;;;     The following code tests that at least three objects are finalized.
;;;
;;; Note: this test fails in multithreaded mode. GC takes too long!
(test cmp.0009.finalization
  (is-equal '(0 1 2 3 4)
            (let ((all-tags))
              (flet ((custom-finalizer (tag)
                       #'(lambda (o)
                           (declare (ignore o))
                           (push tag all-tags))))
                (let ((a '()))
                  (dotimes (i 5)
                    (let ((x (cons i i)))
                      (si::set-finalizer x (custom-finalizer i))
                      (push x a))))
                ;; mitigate GC slowness
                (sleep 1)
                (dotimes (j 100)
                  (dotimes (i 10000)
                    (cons 1.0 1.0))
                  (ext:gc t)))
              (sort all-tags #'<))))

;;; Date: 8/10/2006 (Dustin Long)
;;; Fixed: 10/10/2006 (Dustin Long)
;;; Description:
;;;
;;;     Hash table iterators have to check that their argument is
;;;     really a hash table.
;;;
(test cmp.0010.hash-iterator
  (is-false
   (loop for i in *mini-universe*
      when (and (not (hash-table-p i))
                (handler-case (progn (loop for k being the hash-keys of i) t)
                  (error (c) nil)))
      collect (type-of i))))

;;; Date: 31/12/2006 (Richard M. Kreuter)
;;; Fixed: 5/1/2007 (Juanjo)
;;; Description:
;;;
;;;     The keyword :BACK does not work as expected when creating pathnames
;;;     and causes an error when at the beginning: (:RELATIVE :BACK)
;;;
(test cmp.0011.make-pathname-with-back
  (is-false
   (loop for i from 0 to 200
      with l = (random 10)
      with x = (if (zerop l) 0 (random (1+ l)))
      with y = (if (= l x) 0 (random (- l x)))
      nconc (let* ((l (loop for i from 0 below l collect (princ-to-string i)))
                   (l2 (append (subseq l 0 y) '("break" :back) (subseq l y nil)))
                   (d1 (list* :absolute (subseq l2 0 x)))
                   (d2 (list* :relative (subseq l2 x nil)))
                   (d3 (list* :absolute l2))
                   (d4 (list* :relative l2))
                   (p1 (handler-case (make-pathname :directory d1)
                         (error (c) nil)))
                   (p2 (handler-case (make-pathname :directory d2)
                         (error (c) nil)))
                   (p3 (handler-case (make-pathname :directory d3)
                         (error (c) nil)))
                   (p4 (handler-case (make-pathname :directory d4)
                         (error (c) nil))))
              (if (and p1 p2 p3 p4
                       ;; MERGE-PATHNAMES eliminates :BACK
                       (equalp l (rest (pathname-directory (merge-pathnames p2 p1))))
                       ;; MAKE-PATHNAME does not eliminate :BACK
                       (not (equalp l (rest (pathname-directory (make-pathname :directory d3)))))
                       (not (equalp l (rest (pathname-directory (make-pathname :directory d4))))))
                  nil
                  (list (list l d1 d2 d3 d4 l2 x y)))))))

;;; Date: 11/03/2007 (Fare)
;;; Fixed: 23/03/2007 (Juanjo)
;;; Description:
;;;
;;;     COPY-READTABLE did not copy the entries of the "from" table
;;;     when a second argument, i.e. a "destination" table was supplied.
;;;
(test cmp.0012.copy-readtable
  (is-false
   (let ((from-readtable (copy-readtable))
         (to-readtable (copy-readtable))
         (char-list '()))
     (dotimes (i 20)
       (let* ((code (+ 32 (random 70)))
              (c (code-char code)))
         (push c char-list)
         (set-macro-character c
                              (eval `(lambda (str ch) ,code))
                              nil
                              from-readtable)))
     (copy-readtable from-readtable to-readtable)
     (loop for c in char-list
        unless (and (eql (char-code c)
                         (let ((*readtable* from-readtable))
                           (read-from-string (string c))))
                    (eq (get-macro-character c from-readtable)
                        (get-macro-character c to-readtable)))
        collect c))))

;;; Date: 05/01/2008 (Anonymous, SF bug report)
;;; Fixed: 06/01/2008 (Juanjo)
;;; Description:
;;;
;;;     For a file linked as follows "ln -s //usr/ /tmp/foo", 
;;;     (truename #p"/tmp/foo") signals an error because //usr is
;;;     parsed as a hostname.
;;;
#-(or cygwin haiku windows)
(test cmp.0013.truename
    (si:system "rm -rf foo; ln -sf //usr/ foo")
    (is (equal (namestring (truename "./foo")) "/usr/"))
    (si::system "rm foo"))

;;; Date: 30/08/2008 (Josh Elsasser)
;;; Fixed: 01/09/2008 (Juanjo)
;;; Description:
;;;
;;;     Inside the form read by #., recursive definitions a la #n=
;;;     and #n# were not properly expanded
;;;
(test cmp.0014.sharp-dot
  (is
   (equal (with-output-to-string (*standard-output*)
            (let ((*print-circle* t))
              (read-from-string "'#.(princ (list '#1=(1 2) '#1#))")))
          "(#1=(1 2) #1#)")))

;;; Date: 30/08/2008 (Josh Elsasser)
;;; Fixed: 30/08/2008 (Josh Elsasser)
;;; Description:
;;;
;;;     A setf expansion that produces a form with a macro that also has
;;;     its own setf expansion does not giver rise to the right code.
;;;
(test cmp.0015.setf-expander
  (define-setf-expander triple (place &environment env)
    (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
      (let ((store (gensym)))
        (values dummies
                vals
                `(,store)
                `(let ((,(car newval) (/ ,store 3)))
                   (triple ,setter))
                `(progn
                   (triple ,getter))))))
  (defmacro hidden (val)
    `(triple ,val))
  (defmacro triple (val)
    `(* 3 ,val))
  (is-true (equalp
            (eval '(let ((foo 5))
                    (list foo (triple foo) (setf (triple foo) 6) foo (triple foo))))
            (eval '(let ((foo 5))
                    (list foo (hidden foo) (setf (hidden foo) 6) foo (hidden foo))))))
  (fmakunbound 'hidden)
  (fmakunbound 'triple))

;;; Date: 17/2/2009
;;; Fixed: 17/2/2009
;;; Description:
;;;
;;;     The defstruct form fails with an :include field that overwrites
;;;     a slot that is read only.
;;;
(defstruct compiler.0016-a (a 1 :read-only t))
(defstruct (compiler.0016-b (:include compiler.0016-a (a 2))))
(defstruct (compiler.0016-c (:include compiler.0016-a (a 3 :read-only t))))
(test cmp.0016.defstruct-include
  (is-true
   (handler-case
       (eval '(defstruct (compiler.0016-d (:include compiler.0016-a (a 2 :read-only nil)))))
     (error (c) t)))
  (is (= (compiler.0016-a-a (make-compiler.0016-a)) 1))
  (is (= (compiler.0016-b-a (make-compiler.0016-b)) 2))
  (is (= (compiler.0016-c-a (make-compiler.0016-c)) 3))
  (is-true
   (handler-case
       (eval '(setf (compiler.0016-c-a (make-compiler.0016-c)) 3))
     (error (c) t))))

;;; Date: 9/11/2009
;;; Fixed: 9/11/2009
;;; Description:
;;;
;;;     LOAD does not work with special files (/dev/null)
;;;
(test cmp.0017.load-special
  (finishes
   (load #+(or windows mingw32) "NUL"
         #-(or windows mingw32) "/dev/null")))

;;; Date: 16/11/2009 (Gabriel)
;;; Fixed: 20/11/2009 (Juanjo)
;;; Description:
;;;
;;;     #= and ## reader macros do not work well with #.
;;;
(test cmp.0018.sharp-eq
  (is
   (equal (handler-case (values (read-from-string "(#1=(0 1 2) #.(length '#1#))"))
            (serious-condition (c) nil))
          '((0 1 2) 3))))

;;; Date: 14/11/2009 (M. Mondor)
;;; Fixed: 20/11/2009 (Juanjo)
;;; Description:
;;;
;;;     FDEFINITION and SYMBOL-FUNCTION cause SIGSEGV when acting on NIL.
;;;
(test cmp.0019.fdefinition
  (is-true
   (handler-case (fdefinition nil)
     (undefined-function (c) t)
     (serious-condition (c) nil)))
  (is-true
   (handler-case (symbol-function nil)
     (undefined-function (c) t)
     (serious-condition (c) nil))))

;;; Date: 29/11/2009 (P. Costanza)
;;; Fixed: 29/11/2009 (Juanjo)
;;; Description:
;;;
;;;     Updating of instances is not triggered by MAKE-INSTANCES-OBSOLETE.
;;;
(ext:with-clean-symbols (class-a class-a-b class-a-c class-a-x)
  (test cmp.0020.make-instances-obsolete
    (defclass class-a ()
      ((b :accessor class-a-b :initarg :b)
       (c :accessor class-a-c :initarg :c)))
    (let ((instance (make-instance 'class-a :b 2 :c 3))
          (update-guard nil))
      (defmethod update-instance-for-redefined-class :before
          ((instance standard-object) added-slots discarded-slots property-list
           &rest initargs)
        (setf update-guard t))
      (macrolet ((check-situation (change-form trigger-form result doc)
                   `(progn
                      (setf update-guard nil)
                      ,change-form
                      (is (and (null update-guard)
                               (progn ,trigger-form
                                      (eq update-guard ,result)))
                          ,doc))))
        (check-situation
         (make-instances-obsolete (find-class 'class-a))
         (class-a-b instance)
         t
         "Direct call to MAKE-INSTANCES-OBSOLETE doesn't work.")
        (check-situation
         (defclass class-a ()
           ((b :accessor class-a-b :initarg :b)))
         (class-a-b instance)
         t
         "Removing a slot does not obsolete class instances.")
        (check-situation
         (defclass class-a ()
           ((b :accessor class-a-b :initarg :b)
            (c :accessor class-a-c :initarg :c)))
         (class-a-b instance)
         t
         "Adding a slot does not obsolete class instances.")
        (check-situation
         (defclass class-a ()
           ((c :accessor class-a-c :initarg :c)
            (b :accessor class-a-b :initarg :b)))
         (class-a-b instance)
         t
         "Shuffling slots does not obsolete class instances.")
        (check-situation
         (defclass class-a ()
           ((c :accessor class-a-c :initarg :c :allocation :class)
            (b :accessor class-a-b :initarg :b)))
         (class-a-b instance)
         t
         "Changing slot allocation does not obsolete class instances.")
        (check-situation
         (defclass class-a ()
           ((b :accessor class-a-b :initarg :b)
            (c :accessor class-a-c :initarg :c)))
         (class-a-b instance)
         t
         "Redefining class does not obsolete class instances.")
        (check-situation
         (defclass class-a ()
           ((b :accessor class-a-b :initarg :b)
            (c :accessor class-a-c :initarg :c)))
         (class-a-b instance)
         nil
         "Without a change system should not make instances obsolete.")
        (check-situation
         (defclass class-a ()
           ((b :accessor class-a-x :initarg :b)
            (c :accessor class-a-c :initarg :c)))
         (class-a-x instance)
         nil
         "Changing accessors should not make instances obsolete.")
        ;; The old accessor is removed (the generic function).
        (signals error (class-a-b instance)
                 "Reader method is not removed after redefinition.")
        (is (fboundp 'class-a-b)
            "Redefining a class removes generic functions.")))))

;;; Date: 25/03/2009 (R. Toy)
;;; Fixed: 4/12/2009 (Juanjo)
;;; Description:
;;;
;;;     Conversion of rationals into floats is done by truncating, not by
;;;     rounding, what implies a loss of accuracy.
;;;
(test cmp.0021.ratio-to-float
  ;; The test builds a ratio which is very close to 1 but which is below it
  ;; If we truncate instead of rounding the output will not be 1 coerced
  ;; to that floating point type.
  (is-false
   (loop for type in '(short-float single-float double-float long-float)
      for bits = (float-precision (coerce 1 type))
      do (loop for i from (+ bits 7) to (+ bits 13)
            nconc (loop with value = (ash 1 i)
                     with expected = (coerce 1 type)
                     for j from 0 to 10
                     for x = (- value j)
                     for r = (/ (1- x) x)
                     for f1 = (coerce r type)
                     for f2 = (- (coerce (- r) type))
                     unless (and (= f1 expected) (= f2 expected))
                     collect (list type r))))))

;;; Date: 06/04/2010 (M. Kocic)
;;; Fixed: 4/12/2009
;;; Description:
;;;
;;;     Inspection of structs is broken due to undefined inspect-indent
;;;
(ext:with-clean-symbols (st1)
  (test cmp.0022.inspect-struct
    (is-true
     (let ((*query-io* (make-string-input-stream "q
")))
       (defstruct st1 p1)
       (let ((v1 (make-st1 :p1 "tttt")))
         (handler-case (progn (inspect v1) t)
           (error (c) nil)))))))


;; cmp-001

;;; Date: 12/03/2006
;;; From: Dan Corkill
;;; Fixed: 14/04/2006 (juanjo)
;;; Description:
;;;
;;;     The inner RETURN form should return to the outer block.
;;;     However, the closure (lambda (x) ...) is improperly translated
;;;     by the compiler to (lambda (x) (block nil ...) and thus this
;;;     form outputs '(1 2 3 4).
;;;
(test cmp.0023.block
  (is
   (= (funcall (compile nil
                        '(lambda ()
                          (block nil
                            (funcall 'mapcar
                                     #'(lambda (x)
                                         (when x (return x)))
                                     '(1 2 3 4)))))))))

;;; Fixed: 12/01/2006 (juanjo)
;;; Description:
;;;
;;;     COMPILE-FILE-PATHNAME now accepts both :FAS and :FASL as
;;;     synonyms.
;;;
;;;
(test cmp.0024.pathname
  (is (equalp (compile-file-pathname "foo" :type :fas)
              (compile-file-pathname "foo" :type :fasl))))

;;; Fixed: 21/12/2005 (juanjo)
;;; Description:
;;;
;;;     Compute the path of the intermediate files (*.c, *.h, etc)
;;;     relative to that of the fasl or object file.
;;;
(ext:with-clean-symbols (foo)
  (test cmp.0025.paths
    (let* ((output (compile-file-pathname "tmp/ecl-aux" :type :fasl))
           #-ecl-bytecmp
           (h-file (compile-file-pathname output :type :h))
           #-ecl-bytecmp
           (c-file (compile-file-pathname output :type :c))
           #-ecl-bytecmp
           (data-file (compile-file-pathname output :type :data)))
      (and
       (zerop (si::system "rm -rf tmp; mkdir -p tmp"))
       (null (nth-value 2 (ext:run-program "rm" '("-rf" "tmp"))))
       (null (nth-value 2 (ext:run-program "mkdir" '("-p" "tmp"))))
       (is (with-compiler ("aux-compiler.0103-paths.lsp"
                           :output-file output
                           :c-file t :h-file t :data-file t)
             '(defun foo (x) (1+ x))))
       (is (probe-file output))
       #-ecl-bytecmp
       (is (probe-file c-file))
       #-ecl-bytecmp
       (is (probe-file h-file))
       #-ecl-bytecmp
       (is (probe-file data-file))
       (null (nth-value 2 (ext:run-program "rm" '("-rf" "tmp"))))
       (null (nth-value 2 (ext:run-program "mkdir" '("-p" "tmp"))))
       (delete-file "aux-compiler.0103-paths.lsp")))))

;;; Date: 08/03/2006
;;; From: Dan Corkill
;;; Fixed: 09/03/2006 (juanjo)
;;; Description:
;;;
;;;     DEFCONSTANT does not declare the symbol as global and thus the
;;;     compiler issues warnings when the symbol is referenced in the
;;;     same file in which it is defined as constant.
;;;
#-ecl-bytecmp
(test cmp.0026.defconstant-warn
  (is-false
   (let ((warn nil))
     (with-dflet ((c::cmpwarn (setf warn t)))
       (with-compiler ("aux-compiler.0104.lsp")
         '(defconstant +foo+ (list 1 2 3))
         '(print +foo+)))
     (delete-file "aux-compiler.0104.lsp")
     (delete-file (compile-file-pathname "aux-compiler.0104.lsp" :type :fas))
     warn)))

;;; Date: 16/04/2006
;;; From: Juanjo
;;; Fixed: 16/04/2006 (juanjo)
;;; Description:
;;;
;;;     Special declarations should only affect the variable bound and
;;;     not their initialization forms. That, even if the variables are
;;;     the arguments of a function.
;;;
(test cmp.0027.declaration
  (let ((form '(lambda (y)
                (flet ((faa (&key (x y))
                         (declare (special y))
                         x))
                  (let ((y 4))
                    (declare (special y))
                    (faa))))))
    ;; We must test that both the intepreted and the compiled form
    ;; output the same value.
    (is (= (funcall (compile 'nil form) 3) 3))
    (is (= (funcall (coerce form 'function) 3) 3))))

;;; Date: 26/04/2006
;;; From: Michael Goffioul
;;; Fixed: ----
;;; Description:
;;;
;;;     Functions with more than 64 arguments have to be invoked using
;;;     the lisp stack.
;;;
(test cmp.0028.call-arguments-limit
  (let ((form '(lambda ()
                (list (list
                       'a0 'b0 'c0 'd0 'e0 'f0 'g0 'h0 'i0
                       'j0 'k0 'l0 'm0 'n0 'o0 'p0 'q0
                       'r0 's0 't0 'u0 'v0 'w0 'x0 'y0 'z0
                       'a1 'b1 'c1 'd1 'e1 'f1 'g1 'h1 'i1
                       'j1 'k1 'l1 'm1 'n1 'o1 'p1 'q1
                       'r1 's1 't1 'u1 'v1 'w1 'x1 'y1 'z1
                       'a2 'b2 'c2 'd2 'e2 'f2 'g2 'h2 'i2
                       'j2 'k2 'l2 'm2 'n2 'o2 'p2 'q2
                       'r2 's2 't2 'u2 'v2 'w2 'x2 'y2 'z2
                       'a3 'b3 'c3 'd3 'e3 'f3 'g3 'h3 'i3
                       'j3 'k3 'l3 'm3 'n3 'o3 'p3 'q3
                       'r3 's3 't3 'u3 'v3 'w3 'x3 'y3 'z3
                       'a4 'b4 'c4 'd4 'e4 'f4 'g4 'h4 'i4
                       'j4 'k4 'l4 'm4 'n4 'o4 'p4 'q4
                       'r4 's4 't4 'u4 'v4 'w4 'x4 'y4 'z4
                       'a5 'b5 'c5 'd5 'e5 'f5 'g5 'h5 'i5
                       'j5 'k5 'l5 'm5 'n5 'o5 'p5 'q5
                       'r5 's5 't5 'u5 'v5 'w5 'x5 'y5 'z5
                       'a6 'b6 'c6 'd6 'e6 'f6 'g6 'h6 'i6
                       'j6 'k6 'l6 'm6 'n6 'o6 'p6 'q6
                       'r6 's6 't6 'u6 'v6 'w6 'x6 'y6 'z6)))))
    (is (equal (funcall (compile 'foo form))
               (funcall (coerce form 'function))))))

;;; Date: 16/05/2005
;;; Fixed: 18/05/2006 (juanjo)
;;; Description:
;;;
;;;     The detection of when a lisp constant has to be externalized
;;;     using MAKE-LOAD-FORM breaks down with some circular structures
;;;
(defclass compiler-test-class ()
  ((parent :accessor compiler-test-parent :initform nil)
   (children :initarg :children :accessor compiler-test-children :initform nil)))

(defmethod make-load-form ((x compiler-test-class) &optional environment)
  (declare (ignore environment))
  (values
   ;; creation form
   `(make-instance ',(class-of x) :children ',(slot-value x 'children))
   ;; initialization form
   `(setf (compiler-test-parent ',x) ',(slot-value x 'parent))))

(test cmp.0029.circular-load-form
  (is
   (equal
    (loop for object in
         (let ((l (list 1 2 3)))
           (list l
                 (subst 3 l l)
                 (make-instance 'compiler-test-class)
                 (subst (make-instance 'compiler-test-class) 3 l)))
       collect (si::need-to-make-load-form-p object))
    '(nil nil t t))))

;;; Date: 18/05/2005
;;; Fixed: 17/05/2006 (Brian Spilsbury & juanjo)
;;; Description:
;;;
;;;     The compiler is not able to externalize constants that have no
;;;     printed representation.  In that case MAKE-LOAD-FORM should be
;;;     used.
;;;
;;;
;;; Date: 2020-02-12
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/issues/562
;;; Description:
;;;
;;;     Circular structures are not properly initialized because make
;;;     and init form order of evaluation is not always correct.
(test cmp.0030.make-load-form
  (multiple-value-bind (file output)
      (with-compiler ("make-load-form.lsp")
        "(in-package cl-test)"
        "(eval-when (:compile-toplevel)
           (defparameter s4.0030 (make-instance 'compiler-test-class))
           (defparameter s5.0030 (make-instance 'compiler-test-class))
           (setf (compiler-test-parent s5.0030) s4.0030)
           (setf (compiler-test-children s4.0030) (list s5.0030)))"
        "(defparameter a.0030 '#.s5.0030)"
        "(defparameter b.0030 '#.s4.0030)"
        "(defparameter c.0030 '#.s5.0030)"
        "(defun foo.0030 ()
           (let ((*print-circle* t))
             (with-output-to-string (s) (princ '#1=(1 2 3 #.s4.0030 #1#) s))))")
    (declare (ignore output))
    (load file)
    (delete-file "make-load-form.lsp")
    (delete-file file))
  (let ((str (foo.0030)))
    (is (and (search "#1=(1 2 3 #<a CL-TEST::COMPILER-TEST-CLASS" str)
             (search "> #1#)" str))))
  (is (eq (compiler-test-parent a.0030) b.0030))
  (is (eq (first (compiler-test-children b.0030)) a.0030))
  (is (eq a.0030 c.0030)))

;;; Date: 9/06/2006 (Pascal Costanza)
;;; Fixed: 13/06/2006 (juanjo)
;;; Description:
;;;
;;;     A MACROLET function creates a set of local macro definitions.
;;;     The forms that expand these macros are themselves affected by
;;;     enclosing MACROLET and SYMBOL-MACRO definitions:
;;;             (defun bar ()
;;;              (macrolet ((x () 2))
;;;               (macrolet ((m () (x)))
;;;                (m))))
;;;             (compile 'bar)
;;;             (bar) => 2
;;;
(ext:with-clean-symbols (bar)
  (test cmp.0031.macrolet
    (is (= 2 (progn
               (defun bar ()
                 (macrolet ((x () 2))
                   (macrolet ((m () (x)))
                     (m))))
               (compile 'bar)
               (bar))))
    (is (= 2 (progn
               (defun bar ()
                 (symbol-macrolet ((x 2))
                   (macrolet ((m () x))
                     (m))))
               (compile 'bar)
               (bar))))))

;;; Fixed: 13/06/2006 (juanjo)
;;; Description:
;;;
;;;     A MACROLET that references a local variable from the form in
;;;     which it appears can cause corruption in the interpreter. We
;;;     solve this by signalling errors whenever such reference
;;;     happens.
;;;
;;;     Additionally MACROLET forms should not see the other macro
;;;     definitions on the same form, much like FLET functions cannot
;;;     call their siblings.
;;;
(ext:with-clean-symbols (compiler-foo)
  (test cmp.0032.macrolet-2
    (is-equal '(error 1 error error 6 (7 8) error error)
              (flet ((eval-with-error (form)
                       (handler-case (eval form)
                         (error (c) 'error))))
                (let ((faa 1))
                  (declare (special faa))
                  (mapcar #'eval-with-error
                          '((let ((faa 2))
                              (macrolet ((m () faa))
                                (m)))
                            (let ((faa 4))
                              (declare (special faa))
                              (macrolet ((m () faa))
                                (m)))
                            (let ((faa 4))
                              (declare (special compiler-foo))
                              (macrolet ((m () compiler-foo))
                                (m)))
                            (let ((faa 5))
                              (macrolet ((m () compiler-foo))
                                (m)))
                            (macrolet ((compiler-foo () 6))
                              (macrolet ((m () (compiler-foo)))
                                (m)))
                            (macrolet ((f1 () 7)
                                       (f2 () 8))
                              ;; M should not see the new definitions F1 and F2
                              (macrolet ((f1 () 9)
                                         (f2 () 10)
                                         (m () (list 'quote (list (f1) (f2)))))
                                (m)))
                            (flet ((compiler-foo () 1))
                              (macrolet ((m () (compiler-foo)))
                                (m)))
                            (labels ((compiler-foo () 1))
                              (macrolet ((m () (compiler-foo)))
                                (m))))))))
    (makunbound 'compiler-foo)
    (fmakunbound 'compiler-foo)))

;;; Date: 22/06/2006 (juanjo)
;;; Fixed: 29/06/2006 (juanjo)
;;; Description:
;;;
;;;     ECL only accepted functions with less than 65 required
;;;     arguments. Otherwise it refused to compile the function. The fix must
;;;     respect the limit in the number of arguments passed in the C stack and
;;;     use the lisp stack for the other required arguments.
;;;
#-ecl-bytecmp
(test cmp.0033.c-arguments-limit
  (is (equal '((10 :ERROR :ERROR) (20 :ERROR :ERROR) (30 :ERROR :ERROR)
               (40 :ERROR :ERROR) (50 :ERROR :ERROR) (63 :ERROR :ERROR)
               (64 :ERROR :ERROR) (65 :ERROR :ERROR) (70 :ERROR :ERROR))
             (mapcar
              #'(lambda (nargs)
                  (let* ((arg-list (loop for i from 0 below nargs
                                      collect (intern (format nil "arg~d" i))))
                         (data (loop for i from 0 below nargs collect i))
                         (lambda-form `(lambda ,arg-list
                                         (and (equalp (list ,@arg-list) ',data)
                                              ,nargs)))
                         (c:*compile-verbose* nil)
                         (c:*compile-print* nil)
                         (function (compile 'foo lambda-form)))
                    (list (apply function (subseq data 0 nargs))
                          (handler-case (apply function (make-list (1+ nargs)))
                            (error (c) :error))
                          (handler-case (apply function (make-list (1- nargs)))
                            (error (c) :error)))))
              '(10 20 30 40 50 63 64 65 70)))))

;;; Date: 12/07/2008 (Josh Elsasser)
;;; Fixed: 02/08/2008 (Juanjo)
;;; Description:
;;;
;;;     ECL fails to properly compute the closure type of a function that
;;;     returns a lambda that calls the function itself.
;;;
(test cmp.0034.compute-closure
  (is
   (with-compiler ("aux-compiler.0103-paths.lsp" :load t)
     '(defun testfun (outer)
       (labels ((testlabel (inner)
                  (if inner
                      (testfun-map
                       (lambda (x) (testlabel x))
                       inner))
                  (print outer)))
         (testlabel outer))))))

;;; Date: 02/09/2008 (Josh Elsasser)
;;; Fixed: 12/09/2008 (Josh Elsasser)
;;; Description:
;;;
;;;     FTYPE proclamations and declarations do not accept user defined
;;;     function types.
;;;
(ext:with-clean-symbols (compiler.float-function
                         compiler.float)
  (test cmp.0035.ftype-user-type
    (progn
      (deftype compiler.float-function () '(function (float) float))
      (deftype compiler.float () 'float)
      (loop for (type . fails) in
           '(((function (float) float) . nil)
             (cons . t)
             (compiler.float-function . nil)
             (compiler.float . t))
         always (let ((form1 `(proclaim '(ftype ,type foo)))
                      (form2 `(compile nil '(lambda ()
                                             (declare (ftype ,type foo))
                                             (foo)))))
                  (cond (fails
                         (signals simple-error (eval form1))
                         (signals warning (eval form2)))
                        (:otherwise
                         (finishes (eval form1))
                         (finishes (eval form2)))))))))

;;; Date: 01/11/2008 (E. Marsden)
;;; Fixed: 02/11/2008 (Juanjo)
;;; Description:
;;;
;;;     When compiled COERCE with type INTEGER may cause double
;;;     evaluation of a form.
;;;
;;; ------------------------------------------------------------
;;; Date: 03/11/2008 (E. Marsden)
;;; Fixed: 08/11/2008 (Juanjo)
;;; Description:
;;;
;;;     TYPEP, with a real type, produces strange results.
;;;
(test cmp.0036.coerce
  (is-true (= 1
              (funcall
               (compile 'foo '(lambda (x)
                               (coerce (shiftf x 2) 'integer)))
               1)))
  (is-false (funcall
             (compile 'foo '(lambda (x)
                             (typep (shiftf x 1) '(real 10 20))))
             5)))

;;; Date: 20/07/2008 (Juanjo)
;;; Fixed: 20/07/2008 (Juanjo)
;;; Description:
;;;
;;;     In the new compiler, when compiling LET forms with special variables
;;;     the values of the variables are not saved to make the assignments
;;;     really parallel.
;;;
(test cmp.0037.let-with-specials
  (is
   (=
    7
    (progn
      (defvar *stak-x*)
      (defvar *stak-y*)
      (defvar *stak-z*)
      (funcall
       (compile
        nil
        '(lambda (*stak-x* *stak-y* *stak-z*)
          (labels
              ((stak-aux ()
                 (if (not (< (the fixnum *stak-y*) (the fixnum *stak-x*)))
                     *stak-z*
                     (let ((*stak-x* (let ((*stak-x* (the fixnum (1- (the fixnum *stak-x*))))
                                           (*stak-y* *stak-y*)
                                           (*stak-z* *stak-z*))
                                       (stak-aux)))
                           (*stak-y* (let ((*stak-x* (the fixnum (1- (the fixnum *stak-y*))))
                                           (*stak-y* *stak-z*)
                                           (*stak-z* *stak-x*))
                                       (stak-aux)))
                           (*stak-z* (let ((*stak-x* (the fixnum (1- (the fixnum *stak-z*))))
                                           (*stak-y* *stak-x*)
                                           (*stak-z* *stak-y*))
                                       (stak-aux))))
                       (stak-aux)))))
            (stak-aux)))) 18 12 6)))))

;;; Date: 06/10/2009 (J. Pellegrini)
;;; Fixed: 06/10/2009 (Juanjo)
;;; Description:
;;;     Extended strings were not accepted as documentation by the interpreter.
;;;
(ext:with-clean-symbols (foo)
  (test cmp.0038.docstrings
    (eval `(defun foo ()
             ,(make-array 10 :initial-element #\Space :element-type 'character)
             2))
    (is (= (eval (funcall 'foo)) 2))))

;;; Date: 07/11/2009 (A. Hefner)
;;; Fixed: 07/11/2009 (A. Hefner + Juanjo)
;;; Description:
;;;     ECL ignores the IGNORABLE declaration
;;;
(test cmp.0039.ignorable
  (let ((c::*suppress-compiler-messages* t))
    ;; Issue a warning for unused variables
    (is-true
     (handler-case (and (compile nil '(lambda (x y) (print x))) nil)
       (warning (c) t)))
    ;; Do not issue a warning for unused variables declared IGNORE
    (is-true
     (handler-case (and (compile nil '(lambda (x y) (declare (ignore y))
                                       (print x))) t)
       (warning (c) nil)))
    ;; Do not issue a warning for unused variables declared IGNORABLE
    (is-true
     (handler-case (and (compile nil '(lambda (x y) (declare (ignorable y))
                                       (print x))) t)
       (warning (c) nil)))
    ;; Do not issue a warning for used variables declared IGNORABLE
    (is-true
     (handler-case (and (compile nil '(lambda (x y) (declare (ignorable x y))
                                       (print x))) t)
       (warning (c) nil)))))

;;; Date: 29/11/2009 (P. Costanza)
;;; Fixed: 29/11/2009 (Juanjo)
;;; Description:
;;;     When calling a bytecodes (SETF ...) function from a compiled function
;;;     an invalid memory access is produced. This is actually a consequence
;;;     of a mismatch between the position of the fields bytecodes.entry
;;;     and cfun.entry
;;;
#-ecl-bytecmp
(test cmp.0040.bytecodes-entry-position
  (let ((indices (funcall (compile nil
                                   '(lambda ()
                                     (ffi:c-inline () () list "
        union cl_lispunion x[1];
        cl_index bytecodes = (char*)(&(x->bytecodes.entry)) - (char*)x;
        cl_index bclosure  = (char*)(&(x->bclosure.entry)) - (char*)x;
        cl_index cfun      = (char*)(&(x->cfun.entry)) - (char*)x;
        cl_index cfunfixed = (char*)(&(x->cfunfixed.entry)) - (char*)x;
        cl_index cclosure  = (char*)(&(x->cclosure.entry)) - (char*)x;
        @(return) = cl_list(5, MAKE_FIXNUM(bytecodes),
                            MAKE_FIXNUM(bclosure),
                            MAKE_FIXNUM(cfun),
                            MAKE_FIXNUM(cfunfixed),
                            MAKE_FIXNUM(cclosure));" :one-liner nil))))))
    (is-true (apply #'= indices)) t))

;;; Date: 07/02/2010 (W. Hebich)
;;; Fixed: 07/02/2010 (Juanjo)
;;; Description:
;;;     THE forms do not understand VALUES types
;;;             (the (values t) (funcall sym))
;;;
(test cmp.0041.the-and-values
  (is
   (handler-case (compile nil '(lambda () (the (values t) (faa))))
     (warning (c) nil))))


;;; Date: 28/03/2010 (M. Mondor)
;;; Fixed: 28/03/2010 (Juanjo)
;;; Description:
;;;     ECL does not compile type declarations of a symbol macro
;;;
(test cmp.0042.symbol-macro-declaration
  (is
   (handler-case (compile 'nil
                          '(lambda (x)
                            (symbol-macrolet ((y x))
                              (declare (fixnum y))
                              (+ y x))))
     (warning (c) nil))))

;;; Date: 24/04/2010 (Juanjo)
;;; Fixed 24/04/2010 (Juanjo)
;;; Description:
;;;     New special form, WITH-BACKEND.
;;;
(ext:with-clean-symbols (*compiler.0122* compiler.0122a)
  (defparameter *compiler.0122* nil)
  (test cmp.0043.with-backend
    ;; we ensure compiler.0122a isn't compiled upfront
    (eval '(defun compiler.0122a ()
            (ext:with-backend
                :bytecodes (setf *compiler.0122* :bytecodes)
                :c/c++ (setf *compiler.0122* :c/c++))))
    (is-eql :bytecodes
            (progn (compiler.0122a)
                   *compiler.0122*))
    (is-eql :bytecodes
            (compiler.0122a))
    #-ecl-bytecmp
    (is-eql :c/c++
            (progn (compile 'compiler.0122a)
                   (compiler.0122a)
                   *compiler.0122*))
    #-ecl-bytecmp
    (is-eql :c/c++
            (compiler.0122a))))



;;; Date: 10/08/2008
;;; From: Juanjo
;;; Fixed: 10/08/2008
;;; Description:
;;;
;;;     COS, SIN and TAN were expanded using a wrong C expression.
;;;

(test cmp.0044.inline-cos
  (is-false
   (loop with *compile-verbose* = nil
      with *compile-print* = nil
      for type in '(short-float single-float double-float long-float)
      for sample = (coerce 1.0 type)
      for epsilon in '(#.short-float-epsilon #.single-float-epsilon #.double-float-epsilon #.long-float-epsilon)
      unless (loop for op in '(sin cos tan sinh cosh tanh)
                for f = (compile 'nil `(lambda (x)
                                         (declare (,type x)
                                                  (optimize (safety 0)
                                                            (speed 3)))
                                         (+ ,sample (,op x))))
                always (loop for x from (- pi) below pi by 0.05
                          for xf = (float x sample)
                          for error =  (- (funcall f xf) (+ 1 (funcall op xf)))
                          always (< (abs error) epsilon)))
      collect type)))



;;; Description:
;;;
;;;     The interpreter selectively complains when assigning a variable
;;;     that has not been declared as special and is not local.
;;;
;;; Fixed: 03/2006 (juanjo)
;;;
(test cmp.0045.global-setq
  (is (equal
       '(:no-error :error)
       (mapcar
        (lambda (ext:*action-on-undefined-variable*)
          (handler-case
              (progn (eval `(setq ,(gensym) 1)) :no-error)
            (error (c) :error)))
        '(nil ERROR)))))

;;; Date: 24/04/2010 (Juanjo)
;;; Fixed: 24/04/2010 (Juanjo)
;;; Description:
;;;     The interpreter does not increase the lexical environment depth when
;;;     optimizing certain forms (LIST, LIST*, CONS...) and thus causes some
;;;     of the arguments to be eagerly evaluated.
;;;
(test cmp.0046.list-optimizer-error
  (is (string-equal
       (with-output-to-string (*standard-output*)
         (eval '(list (print 1) (progn (print 2) (print 3)))))
       "
1 
2 
3 ")))



;;; Date: 2015-09-04
;;; Fixed: Daniel Kochmański
;;; Description
;;;     Compiler signalled arithmetic-error when producing C code for infinity
;;;     and NaN float values (part of ieee floating point extensions).

#+ieee-floating-point
(test cmp.0047.infinity-test
  (finishes
   (compile nil
            (lambda ()
              (> 0.0 ext:single-float-negative-infinity))))
  (is-true
   (let ((ofile
          (with-compiler ("aux-compiler-0048.infty-test.2.lsp" :load t)
            '(defun doit () (> 0.0 ext:single-float-negative-infinity)))))
     (delete-file "aux-compiler-0048.infty-test.2.lsp")
     (delete-file ofile)
     (doit))))



;;; Date: 2015-12-18
;;; Fixed: Daniel Kochmański
;;; Description
;;;     Compiler expanded FIND incorrectly (ignored START and END arguments)

(ext:with-clean-symbols (check-single-wildcard)
  (test cmp.0048.cmpopt-sequences
    (defun check-single-wildcard (identifier wildcard-pos)
      (not (find #\* identifier :start (1+ wildcard-pos))))
    (is-true (check-single-wildcard "dan*" 3))))

;;; Date: 2016-02-10
;;; Fixed: Daniel Kochmański
;;; Description
;;;     Aux closures created by C compiler weren't handled correctly
;;;     in respect of the environment and declarations of the
;;;     variables
(test cmp.0049.cmptop/call
  (finishes
    (funcall (compile nil '(lambda ()
                            (labels
                                ((fun-2 () (fun-3 'cool))
                                 (fun-3 (clause-var)
                                   (flet ((fun-4 () clause-var))
                                     (fun-4))))
                              (let ((fun-1 (lambda () (fun-2))))
                                (funcall fun-1))))))))


;;; Date 2016-04-21
;;; Fixed: Daniel Kochmański
;;; Description
;;;    typep didn't recognize * as a t abberv
;;;
(test cmp.0050.ftype-args*
  (declaim (ftype (function (*) (values T)) ce))
  (defun ce (expression) expression)
  (is-false (ce nil)))


;;; Date 2016-08-09 (jd)
;;; Description
;;;    No adequate specialization of MAKE-LOAD-FORM for an object of
;;;    type RANDOM-TYPE
(test cmp.0051.make-load-form.random-state
  (finishes (make-load-form (make-random-state))))


;;; Date 2016-12-20
;;; Reported by Paul F. Dietz
;;; Description
;;;
;;;    Order of VALUES evaluation in compiled code is wrong.
;;;
;;; Bug https://gitlab.com/embeddable-common-lisp/ecl/issues/330
(ext:with-clean-symbols (f2)
  (test cmp.0052.values-evaluation-order
    (defun f2 (a) (lcm (values a (setq a 1))))
    (is-eql 10 (f2 10))
    (compile 'f2)
    (is-eql 10 (f2 10))))

;;; Date 2017-06-27
;;; Reported by Fabrizio Fabbri
;;; Description
;;;
;;;    Compiled function drop argument type checkin
;;;    on constant.
;;;
;;; Bug https://gitlab.com/embeddable-common-lisp/ecl/issues/353
(test cmp.0053.check-values-type-on-constant
      (handler-case 
          (funcall (compile nil
                            '(lambda () (rplaca 'A 1))))
        (simple-type-error () t)
        (error () nil)
        (:no-error (v) (declare (ignore v)) nil)))

;;; Date 2017-06-28
;;; Reported by Fabrizio Fabbri
;;; Description
;;;
;;;    Compiled assoc does not check that alist argument
;;;    is a valid association list.
;;;
;;; Bug https://gitlab.com/embeddable-common-lisp/ecl/issues/353
(test cmp.0054.invalid-argument-type
      (handler-case 
          (funcall (compile nil
                            '(lambda () (assoc 'z '((a . b) :bad (c . d))))))
        (simple-type-error () t)
        (error () nil)
        (:no-error (v) (declare (ignore v)) nil)))

;;; Date 2017-07-05
;;; Reported by Fabrizio Fabbri
;;; Description
;;;
;;;    Compiled vector-push and vector-push-extend
;;;    does not check for invalid argument and
;;;    SIGSEGV
;;;
;;; Bug https://gitlab.com/embeddable-common-lisp/ecl/issues/353
(test cmp.0055.invalid-argument-type
  (is-true
   (handler-case
       (funcall (compile nil
                         '(lambda () (vector-push))))
     (program-error () t)
     (error () nil)
     (:no-error (v) (declare (ignore v)) nil))))

;;; Date 2017-08-10
;;; Description
;;;
;;;    On some platforms (without feenableexcept) compiling code with
;;;    constants being infinity cause fpe-exception.
#+ieee-floating-point
(test cmp.0056.artificial-fpe
  (finishes
    (funcall (compile nil
                      '(lambda ()
                        (eql 10d0 ext:double-float-positive-infinity))))))

;;; Date 2017-08-10
;;; Description
;;;
;;;    Confirm, that malformed code compiles (errors should be issued
;;;    at runtime).
(test cmp.0057.expand
  (let (fun)
    ;; expand-mapcar
    (is (setf fun (compile nil '(lambda () (mapcar)))))
    (signals program-error (funcall fun))
    ;; expand-vector-push
    (is (setf fun (compile nil '(lambda () (vector-push)))))
    (signals program-error (funcall fun))))

;;; Date 2017-08-16
;;;
;;; Description
;;;
;;;    `si:coerce-to-vector' (called from cmpopt) had invalid
;;;    check-type statements preventing compilation of valid code.
(test cmp.0058.coerce-expand
  (finishes (load (with-compiler ("aux-compiler.0058-coerce.lsp")
                    '(defun flesh-failures ()
                      (load-time-value
                       (coerce #(0) '(simple-array (unsigned-byte 8) (1)))))))))

;;; Date 2017-09-29
;;;
;;; Description
;;;
;;;   `typep' compiler macroexpansion did treat
;;;   `gray:fundamental-stream' as subtype of `ext:ansi-stream'.
(test cmp.0059.gray-is-not-ansi
  (let ((stream (make-instance 'gray:fundamental-stream)))
    (is-false (typep stream 'ext:ansi-stream))))

;;; Date 2017-11-21
;;; Description
;;;
;;;   loop on dotted lists with destructuring bind gave a type error
;;;
;;; Bug https://gitlab.com/embeddable-common-lisp/ecl/issues/418
(test cmp.0060.loop-on-dotted-list
  (finishes (funcall (compile nil
                              '(lambda () (loop for (i) on '(1 2 . 3)))))))

;;; Date 2017-12-02
;;; Description
;;;
;;;   type declarations for optional and keyword function arguments
;;;   resulted in an error, when the default values weren't of the
;;;   specified type.
;;;
;;; Bug https://gitlab.com/embeddable-common-lisp/ecl/issues/292
(test cmp.0061.optional-type-declaration
  (declaim (ftype (function (real &optional real &key (:c symbol)) real) foo))
  (defun foo (a &optional (b 'test) &key (c 1)) (if b c a))
  (compile 'foo)
  (finishes (foo 1)))

;;; Date 2018-01-23
;;; Description
;;;
;;;   block referenced across closure boundries could lead to a local variable
;;;   corruption due to the change of _when_ closure type is computed.
;;;
;;; Bug https://gitlab.com/embeddable-common-lisp/ecl/issues/374
(test cmp.0062.ccb-block-variable-corruption
  (defun fooman ()
    (let ((foo-1 0)
          (second-time nil))
      (tagbody :G124
         (block exitpoint
           (handler-bind ((simple-error
                           #'(lambda (c)
                               (declare (ignore c))
                               (if (= foo-1 0)
                                   (format nil "all is fine folks~%")
                                   (error "foo-1 is ~s should be 0" foo-1))
                               ;; exitpoint is referenced across closure boundries
                               (return-from exitpoint nil))))
             (signal 'simple-error)))
         (unless second-time
           (setf second-time t)
           (GO :G124)))))
  (compile 'fooman)
  (finishes (fooman)))

;;; Date 2018-02-10
;;; Description
;;;
;;;   Compiler macros do not get shadowed by lexical function bindings.
;;;
;;; Spec: http://www.lispworks.com/documentation/HyperSpec/Body/03_bba.htm
;;; Bug https://gitlab.com/embeddable-common-lisp/ecl/issues/83
;;; Bug https://gitlab.com/embeddable-common-lisp/ecl/issues/237
(test cmp.0063.lexical-macrolet
  (defun foo () :function)
  (define-compiler-macro foo () :compiler-macro)
  (let ((result (funcall (compile nil '(lambda ()
                                        (macrolet ((foo () :macrolet))
                                          (foo)))))))
    (is (eq :macrolet result) "Expected :MACROLET, got ~s." result)))

;;; Date 2018-02-11
;;; Description
;;;
;;;   ecl_bclosure lexenv is not used during complation (both bytecmp and ccmp).
;;;   That leads to dangling references in compiled code.
;;;
;;; Bug https://gitlab.com/embeddable-common-lisp/ecl/issues/429
(test cmp.0064.bytecmp-compile-bclosure
  (let ((fun-1                                 (lambda () :fun-1-nil))
        (fun-2 (let      ((fun-2-var    :var)) (lambda () fun-2-var)))
        (fun-3 (flet     ((fun-3-fun () :fun)) (lambda () (fun-3-fun))))
        (fun-4 (macrolet ((fun-4-mac () :mac)) (lambda () (fun-4-mac))))
        (fun-5 (symbol-macrolet ((fun-5-sym :sym)) (lambda () fun-5-sym))))
    (is (eq :fun-1-nil (funcall fun-1)))
    (is (eq :var (funcall fun-2)))
    (is (eq :fun (funcall fun-3)))
    (is (eq :mac (funcall fun-4)))
    (is (eq :sym (funcall fun-5)))
    (let ((fun-1 (ext::bc-compile nil fun-1))
          (fun-2 (ext::bc-compile nil fun-2))
          (fun-3 (ext::bc-compile nil fun-3))
          (fun-4 (ext::bc-compile nil fun-4))
          (fun-5 (ext::bc-compile nil fun-5)))
      (is (eq :fun-1-nil (funcall fun-1)))
      (is (eq :var (ignore-errors (funcall fun-2))) "fun-2-var from lexenv is not used.")
      (is (eq :fun (ignore-errors (funcall fun-3))) "fun-3-fun from lexenv is not used.")
      (is (eq :mac (ignore-errors (funcall fun-4))) "fun-4-mac from lexenv is not used.")
      (is (eq :sym (ignore-errors (funcall fun-5))) "fun-5-sym from lexenv is not used."))))

(test cmp.0065.cmp-compile-bclosure
  (let ((fun-1                                 (lambda () :fun-1-nil))
        (fun-2 (let      ((fun-2-var    :var)) (lambda () fun-2-var)))
        (fun-3 (flet     ((fun-3-fun () :fun)) (lambda () (fun-3-fun))))
        (fun-4 (macrolet ((fun-4-mac () :mac)) (lambda () (fun-4-mac))))
        (fun-5 (symbol-macrolet ((fun-5-sym :sym)) (lambda () fun-5-sym))))
    (is (eq :fun-1-nil (funcall fun-1)))
    (is (eq :var (funcall fun-2)))
    (is (eq :fun (funcall fun-3)))
    (is (eq :mac (funcall fun-4)))
    (is (eq :sym (funcall fun-5)))
    (let ((fun-1 (compile nil fun-1))
          (fun-2 (compile nil fun-2))
          (fun-3 (compile nil fun-3))
          (fun-4 (compile nil fun-4))
          (fun-5 (compile nil fun-5)))
      (is (eq :fun-1-nil (funcall fun-1)))
      (is (eq :var (ignore-errors (funcall fun-2))) "fun-2-var from lexenv is not used.")
      (is (eq :fun (ignore-errors (funcall fun-3))) "fun-3-fun from lexenv is not used.")
      (is (eq :mac (ignore-errors (funcall fun-4))) "fun-4-mac from lexenv is not used.")
      (is (eq :sym (ignore-errors (funcall fun-5))) "fun-5-sym from lexenv is not used."))))

;;; Date 2018-02-12
;;; Description
;;;
;;;   bytecmp always makes flet functions closures even if lexenv is empty.
(test cmp.0066.bytecodes-flet-closure
  (let ((fun-1 (flet ((a () 1)) #'a))
        (fun-2 (let ((b 3))    ; this make break if we replace B with a constant
                 (flet ((a () b)) #'a))))
    (is (null (nth-value 1 (function-lambda-expression fun-1))))
    (is (nth-value 1 (function-lambda-expression fun-2)))))

;;; Date 2018-02-13
;;; Description
;;;
;;;   ext::bc-compile executed compiled form.
(test cmp.0067.bytecodes-compile-exec
  (multiple-value-bind (fun warn err)
      (ext::bc-compile nil '(flet ((a () 3)) #'a))
    (is (and (null fun) warn err)
        "bc-compile: invalid lambda expression should signal error.")))

;;; Date 2018-02-13
;;; Description
;;;
;;; compile / ext::bc-compile doesn't accept (setf foo).
(ext:with-clean-symbols (foo bar)
  (test cmp.0068.bytecmp-setf-foo
    (defun (setf foo) (x) x)
    (multiple-value-bind (fun warn err)
        (ext::bc-compile '(setf foo))
      (is (and fun (null warn) (null err))
          "bc-compile: (setf foo) is a valid function name.")))
  (test cmp.0069.cmp-setf-foo
    (defun (setf foo) (x) x)
    (multiple-value-bind (fun warn err)
        (compile '(setf foo))
      (is (and fun (null warn) (null err))
          "compile: (setf foo) is a valid function name."))))

;;; Date 2019-04-02
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/issues/468
;;; Fixed: 177ad215ea91524756a00b24436273b065628081
;;; Description
;;;
;;;   TYPECASE doesn't distinguish between different complex types
;;;   when compiled.
(ext:with-clean-symbols (xxx)
  (test cmp.0070.cmp-typecase-complex
        (defun xxx ()
          (let ((ci #c(5 7))
                (cs #c(1.0s0 1.0s0))
                (cd #c(1.0d0 1.0d0)))
            (list (typecase ci
                    ((complex integer) 'ci)
                    ((complex single-float) 'csf)
                    ((complex double-float) 'cdf))
                  (typecase cs
                    ((complex integer) 'ci)
                    ((complex single-float) 'csf)
                    ((complex double-float) 'cdf))
                  (typecase cs
                    ((complex integer) 'ci)
                    ((complex double-float) 'cdf)
                    ((complex single-float) 'csf))
                  (typecase cd
                    ((complex integer) 'ci)
                    ((complex single-float) 'csf)
                    ((complex double-float) 'cdf))
                  (typecase cd
                    ((complex integer) 'ci)
                    ((complex double-float) 'cdf)
                    ((complex single-float) 'csf))
                  (typecase ci
                    ((complex (integer 0 3)) 'invalid)
                    ((complex (integer 0 6)) 'invalid)
                    ((complex (integer 4 8)) 'ci)
                    ((complex integer) 'overboard)))))
        (is-equal (xxx) '(ci csf csf cdf cdf ci))
        (compile 'xxx)
        (is-equal (xxx) '(ci csf csf cdf cdf ci))))

;;; Date 2019-04-19
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/issues/493
;;; Fixed: a73df694
;;; Description
;;;
;;;   SUBTYPEP and TYPEP are not consistent for COMPLEX type in ANSI
;;;   spec. SUBTYPEP pursues the internal representation (with
;;;   UPGRADED-COMPLEX-PART-TYPE) while TYPEP goes after the type of
;;;   the complex number parts to match the typespec. Problem was
;;;   exhibited in compiled code. These are just a few examples which
;;;   explore ECL potential failures. Test which goes more
;;;   systematically across more types is defined in ansi-tests under
;;;   name SUBTYPEP-COMPLEX.8.
(test cmp.0071.cmp-typep-subtypep
      (is (typep #c(1.0 2.0) '(complex single-float)))
      (is (typep #c(1 2) '(complex fixnum)))
      (is (typep #c(1 2) '(complex (integer 0 8))))
      (is (not (typep #c(1.0 2.0) '(complex double-float))))
      (is (not (typep #c(1 2) '(complex (integer 0 1)))))
      (is (not (typep #c(1/2 2/3) '(complex (integer 0 1)))))
      ;;
      #-complex-float (is (subtypep '(complex single-float) '(complex double-float)))
      #-complex-float (is (subtypep '(complex double-float) '(complex single-float)))
      #-complex-float (is (subtypep '(complex double-float) '(complex float)))
      #+complex-float (is (not (subtypep '(complex single-float) '(complex double-float))))
      #+complex-float (is (not (subtypep '(complex double-float) '(complex single-float))))
      (is (subtypep '(complex double-float) '(complex float)))
      (is (subtypep '(complex fixnum) '(complex integer)))
      (is (subtypep '(complex integer) '(complex fixnum)))
      (is (subtypep '(complex ratio) '(complex fixnum)))
      (is (subtypep '(complex bit) '(complex ratio)))
      ;; this should be true even if single-float has a specialized
      ;; representation because of the first rule:
      ;;
      ;;   (subtypep (complex t1) (complex t2)) is T, T when
      ;;
      ;;   1. (subtypep t1 t2) is T, T or
      ;;   2. (equal (ucpt t1) (ucpt t2))
      (is (subtypep '(complex single-float) '(complex real)))
      #-complex-float
      (is (and (subtypep '(complex bit) '(complex double-float))
               (subtypep '(complex double-float) '(complex bit))))
      #+complex-float
      (is (and (not (subtypep '(complex bit) '(complex double-float)))
               (not (subtypep '(complex double-float) '(complex bit) )))))

;;; Date 2019-04-26
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/issues/497
;;; Fixed: 60978163
;;; Description
;;;
;;;     Constant folding first dropped the multiple values, then
;;;     refused to compile properly for not self-evaluating ones. This
;;;     test checks if both cases are compiled correctly.
(test cmp.0072.cmp-constant-fold
  (let (f1 f2)
    (finishes (setq f1  (compile nil '(lambda () (byte 0 0)))))
    (finishes (setq f2  (compile nil '(lambda () (truncate 2 1)))))
    (is (equal '(0 . 0) (funcall f1)))
    (is (equal '(2 0) (multiple-value-list (funcall f2))))))

;;; Date 2019-07-02
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/issues/513
;;; Description
;;;
;;;     When number of args in unoptimized long call is exactly
;;;     ECL_C_ARGUMENTS limit we have a segfault, when it is greater
;;;     parse_key signals a condition.
(test cmp.0073.c-arguments-limit.miscompilation
  (with-compiler ("aux-cmp-0073.lsp" :load t)
    `(progn
       (defclass modual-bleh ()
         ((xxx :initarg :foo :initform nil)))
       (defmethod shared-initialize :after
         ((instance modual-bleh) (slot-names t) &key)
         42)
       (defun run-1 ()
         ">=63 arguments parse-key problem (first :foo is eaten)."
         (make-instance
          'modual-bleh
          :foo 00 :foo 01 :foo 02 :foo 03 :foo 04 :foo 05 :foo 06 :foo 07 :foo 08 :foo 09
          :foo 10 :foo 11 :foo 12 :foo 13 :foo 14 :foo 15 :foo 16 :foo 17 :foo 18 :foo 19
          :foo 20 :foo 21 :foo 22 :foo 23 :foo 24 :foo 25 :foo 26 :foo 27 :foo 28 :foo 29
          :foo 30 :foo 31))
       (defun run-2 ()
         "=62 arguments segmentation fault."
         (make-instance
          'modual-bleh
          :foo 00 :foo 01 :foo 02 :foo 03 :foo 04 :foo 05 :foo 06 :foo 07 :foo 08 :foo 09
          :foo 10 :foo 11 :foo 12 :foo 13 :foo 14 :foo 15 :foo 16 :foo 17 :foo 18 :foo 19
          :foo 20 :foo 21 :foo 22 :foo 23 :foo 24 :foo 25 :foo 26 :foo 27 :foo 28 :foo 29
          :foo 30))
       (defun run-3 ()
         "<=61 arguments all fine."
         (make-instance
          'modual-bleh
          :foo 00 :foo 01 :foo 02 :foo 03 :foo 04 :foo 05 :foo 06 :foo 07 :foo 08 :foo 09
          :foo 10 :foo 11 :foo 12 :foo 13 :foo 14 :foo 15 :foo 16 :foo 17 :foo 18 :foo 19
          :foo 20 :foo 21 :foo 22 :foo 23 :foo 24 :foo 25 :foo 26 :foo 27 :foo 28 :foo 29))))
  (finishes (run-1))
  (finishes (run-2))
  (finishes (run-3)))

;;; Date 2020-01-12
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/issues/550
;;; Description
;;;
;;;     When we invoke an unopitmized long call (that is we apply from
;;;     a stack frame), function argument is evaluated after the
;;;     arguments what is wrong for operators where function is the
;;;     first argument (and should be evaluated first).
(test cmp.0074.c-arguments-limit.evaluation-rule
      (flet ((make-fn (n)
               `(lambda ()
                  (let ((se-var '()))
                    (funcall (prog1 #'list (push :fun se-var))
                             ,@(list* `(push :arg se-var)
                                      (make-list (1- n))))
                    (nreverse se-var))))
             (check-fn (form)
               (is (equal '(:fun :arg) (funcall (compile nil form))))))
        (check-fn (make-fn 10))
        (check-fn (make-fn (1+ si::c-arguments-limit)))
        (check-fn (make-fn (1- si::c-arguments-limit)))
        (check-fn (make-fn si::c-arguments-limit))))

;;; Date 2020-03-18
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/issues/545
;;; Description
;;;
;;; The closure type for local functions calling global closures was
;;; not determined correctly to also be a global closure.
(test cmp.0075.local-fun.closure-type
  (ext:with-clean-symbols (*function*)
    (defvar *function*)
    (let ((result
           (funcall
            (compile nil
                     (lambda (b)
                       (flet ((%f10 () b))
                         (flet ((%f4 () (%f10)))
                           (incf b)
                           (setf *function* #'%f10) ; makes a global
                                                    ; closure out of %f10
                           (%f4)))))
            3)))
    (is (eq result 4))
    (is (eq (funcall *function*) 4)))))

;;; Date 2020-03-13
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/-/issues/565
;;; Description
;;;
;;;     This test checks whether the same constant is coalesced to the EQ
;;;     value among three distinct top-level forms.
;;;
;;;     ccmp's COMPILE-FILE produces two vectors VV and VVtemp which represent
;;;     the fasl data segment. The latter is deallocated after all top-level
;;;     forms are evaluated. As compiler processes them currently if the
;;;     object is first pushed to the temporary segment and then we try to add
;;;     it to the permanent segment we have two versions of the same objects
;;;     which are not EQ. File src/cmp/cmpwt.lsp has an appropriate FIXME in
;;;     the ADD-OBJECT function definition.
(test cmp.0076.make-load-form-non-eq
  (multiple-value-bind (file output)
      (with-compiler ("make-temp.lsp")
        "(in-package #:cl-test)"
        "(eval-when (:compile-toplevel :load-toplevel :execute)
          (defclass my-class ()
            ((name :initarg :name :accessor name)))
          (defmethod print-object ((obj my-class) stream)
            (print-unreadable-object (obj stream :identity t)
              (format stream \"~s ~s\" (name obj) (class-name (class-of obj)))))
          (defmethod make-load-form ((x my-class) &optional environment)
            (declare (ignore environment))
            `(make-instance ',(class-of x) :name ',(slot-value x 'name))))"
        "(eval-when (:compile-toplevel)
          (defparameter s4 (make-instance 'my-class :name :s4)))"
        "(defparameter *s4-a* nil)"
        "(defparameter *s4-b* nil)"
        "(let ((a '#.s4))
          (setf *s4-a* a))"
        "(defun foo ()
          (let ((x #.s4))
            (values x *s4-a* *s4-b*)))"
        "(let ((b '#.s4))
          (setf *s4-b* b))")
    (declare (ignore output))
    (load file)
    (delete-file "make-temp.lsp")
    (delete-file file))
  (multiple-value-bind (x a b) (foo)
    (is (eq x a) "~a is not eq to ~a" x a)
    (is (eq x b) "~a is not eq to ~a" x b)
    (is (eq a b) "~a is not eq to ~a" a b)))

(ext:with-clean-symbols (class)
  (test cmp.0077.make-load-form.circular-dep
    (macrolet ((make-template (&body extra)
                 `(with-compiler ("make-circle.lsp")
                    '(progn
                      (in-package #:cl-test)
                      (eval-when (:compile-toplevel :load-toplevel :execute)
                        (defclass class ()
                          ((peer  :initform nil :initarg :peer  :accessor peer)
                           (peer* :initform nil :initarg :peer* :accessor peer*)))
                        (defmethod make-load-form ((x class) &optional env)
                          (declare (ignore env))
                          (values `(make-instance 'class :peer ',(peer x))
                                  `(setf (peer* ',x) ',(peer* x)))))
                      (eval-when (:compile-toplevel)
                        (defparameter var1 (make-instance 'class))
                        (defparameter var2 (make-instance 'class :peer var1))
                        ,@extra))
                    "(defun foo () (values '#.var1 '#.var2))")))
      ;; Ordinary case (reference).
      (multiple-value-bind (file output)
          (make-template)
        (load file)
        (delete-file "make-circle.lsp")
        (delete-file file)
        (multiple-value-bind (v1 v2) (foo)
          (is (eq (peer v2) v1))))
      ;; Circularity between make forms (should signal an error).
      (signals error
        (unwind-protect (multiple-value-bind (file output)
                            (make-template (setf (peer var1) var2))
                          (when file (delete-file file)))
          (delete-file "make-circle.lsp"))
        "Successfully compiled a file with a circular dependency.")
      ;; Circularity between make and init forms (is not an error!).
      (multiple-value-bind (file output)
          (make-template (setf (peer* var1) var2))
        (load file)
        (delete-file "make-circle.lsp")
        (delete-file file)
        (multiple-value-bind (v1 v2) (foo)
          (is (eq (peer v2) v1))
          (is (eq (peer* v1) v2))))
      ;; Circularity between init forms (is not an error!).
      (multiple-value-bind (file output)
          (make-template (setf (peer* var1) var2)
                         (setf (peer* var2) var1))
        (load file)
        (delete-file "make-circle.lsp")
        (delete-file file)
        (multiple-value-bind (v1 v2) (foo)
          (is (eq (peer v2) v1))
          (is (eq (peer* v1) v2))
          (is (eq (peer* v2) v1)))))))

;;; Date 2020-03-13
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/-/issues/571
;;; Description
;;;
;;;     LOAD-TIME-VALUE inside a DEFMETHOD is evaluated at the
;;;     compilation time.
(test cmp.0078.defmethod-not-eager
  (finishes (with-compiler ("aux-compiler.0078.lsp")
              `(defclass class () ())
              `(defmethod method ()
                 (load-time-value (find-class class))))))

;;; Date 2020-05-01
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/issues/577
;;; Description
;;;
;;; Inlining of closures did not work properly if closed over
;;; variables were in the scope in which the inlined function was
;;; called.
(test cmp.0079.inline-closure
  ;; local function
  (is (equal
       (funcall (compile
                 nil
                 (lambda ()
                   (let ((b 123)
                         results)
                     (flet ((set-b (x) (setf b x))
                            (get-b () b))
                       (declare (inline set-b get-b))
                       (push (get-b) results)
                       (push b results)
                       (let ((b 345))
                         (push (get-b) results)
                         (push b results)
                         (set-b 0)
                         (push (get-b) results)
                         (push b results))
                       (push (get-b) results)
                       (push b results))
                     (nreverse results)))))
       '(123 123 123 345 0 345 0 0)))
  ;; global function from bytecodes compiler, proclaimed inline
  (ext:with-clean-symbols (set-b get-b)
    (proclaim '(inline set-b get-b))
    (eval
     '(let ((b 123))
        (defun set-b (x)
          (setf b x))
        (defun get-b () b)))
    (is (equal
         (funcall (compile
                   nil
                   (lambda ()
                     (let (results)
                       (push (get-b) results)
                       (let ((b 345))
                         (push (get-b) results)
                         (push b results)
                         (set-b 0)
                         (push (get-b) results)
                         (push b results))
                       (push (get-b) results)
                       (nreverse results)))))
         '(123 123 345 0 345 0))))
  ;; global function in same file, declaimed inline
  (load (with-compiler ("inline-closure.lsp")
          '(in-package #:cl-test)
          '(declaim (inline set-b.0079 get-b.0079))
          '(let ((b 123))
            (defun set-b.0079 (x)
              (setf b x))
            (defun get-b.0079 () b))
          '(defun foo.0079 ()
            (let (results)
              (push (get-b.0079) results)
              (let ((b 345))
                (push (get-b.0079) results)
                (push b results)
                (set-b.0079 0)
                (push (get-b.0079) results)
                (push b results))
              (push (get-b.0079) results)
              (nreverse results)))))
  (is (equal
       (funcall 'foo.0079)
       '(123 123 345 0 345 0))))

;;; Date 2020-05-08
;;;
;;; Regression from a fix for #577.
(test cmp.0080.inline-closure
  (is (funcall (compile
                nil
                '(lambda ()
                  (labels ((bam (pos)
                             (declare (inline bam))
                             (if (= pos 42)
                                 :banzai
                                 (let ((my-new-val 42))
                                   (bam my-new-val)))))
                    (eq :banzai (bam 30))))))))

;;; Date 2020-05-28
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/issues/591
;;; Description
;;;
;;; MULTIPLE-VALUE-SETQ would wrongly assign NIL to special variables
;;; due to not saving env->nvalues before calling SET
(ext:with-clean-symbols (*a* *b* foo)
  (defvar *a* :wrong-a)
  (defvar *b* :wrong-b)
  (defun foo () (values :right-a :right-b))
  (test cmp.0081.m-v-setq-special
    (is (funcall (compile
                  nil
                  '(lambda ()
                    (multiple-value-setq (*a* *b*) (foo))
                    (and (eq *a* :right-a)
                         (eq *b* :right-b))))))))

;;; Date 2020-08-14
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/-/issues/594
;;; Description
;;;
;;;     The code walker used in DEFMETHOD would call MAKE-LOAD-VALUE
;;;     for literal objects encountered during code walking, even while
;;;     loading a file or using eval.
(ext:with-clean-symbols (test-class test-method)
  (eval '(defclass test-class () ()))
  (eval '(defmethod make-load-form ((obj test-class) &optional env)
          (error "We shouldn't have called MAKE-LOAD-FORM here.")))
  (test cmp.0082.defmethod-make-load-form
    (let* ((test-obj (make-instance 'test-class))
           (code `(defmethod test-method ()
                    ,test-obj)))
      (finishes (eval code)))))

;;; Date 2021-01-02
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/-/issues/620
;;; Description
;;;
;;;     RETURN inside the symbol or value arguments for PROGV leads to
;;;     a segfault
(ext:with-clean-symbols (*s*)
  (test cmp.0083.progv-return
    (proclaim '(special *s*))
    (is (eql 0 (funcall (compile nil
                                 '(lambda ()
                                   (block nil
                                     (progv (list (return 0)) (list 1))))))))
    (is (eql 0 (funcall (compile nil
                                 '(lambda ()
                                   (block nil
                                     (progv '(*s*) (list (return 0)))))))))
    (is (not (boundp '*s*)))
    (is (eql 1 (funcall (compile nil
                                 '(lambda ()
                                   (block nil
                                     (progv '(*s*) (list 0) (return 1) *s*)))))))
    (is (not (boundp '*s*)))))

;;; Date 2021-01-16
;;; Description
;;;
;;;     Compiling a local function of type CLOSURE can lead to an
;;;     internal compiler error if the function is later inlined
;;;     because the compiler would indiscriminantly change the closure
;;;     type to LEXICAL during inlining.
(ext:with-clean-symbols (some-global-fun another-global-fun)
  (defun some-global-fun (fun)
    (funcall fun))
  (defun another-global-fun (x)
    x)
  (test cmp.0084.inline-local-closure-type
    (let ((fun '(lambda (arg)
                 (declare (optimize speed))
                 (labels
                     ((a ()
                        (some-global-fun #'b)
                        (c))
                      (b ()
                        (c))
                      (c ()
                        ;; c is of type CLOSURE (arg is passed to
                        ;; a global function). This "infects" a
                        ;; and b to be of type CLOSURE too.
                        (incf arg)
                        (another-global-fun arg)))
                   (declare (inline a))
                   (a))))
          compiled-fun warnings-p errors-p)
      (finishes (multiple-value-setq (compiled-fun warnings-p error-p)
                  (compile nil fun)))
      (is (null errors-p))
      (is (= (funcall compiled-fun 0) 2)))))

;;; Date 2020-08-14
;;; Description
;;;
;;;     (values (values)) was miscompiled and returned no value
;;;     instead of the correct nil
(test cmp.0085.values-values
  (is (equal '(nil)
             (multiple-value-list
              (funcall
               (compile nil '(lambda () (values (values)))))))))

;;; Date 2021-03-25
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/-/issues/633
;;; Description
;;;
;;;     The order of function arguments was wrong when inlining a
;;;     function with &key and &aux arguments. This test checks
;;;     correct ordering in general.
(test cmp.0086.inline-ordering-function-arguments
  (is (equal (multiple-value-list
              (funcall (compile nil '(lambda ()
                                      (flet ((f (a
                                                 &optional (b a)
                                                 &rest c
                                                 &key (d c)
                                                 &aux (e d))
                                               (list a b c d e)))
                                        (declare (inline f))
                                        (values (f 1)
                                                (f 1 2)
                                                (f 1 2 :d 3)))))))
             '((1 1 nil nil nil)
               (1 2 nil nil nil)
               (1 2 (:d 3) 3 3)))))

;;; Date 2021-03-30
;;; Description
;;;
;;;     let bindings of lists like '(quote ...) were miscompiled
(test cmp.0087.let-list-containing-quote
  (is (equal '((quote) (quote a b c))
             (funcall
              (compile nil '(lambda () (let ((x '(quote)) (y '(quote a b c))) (list x y))))))))

;;; Date 2021-11-19
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/-/issues/662
;;; Description
;;;
;;;     In ccmp a global symbol macro cannot be lexically rebound.
;;;
(test cmp.0089.symbol-macro
  (when (finishes
         (with-compiler ("cmp.0089.symbol-macro.lsp" :load t)
           `(define-symbol-macro cmp.0089.sym 42)
           `(defun cmp.0089.fun1 (cmp.0089.sym) cmp.0089.sym)
           `(defun cmp.0089.fun2 () cmp.0089.sym)))
    (is (= 15 (cmp.0089.fun1 15)))
    (is (= 42 (cmp.0089.fun2)))))

;;; Date 2022-01-29
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/-/issues/672
;;; Description
;;;
;;;     Apply, funcall and multiple-value-call did not check the
;;;     number of arguments when lambda expressions. This test fairly
;;;     comprehensively checks that we signal an error if we get wrong
;;;     number of arguments and also includes some non-error cases.
;;;
(test cmp.0090.funcall/apply-inline-and-number-of-arguments
  (let ((*standard-output* (make-broadcast-stream)))
    (signals error (funcall (compile nil '(lambda () (funcall (lambda (a b) (list a b)) 1)))))
    (signals error (funcall (compile nil '(lambda () (funcall (lambda (a b) (list a b)) 1 2 3)))))
    (signals error (funcall (compile nil '(lambda () (funcall (lambda (a &optional b) (list a b)) 1 2 3)))))
    (is (equal (funcall (compile nil '(lambda () (funcall (lambda (a &optional b) (list a b)) 1)))) '(1 nil)))
    (is (equal (funcall (compile nil '(lambda () (funcall (lambda (a &optional b) (list a b)) 1 2)))) '(1 2)))
    (signals error (funcall (compile nil '(lambda () (apply (lambda (a b) (list a b)) '(1))))))
    (signals error (funcall (compile nil '(lambda () (apply (lambda (a b) (list a b)) '(1 2 3))))))
    (signals error (funcall (compile nil '(lambda () (apply (lambda (a b) (list a b)) 1 '(2 3))))))
    (is (equal (funcall (compile nil '(lambda (x) (apply (lambda (a b) (list a b)) x))) '(1 2)) '(1 2)))
    (is (equal (funcall (compile nil '(lambda (x) (apply (lambda (a b) (list a b)) 1 x))) '(2)) '(1 2)))
    (signals error (funcall (compile nil '(lambda (x) (apply (lambda (a b) (list a b)) 1 x))) '(2 3)))
    (is (equal (funcall (compile nil '(lambda () (apply (lambda (a &optional b) (list a b)) '(1))))) '(1 nil)))
    (signals error (funcall (compile nil '(lambda () (apply (lambda (a &optional b) (list a b)) '(1 2 3))))))
    (signals error (funcall (compile nil '(lambda () (apply (lambda (a &optional b) (list a b)) 1 '(2 3))))))
    (is (equal (funcall (compile nil '(lambda (x) (apply (lambda (a &optional b) (list a b)) x))) '(1 2)) '(1 2)))
    (is (equal (funcall (compile nil '(lambda (x) (apply (lambda (a &optional b) (list a b)) 1 x))) '(2)) '(1 2)))
    (signals error (funcall (compile nil '(lambda (x) (apply (lambda (a &optional b) (list a b)) 1 x))) '(2 3)))
    (signals error (funcall (compile nil '(lambda () (multiple-value-call (lambda (a b) (list a b)) (values 1))))))
    (signals error (funcall (compile nil '(lambda () (multiple-value-call (lambda (a b) (list a b)) (values  1 2 3))))))
    (signals error (funcall (compile nil '(lambda () (multiple-value-call (lambda (a &optional b) (list a b)) (values  1 2 3))))))
    (is (equal (funcall (compile nil '(lambda () (multiple-value-call (lambda (a &optional b) (list a b)) (values  1))))) '(1 nil)))
    (is (equal (funcall (compile nil '(lambda () (multiple-value-call (lambda (a &optional b) (list a b)) (values  1 2))))) '(1 2)))))

;;; Date 2022-08-13
;;; URL: https://gitlab.com/embeddable-common-lisp/ecl/-/issues/630
;;; Description
;;;
;;;     The compiler would run into an infinite loop when two lists
;;;     with the same circular structure were contained as literal
;;;     data in the same file.
;;;
(test cmp.0091.infinite-loop-circular-data
  (finishes (with-compiler ("infinite-loop-circular-data.lsp")
              '(eq '#1=(a . #1#) '#2=(a . #2#))
              '(eq '#3=(a b . #3#) '#4=(a b . #4#))
              '(eq '#5=(#5# . a) '#6=(#6# . a))
              '(eq '#7=((#7# . a) . b) '#8=((#8# . a) . b))
              '(eq '#9=((a . #9#) . b) '#10=((a . #10#) . b))
              '(eq '#11=(#11# . #11#) '#12=(#12# . #12#)))))
