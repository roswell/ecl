;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Compiler regression tests

(in-package :cl-test)


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

(deftest compiler.0001.import
   (progn
    (defpackage "FOO" (:USE) (:IMPORT-FROM "CL" "NIL" "T"))
    (prog1 (multiple-value-list (find-symbol "NIL" (find-package "FOO")))
     (delete-package "FOO")))
 (NIL :INTERNAL))

;;; Date: 09/05/2006
;;; From: Brian Spilsbury
;;; Fixed: 20/05/2006 (Brian Spilsbury)
;;; Description:
;;;
;;;      Compiled FLET forms failed to shadow global macro definitions, if not
;;;      for the compiler, at least for MACRO-FUNCTION and MACROEXPAND[-1]
;;;

(deftest compiler.0002.macro-shadow
   (progn
     (with-compiler ("aux-cl-0002.lsp")
       '(defmacro foo () 2)
       '(defmacro test (symbol &environment env)
          (and (macro-function symbol env) t))
       '(defun doit () (flet ((foo () 1)) (test foo))))
     (load "aux-cl-0002")
     (delete-file "aux-cl-0002.lsp")
     (delete-file (compile-file-pathname "aux-cl-0002" :type :fas))
     (prog1
         (doit)
       (fmakunbound 'doit)
       (fmakunbound 'test)
       (fmakunbound 'foo)))
   NIL)

;;;
;;; Fixed: 14/06/2006 (juanjo)
;;; Description:
;;;
;;;     APROPOS, APROPOS-LIST and HELP* are case sensitive.
;;;

(deftest compiler.0003.apropos
  (and (equal (apropos-list "bin")
              (apropos-list "bin"))
       t)
  t)

;;; Date: 08/07/2006 (Dave Roberts)
;;; Fixed: 02/08/2006 (juanjo)
;;; Description:
;;;
;;;     SLIME traps when invoking DESCRIBE. Reason is that STREAMP breaks on
;;;     Gray streams.
;;;

(deftest compiler.0004.streamp
    (streamp (make-instance 'gray:fundamental-stream))
  t)

;;; Date: 02/08/2006 (juanjo)
;;; Description:
;;;
;;;     There is a problem with SUBTYPEP and type STREAM
;;;

(deftest compiler.0005.subtypep-stream
    (subtypep (find-class 'gray:fundamental-stream) 'stream)
  t t)

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

(deftest compiler.0006.enough-namestring
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
      (every #'test-default+paths *enough-namestring_tests*))
  t)

;;; Date: 10/08/2006 (Lars Brinkhoff)
;;; Fixed: 1/09/2006 (juanjo)
;;; Details:
;;;
;;;     ADJUST-ARRAY must signal a type error when the value of :FILL-POINTER is
;;;     not NIL and the adjustable array does not have a fill pointer
;;;

(deftest compiler.0007.adjustable-array
    (loop for fp in '(nil t) collect
          (loop for i in '(t nil 0 1 2 3) collect
                (and
                 (handler-case (adjust-array (make-array 3 :adjustable t :fill-pointer fp) 4
                                             :fill-pointer i)
                   (type-error (c) nil)
                   (error (c) t))
                 t)))
  ((nil t nil nil nil nil) (t t t t t t)))

;;; Date: 09/10/2006 (Dustin Long)
;;; Fixed: 10/10/2006
;;; Description:
;;;
;;;     The namestring "." is improperly parsed, getting a file type of ""
;;;     Additionally we found it more convenient to have the _last_ dot mark
;;;     the file type, so that (pathname-type "foo.mpq.txt") => "txt"
;;;

(deftest compiler.0008.parse-namestring
    (loop for (namestring name type) in
          '(("." "." NIL) (".." "." "") (".foo" ".foo" NIL) (".foo.mpq.txt" ".foo.mpq" "txt")
            ("foo.txt" "foo" "txt") ("foo.mpq.txt" "foo.mpq" "txt"))
          unless (let ((x (parse-namestring namestring)))
                   (and (equal name (pathname-name x))
                        (equal type (pathname-type x))
                        (equal '() (pathname-directory x))))
          collect namestring)
  ())

;;; Date: 28/09/2006
;;; Fixed: 10/10/2006
;;; Description:
;;;
;;;     Nested calls to queue_finalizer trashed the value of cl_core.to_be_finalized
;;;     The following code tests that at least three objects are finalized.
;;;
;;; Note: this test fails in multithreaded mode. GC takes too long!
(deftest compiler.0009.finalization
    (let ((*all-tags* '()))
      (declare (special *all-tags*))
      (flet ((custom-finalizer (tag)
               #'(lambda (o) (push tag *all-tags*))))
        (let ((a '()))
          (dotimes (i 5)
            (let ((x (cons i i)))
              (si::set-finalizer x (custom-finalizer i))
              (push x a))))
        (dotimes (j 100)
          (dotimes (i 10000)
            (cons 1.0 1.0))
          (si::gc t)))
      (sort *all-tags* #'<))
  (0 1 2 3 4))


;;; Date: 8/10/2006 (Dustin Long)
;;; Fixed: 10/10/2006 (Dustin Long)
;;; Description:
;;;
;;;     Hash table iterators have to check that their argument is
;;;     really a hash table.
;;;

(deftest compiler.0010.hash-iterator
    (loop for i in *mini-universe*
          when (and (not (hash-table-p i))
                    (handler-case (progn (loop for k being the hash-keys of i) t)
                      (error (c) nil)))
          collect (type-of i))
  nil)

;;; Date: 31/12/2006 (Richard M. Kreuter)
;;; Fixed: 5/1/2007 (Juanjo)
;;; Description:
;;;
;;;     The keyword :BACK does not work as expected when creating pathnames
;;;     and causes an error when at the beginning: (:RELATIVE :BACK)
;;;

(deftest compiler.0011.make-pathname-with-back
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
                   (list (list l d1 d2 d3 d4 l2 x y)))))
  nil)

;;; Date: 11/03/2007 (Fare)
;;; Fixed: 23/03/2007 (Juanjo)
;;; Description:
;;;
;;;     COPY-READTABLE did not copy the entries of the "from" table
;;;     when a second argument, i.e. a "destination" table was supplied.
;;;

(deftest compiler.0012.copy-readtable
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
            collect c))
    nil)

;;; Date: 05/01/2008 (Anonymous, SF bug report)
;;; Fixed: 06/01/2008 (Juanjo)
;;; Description:
;;;
;;;     For a file linked as follows "ln -s //usr/ /tmp/foo", 
;;;     (truename #p"/tmp/foo") signals an error because //usr is
;;;     parsed as a hostname.
;;;

#-windows
(deftest compiler.0013.truename
    (progn
      (si:system "rm -rf foo; ln -sf //usr/ foo")
      (prog1 (namestring (truename "./foo"))
        (si::system "rm foo")))
  "/usr/")

;;; Date: 30/08/2008 (Josh Elsasser)
;;; Fixed: 01/09/2008 (Juanjo)
;;; Description:
;;;
;;;     Inside the form read by #., recursive definitions a la #n=
;;;     and #n# were not properly expanded
;;;
(deftest compiler.0014.sharp-dot
    (with-output-to-string (*standard-output*)
      (let ((*print-circle* t))
        (read-from-string "'#.(princ (list '#1=(1 2) '#1#))")))
  "(#1=(1 2) #1#)")

;;; Date: 30/08/2008 (Josh Elsasser)
;;; Fixed: 30/08/2008 (Josh Elsasser)
;;; Description:
;;;
;;;     A setf expansion that produces a form with a macro that also has
;;;     its own setf expansion does not giver rise to the right code.
;;;
(deftest compiler.0015-setf-expander
    (progn
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
      (prog1
          (equalp (eval '(let ((foo 5))
                          (list foo (triple foo) (setf (triple foo) 6) foo (triple foo))))
                  (eval '(let ((foo 5))
                          (list foo (hidden foo) (setf (hidden foo) 6) foo (hidden foo)))))
        (fmakunbound 'hidden)
        (fmakunbound 'triple)))
  T)

;;; Date: 17/2/2009
;;; Fixed: 17/2/2009
;;; Description:
;;;
;;;     The defstruct form fails with an :include field that overwrites
;;;     a slot that is read only.
;;;
(deftest compiler.0016.defstruct-include
    (progn
      (eval '(progn
              (defstruct compiler.0016-a (a 1 :read-only t))
              (defstruct (compiler.0016-b (:include compiler.0016-a (a 2))))
              (defstruct (compiler.0016-c (:include compiler.0016-a (a 3 :read-only t))))))
      (values
       (handler-case (eval '(defstruct (compiler.0016-d (:include compiler.0016-a (a 2 :read-only nil)))))
         (error (c) t))
       (compiler.0016-a-a (make-compiler.0016-a))
       (compiler.0016-b-a (make-compiler.0016-b))
       (compiler.0016-c-a (make-compiler.0016-c))
       (handler-case (eval '(setf (compiler.0016-c-a (make-compiler.0016-c)) 3))
         (error (c) t))))
  t 1 2 3 t)

;;; Date: 9/11/2009
;;; Fixed: 9/11/2009
;;; Description:
;;;
;;;     LOAD does not work with special files (/dev/null)
;;;
(deftest compiler.0017.load-special
    (handler-case (and (load #+(or windows mingw32) "NULL"
                             #-(or windows mingw32) "/dev/null")
                       t)
      (serious-condition (c) nil))
  t)

;;; Date: 16/11/2009 (Gabriel)
;;; Fixed: 20/11/2009 (Juanjo)
;;; Description:
;;;
;;;     #= and ## reader macros do not work well with #.
;;;
(deftest compiler.0018.sharp-eq
  (handler-case (values (read-from-string "(#1=(0 1 2) #.(length '#1#))"))
     (serious-condition (c) nil))
  ((0 1 2) 3))

;;; Date: 14/11/2009 (M. Mondor)
;;; Fixed: 20/11/2009 (Juanjo)
;;; Description:
;;;
;;;     FDEFINITION and SYMBOL-FUNCTION cause SIGSEGV when acting on NIL.
;;;
(deftest compiler.0019.fdefinition
  (and (handler-case (fdefinition nil)
                     (undefined-function (c) t)
                     (serious-condition (c) nil))
       (handler-case (symbol-function nil)
                     (undefined-function (c) t)
                     (serious-condition (c) nil)))
  t)


;;; Date: 29/11/2009 (P. Costanza)
;;; Fixed: 29/11/2009 (Juanjo)
;;; Description:
;;;
;;;     Updating of instances is not triggered by MAKE-INSTANCES-OBSOLETE.
;;;
(deftest compiler.0020.make-instances-obsolete
    (progn
      (defparameter *update-guard* nil)
      (defclass compiler.0020-a () ((b :accessor compiler.0020-a-b :initarg :b)))
      (let ((*a* (make-instance 'compiler.0020-a :b 2)))
        (defmethod update-instance-for-redefined-class :before
            ((instance standard-object) added-slots discarded-slots property-list
             &rest initargs)
          (setf *update-guard* t))
        (and (null *update-guard*)
             (progn (compiler.0020-a-b *a*) (null *update-guard*))
             (progn (make-instances-obsolete (find-class 'compiler.0020-a))
                    (null *update-guard*))
             (progn (compiler.0020-a-b *a*) *update-guard*)
             (progn (setf *update-guard* nil)
                    (defclass compiler.0020-a () ((b :accessor compiler.0020-a-b :initarg :b)))
                    (compiler.0020-a-b *a*)
                    *update-guard*)
             t)))
  t)

;;; Date: 25/03/2009 (R. Toy)
;;; Fixed: 4/12/2009 (Juanjo)
;;; Description:
;;;
;;;     Conversion of rationals into floats is done by truncating, not by
;;;     rounding, what implies a loss of accuracy.
;;;
(deftest compiler.0021.ratio-to-float
    ;; The test builds a ratio which is very close to 1 but which is below it
    ;; If we truncate instead of rounding the output will not be 1 coerced
    ;; to that floating point type.
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
                      collect (list type r))))
  nil)

;;; Date: 06/04/2010 (M. Kocic)
;;; Fixed: 4/12/2009
;;; Description:
;;;
;;;     Inspection of structs is broken due to undefined inspect-indent
;;;
(deftest compiler.0022.inspect-struct
    (let ((*query-io* (make-string-input-stream "q
")))
      (defstruct st1 p1)
      (let ((v1 (make-st1 :p1 "tttt")))
        (handler-case (progn (inspect v1) t)
          (error (c) nil))))
  t)


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
(deftest compiler.0023.block
    (funcall (compile nil
                      '(lambda ()
                        (block nil
                          (funcall 'mapcar
                                   #'(lambda (x)
                                       (when x (return x)))
                                   '(1 2 3 4))))
                      ))
  1)

;;; Fixed: 12/01/2006 (juanjo)
;;; Description:
;;;
;;;     COMPILE-FILE-PATHNAME now accepts both :FAS and :FASL as
;;;     synonyms.
;;;
;;;
(deftest compiler.0024.pathname
    (and (equalp (compile-file-pathname "foo" :type :fas)
                 (compile-file-pathname "foo" :type :fasl))
         t)
  t)

;;; Fixed: 21/12/2005 (juanjo)
;;; Description:
;;;
;;;     Compute the path of the intermediate files (*.c, *.h, etc)
;;;     relative to that of the fasl or object file.
;;;

(deftest compiler.0025.paths
    (let* ((output (compile-file-pathname "tmp/aux" :type :fasl))
           (h-file (compile-file-pathname output :type :h))
           (c-file (compile-file-pathname output :type :c))
           (data-file (compile-file-pathname output :type :data)))
      (and 
       (zerop (si::system "rm -rf tmp; mkdir tmp"))
       (with-compiler ("aux-compiler.0103-paths.lsp" :output-file output :c-file t
                       :h-file t :data-file t)
         '(defun foo (x) (1+ x)))
       (probe-file output)
       (probe-file c-file)
       (probe-file h-file)
       (probe-file data-file)
       (delete-file "aux-compiler.0103-paths.lsp")
       t))
    t)

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
(deftest compiler.0026.defconstant-warn
    (let ((warn nil))
      (with-dflet ((c::cmpwarn (setf warn t)))
        (with-compiler ("aux-compiler.0104.lsp")
          '(defconstant foo (list 1 2 3))
          '(print foo)))
      (delete-file "aux-compiler.0104.lsp")
      (delete-file (compile-file-pathname "aux-compiler.0104.lsp" :type :fas))
      warn)
  nil)

;;; Date: 16/04/2006
;;; From: Juanjo
;;; Fixed: 16/04/2006 (juanjo)
;;; Description:
;;;
;;;     Special declarations should only affect the variable bound and
;;;     not their initialization forms. That, even if the variables are
;;;     the arguments of a function.
;;;

(deftest compiler.0027.declaration
    (let ((form '(lambda (y)
                  (flet ((faa (&key (x y))
                           (declare (special y))
                           x))
                    (let ((y 4))
                      (declare (special y))
                      (faa))))))
      ;; We must test that both the intepreted and the compiled form
      ;; output the same value.
      (list (funcall (compile 'nil form) 3)
            (funcall (coerce form 'function) 3)))
  (3 3))

;;; Date: 26/04/2006
;;; From: Michael Goffioul
;;; Fixed: ----
;;; Description:
;;;
;;;     Functions with more than 64 arguments have to be invoked using
;;;     the lisp stack.
;;;

(deftest compiler.0028.call-arguments-limit
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
      (equal (funcall (compile 'foo form))
             (funcall (coerce form 'function))))
  t)

;;; Date: 16/05/2005
;;; Fixed: 18/05/2006 (juanjo)
;;; Description:
;;;
;;;     The detection of when a lisp constant has to be externalized using MAKE-LOAD-FORM
;;;     breaks down with some circular structures
;;;

(defclass compiler.017-class ()
  ((parent :accessor compiler.017-parent :initform nil)
   (children :initarg :children :accessor compiler.017-children :initform nil)))

(defmethod make-load-form ((x compiler.017-class) &optional environment)
  (declare (ignore environment))
  (values
    ;; creation form
    `(make-instance ',(class-of x) :children ',(slot-value x 'children))
    ;; initialization form
    `(setf (compiler.017-parent ',x) ',(slot-value x 'parent))
     ))

(deftest compiler.0029.circular-load-form
    (loop for object in
         (let ((l (list 1 2 3)))
           (list l
                 (subst 3 l l)
                 (make-instance 'compiler.017-class)
                 (subst (make-instance 'compiler.017-class) 3 l)))
       collect (clos::need-to-make-load-form-p object nil))
  (nil nil t t))

;;; Date: 18/05/2005
;;; Fixed: 17/05/2006 (Brian Spilsbury & juanjo)
;;; Description:
;;;
;;;     The compiler is not able to externalize constants that have no printed representation.
;;;     In that case MAKE-LOAD-FORM should be used.
;;;

(deftest compiler.0030.make-load-form
    (let ((output (compile-file-pathname "aux-compiler.0108.lsp" :type :fasl)))
      (with-open-file (s "aux-compiler.0108.lsp" :if-exists :supersede :if-does-not-exist :create :direction :output)
        (princ "
(eval-when (:compile-toplevel)
 (defvar s4 (make-instance 'compiler.017-class))
 (defvar s5 (make-instance 'compiler.017-class))
 (setf (compiler.017-parent s5) s4)
 (setf (compiler.017-children s4) (list s5)))

(defvar a '#.s5)
(defvar b '#.s4)
(defvar c '#.s5)
(defun foo ()
  (let ((*print-circle* t))
    (with-output-to-string (s) (princ '#1=(1 2 3 #.s4 #1#) s))))
" s))
      (compile-file "aux-compiler.0108.lsp")
      (load output)
      (prog1 (foo)
        (delete-file output)
        (delete-file "aux-compiler.0108.lsp")))
  "#1=(1 2 3 #<a CL-TEST::COMPILER.017-CLASS> #1#)")

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
(deftest compiler.0031.macrolet
    (list
     (progn
       (defun bar ()
         (macrolet ((x () 2))
           (macrolet ((m () (x)))
             (m))))
       (compile 'bar)
       (bar))
     (progn
       (defun bar ()
         (symbol-macrolet ((x 2))
           (macrolet ((m () x))
             (m))))
       (compile 'bar)
       (bar)))
  (2 2))

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
(deftest compiler.0032.macrolet
  (flet ((eval-with-error (form)
           (handler-case (eval form)
             (error (c) 'error))))
    (makunbound 'compiler.0110-foo)
    (fmakunbound 'compiler.0110-foo)
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
                 (declare (special compiler.0110-foo))
                 (macrolet ((m () compiler.0110-foo))
                   (m)))
                (let ((faa 5))
                 (macrolet ((m () compiler.0110-foo))
                   (m)))
                (macrolet ((compiler.0110-foo () 6))
                  (macrolet ((m () (compiler.0110-foo)))
                    (m)))
                (macrolet ((f1 () 7)
                           (f2 () 8))
                  ;; M should not see the new definitions F1 and F2
                  (macrolet ((f1 () 9)
                             (f2 () 10)
                             (m () (list 'quote (list (f1) (f2)))))
                    (m)))
                (flet ((compiler.0110-foo () 1))
                  (macrolet ((m () (compiler.0110-foo)))
                    (m)))
                (labels ((compiler.0110-foo () 1))
                  (macrolet ((m () (compiler.0110-foo)))
                    (m)))))))
  (error 1 error error 6 (7 8) error error ))

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
(deftest compiler.0033.c-arguments-limit
    (mapcar #'(lambda (nargs)
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
            '(10 20 30 40 50 63 64 65 70))
  ((10 :ERROR :ERROR) (20 :ERROR :ERROR) (30 :ERROR :ERROR) (40 :ERROR :ERROR)
   (50 :ERROR :ERROR) (63 :ERROR :ERROR) (64 :ERROR :ERROR) (65 :ERROR :ERROR)
   (70 :ERROR :ERROR)))

(let* ((nargs 10)
       (arg-list (loop for i from 0 below nargs
                       collect (intern (format nil "arg~d" i))))
       (arguments (make-list nargs)))
  (apply (compile 'foo `(lambda ,arg-list
                         (length (list ,@arg-list))))
         arguments))

;;; Date: 12/07/2008 (Josh Elsasser)
;;; Fixed: 02/08/2008 (Juanjo)
;;; Description:
;;;
;;;     ECL fails to properly compute the closure type of a function that
;;;     returns a lambda that calls the function itself.
;;;
(deftest compiler.0034.compute-closure
  (and (with-compiler ("aux-compiler.0103-paths.lsp" :load t)
         (defun testfun (outer)
           (labels ((testlabel (inner)
                      (if inner
                          (testfun-map
                           (lambda (x) (testlabel x))
                           inner))
                      (print outer)))
             (testlabel outer))))
       t)
  t)

;;; Date: 02/09/2008 (Josh Elsasser)
;;; Fixed: 12/09/2008 (Josh Elsasser)
;;; Description:
;;;
;;;     FTYPE proclamations and declarations do not accept user defined
;;;     function types.
;;;
(deftest compiler.0035.ftype-user-type
    (progn
      (deftype compiler.0113-float-function () '(function (float) float))
      (deftype compiler.0113-float () 'float)
      (loop for (type . fails) in
           '(((function (float) float) . nil)
             (cons . t)
             (compiler.0113-float-function . nil)
             (compiler.0113-float . t))
         always (let ((form1 `(proclaim '(ftype ,type foo)))
                      (form2 `(compile nil '(lambda ()
                                             (declare (ftype ,type foo))
                                             (foo)))))
                  (if fails
                      (and (signals-error (eval form1) error)
                           (signals-error (eval form2) error)
                           t)
                      (progn
                        (eval form1)
                        (eval form2)
                        t)))))
  t)

;;; Date: 01/11/2008 (E. Marsden)
;;; Fixed: 02/11/2008 (Juanjo)
;;; Description:
;;;
;;;     When compiled COERCE with type INTEGER may cause double
;;;     evaluation of a form.
(deftest compiler.0036.coerce
    (funcall
     (compile 'foo '(lambda (x) (coerce (shiftf x 2) 'integer)))
     1)
  1)

;;; Date: 03/11/2008 (E. Marsden)
;;; Fixed: 08/11/2008 (Juanjo)
;;; Description:
;;;
;;;     TYPEP, with a real type, produces strange results.
;;;
(deftest compiler.0037.coerce
    (funcall
     (compile 'foo '(lambda (x) (typep (shiftf x 1) '(real 10 20))))
     5)
  NIL)

;;; Date: 20/07/2008 (Juanjo)
;;; Fixed: 20/07/2008 (Juanjo)
;;; Description:
;;;
;;;     In the new compiler, when compiling LET forms with special variables
;;;     the values of the variables are not saved to make the assignments
;;;     really parallel.
;;;
(deftest compiler.0038.let-with-specials
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
            (stak-aux)))) 18 12 6))
  7)

;;; Date: 06/10/2009 (J. Pellegrini)
;;; Fixed: 06/10/2009 (Juanjo)
;;; Description:
;;;     Extended strings were not accepted as documentation by the interpreter.
;;;
(deftest compiler.0039.docstrings
  (handler-case
   (progn
     (eval `(defun foo () ,(make-array 10 :initial-element #\Space :element-type 'character) 2))
     (eval (funcall 'foo)))
   (serious-condition (c) nil))
  2)

;;; Date: 07/11/2009 (A. Hefner)
;;; Fixed: 07/11/2009 (A. Hefner + Juanjo)
;;; Description:
;;;     ECL ignores the IGNORABLE declaration
;;;
(deftest compiler.0040.ignorable
    (let ((c::*suppress-compiler-messages* t))
      (and
       ;; Issue a warning for unused variables
       (handler-case (and (compile nil '(lambda (x y) (print x))) nil)
         (warning (c) t))
       ;; Do not issue a warning for unused variables declared IGNORE
       (handler-case (and (compile nil '(lambda (x y) (declare (ignore y))
                                         (print x))) t)
         (warning (c) nil))
       ;; Do not issue a warning for unused variables declared IGNORABLE
       (handler-case (and (compile nil '(lambda (x y) (declare (ignorable y))
                                         (print x))) t)
         (warning (c) nil))
       ;; Do not issue a warning for used variables declared IGNORABLE
       (handler-case (and (compile nil '(lambda (x y) (declare (ignorable x y))
                                         (print x))) t)
         (warning (c) nil))))
  t)

;;; Date: 29/11/2009 (P. Costanza)
;;; Fixed: 29/11/2009 (Juanjo)
;;; Description:
;;;     When calling a bytecodes (SETF ...) function from a compiled function
;;;     an invalid memory access is produced. This is actually a consequence
;;;     of a mismatch between the position of the fields bytecodes.entry
;;;     and cfun.entry
;;;
#-ecl-bytcmp
(deftest compiler.0041.bytecodes-entry-position
    (let ((indices (funcall (compile nil
                                     '(lambda ()
                                       (ffi:c-inline () () list "
        union cl_lispunion x[0];
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
      (and (apply #'= indices) t))
  t)

;;; Date: 07/02/2010 (W. Hebich)
;;; Fixed: 07/02/2010 (Juanjo)
;;; Description:
;;;     THE forms do not understand VALUES types
;;;             (the (values t) (funcall sym))
;;;
(deftest compiler.0042.the-and-values
  (handler-case (and (compile 'foo '(lambda () (the (values t) (faa))))
                     t)
    (warning (c) nil))
  t)


;;; Date: 28/03/2010 (M. Mondor)
;;; Fixed: 28/03/2010 (Juanjo)
;;; Description:
;;;     ECL does not compile type declarations of a symbol macro
;;;
(deftest compiler.0043.symbol-macro-declaration
    (handler-case (and (compile 'nil
                                '(lambda (x)
                                  (symbol-macrolet ((y x))
                                    (declare (fixnum y))
                                    (+ y x))))
                       nil)
      (warning (c) t))
  nil)

;;; Date: 24/04/2010 (Juanjo)
;;; Fixed 24/04/2010 (Juanjo)
;;; Description:
;;;     New special form, WITH-BACKEND.
;;;
(deftest compiler.0044.with-backend
    (progn
      (defparameter *compiler.0122* nil)
      (defun compiler.0122a ()
        (ext:with-backend
            :bytecodes (setf *compiler.0122* :bytecodes)
            :c/c++ (setf *compiler.0122* :c/c++)))
      (list
       (progn (compiler.0122a) *compiler.0122*)
       (compiler.0122a)
       (progn (compile 'compiler.0122a) (compiler.0122a) *compiler.0122*)
       (compiler.0122a)))
  (:bytecodes :bytecodes :c/c++ :c/c++))



;;; Date: 10/08/2008
;;; From: Juanjo
;;; Fixed: 10/08/2008
;;; Description:
;;;
;;;     COS, SIN and TAN were expanded using a wrong C expression.
;;;

(deftest compiler.0045.inline-cos
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
       collect type)
  nil)



;;; Description:
;;;
;;;     The interpreter selectively complains when assigning a variable
;;;     that has not been declared as special and is not local.
;;;
;;; Fixed: 03/2006 (juanjo)
;;;
(deftest compiler.0046.global-setq
    (mapcar
     (lambda (ext:*action-on-undefined-variable*)
       (handler-case
           (progn (eval `(setq ,(gensym) 1)) :no-error)
         (error (c) :error)))
     '(nil ERROR))
  (:no-error :error))

;;; Date: 24/04/2010 (Juanjo)
;;; Fixed: 24/04/2010 (Juanjo)
;;; Description:
;;;     The interpreter does not increase the lexical environment depth when
;;;     optimizing certain forms (LIST, LIST*, CONS...) and thus causes some
;;;     of the arguments to be eagerly evaluated.
;;;
(deftest compiler.0046.list-optimizer-error
    (with-output-to-string (*standard-output*)
      (eval '(list (print 1) (progn (print 2) (print 3)))))
  "
1 
2 
3 ")



;;; Date: 2015-09-04
;;; Fixed: Daniel Kochmański
;;; Description
;;;     Compiler signalled arithmetic-error when producing C code for infinity
;;;     and NaN float values (part of ieee floating point extensions).

#+ieee-floating-point
(deftest compiler.0047.infinity-test.1
    (progn
      (defun aux-compiler-0047.infty-test.1 ()
        (> 0.0 ext:single-float-negative-infinity))
      (compile 'aux-compiler-0047.infty-test.1))
  aux-compiler-0047.infty-test.1 NIL NIL)

#+ieee-floating-point
(deftest compiler.0048.infinity-test.2
    (progn
      (with-compiler ("aux-compiler-0048.infty-test.2.lsp")
        '(defun doit () (> 0.0 ext:single-float-negative-infinity)))
      (load "aux-compiler-0048.infty-test.2.fas")
      (delete-file "aux-compiler-0048.infty-test.2.lsp")
      (delete-file "aux-compiler-0048.infty-test.2.fas")
      (doit))
  T)



;;; Date: 2015-12-18
;;; Fixed: Daniel Kochmański
;;; Description
;;;     Compiler expanded FIND incorrectly (ignored START and END arguments)

(deftest compiler.0049.cmpopt-sequences.1
    (progn
      (defun check-single-wildcard (identifier wildcard-pos)
        (not (find #\* identifier :start (1+ wildcard-pos))))
      (compile 'check-single-wildcard)
      (check-single-wildcard "dan*" 3))
  T)

;;; Date: 2016-02-10
;;; Fixed: Daniel Kochmański
;;; Description
;;;     Aux closures created by C compiler weren't handled correctly
;;;     in respect of the environment and declarations of the
;;;     variables
(deftest compiler.0050.cmptop/call.1
    (funcall (compile nil '(lambda ()
                            (labels
                                ((fun-2 () (fun-3 'cool))
                                 (fun-3 (clause-var)
                                   (flet ((fun-4 () clause-var))
                                     (fun-4))))
                              (let ((fun-1 (lambda () (fun-2))))
                                (funcall fun-1))))))
  cool)


;;; Date 2016-04-21
;;; Description
(deftest compiler.0051.ftype-args*
    (progn
      (declaim (ftype (function (*) (values T)) ce))
      (defun ce (expression) nil)
      (compile 'ce)
      (ce nil))
  nil)
