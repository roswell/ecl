;;;; -*- encoding:utf-8; Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Vladimir Sedach <vas@oneofus.la>
;;;; Created:  2020-05-03
;;;; Contains: Unicode handling in the compiler and runtime

(in-package :cl-test)

(suite 'unicode)

;;; Date: 2020-05-03
;;; From: Vladimir Sedach <vas@oneofus.la>
;;; Fixed: 2020-05-03 (Vladimir Sedach)
;;; Description:
;;;
;;;     Compiler does not handle non-ASCII symbols correctly when
;;;     READTABLE-CASE is :INVERT
;;;
(test unicode.0001.compiler-unicode-inverted-case
  (let ((test-readtable (copy-readtable)))
    (dolist (case '(:invert :upcase :downcase :preserve))
      (setf (readtable-case test-readtable) case)
      (let ((*readtable* test-readtable))
        (is (= 3
               (funcall
                (compile nil
                         (cons 'lambda
                               (read-from-string "((𝛅 𝛜) (+ 𝛅 𝛜))")))
                1 2)))))))

;;; Date: 2020-05-03
;;; From: Vladimir Sedach <vas@oneofus.la>
;;; Fixed: 2020-05-03 (Vladimir Sedach)
;;; Description:
;;;
;;;     Pathname :common case conversion fails on Unicode pathnames
;;;
(test unicode.0002.pathname-common-unicode
  (is (equal
       "ДАННЫЕ"
       (pathname-name (pathname "/tmp/данные.txt") :case :common))))

;;; Date: 2020-05-03
;;; From: Vladimir Sedach <vas@oneofus.la>
;;; Fixed: 2020-05-03 (Vladimir Sedach)
;;; Description:
;;;
;;;     Symbol names that contain Unicode are not printed correctly
;;;
(test unicode.0003.print-unicode-symbols
  (is (string-equal
       "АБРАКАДАБРА"
       (format nil "~A" (read-from-string "абракадабра")))))

;;; Date: 2020-05-03
;;; From: Vladimir Sedach <vas@oneofus.la>
;;; Fixed: 2020-05-03 (Vladimir Sedach)
;;; Description:
;;;
;;;     unicode.0003.print-unicode-symbols shows up as a compiler
;;;     error for symbol names that contain char codes between 128
;;;     and 255, possibly others
;;;
(test unicode.0004.compile-unicode-symbols
  (with-compiler ("unicode.0004.lsp" :load t)
    "(defun unicode.0004 (x¹ y²)
       (+ x¹ y²))")
  (is (= 7 (unicode.0004 5 2))))

;;; Date: 2021-02-28
;;; From: Marius Gerbershagen
;;; Description:
;;;
;;;     Check that various pathname functions work correctly with
;;;     unicode characters
;;;
(test unicode.0005.pathnames
  ;; ensure-directories-exist
  (multiple-value-bind (pathspec created)
      (ensure-directories-exist #P"рецепты/")
    (is (equal pathspec #P"рецепты/")))
  (let ((files '(#P"рецепты/Spätzle-mit-Soß.txt"
                 #P"рецепты/Kartoffelklöße.txt"
                 #P"рецепты/Thüringer-Klöße.txt")))
    ;; open, truename
    (with-open-file (s (first files) :if-does-not-exist :create :if-exists :supersede :direction :output)
      (is (equal (truename s) (merge-pathnames (first files)))))
    (with-open-file (s (second files) :if-does-not-exist :create :if-exists :supersede :direction :output)
      (is (equal (truename s) (merge-pathnames (second files)))))
    ;; ext:copy-file
    (is (ext:copy-file (second files) (third files)))
    ;; directory
    (is (equal (sort (directory "рецепты/*.txt") #'string< :key #'pathname-name)
               (sort (mapcar #'merge-pathnames files) #'string< :key #'pathname-name)))
    (is (equal (sort (directory "рецепты/*löße.txt") #'string< :key #'pathname-name)
               (mapcar #'merge-pathnames (rest files))))
    ;; probe-file
    (is (equal (mapcar #'probe-file files) (mapcar #'merge-pathnames files)))
    (is (null (probe-file #P"рецепты/Dosenkohl.txt")))
    ;; file-author
    (finishes (mapcar #'file-author files))
    ;; file-write-date
    (finishes (mapcar #'file-write-date files))
    ;; rename-file
    (let ((truename-2nd-file (truename (second files))))
      (multiple-value-bind (default-new-name old-truename new-truename)
          (rename-file (second files) #P"Semmelknödel.txt")
        (is (equal default-new-name #P"рецепты/Semmelknödel.txt"))
        (is (equal old-truename truename-2nd-file))
        (is (equal new-truename (truename #P"рецепты/Semmelknödel.txt")))))
    ;; delete-file
    (is (delete-file #P"рецепты/Semmelknödel.txt"))
    ;; file-error-pathname
    (is (handler-case
            (progn (open #P"рецепты/Semmelknödel.txt" :if-does-not-exist :error :if-exists nil)
                   nil)
          (file-error (f)
            (equal (file-error-pathname f) #P"рецепты/Semmelknödel.txt"))))
    ;; check that we respect ext:*default-external-format*
    #-windows
    (let ((ext:*default-external-format* :pass-through))
      (is (member (ext:string-to-octets "рецепты" :external-format :utf-8)
                  (directory "*/")
                  :test #'equalp
                  :key #'(lambda (p)
                           (map 'vector #'char-code
                                (first (last (pathname-directory p))))))))
    ;; clean up
    (is (delete-file (first files)))
    (is (delete-file (third files)))
    (is (delete-file #P"рецепты/"))))
