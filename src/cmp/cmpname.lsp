;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2007, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPNAME Unambiguous init names for object files
;;;;
;;;; Every object file in a lisp library or combined FASL (such as the
;;;; compiler), needs a function that creates its data and installs the
;;;; functions. This initialization function has a C name which needs
;;;; to be unique. This file has functions to create such names.

(in-package "COMPILER")

(defun encode-number-in-name (number)
  ;; Encode a number in an alphanumeric identifier which is a valid C name.
  (declare (si::c-local))
  (cond ((zerop number) "0")
        ((minusp number) (encode-number-in-name (- number)))
        (t
         (do* ((code "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
               (base (length code))
               (output '())
               (digit 0))
              ((zerop number) (coerce (nreverse output) 'base-string))
           (multiple-value-setq (number digit) (floor number base))
           (push (char code digit) output)))))

(defun unique-init-name (file)
  "Create a unique name for this initialization function. The current algorithm
relies only on the name of the source file and the time at which it is built. This
should be enough to prevent name collisions for object files built in the same
machine."
  (let* ((path (pathname file))
         (path-hash (logxor (ash (sxhash path) 8)
                            (ash (sxhash (cddr (pathname-directory path))) 16)
                            (sxhash (pathname-name path))))
         (seconds (get-universal-time))
         (ms (+ (* seconds 1000)
                (mod (floor (* 1000 (get-internal-real-time))
                            internal-time-units-per-second)
                     1000)))
         (tag (concatenate 'base-string
                           "_ecl"
                           (encode-number-in-name path-hash)
                           "_"
                           (encode-number-in-name ms))))
    tag))

(defun kind->tag (kind)
  (case kind
    ((:object :c)           "@EcLtAg")
    ((:fasl :fas)           "@EcLtAg_fas")
    ((:static-library :lib) "@EcLtAg_lib")
    ((:shared-library :dll) "@EcLtAg_dll")
    ((:program)             "@EcLtAg_exe")
    (otherwise (error "C::BUILDER cannot accept files of kind ~s" kind))))

(defun init-name-tag (init-name &key (kind :object))
  (concatenate 'base-string (kind->tag kind) ":" init-name "@"))

(defun search-tag (stream tag)
  (declare (si::c-local))
  (do* ((eof nil)
        (key (concatenate 'list tag ":"))
        (string key))
       (nil)
    (let ((c (read-byte stream nil nil)))
      (cond ((null c) (return nil))
            ((not (= c (char-code (pop string))))
             (setf string key))
            ((null string)
             (return t))))))

(defun read-name (stream)
  (declare (si::c-local))
  (concatenate 'string
               (loop with c = t
                  until (or (null (setf c (read-byte stream nil nil)))
                            (= c #.(char-code #\@)))
                  collect (code-char c))))

(defun find-init-name (file &key (tag "@EcLtAg"))
  "Search for the initialization function in an object file. Since the
initialization function in object files have more or less unpredictable
names, we store them in a string in the object file. This string is recognized
by the TAG it has at the beginning This function searches that tag and retrieves
the function name it precedes."
  #-pnacl
  (with-open-file (stream file :direction :input :element-type '(unsigned-byte 8))
    (when (search-tag stream tag)
      (let ((name (read-name stream)))
        name)))
  #+pnacl
  (let* ((pnacl-dis
          (or (ext:getenv "PNACL_DIS")
              (error "Please set the PNACL_DIS environment variable to your ~
                      toolchain's pnacl-dis location")))
         (stream (ext:run-program
                  pnacl-dis
                  (list (namestring (translate-logical-pathname file)))
                  :wait nil :input nil :output :stream :error :output)))
    (unless stream
      (error "Unable to disasemble file ~a" file))
    (when (search-tag stream tag)
      (let ((name (read-name stream)))
        name))))

(defun guess-init-name (pathname kind)
  (case kind
    ((:object :c :static-library :lib :shared-library :dll)
     (or (and (probe-file pathname)
              (find-init-name pathname :tag (kind->tag kind)))
         (cmpnote "Cannot find out entry point for binary file ~A" pathname)))
    (otherwise (compute-init-name pathname :kind kind))))

(defun compute-init-name (pathname
                          &key
                            (kind (guess-kind pathname))
                            (prefix nil)
                          &aux
                            (unique-name (unique-init-name pathname)))
  "Computes initialization function name. Libraries, FASLS and
programs init function names can't be randomized to allow
initialization from the C code which wants to use it."
  (case kind
    ((:object :c)
     unique-name)
    ((:fasl :fas)
     (init-function-name "CODE" :kind :fas :prefix prefix))
    ((:static-library :lib)
     (init-function-name unique-name :kind :lib :prefix prefix))
    ((:shared-library :dll)
     (init-function-name unique-name :kind :dll :prefix prefix))
    ((:program)
     (concatenate 'string (or prefix "init_") "ECL_PROGRAM"))
    (otherwise
     (error "C::BUILDER cannot accept files of kind ~s" kind))))

(defun init-function-name (s &key (kind :object) (prefix nil))
  (flet ((translate-char (c)
           (cond ((and (char>= c #\a) (char<= c #\z))
                  (char-upcase c))
                 ((and (char>= c #\A) (char<= c #\Z))
                  c)
                 ((or (eq c #\-) (eq c #\_))
                  #\_)
                 ((eq c #\*)
                  #\x)
                 ((eq c #\?)
                  #\a)
                 ((digit-char-p c)
                  c)
                 (t
                  #\p)))
         (disambiguation (kind)
           (case kind
             ((:object :c) "")
             ((:fasl :fas) "fas_")
             ((:library :static-library :lib) "lib_")
             ((:shared-library :dll) "dll_")
             ((:program) "exe_")
             (otherwise (error "Not a valid argument to INIT-FUNCTION-NAME: kind = ~S"
                               kind)))))
    (setq s (map 'string #'translate-char (string s)))
    (concatenate 'string
                 (or prefix "init_")
                 (disambiguation kind)
                 (map 'string #'translate-char (string s)))))
