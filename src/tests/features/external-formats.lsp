;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Sat Jan 03 2:56:03 CEST 2007
;;;; Contains: External format tests
;;;;
;;;; Based on the code and files from FLEXI-STREAMS 1.0.7
;;;;

(in-package :cl-test)

(suite 'features/eformat)


;;; eformat-001

(defconstant +buffer-size+ 8192
  "Size of buffers for COPY-STREAM* below.")

(defparameter *copy-function* 'copy-stream
  "Which function to use when copying from one stream to the other -
see for example COPY-FILE below.")

(defparameter *eformat-test-files*
  '(("unicode_demo" (:utf8 :ucs2 :ucs4))
    ("kafka" (:utf8 :latin1 :cp1252))
    ("hebrew" (:utf8 :latin8))
    ("russian" (:utf8 :koi8r))
    ("tilton" (:utf8 :ascii))
    )
  "A list of test files where each entry consists of the name
prefix and a list of encodings.")

(defun create-file-variants (file-name symbol)
  "For a name suffix FILE-NAME and a symbol SYMBOL denoting an
encoding returns a list of pairs where the car is a full file
name and the cdr is the corresponding external format.  This list
contains all possible variants w.r.t. to line-end conversion and
endianness."
  (let ((variants (ecase symbol
                    (:ascii '(:us-ascii))
                    (:latin1 '(:latin-1))
                    (:latin8 '(:iso-8859-8))
                    (:cp1252 '(:windows-cp1252))
                    (:koi8r '(:koi8-r))
                    (:utf8 '(:utf-8))
                    (:ucs2 '(:ucs-2be :ucs-2le))
                    (:ucs4 '(:ucs-4be :ucs-4le)))))
    (loop for arg in variants
          nconc (let* ((endian-suffix (case arg
                                        ((:ucs-2be :ucs-4be) "_be")
                                        ((:ucs-2le :ucs-4le) "_le")
                                        (t ""))))
                  (loop for eol-style in '(:lf :cr :crlf)
                     collect (cons (format nil "~A_~(~A~)_~(~A~)~A.txt"
                                           file-name symbol eol-style endian-suffix)
                                   (list eol-style arg)))))))

(defun create-test-combinations (file-name symbols &optional simplep)
  "For a name suffix FILE-NAME and a list of symbols SYMBOLS denoting
different encodings of the corresponding file returns a list of lists
which can be used as arglists by DO-EFORMAT-TEST-001.  If SIMPLEP is true, a
list which can be used for the string and sequence tests below is
returned."
  (let ((file-variants (loop for symbol in symbols
                             nconc (create-file-variants file-name symbol))))
    (loop for (name-in . external-format-in) in file-variants
          when simplep
          collect (list name-in external-format-in)
          else
          nconc (loop for (name-out . external-format-out) in file-variants
                      collect (list name-in external-format-in name-out external-format-out)))))

(defun file-equal (file1 file2)
  "Returns a true value iff FILE1 and FILE2 have the same
contents \(viewed as binary files)."
  (with-open-file (stream1 file1 :element-type '(unsigned-byte 8))
    (with-open-file (stream2 file2 :element-type '(unsigned-byte 8))
      (if (= (file-length stream1) (file-length stream2))
          (loop for p1 = (file-position stream1)
             for byte1 = (read-byte stream1 nil nil)
             for byte2 = (read-byte stream2 nil nil)
             while (and byte1 byte2)
             unless (= byte1 byte2)
             do (return (values nil p1))
             finally (return (values t 0)))
          (values nil -1)))))

(defun copy-stream (in out)
  "Copies the contents of the binary stream STREAM-IN to the
binary stream STREAM-OUT using flexi streams - STREAM-IN is read
with the external format EXTERNAL-FORMAT-IN and STREAM-OUT is
written with EXTERNAL-FORMAT-OUT."
  (loop for line = (read-line in nil nil)
     while line
     do (write-line line out)))

(defun copy-stream* (in out)
  "Like COPY-STREAM, but uses READ-SEQUENCE and WRITE-SEQUENCE instead
of READ-LINE and WRITE-LINE."
  (let ((buffer (make-array +buffer-size+ :element-type 'char*)))
    (loop
     (let ((position (read-sequence buffer in)))
       (when (zerop position) (return))
       (write-sequence buffer out :end position)))))

(defun do-eformat-test-001 (*copy-function*)
  "Each test in this suite copies the contents of one file \(in the
`test' directory) to another file \(in a temporary directory) using
flexi streams with different external formats.  The resulting file is
compared with an existing file in the `test' directory to check if the
outcome is as expected.  Uses various variants of the :DIRECTION
keyword when opening the files.

Returns a true value iff all tests succeeded.  Prints information
about each individual comparison if VERBOSE is true."
  (labels
      ((copy-file (path-in external-format-in path-out external-format-out
                           direction-out direction-in)
         (with-open-file (in path-in
                             :element-type 'character
                             :direction direction-in
                             :if-does-not-exist :error
                             :if-exists :overwrite
                             :external-format external-format-in)
           (with-open-file (out path-out
                                :element-type 'character
                                :direction direction-out
                                :if-does-not-exist :create
                                        :if-exists :supersede
                                        :external-format external-format-out)
                     (funcall *copy-function* in out))))
       (one-comparison (path-in external-format-in path-out external-format-out)
         (loop with full-path-in = (merge-pathnames path-in "features/eformat-tests/")
            and full-path-out = (ensure-directories-exist
                                 (merge-pathnames path-out "sandbox/eformat-tmp/"))
            and full-path-orig = (merge-pathnames path-out "features/eformat-tests/")
            for direction-out in '(:output :io)
            nconc (loop for direction-in in '(:input :io)
                       for args = (list path-in external-format-in direction-in
                                        path-out external-format-out direction-out)
                     with ok = nil
                     with pos = 0
                     unless (progn
                              (copy-file full-path-in external-format-in
                                         full-path-out external-format-out
                                         direction-out direction-in)
                              (is (multiple-value-setq (ok pos)
                                    (file-equal full-path-out full-path-orig))
                                  "~%~A -> ~A" path-in path-out))
                     collect (progn
                                 (format t "~%;;; Discordance at pos ~D~%between ~A~% and ~A~%"
                                         pos full-path-out full-path-orig)
                                 args)))))
    (loop with do-eformat-test-001-args-list =
         (loop for (file-name symbols) in *eformat-test-files*
            nconc (create-test-combinations file-name symbols))
       for (path-in external-format-in path-out external-format-out) in do-eformat-test-001-args-list
       nconc (one-comparison path-in external-format-in path-out external-format-out))))

;;; Date: 02/01/2007
;;; From: Juanjo
;;; Fixed: Not a bug
;;; Description:
;;;
;;;     Test external formats by transcoding several files into all possible
;;;     supported formats and checking against the expected results. This
;;;     test uses READ/WRITE-CHAR via READ/WRITE-LINE.
;;;
(test external-format.0001-transcode-read-char
  (is-false (do-eformat-test-001 'copy-stream)))

;;; Date: 02/01/2007
;;; From: Juanjo
;;; Fixed: Not a bug
;;; Description:
;;;
;;;     Test external formats by transcoding several files into all possible
;;;     supported formats and checking against the expected results. This
;;;     test uses READ/WRITE-CHAR via READ/WRITE-LINE.
;;;
(test external-format.0002-transcode-read-char
  (is-false (do-eformat-test-001 'copy-stream*)))


;;; eformat-002

(load "sys:encodings;tools")

(setf *print-circle* t) ; some mappings contain circular structures

(defun binary-dump (filename &optional (position 0) (limit nil))
  (format t "~%FILE: ~A from ~D, ~D bytes" filename position limit)
  (with-open-file (file filename :element-type '(unsigned-byte 8))
    (file-position file position)
    (loop for i from 0
       for byte = (read-byte file nil nil)
       for c = (and byte (code-char byte))
       while (and byte (or (null limit) (< i limit)))
       do (progn (when (zerop (mod i 8)) (terpri))
                 (format t "~5X ~3A" byte
                         (cond ((and (< 31 byte 127) (standard-char-p c))
                                c)
                               ((eql c #\Esc) "ESC")
                               (t " ")))
                 )))
  (terpri)
  (force-output))

(defun random-strings (char-bag n)
  (if (consp char-bag)
      (apply #'concatenate 'string
             (loop for i from 0 below 2
                for actual-bag = (elt char-bag (random (length char-bag)))
                collect (random-strings actual-bag (random n))))
      (concatenate 'string
                   (loop for i from 0 to n
                      for c = (char char-bag (random (length char-bag)))
                      unless (eql c #\Newline)
                      collect c))))

(defun compare-files (a b &optional all-chars)
  (with-open-file (sa a :direction :input :element-type '(unsigned-byte 8))
    (with-open-file (sb b :direction :input :element-type '(unsigned-byte 8))
      (loop for b1 = (read-byte sa nil nil)
         for b2 = (read-byte sb nil nil)
         while (or b1 b2)
         do (unless (eql b1 b2)
              (let* ((position (1- (file-position sa)))
                     (start-dump (max 0 (- position 8))))
                (setf position (logandc2 position 3))
                (binary-dump a start-dump 32)
                (binary-dump b start-dump 32)
                (format t "~%Mismatch between~%~T~A~% and~T~A~% at file position ~D~%"
                        a b position)
                (when all-chars
                  (loop with imin = (floor start-dump 4)
                     with imax = (min (+ imin 9) (length all-chars))
                     for i from imin below imax
                     for j from 0
                     for c = (char all-chars i)
                     do (progn (when (zerop (mod j 8)) (terpri))
                               (format t "~4X " (char-code c))))
                  (terpri))
                (return nil)))
         finally (return t)))))

(defun test-output (format-name &optional iconv-name (nlines 128) (nchars 10))
  (set 'ext::foo format-name)
  (let* ((*print-circle* t)
         (mappings (loop for table = (ext::make-encoding format-name)
                      while (and table (symbolp table))
                      do (setf format-name table)
                      finally (return (or table format-name))))
         (char-bags (all-valid-unicode-chars mappings))
         (encoded-filename (format nil "sandbox/eformat-tmp/iconv-~A.txt" format-name))
         (decoded-filename (format nil "sandbox/eformat-tmp/iconv-~A-utf32.txt" format-name))
         (iconv-filename (format nil "sandbox/eformat-tmp/iconv-~A-iconv-utf32.txt" format-name))
         (random-lines (loop for line from 1 to nlines
                          collect (random-strings char-bags nchars)))
         (all-chars (apply #'concatenate 'string
                           (loop for i in random-lines
                              nconc (list i (list #\Newline))))))
    (ensure-directories-exist encoded-filename)
    ;; Output in that format
    (with-open-file (out encoded-filename :direction :output :external-format format-name
                         :if-exists :supersede)
      (loop for i in random-lines
         do (write-line i out)))
    (with-open-file (out decoded-filename :direction :output :external-format :ucs-4be
                         :if-exists :supersede)
      (loop for i in random-lines
         do (write-line i out)))
    (with-open-file (in encoded-filename :direction :input :external-format format-name)
      (loop for line = (read-line in nil nil)
         for i in random-lines
         for n from 1
         while line
         unless (string= i line)
         do (progn
              (format t "Mismatch on line ~D between~% ~S and~% ~S" n line i)
              (return-from test-output nil))))     
    (when iconv-name
      (let ((command (format nil "iconv -f ~A -t UTF-32BE ~A > ~A"
                             iconv-name encoded-filename iconv-filename)))
        (if (zerop
             (si::system command))
            (compare-files decoded-filename iconv-filename all-chars)
            (prog1 T
              (format t "~&;;; iconv command failed:~A~%" command)))))))

;;; Date: 09/01/2007
;;; From: Juanjo
;;; Fixed: Not a bug
;;; Description:
;;;
;;;     Test external formats by transcoding random sequences of characters using
;;;     ECL and iconv.
;;;
#-msvc
;; In Windows SYSTEM does not fail with a nonzero code when it
;; fails to execute a command. Hence in that case we assume
;; we simply can not run these tests
(when (zerop (si::system "iconv -l >/dev/null 2>&1"))
  (test external-format.simple-iconv-check
    (is-false
     (loop for name in '(:ISO-8859-1 :ISO-8859-2 :ISO-8859-3 :ISO-8859-4
                         :ISO-8859-5 :ISO-8859-6 :ISO-8859-7 :ISO-8859-8
                         :ISO-8859-9 :ISO-8859-10 :ISO-8859-11 :ISO-8859-13
                         :ISO-8859-14 :ISO-8859-15 :ISO-8859-16

                         :KOI8-R :KOI8-U

                         :IBM437 :IBM850 :IBM852 :IBM855 :IBM857 :IBM860
                         :IBM861 :IBM862 :IBM863 :IBM864 :IBM865 :IBM866
                         :IBM869

                         :CP936 :CP949 :CP950

                         :WINDOWS-1250 :WINDOWS-1251 :WINDOWS-1252 :WINDOWS-1253
                         :WINDOWS-1254 :WINDOWS-1256 :WINDOWS-1257

                         ;; :CP932 :WINDOWS-1255 :WINDOWS-1258 with
                         ;; iconv may output combined characters, when ECL would
                         ;; output the base and the comibining one. Hence, no simple
                         ;; comparison is possible.

                         :ISO-2022-JP
                         ;; :ISO-2022-JP-1
                         ;; iconv doesn't support ISO-2022-JP-1 (hue hue hue)
                         )
        unless (progn
                 (loop for i from 1 to 10
                    always (is (test-output name (symbol-name name))
                               "iconv test ~s failed" name)))
        collect name))))


