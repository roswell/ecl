(in-package :cl-test)



;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.* Readtable tests ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(symbol-macrolet ((lookup-table
                   '(:SYMBOL   ("zebra" "Zebra" "ZEBRA" "zebr\\a" "zebr\\A"
                                "ZEBR\\a""ZEBR\\A" "Zebr\\a" "Zebr\\A")
                     :UPCASE   (|ZEBRA| |ZEBRA| |ZEBRA| |ZEBRa|
                                |ZEBRA| |ZEBRa| |ZEBRA| |ZEBRa| |ZEBRA|)
                     :DOWNCASE (|zebra| |zebra| |zebra| |zebra|
                                |zebrA| |zebra| |zebrA| |zebra| |zebrA|)
                     :PRESERVE (|zebra| |Zebra| |ZEBRA| |zebra|
                                |zebrA| |ZEBRa| |ZEBRA| |Zebra| |ZebrA|)
                     :INVERT   (|ZEBRA| |Zebra| |zebra| |ZEBRa|
                                |ZEBRA| |zebra| |zebrA| |Zebra| |ZebrA|))))
  (macrolet
      ((def-readtable-case-test (reader-case)
         `(deftest ,(concatenate 'string "TEST-ANSI.READTABLE.CASE-"
                                 (symbol-name reader-case))
              (let ((*readtable* (copy-readtable)))
                (setf (readtable-case *readtable*) ,reader-case)
                (mapcar #'(lambda (x)
                            (read-from-string x))
                        ',(getf lookup-table :symbol)))
            ,(getf lookup-table reader-case))))
    (def-readtable-case-test :upcase)
    (def-readtable-case-test :downcase)
    (def-readtable-case-test :preserve)
    (def-readtable-case-test :invert)))

;; when readtable was :invert characters got inverted too
(deftest test-ansi.readtable.invert-char
    (let ((*readtable* (copy-readtable)))
      (setf (readtable-case *readtable*) :invert)
      (read-from-string "#\\a"))
  #\a 3)



;; HyperSpec – 3.*

;;;;;;;;;;;;;;;;;;;
;; Deftype tests ;;
;;;;;;;;;;;;;;;;;;;

(deftest test-ansi.deftype.ordinary.1
    (progn
      (deftype ordinary1 () `(member nil t))
      (values (typep T  'ordinary1)
              (typep :a 'ordinary1)))
  T NIL)

(deftest test-ansi.deftype.ordinary.2
    (progn
      (deftype ordinary2 (a b)
        (if a
            'CONS
            `(INTEGER 0 ,b)))
      (values (typep T        '(ordinary2 nil 3))
              (typep 3        '(ordinary2 nil 4))
              (typep T        '(ordinary2 T nil))
              (typep '(1 . 2) '(ordinary2 T nil))))
  nil t nil t)

(deftest test-ansi.deftype.optional
    (progn
      (deftype optional (a &optional b)
        (if a
            'CONS
            `(INTEGER 0 ,b)))
      (values (typep 5        '(optional nil))
              (typep 5        '(optional nil 4))))
  t nil)

(deftest test-ansi.deftype.nested
    (progn
      (deftype nested ((a &optional b) c . d)
        (assert (listp d))
        `(member ,a ,b ,c))
      (values
       (typep  1 '(nested (1 2) 3 4 5 6))
       (typep  1 '(nested (2 2) 3 4 5 6))
       (typep '* '(nested (3)   3))
       (typep  3 '(nested (2)   3))))
  t nil t t)



;;;;;;;;;;;;;;;;;;;;;;;;;
;; 19.* Pathname tests ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Issue #103 ;; logical-pathname-translations not translating
;; https://gitlab.com/embeddable-common-lisp/ecl/issues/103
(deftest* test-ansi.pathname.wildcards.1
    (namestring
     (progn
       (setf (logical-pathname-translations "prog")
             '(("CODE;*.*.*" "/tmp/prog/")))
       (translate-logical-pathname "prog:code;documentation.lisp")))
  (list (namestring #P"/tmp/prog/documentation.lisp")))



;;;;;;;;;;;;;;;;;;;;;;;
;; 23.* Reader tests ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defstruct sharp-s-reader.1.example-struct a)

(deftest test-ansi.reader.sharp-s-reader.1
    (prog1
        (signals-error
         (read-from-string
          "(#1=\"Hello\" #S(sharp-s-reader.1.example-struct :A #1#))")
         program-error))
  nil)


