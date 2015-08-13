(in-package :cl-test)

;;;;;;;;;;;;;;;;;;;;;
;; Readtable tests ;;
;;;;;;;;;;;;;;;;;;;;;

(symbol-macrolet ((lookup-table
       '(:SYMBOL   ("zebra" "Zebra" "ZEBRA" "zebr\\a" "zebr\\A" "ZEBR\\a" "ZEBR\\A" "Zebr\\a" "Zebr\\A")
         :UPCASE   (|ZEBRA| |ZEBRA| |ZEBRA| |ZEBRa|   |ZEBRA|   |ZEBRa|   |ZEBRA|   |ZEBRa|   |ZEBRA|)
         :DOWNCASE (|zebra| |zebra| |zebra| |zebra|   |zebrA|   |zebra|   |zebrA|   |zebra|   |zebrA|)
         :PRESERVE (|zebra| |Zebra| |ZEBRA| |zebra|   |zebrA|   |ZEBRa|   |ZEBRA|   |Zebra|   |ZebrA|)
         :INVERT   (|ZEBRA| |Zebra| |zebra| |ZEBRa|   |ZEBRA|   |zebra|   |zebrA|   |Zebra|   |ZebrA|))))
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


