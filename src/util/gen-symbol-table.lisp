;;; This file is used to generate symbols_list.h. Go to the last page to see
;;; an ad-hoc parser for the old symbols_list.h from which the current
;;; definitions were created. Keep in mind that it is only a helper to
;;; generate the cl_symbols table in a desired format, some manual curation is
;;; also needed (i.e modification of the structure etc).


;;; Utilities

(defvar *pkg+kind-mapping*
  `(("CL_ORDINARY"             :cl      :ordinary)
    ("CL_SPECIAL"              :cl      :special)
    ("CL_CONSTANT"             :cl      :constant)
    ("CL_FORM"                 :cl      :form)
    ("SI_ORDINARY"             :si      :ordinary)
    ("SI_SPECIAL"              :si      :special)
    ("SI_CONSTANT"             :si      :constant)
    ("EXT_ORDINARY"            :ext     :ordinary)
    ("EXT_SPECIAL"             :ext     :special)
    ("EXT_CONSTANT"            :ext     :constant)
    ("EXT_FORM"                :ext     :form)
    ("MP_ORDINARY"             :mp      :ordinary)
    ("MP_SPECIAL"              :mp      :special)
    ("MP_CONSTANT"             :mp      :constant)
    ("CLOS_ORDINARY"           :clos    :ordinary)
    ("CLOS_SPECIAL"            :clos    :special)
    ("CLOS_ORDINARY | PRIVATE" :clos    :private)
    ("KEYWORD"                 :keyword :constant)
    ("GRAY_ORDINARY"           :gray    :ordinary)
    ("FFI_ORDINARY"            :ffi     :ordinary)
    ("FFI_CONSTANT"            :ffi     :constant)))

(defun pkg+kind->type (pkg kind)
  (car (find (list pkg kind) *pkg+kind-mapping* :key #'cdr :test #'equal)))

(defun type->pkg+kind (type)
  (cdr (find type *pkg+kind-mapping* :key #'car :test #'equal)))

(defun split-sequence (char string &aux acc)
  (do ((line string (subseq line (1+ (position char line)))))
      ((null (position char line))
       (push line acc)
       (nreverse (mapcar (lambda (str)
                           (string-trim '(#\space) str))
                         acc)))
    (push (subseq line 0 (position char line)) acc)))


;;; Parser returns a list of "lines" in a preprocessed form:
;;;
;;;   (line-type ,@arguments) ; or
;;;   (pack name kind value cfun narg)
;;;
;;; LINE-TYPE is either :macro, :comment, :newline, :end-tag or :entry. PACK
;;; is one of core packages (i.e :ffi), NAME is the symbol name etc. CFUN is
;;; the name of the corresponding C function.
;;;
;;; Entry parsers have versions so it is possible to parse and generate older
;;; versions of the table.
;;;
;;; Example usage:
;;;
#+(or)
(let ((symbols-list (parse-symbols-list "path/to/symbols_list.h" #'v1-parse-entry)))
  (generate-symbols-list symbols_list #'v1-generate-entry))

(defun parse-symbols-list (path parser)
  (with-open-file (s path)
    (do ((line (read-line s nil :eof)
               (read-line s nil :eof))
         results)
        ((eq line :eof) (nreverse results))
      (push
       (cond ((zerop (length line))
              `(:newline))
             ((search "}};" line)
              `(:end-tag ,(funcall parser line)))
             ((case (char line 0)
                (#\{       `(:entry ,(funcall parser line)))
                (#\#       `(:macro ,line))
                (otherwise (if (zerop (length line))
                               `(:newline)
                               `(:comment ,line))))))
       results))))

(defun generate-symbols-list (results generator)
  (loop for line in results
        do (case (first line)
             (:macro    (format t "~a~%" (second line)))
             (:comment  (format t "~a~%" (second line)))
             (:newline  (terpri))
             (:end-tag  (format t "~a};~%" (apply generator (second line))))
             (:entry    (format t "~a,~%"  (apply generator (second line)))))))

(defun v1-parse-entry (line)
  (destructuring-bind (name package+kind cfun narg value)
      (split-sequence #\, (subseq line
                                  (1+ (position #\{ line))
                                  (position #\} line)))
    (let* ((pkg-kind (type->pkg+kind package+kind))
           (pack (first pkg-kind))
           (kind (second pkg-kind))
           (narg (parse-integer narg)))
      (list pack name kind value cfun narg))))

(defun v1-generate-entry (package name kind value cfun narg)
  (let ((type (pkg+kind->type package kind))
        (narg (or narg -1)))
    (format nil "{~a, ~a, ~a, ~d, ~a}"
            name type cfun narg value)))

;;; V2 is defined to unify symbols_list.h files between ECL core and DPP. Also
;;; it gives the all_symbols.d module an insight for the predefined symbol
;;; name mappings (see #534).
(defun v2-parse-entry (line)
  (let* ((fun-pos (search "ECL_FUN" line))
         (var-pos (search "ECL_VAR" line))
         (end-pos (position #\) line :from-end t))
         (name (subseq line 1 (1- fun-pos)))
         (fun-arg (split-sequence #\, (subseq line
                                              (+ fun-pos 8)
                                              (- var-pos 2))))
         (var-arg (split-sequence #\, (subseq line
                                              (+ var-pos 8)
                                              end-pos))))
    (destructuring-bind (fun-name cfun narg) fun-arg
      (declare (ignore fun-name))
      (destructuring-bind (type value) var-arg
        (let* ((pkg-kind (type->pkg+kind type))
               (pack (first pkg-kind))
               (kind (second pkg-kind))
               (narg (parse-integer narg)))
          (list pack name kind value cfun narg))))))

(defun v2-generate-entry (package name kind value cfun narg)
  (let ((type (pkg+kind->type package kind))
        (narg (or narg -1))
        (fun-name (let* ((start (position #\( cfun :from-end t))
                         (end (position #\) cfun))
                         ;; Find the "innermost" symbol, i.e for
                         ;; IF_DFFI(ECL_NAME(foobar)) it is foobar
                         (fname (if start
                                    (subseq cfun
                                            (1+ start)
                                            end)
                                    cfun)))
                    ;; If it is NULL, we want to specify NULL, otherwise we
                    ;; wrap the symbol in quotes to have a string.
                    (if (string= fname "NULL")
                        "NULL"
                        (format nil "~s" fname)))))
    (format nil "{~a ECL_FUN(~a, ~a, ~a) ECL_VAR(~a, ~a)}"
            name fun-name cfun narg type value)))
