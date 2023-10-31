;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;;
;;;; CMPPASS2-BIR -- BIR->CXX code generator. This code generator is a rewrite of
;;;; the old one that tranpsiled AST->CXX.

(in-package "COMPILER")

(defun compiler-pass/generate-cxx*
    (module c-pathname h-pathname data-pathname init-name source)
  (with-cxx-env ()
    (optimize-cxx-data *referenced-objects*)
    (setq *compiler-phase* 't2)
    (with-open-file (*compiler-output1* c-pathname :direction :output
                                                   :if-does-not-exist :create
                                                   :if-exists :supersede)
      (with-open-file (*compiler-output2* h-pathname :direction :output
                                                     :if-does-not-exist :create
                                                     :if-exists :supersede)
        (wt-print-header source)
        (transpile-module module init-name c-pathname h-pathname data-pathname)
        (terpri *compiler-output1*)
        (terpri *compiler-output2*)))
    (data-c-dump data-pathname)))

(defvar *cxx-pprint-dispatch* (copy-pprint-dispatch))

(defmacro define-cxx-pprint ((object type-specifier)
                             (&key (priority 0)
                                   (table '*cxx-pprint-dispatch*)
                                   (stream '*standard-output*))
                             &body body)
  `(set-pprint-dispatch ',type-specifier
                        (lambda (,stream ,object) ,@body)
                        ,priority ,table))

(defun format-iblocks (enter)
  (let ((visited (make-hash-table)))
    (labels ((iblock-index (i)
               (gethash i visited))
             (count-iblock (i)
               (unless (gethash i visited)
                 (setf (gethash i visited)
                       (hash-table-count visited))))
             (print-iblock (i)
               (cond ((not (emptyp (iblock-outputs i)))
                      (wt-comment-nl "~2,'0d: ~20a -> ~{~2,'0d~^ ~}"
                                     (gethash i visited)
                                     (iblock-name i)
                                     (map 'list #'iblock-index (iblock-outputs i))))
                     ((not (null (iblock-unwind i)))
                      (wt-comment-nl "~2,'0d: ~20a >> ~2,'0d"
                                     (gethash i visited)
                                     (iblock-name i)
                                     (iblock-index (iblock-unwind i))))
                     (t
                      (wt-comment-nl "~2,'0d: ~20a" (gethash i visited) i)))
               (b2expr i)))
      (map-iblocks #'count-iblock enter)
      (map-iblocks #'print-iblock enter))))

(define-cxx-pprint (module module) ()
  (wt-comment-nl (format nil "~74,,,'-a" ""))
  (wt-comment-nl "Constants: ~s" (length (constants module)))
  (wt-comment-nl "Functions: ~s" (length (functions module)))
  (wt-comment-nl "Top-level: ~s" (top-level module))
  (wt-nl)
  (wt-nl1 "#include " *cmpinclude*)
  (wt-nl)
  (dolist (fun (functions module))
    (wt-comment-nl (format nil "~a" (fun-name (bir-parent fun))))
    (wt-nl "void lfun(...)")
    (wt-nl-open-brace)
    (format-iblocks (bir-enter fun))
    (wt-nl-close-brace)
    (wt-nl))
  (wt-nl "void lambda(...)")
  (wt-nl-open-brace)
  (format-iblocks (bir-enter (top-level module)))
  (wt-nl-close-brace))

(defun format-bir (bir &optional (*compiler-output1* *standard-output*)) ()
  (wt-nl "void lambda(...)")
  (wt-nl-open-brace)
  (format-iblocks (bir-enter bir))
  (wt-nl-close-brace))

(defun b2expr (iblock)
  (b2instr (iblock-instructions iblock)))

(defun emit-c1form (opcode)
  (case (c1form-name opcode)
    ;; (location (wt-nl "#<loc> = ") (wt-loc (c1form-arg 0 opcode)) (wt ";"))
    ;; (variable (wt-nl "#<var> = ") (wt-var (c1form-arg 0 opcode)) (wt ";"))
    ;; (variable (wt-nl "#<var> = ") (wt "...") (wt ";"))
    ;; (cl:setq  (wt-nl "#<var> <- ...;"))
    ;; (fcall    (wt-nl "#<fun>(#<arg>, #<arg>, ...);"))
    ;; (si:fset  (wt-nl "#<fun> = ...;"))
    ;; (fmla-not (wt-nl "#<test> = (not ...);"))
    ;; (fmla-and (wt-nl "#<test> = (and ...);"))
    ;; (fmla-or  (wt-nl "#<test> = (or ...);"))
    ;; (cl:if    (wt-nl "if (#<test>)"))
    ;; (cl:go    (wt-nl "goto " (format nil "~a" (tag-name (c1form-arg 0 opcode))) ";"))
    (otherwise
     (wt-nl "unknown " (format nil "~a" (c1form-name opcode)) ";"))))

(defun emit-other (opcode)
  (wt-comment-nl (format nil "    ~a" opcode) ";"))

(defun emit-opcode (opcode)
  (typecase opcode
    (c1form (emit-c1form opcode))
    (otherwise (emit-other opcode))))

(defun b2instr (instructions)
  (loop for instr across instructions
        for opcode = (instruction-opcode instr) do
          (emit-opcode opcode)))

;;; Default pretty printers output comments.

#+ (or)
(define-cxx-pprint (i iblock) ()
  ;(wt-comment-nl "this is iblock")
  ;(map nil #'print (iblock-instructions i))
  )

;; (define-cxx-pprint (instr instruction) ()
;;   (ext:if-let ((output (instruction-output instr)))
;;     (wt-comment-nl "    ~24a -> ~a" instr output)
;;     (wt-comment-nl "    ~a" instr)))

(defun transpile-module (module init-name c-pathname h-pathname data-pathname)
  (let ((*print-pprint-dispatch* *cxx-pprint-dispatch*))
    (pprint module *compiler-output1*)))


