#-ecl(in-package "CL-USER")
#+ecl(in-package "COMPILER")

;;; C1FORM is a single node in the AST. These definitions mimic ECL.
#-ecl
(progn
  (defun cmpprogress (fmt &rest args)
    (format *debug-io* fmt args))
  
  (defclass c1form ()
    ((name :initarg :name :accessor c1form-name)
     (args :initarg :args :accessor c1form-args)
     (denv :initarg :denv :accessor c1form-env)))

  (defmethod print-object ((object c1form) stream)
    (print-unreadable-object (object stream :type nil :identity nil)
      (format stream "FORM ~s" (c1form-name object))))

  (defun make-c1form (name &rest args)
    (make-instance 'c1form :name name :args args))

;;; TAG instances are TAGBODY targets.
  (defclass ref ()
    ((name :initarg :name :reader ref-name)))

  (defmethod print-object ((object ref) stream)
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~s" (ref-name object))))

  (defclass tag (ref)
    ((name :initarg :name :reader tag-name)))

  (defun tag-p (tag?)
    (typep tag? 'tag))

  (defun make-tag (name)
    (make-instance 'tag :name name))

  (defclass blk (ref)
    ((name :initarg :name :reader blk-name)))

  (defun blk-p (blk?)
    (typep blk? 'blk))

  (defun make-blk (name)
    (make-instance 'blk :name name)))

;;; Each block has an associated dynamic environment. When two blocks have a
;;; different dynamic environment then we cross the boundary that may involve
;;; additional computations (i.e unwind-protect cleanup forms).
;;;
;;; In principle we have no need to create separate dynamic environments for
;;; tagbody and block (they could be stored for the whole function) because
;;; block and tag variables are resolved on the ast level and are unique.
;;;
;;; Cleavir uses hash tables but alists seem to be much more natural choice.
;;; On the other hand a hash-table and "parent" provide a clear delimiter
;;; between nested dynamic environments. For now let's go with a simple
;;; protocol to hide these details. ECL currently does not have an explicit
;;; dynamic environment at runtime.
#+ (or)
(progn
  (defclass dynamic-environment ()
    ((parent :initarg :parent :reader denv-parent)
     (bindings :initform (make-hash-table) :reader denv-binds)))

  (defun get-binding (key denv)
    (when (typep denv 'iblock)
      (setf denv (dynamic-environment denv)))
    (multiple-value-bind (value foundp) (gethash key (denv-binds denv))
      (when foundp
        (return-from get-binding value))
      (let ((parent (denv-parent denv)))
        (if parent
            (get-binding key parent)
            (error "Binding ~s not found." key)))))

  (defun set-binding (new-value key denv)
    (when (typep denv 'iblock)
      (setf denv (dynamic-environment denv)))
    (multiple-value-bind (value foundp) (gethash key (denv-binds denv))
      (declare (ignore value))
      (when foundp
        (error "Binding ~s already exists." key))
      (setf (gethash key (denv-binds denv)) new-value)))

  (defun make-dynamic-environment (parent)
    (make-instance 'dynamic-environment :parent parent)))

;#+ (or)
(progn
  ;; CAR stores variables, blocks and such.
  ;; CDR stores functions, macros and such.
  ;;
  ;; These (simplified) definitions are compatible with ECL.
  (defmacro denv-variables (denv) `(car ,denv))
  (defmacro denv-functions (denv) `(cdr ,denv))
  
  (defun get-binding (key denv)
    (cdr (assoc key (denv-variables denv))))

  (defun set-binding (value key denv)
    (push (cons key value)
          (denv-variables denv))
    denv))

;;; Helper functions:

(define-bir-method ordinary (form bir) (c1form)
  (bir-from-c1form c1form bir))

(define-bir-method call (form bir) (&rest args)
  (add-instruction `(:call ,@args) bir))

(define-bir-method progn (form bir) (body)
  (loop while (bir-trail bir)
        for form in body
        do (bir-from-c1form form bir)))

(define-bir-method if (form bir) (fmla-c1form true-c1form false-c1form)
  (bir-from-c1form fmla-c1form bir)
  (let ((entry (bir-trail bir))
        (denv (dynamic-environment bir)))
    (when entry
      (flet ((go-path (name form)
               (bir-insert (make-iblock name denv) bir)
               (bir-from-c1form form bir)
               (prog1 (bir-trail bir)
                 (bir-return entry bir))))
        (let ((a-returns (go-path :if-true true-c1form))
              (b-returns (go-path :if-false false-c1form)))
          (if (and a-returns b-returns)
              (let ((join (make-iblock :if-join bir)))
                ;; Connect "true"
                (bir-return a-returns bir)
                (bir-insert join bir)
                ;; Connect "false"
                (bir-return b-returns bir)
                (bir-insert join bir))
              (bir-return (or a-returns b-returns) bir)))))))

(define-bir-method block (form bir) (blk-var progn-c1form)
  (let* ((old-env (dynamic-environment bir))
         (new-env (make-dynamic-environment old-env))
         (enter (make-iblock `(:block-enter ,(blk-name blk-var)) new-env))
         (leave (make-iblock `(:block-leave ,(blk-name blk-var)) old-env)))
    (set-binding leave blk-var new-env)
    (bir-insert enter bir)
    (bir-from-c1form progn-c1form bir)
    (bir-insert leave bir)))

(define-bir-method return-from (form bir) (blk-var nonlocal value-form)
  (bir-from-c1form value-form bir)
  ;; FIXME nonlocal
  (if nonlocal
      (bir-escape (make-iblock `(:ecl-unwind "nonlocal") bir) bir)
      (bir-escape (get-binding blk-var (dynamic-environment bir)) bir)))

;;; FIXME we assume only local gotos.
(define-bir-method tagbody (form bir) (tag-var tag-body)
  (declare (ignore tag-var))
  ;; First we need to process tags so the iblock may be found in the dynamic
  ;; environment even if GO is called /before/ the tag occurs. For now we
  ;; fudge it.
  (let* ((old-env (dynamic-environment bir))
         (new-env (make-dynamic-environment old-env))
         (enter (make-iblock :tagbody-enter new-env)))
    (bir-insert enter bir)
    (dolist (form tag-body)
      (when (tag-p form)
        (set-binding (make-iblock `(:label ,(tag-name form)) new-env) form new-env)))
    (dolist (form tag-body)
      (if (tag-p form)
          (bir-insert (get-binding form new-env) bir)
          (bir-from-c1form form bir)))
    (when (bir-trail bir)
      (let ((leave (make-iblock :tagbody-leave old-env)))
        (bir-insert leave bir)))))

(define-bir-method go (form bir) (tag-var nonlocal)
  (if nonlocal
      (bir-escape (make-iblock `(:ecl-unwind "nonlocal") bir) bir)
      (bir-escape (get-binding tag-var (dynamic-environment bir)) bir)))

;;; {
;;;     volatile bool unwinding = FALSE;
;;;     cl_index var0_sp = ecl-stack-index(cl_env_copy),
;;;              var1_nargs;
;;;     ecl_frame_ptr next_fr;
;;;     /* Here we compile the form which is protected.
;;;        When this form is aborted, it takes the first
;;;        branch, otherwise it takes the second. */
;;;     ecl_frs_push(cl_env_copy, ECL_PROTECT_TAG);
;;;     if (__ecl_frs_push_result) {
;;;         unwinding = TRUE;
;;;         next_fr = cl_env_copy->nlj_fr;
;;;     } else {
;;;         /* bind *unwind-exit* and *destination*, then
;;;            compile the protected form ... */
;;;     }
;;;     /* Aborted or not, we end up here. When unwinding is
;;;        TRUE, then we need to call ecl_unwind to ensure that
;;;        we jump to the next catch point. Otherwise continue
;;;        as if nothing has happened! */
;;;     ecl_frs_pop(cl_env_copy);
;;;     /* Here we save the values of the form before executing
;;;        the cleanup code so we can restore it later. */
;;;     var1_nargs = ecl_stack_push_values(cl_env_copy);
;;;     { /* run cleanup code */ }
;;;     ecl_stack_pop_values(cl_env_copy, var1_nargs
;;;     /* if the protected form was aborted, jump to the next
;;;        catch point... */
;;;     if (unwinding) ecl_unwind(cl_env_copy, next_fr);
;;;     /* otherwise simply return values */
;;;     bds_unwind_whatnot();
;;;     return cl_env_copy->values[0];
;;; }

;;; [enter ???]  ---> [unwind-enter] --> [protected form] --+
;;; [return ???] <-+- [unwind-join]  <-- [cleanup form] <---+
;;; [escape ???] <-+

(define-bir-method unwind-protect (form bir) (protected cleanup)
  ;;(add-instruction '(:call (print "UNWIND-PROTECT START")) bir)
  (let* ((old-env (dynamic-environment bir))
         (new-env (make-dynamic-environment old-env)))
    (bir-insert (make-iblock :unwind-protect-enter new-env) bir)
    ;; register cleanup form (somehow!)
    (bir-from-c1form protected bir)
    (let* ((join (make-iblock :unwind-protect-cleanup new-env)))
      (bir-insert join bir)
      (dolist (form-1 cleanup)
        (bir-from-c1form form-1 bir))
      (bir-escape (make-iblock :unwind-protect-escape new-env) bir)
      (bir-return join bir)
      (bir-insert (make-iblock :unwind-protect-return old-env) bir))))


(defvar *trash*
  (make-hash-table :test #'equalp))

(defun make-fake-ast (form)
  (when (atom form)
    (return-from make-fake-ast form))
  (case (first form)
    (cl:progn
      (make-c1form 'progn (mapcar #'make-fake-ast (rest form))))
    (cl:if
     (destructuring-bind (test p1 &optional p2) (rest form)
       (make-c1form 'if
                    (make-fake-ast test)
                    (make-fake-ast p1)
                    (make-fake-ast p2))))
    (cl:block
        (destructuring-bind (name &body body) (rest form)
          (make-c1form 'block
                       (climi::ensure-gethash (cons :block name) *trash*
                         (make-blk name))
                       (make-fake-ast `(progn ,@body)))))
    (cl:return-from
     (destructuring-bind (name &optional result) (rest form)
       (make-c1form 'return-from
                    (climi::ensure-gethash (cons :block name) *trash*
                      (make-blk name))
                    t
                    (make-fake-ast result))))
    (cl:tagbody
       (make-c1form 'tagbody
                    :tag-var
                    (loop for form in (rest form)
                          collect (if (consp form)
                                      (make-fake-ast form)
                                      (climi::ensure-gethash (cons :tag form) *trash*
                                        (make-tag form))))))
    (cl:go
     (destructuring-bind (tag) (rest form)
       (make-c1form 'go (climi::ensure-gethash (cons :tag tag) *trash*
                          (make-tag form))
                    nil)))
    (cl:unwind-protect
         (destructuring-bind (protected &body cleanup) (rest form)
           (make-c1form 'unwind-protect
                        (make-fake-ast protected)
                        (mapcar #'make-fake-ast cleanup))))
    (ordinary
     (destructuring-bind (form-1) (rest form)
       (make-c1form 'ordinary form-1)))
    (otherwise
     (make-c1form 'call form))))


(defun present* (object stream)
  (clim:present object (clim:presentation-type-of object) :stream stream :single-box t))

(defmacro defpresent ((object type) &body body)
  `(clim:define-presentation-method clim:present (,object (type ,type) stream view &key)
     ,@body))

(defpresent (iblock iblock)
  (clim:surrounding-output-with-border (stream)
    (format stream "~a [denv ~a]" (iblock-name iblock)
            (length (denv-variables (dynamic-environment iblock))))
    (clim:with-text-size (stream :normal)
      (flet ((pp (arg) (format stream "~&~s" arg)))
        (map nil #'pp (iblock-instructions iblock))))))

(defun display (frame stream)
  (flet ((print-src ()
           (pprint (src frame) stream))
         (print-cfg ()
           (let* ((bir (cfg frame))
                  (enter (bir-enter bir)))
             (clim:format-graph-from-roots (list enter) #'present* #'iblock-outputs
                                           :orientation :vertical
                                           :merge-duplicates t
                                           :maximize-generations t
                                           :stream stream
                                           :graph-type :dot-digraph))))
    ;; (print-cfg)
    ;; (terpri stream)
    ;; (print-src)
    ;; #+ (or)
    (clim:formatting-table (stream)
      (clim:formatting-column (stream)
        (clim:formatting-cell (stream)
          (print-src)))
      (clim:formatting-column (stream)
        (clim:formatting-cell (stream)
          (print-cfg))))))

(clim:define-application-frame cfg-explorer ()
  ((src :initform nil :accessor src)
   (cfg :initform nil :accessor cfg))
  (:pane :application
   :text-style (clim:make-text-style :fix nil :normal)
   :text-margins '(:left 20 :top 10)
   :end-of-line-action :allow
   :display-function 'display))

(define-cfg-explorer-command com-new-cfg ((new-src 'list) (new-cfg 'bir-result))
  (clim:with-application-frame (frame)
    (setf (src frame) new-src
          (cfg frame) new-cfg)))

(defparameter *src-tagbody*
  `(progn (ordinary 1)
          (ordinary 2)
          (print "HELLO")
          (if (> x 3)
              42
              15)
          (tagbody
             (print "Top 1")
           :tag-1
             (print "Top 2")
           :tag-2
             (print "top 3")
             (if (zerop (random 2))
               (go :tag-1)))))

(defparameter *src-tagbody-2*
  `(progn (ordinary 1)
          (ordinary 2)
          (print "HELLO")
          (if (> x 3)
              42
              15)
          (tagbody
             (print "Top 1")
           :tag-1
             (print "Top 2")
             (unwind-protect
                  (if (not (null t))
                      (progn
                        (print "Escape!")
                        (go :tag-1)
                        (print "Dead code!")
                        (print "Truly dead!"))
                      (if (testp "thing")
                          (if (cosmicp "thing")
                              (go :tag-2)
                              (print "Thing is rad!"))
                          (print "Thing ain't rad!")))
               (print "Leaving truly yours!"))
             (print "Top 3")
           :tag-2
             (print "Top 4")
             (print "Top 5"))))

(defparameter *src-block*
  `(block foobar
     (print "HELLO")
     (if (= x 3)
         (return-from foobar (identity :returned-value))
         (print "CONTINUE"))
     (print "GOOD BYE")))

(defparameter *src-unwind*
  `(block foobar
     (print "HELLO")
     (unwind-protect (if (zerop (random 2))
                         (return-from foobar)
                         (print "Carry on"))
       (print "Cleanup"))
     (print "Good Bye!")))


(defun make-fake-cfg ()
  (let* ((src *src-tagbody*)
         (ast (make-fake-ast src))
         (bir (make-bir-result (make-dynamic-environment))))
    (bir-from-c1form ast bir)
    (clim:execute-frame-command (clim:find-application-frame 'cfg-explorer :activate t)
                                `(com-new-cfg ,src ,bir))))

(make-fake-cfg)
