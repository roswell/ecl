(cl:in-package cl-user)

(defpackage ecl-cmp/ir
  (:use #:alexandria #:clim-lisp)
  (:import-from #:compiler
                #:c1form #:c1form-name #:c1form-type #:c1form-args
                #:ref #:fun #:var #:blk #:tag)
  ;; IR types
  (:import-from #:ffi #:c-inline :c-progn)
  (:import-from #:compiler
                #:location #:var #:setq #:psetq
                #:block #:progn #:progv #:tagbody #:return-from
                #:funcall #:call-local #:call-global
                #:catch #:unwind-protect #:throw #:go
                #:locals
                #:if #:fmla-not #:fmla-and #:fmla-or
                #:lambda #:let* #:values
                #:multiple-value-setq #:multiple-value-bind
                #:function #:rplacd
                #:with-stack #:stack-push-values
                #:ordinary #:load-time-value
                #:make-form #:init-form
                )
  (:import-from #:system
                #:fset
                #:structure-ref
                #:structure-set)
  (:import-from #:ext
                #:compiler-typecase #:compiler-let #:checked-value)

  (:import-from #:clim
                #:define-presentation-type
                #:with-drawing-options
                #:with-text-style
                #:make-text-style
                #:define-presentation-method
                #:define-presentation-action
                #:present
                #:surrounding-output-with-border
                #:format-graph-from-roots
                #:+red+
                #:+dark-grey+)

  (:export
   #:location #:var #:setq #:psetq
   #:block #:progn #:progv #:tagbody #:return-from
   #:funcall #:call-local #:call-global
   #:catch #:unwind-protect #:throw #:go
   #:c-inline #:c-progn #:locals
   #:if #:fmla-not #:fmla-and #:fmla-or
   #:lambda #:let* #:values
   #:multiple-value-setq #:multiple-value-bind
   #:compiler-let #:function #:rplacd
   #:with-stack #:stack-push-values
   #:ordinary #:load-time-value
   #:make-form #:init-form
   #:checked-value

   #:fset
   #:structure-ref
   #:structure-set

   #:compiler-typecase))
(in-package #:ecl-cmp/ir)

(defun present* (object stream)
  (present object (clim:presentation-type-of object) :stream stream :single-box t))

(defun present-list (list stream)
  (clim:format-textual-list list 'present* :stream stream :separator #\newline))

(clim:define-presentation-type node ())

(clim:define-presentation-method clim:present
    (object (type node) stream view &key acceptably for-context-type)
  (declare (ignore type view acceptably for-context-type))
  (typecase object
    (cons (mapc (lambda (object)
                  (present object 'node :stream stream :single-box t)
                  (fresh-line stream))
                object))
    (c1form    (present object 'c1form          :stream stream  :single-box t))
    (c::iblock (present object 'c::iblock       :stream stream  :single-box t))
    (ref       (present object (type-of object) :stream stream  :single-box t))
    (otherwise (present object 'expandable  :stream stream      :single-box t))))

(clim:define-presentation-type expandable ())


(clim:define-presentation-type c1form    () :inherit-from 'expandable)
(clim:define-presentation-type c::iblock () :inherit-from 'expandable)

(clim:define-presentation-type ref () :inherit-from 'expandable)
(clim:define-presentation-type fun () :inherit-from 'ref)
(clim:define-presentation-type var () :inherit-from 'ref)
(clim:define-presentation-type blk () :inherit-from 'ref)
(clim:define-presentation-type tag () :inherit-from 'ref)

(clim:define-presentation-method clim:present
    (object (type expandable) stream view &key acceptably for-context-type)
  (declare (ignore type view acceptably for-context-type))
  (with-drawing-options (stream :ink clim:+dark-grey+)
    (format stream "~s" object)))

(clim:define-presentation-method clim:present
    (object (type c1form) stream view &key acceptably for-context-type)
  (declare (ignore type view acceptably for-context-type))
  (format stream "~a" object
          )
  #+ (or)
  (with-text-style (stream (make-text-style nil :italic :very-small))
    (let ((ctype (c1form-type object)))
      (format stream "~s" ctype))))

(clim:define-presentation-method clim:present
    (object (type ref) stream view &key acceptably for-context-type)
  (declare (ignore type view acceptably for-context-type))
  (with-drawing-options (stream :ink clim:+red+)
    (format stream "~a ~s" (type-of object) (c::ref-name object))))

(clim:define-presentation-method clim:present
    (object (type fun) stream view &key acceptably for-context-type)
  (declare (ignore type view acceptably for-context-type))
  (call-next-method)
  (terpri stream)
  (clim:present (c::fun-lambda object) 'c1form :stream stream :single-box t))

(clim:define-presentation-method clim:present
    (object (type var) stream view &key acceptably for-context-type)
  (declare (ignore type view acceptably for-context-type))
  (call-next-method)
  (clim:with-text-style (stream (clim:make-text-style nil :italic :smaller))
    (format stream " ~s" (c::var-type object))))

(clim:define-presentation-method clim:present
    (object (type c::module) stream view &key acceptably for-context-type)
  (declare (ignore type view acceptably for-context-type))
  (format stream "~a~%" object)
  (format stream "Constants:~%")
  (present-list (c::constants object) stream)
  (terpri stream)
  (format stream "Top Level ~a:~%" (clim:presentation-type-of (c::top-level object)))
  (present* (c::top-level object) stream)
  (terpri stream)
  (format stream "Functions:~%")
  (present-list (c::functions object) stream)
  (terpri stream)
                                        ;#+ ()
  (format-graph-from-roots (list (c::top-level object))
                           #'present*
                           #'c::outputs
                           ;:maximize-generations t
                           :merge-duplicates t
                           ;; :graph-type :dot-digraph
                           :orientation :vertical
                           ))

(clim:define-presentation-method clim:present
    (object (type c::iblock) stream view &key acceptably for-context-type)
  (declare (ignore type view acceptably for-context-type))
  ;(terpri stream)
  (clim:surrounding-output-with-border (stream)
    (format stream "~a :in ~a :out ~a :instr ~a~%" (c::name object)
            (length (c::inputs object))
            (length (c::outputs object))
            (length (c::instructions object)))
    ;; #+ (or)
    (clim:format-textual-list (c::instructions object)
                              #'present*
                              :stream stream :separator #\newline)))

;; (location             loc                                                 :pure :single-valued)
;; (var                  var                                                 :single-valued)
;; (setq                 var value-c1form                                    :side-effects)
;; (psetq                var-list value-c1form-list                          :side-effects)
;; (block                blk-var progn-c1form                                :pure)
;; (progn                body                                                :pure)
;; (progv                symbols values form                                 :side-effects)
;; (tagbody              tag-var tag-body                                    :pure)
;; (return-from          blk-var return-type value                           :side-effects)
;; (funcall              fun-value (arg-value*)                              :side-effects)
;; (call-local           obj-fun (arg-value*)                                :side-effects)
;; (call-global          fun-name (arg-value*))
;; (catch                catch-value body                                    :side-effects)
;; (unwind-protect       protected-c1form body                               :side-effects)
;; (throw                catch-value output-value                            :side-effects)
;; (go                   tag-var return-type                                 :side-effects)
;; (c-inline             (arg-c1form*) (arg-type-symbol*) output-rep-type
;;                         c-expression-string side-effects-p one-liner-p)
;; (c-progn              variables forms)
;; (locals               local-fun-list body labels-p                        :pure)
;; (if                   fmla-c1form true-c1form false-c1form                :pure)
;; (fmla-not             fmla-c1form                                         :pure)
;; (fmla-and             *                                                   :pure)
;; (fmla-or              *                                                   :pure)
;; (lambda               lambda-list doc body-c1form)
;; (let*                 vars-list var-init-c1form-list decl-body-c1form     :pure)
;; (values               values-c1form-list                                  :pure)
;; (multiple-value-setq  vars-list values-c1form-list                        :side-effects)
;; (multiple-value-bind  vars-list init-c1form body                          :pure)
;; (compiler-let         symbols values body)
;; (function             (global/closure) lambda-form fun-object             :single-valued)
;; (rplacd               (dest-c1form value-c1form)                          :side-effects)
;; (si:structure-ref     struct-c1form type-name slot-index (:unsafe/nil)    :pure)
;; (si:structure-set     struct-c1form type-name slot-index value-c1form     :side-effects)
;; (with-stack           body                                                :side-effects)
;; (stack-push-values    value-c1form push-statement-c1form                  :side-effects)
;; (ordinary             c1form                                              :pure)
;; (load-time-value      dest-loc value-c1form                               :pure :single-valued)
;; (fset                 function-object vv-loc macro-p pprint-p lambda-form :side-effects)
;; (make-form            vv-loc value-c1form                                 :side-effects)
;; (init-form            vv-loc value-c1form                                 :side-effects)
;; (compiler-typecase    var expressions)
;; (checked-value        type value-c1form let-form)

(defgeneric node-children (node name)
  (:method (node name)
    nil)
  (:method ((node c::iblock) name)
    (c::outputs node))
  (:method ((node c::c1form) (name null))
    (node-children node (c::c1form-name node)))
  (:method ((node c::c1form) (name symbol))
    (c::c1form-args node))
  (:method ((node c::c1form) (name (eql 'progn)))
    (destructuring-bind (forms) (c::c1form-args node)
      forms))
  (:method ((node c::c1form) (name (eql 'lambda)))
    (destructuring-bind (ll doc body) (c::c1form-args node)
      (let ((children '()))
        (unless (every #'null ll)
          (push ll children))
        (unless (null doc)
          (push doc children))
        (push body children)))))

(defun merge-duplicates-p (node)
  (typep node 'c::iblock))

(defun show-c1form (form stream)
  (clim:format-graph-from-roots (ensure-list form)
                                (lambda (obj stream)
                                  (clim:surrounding-output-with-border (stream)
                                    (clim:present obj 'node :stream stream)))
                                (lambda (obj)
                                  (node-children obj nil))
                                :orientation :horizontal
                                :merge-duplicates (merge-duplicates-p form)
                                :stream stream))

(defun show-c1forms (frame stream)
  (clim:surrounding-output-with-border (stream :ink clim:+light-green+)
    (present* (module frame) stream))
  (terpri stream)
  (clim:surrounding-output-with-border (stream :ink clim:+light-pink+)
    (format stream "make forms~%")
    (show-c1form (makes frame) stream))
  (terpri stream)
  (clim:surrounding-output-with-border (stream :ink clim:+light-blue+)
    (format stream "top-level forms~%")
    (show-c1form (forms frame) stream)))

(defun show-selected (frame stream)
  (show-c1form (selected frame) stream))

(clim:define-application-frame c1form-frame ()
  ((module :initarg :module :initform nil :accessor module)
   (makes :initarg :makes :accessor makes :initform nil)
   (forms :initarg :forms :accessor forms :initform nil)
   (selected :initform nil :accessor selected))
  (:reinitialize-frames t)
  (:panes (out :application :display-time nil :scroll-bars :vertical :end-of-line-action :allow)
          (app :application :width 800 :height 600 :display-function 'show-c1forms)
          (sel :application :width 800 :height 600 :display-function 'show-selected))
  (:layouts (default (clim:horizontally ()
                       app
                       (clim:make-pane 'clime:box-adjuster-gadget)
                       (50 sel)))))

(define-c1form-frame-command (com-prop-1 :menu t)
    ((object 'expandable))
  (when (typep object 'c::c1form)
    (c::p1propagate object)))

(define-c1form-frame-command com-expand
    ((object 'expandable :gesture :select))
  (clim:with-application-frame (frame)
    (setf (selected frame) object)))

(define-c1form-frame-command (com-propagate :menu t) ()
  (clim:with-application-frame (frame)
    (let ((c::*make-forms*      (makes frame))
          (c::*top-level-forms* (forms frame)))
      (c::compiler-pass/propagate-types))
    (dolist (m (makes frame))
      (c::p1propagate m))
    (dolist (f (forms frame))
      (c::p1propagate f))))

(define-c1form-frame-command (com-pass/cfg :menu t) ()
  (clim:with-application-frame (frame)
    (setf (module frame)
          (c::compiler-pass/custom-pass
           (append (makes frame) (forms frame))))))

(define-c1form-frame-command (remake-file :menu t) ()
  (clim:with-application-frame (frame)
    (destructuring-bind (module makes forms)
        (do-it "/home/jack/Warsztat/Repo/ecl/hacks/test-file.lisp")
      (setf (module frame) module
            (makes frame) makes
            (forms frame) forms))))

(defun do-it (file)
  (let* ((c::*compile-file-pathname* (pathname file))
         (c::*compile-file-truename* (pathname file))
         (c::*compiler-in-use* c::*compiler-in-use*)
         (c::*load-time-values* nil)
         (ext::*source-location* (cons c::*compile-file-pathname* 0))
         (conditions nil)
         (foo 42)
         makes
         forms
         module)
    (c::with-compiler-env (conditions)
      (with-open-file (stream file)
        (c::compiler-pass1 stream 0)
        (setf makes c::*make-forms*
              forms c::*top-level-forms*)))
    (list (c::compiler-pass/custom-pass (append makes forms))
          makes forms)
    ))

(defun make-it-happen ()
  (destructuring-bind (module makes forms)
      (do-it "/home/jack/Warsztat/Repo/ecl/hacks/test-file.lisp")
    (clim:run-frame-top-level
     (clim:make-application-frame 'c1form-frame
                                  :module module
                                  :makes makes
                                  :forms forms))))

#+ (or)
(ecl-cmp/ir::make-it-happen)





