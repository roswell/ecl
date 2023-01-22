;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: CLOS -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;  Copyright (c) 2023, Marius Gerbershagen 
;;;;
;;;;    See file '../Copyright' for full details.
;;;;

(in-package "CLOS")

;; ----------------------------------------------------------------------
;; DEFINE-METHOD-COMBINATION
;;
;; METHOD-COMBINATION objects are just a list
;;      (name arg*)
;; where NAME is the name of the method combination type defined with
;; DEFINE-METHOD-COMBINATION, and ARG* is zero or more arguments.
;;
;; For each method combination type there is an associated function,
;; and the list of all known method combination types is kept in
;; *METHOD-COMBINATIONS* in the form of property list:
;;      (mc-type-name1 function1 mc-type-name2 function2 ....)
;;
;; FUNCTIONn is the function associated to a method combination. It
;; is of type (FUNCTION (generic-function method-list) FUNCTION),
;; and it outputs an anonymous function which is the effective method.
;;

#+threads
(defparameter *method-combinations-lock* (mp:make-lock :name 'find-method-combination))
(defparameter *method-combinations* (make-hash-table :size 32 :test 'eq))

(defun search-method-combination (name)
  (mp:with-lock (*method-combinations-lock*)
    (or (gethash name *method-combinations*)
        (error "~A does not name a method combination" name))))

(defun install-method-combination (name function)
  (mp:with-lock (*method-combinations-lock*)
    (setf (gethash name *method-combinations*) function))
  name)

(defun make-method-combination (name compiler options)
  (with-early-make-instance +method-combination-slots+
    (o (find-class 'method-combination)
       :name name
       :compiler compiler
       :options options)
    o))

(defun find-method-combination (gf method-combination-type-name method-combination-options)
  (declare (ignore gf))
  (make-method-combination method-combination-type-name
                           (search-method-combination method-combination-type-name)
                           method-combination-options
                           ))

(defun define-simple-method-combination (name &key documentation
                                         identity-with-one-argument
                                         (operator name))
  `(define-method-combination ,name (&optional (order :MOST-SPECIFIC-FIRST))
     ((around (:AROUND))
      (principal (,name) :REQUIRED t))
     ,documentation
     (let ((main-effective-method
            `(,',operator ,@(mapcar #'(lambda (x) `(CALL-METHOD ,x NIL))
                                    (if (eql order :MOST-SPECIFIC-LAST)
                                        (reverse principal)
                                        principal)))))
       (cond (around
              `(call-method ,(first around)
                (,@(rest around) (make-method ,main-effective-method))))
             (,(if identity-with-one-argument
                   '(rest principal)
                   t)
              main-effective-method)
             (t (second main-effective-method))))))

(defun method-combination-arguments-reshuffling (mc-whole-arg mc-lambda-list gf-lambda-list body)
  ;; Implement the reshuffling of arguments to the generic function
  ;; into arguments for a method-combination arguments lambda list
  ;; required for the long form of DEFINE-METHOD-COMBINATION. The
  ;; generic function arguments are stored in the variable
  ;; .COMBINED-METHOD-ARGS. of type STACK-FRAME. To extract the
  ;; arguments we apply to this stack frame a function whose
  ;; lambda-list is built below by reshaping the method-combination
  ;; lambda-list to fit the form of the generic-function lambda-list.
  (let ((lambda-list '())
        (let-statements '())
        (ignored-vars '())
        n-gf-requireds n-gf-optionals)
    (multiple-value-bind (mc-requireds mc-optionals mc-rest mc-key-flag mc-keywords
                          mc-allow-other-keys mc-aux-vars)
        (si::process-lambda-list mc-lambda-list 'function)
      (declare (ignore mc-allow-other-keys))
      (multiple-value-bind (gf-requireds gf-optionals)
          (si::process-lambda-list gf-lambda-list 'function)
        (setf n-gf-requireds (first gf-requireds)
              n-gf-optionals (first gf-optionals))
        (when mc-whole-arg
          (push `(,mc-whole-arg (apply #'list .combined-method-args.))
                let-statements))
        (loop for r in (rest mc-requireds) by #'cdr
              for i from 0
              if (< i n-gf-requireds)
                do (push r lambda-list)
              else ; excess required args of the method-combination
                   ; are set to nil
              do (push `(,r nil) let-statements))
        ;; excess required args of the generic-function are ignored
        (loop repeat (- n-gf-requireds (first mc-requireds))
              for v = (gensym)
              do (push v lambda-list)
                 (push v ignored-vars))
        (push '&optional lambda-list)
        (loop for o on (rest mc-optionals) by #'cdddr
              for i from 0
              if (< i n-gf-optionals)
                do (push (if (third o)
                             `(,(first o) ,(second o) ,(third o))
                             `(,(first o) ,(second o)))
                         lambda-list)
              else ; excess optional args of the method-combination
                   ; are set to their init forms
                do (push `(,(first o) ,(second o)) let-statements)
                   (when (third o)
                     (push `(,(third o) nil) let-statements)))
        ;; excess args of the generic-function are ignored
        (loop repeat (- n-gf-optionals (first mc-optionals))
              for v = (gensym)
              do (push v lambda-list)
                 (push v ignored-vars))
        (unless mc-rest
          (setf mc-rest (gensym))
          (push mc-rest ignored-vars))
        ;; rest, keyword and aux args are treated as usual
        (push '&rest lambda-list)
        (push mc-rest lambda-list)
        (when mc-key-flag
          (push '&key lambda-list)
          (loop for k on (rest mc-keywords) by #'cddddr
                do (push (if (fourth k)
                             `((,(first k) ,(second k)) ,(third k) ,(fourth k))
                             `((,(first k) ,(second k)) ,(third k)))
                         lambda-list))
          (push '&allow-other-keys lambda-list))
        (when mc-aux-vars
          (push '&aux lambda-list)
          (loop for a on mc-aux-vars by #'cddr
                do (push `(,(first a) ,(second a)) lambda-list)))
        `(apply #'(lambda ,(nreverse lambda-list)
                    (declare (ignore ,@ignored-vars))
                    (let ,(nreverse let-statements)
                      ,body))
                .combined-method-args.)))))

(defun process-define-method-combination-arguments-lambda-list
    (lambda-list generic-function body)
  (declare (si::c-local))
  (when (null lambda-list)
    (return-from process-define-method-combination-arguments-lambda-list body))
  (let ((whole (when (eq (first lambda-list) '&whole)
                 (prog1 (second lambda-list)
                   (setf lambda-list (cddr lambda-list))))))
    (multiple-value-bind (requireds optionals rest key-flag keywords
                          allow-other-keys aux-vars)
        (si::process-lambda-list lambda-list 'function)
      (declare (ignore allow-other-keys key-flag))
      ;; This is a little complicated. We are constructing a form
      ;; which when evaluated constructs another form that finally
      ;; implements the desired destructuring of arguments supplied to
      ;; the generic function.
      ;;
      ;; First evaluate the body form containing the grouping of the
      ;; methods and the user-supplied method-combination body in a
      ;; context in which all free variables are bound to fresh
      ;; symbols (we use the name of the free variable itself). This
      ;; part happens when the method combination is defined.
      `(let ((result (let ,(mapcar #'(lambda (v) `(,v ',v))
                                   (append (when whole (list whole))
                                           (rest requireds)
                                           (loop for o on (rest optionals) by #'cdddr
                                                 collect (first o)
                                                 when (third o) collect (third o))
                                           (when rest (list rest))
                                           (loop for k on (rest keywords) by #'cddddr
                                                 collect (second k)
                                                 when (fourth k) collect (fourth k))
                                           (loop for a on (rest aux-vars) by #'cddr
                                                 collect (first a))))
                       ,body)))
         ;; Second, construct a form which implements the required
         ;; complex reshuffling of arguments. This part happens after
         ;; a generic function using the method combination is
         ;; defined.
         (method-combination-arguments-reshuffling
          ',whole
          ',lambda-list
          (generic-function-lambda-list ,generic-function)
          result)))))

(defmacro with-method-groups ((&rest method-groups) &body body)
  (let (group-names matchers cleanup)
    (flet ((parse-qualifier-pattern (pattern)
             (cond ((eq pattern '*)   't)
                   ((eq pattern nil)  '(null .method-qualifiers.))
                   ((symbolp pattern) `(,pattern .method-qualifiers.))
                   ((listp pattern)
                    `(do ((pattern ',pattern (cdr pattern))
                          (qualifiers .method-qualifiers. (cdr qualifiers)))
                         ((or (eq pattern '*)
                              (and (null pattern)
                                   (null qualifiers)))
                          t)
                       (unless (and pattern qualifiers
                                    (or (eq (car pattern) '*)
                                        (eq (car pattern) (car qualifiers))))
                         (return nil))))
                   (t
                    (error "Invalid method group pattern ~s." pattern)))))
      (dolist (group method-groups)
        (destructuring-bind (name pattern &key description
                                               (order :most-specific-first)
                                               (required nil))
            group
          (declare (ignore description))
          (push name group-names)
          (push `(,(parse-qualifier-pattern pattern) (push .method. ,name))
                matchers)
          (when required
            (push `(unless ,name
                     (error "No methods in required group ~S." ',name))
                  cleanup))
          (case order
            (:most-specific-first
             (push `(setf ,name (nreverse ,name)) cleanup))
            (:most-specific-last)
            (otherwise
             (push `(when (eq ,order :most-specific-first)
                      (setf ,name (nreverse ,name)))
                   cleanup))))))
    `(let ,group-names
       (dolist (.method. .methods-list.)
         (let ((.method-qualifiers. (method-qualifiers .method.)))
           (declare (ignorable .method-qualifiers.))
           (cond ,@(nreverse matchers)
                 ,@(unless (member t matchers :key #'car)
                     `((t (invalid-method-error
                           .method.
                           "Method qualifiers ~s are not allowed." .method-qualifiers.)))))))
       ,@cleanup
       ,@body)))

(defun define-complex-method-combination (form)
  (declare (si::c-local))
  (flet ((syntax-error ()
           (error "~S is not a valid DEFINE-METHOD-COMBINATION form." form)))
    (destructuring-bind (name lambda-list method-groups &rest body
                         &aux (generic-function '.generic-function.)
                              (arguments nil))
        form
      (let ((x (first body)))
        (when (and (consp x) (eql (first x) :ARGUMENTS))
          (setf body (rest body))
          (setf arguments (rest x))))
      (let ((x (first body)))
        (when (and (consp x) (eql (first x) :GENERIC-FUNCTION))
          (setf body (rest body))
          (setf generic-function (second x))))
      (unless (and (symbolp name) (symbolp generic-function))
        (error "~S is not a valid DEFINE-METHOD-COMBINATION form." form))
      (multiple-value-setq (decls body documentation)
        (si::find-declarations body t))
      `(progn
         ,@(si::expand-set-documentation name 'method-combination documentation)
         (install-method-combination ',name
            (lambda (,generic-function .methods-list. ,@lambda-list)
              (declare (ignorable ,generic-function))
              ,@decls
              (block ,name
                (effective-method-function
                 ,(process-define-method-combination-arguments-lambda-list
                   arguments generic-function
                   `(with-method-groups ,method-groups ,@body))
                 t))))))))

(defmacro define-method-combination (name &body body)
  (if (and body (listp (first body)))
      (define-complex-method-combination (list* name body))
      (apply #'define-simple-method-combination name body)))

(defun method-combination-error (format-control &rest args)
  ;; FIXME! We should emit a more detailed error!
  (error "Method-combination error:~%~S"
         (apply #'format nil format-control args)))

(defun invalid-method-error (method format-control &rest args)
  (error "Invalid method error for ~A~%~S"
         method
         (apply #'format nil format-control args)))

(install-method-combination 'standard 'standard-compute-effective-method)
(progn
  (define-method-combination progn :identity-with-one-argument t)
  (define-method-combination and :identity-with-one-argument t)
  (define-method-combination max :identity-with-one-argument t)
  (define-method-combination + :identity-with-one-argument t)
  (define-method-combination nconc :identity-with-one-argument t)
  (define-method-combination append :identity-with-one-argument nil)
  (define-method-combination list :identity-with-one-argument nil)
  (define-method-combination min :identity-with-one-argument t)
  (define-method-combination or :identity-with-one-argument t))
