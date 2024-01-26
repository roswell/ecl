;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

(in-package #:compiler)

(defun c2expr (form)
  (with-c1form-env (form form)
    (let* ((name (c1form-name form))
           (args (c1form-args form))
           (dispatch (gethash name *c2-dispatch-table*)))
      (if dispatch
          (apply dispatch form args)
          (cmperr "Unhandled C2FORM found at the:~%~4I~A" form)))))

(defun c2expr* (form)
  ;; C2EXPR* compiles the giving expression in a context in which
  ;; other expressions will follow this one. We must thus create
  ;; a possible label so that the compiled forms exit right at
  ;; the point where the next form will be compiled.
  (with-exit-label (*exit*)
    (let (;;(*lex* *lex*)
          (*lcl* *lcl*)
          (*temp* *temp*))
      (c2expr form))))

(defun c2progn (c1form forms)
  (declare (ignore c1form))
  ;; INV C1PROGN ensures that the length of forms is not less than 1.
  (loop with lex = *lex*
        for (form next . rest) on forms do
          (if (null next)
              (c2expr form)
              (let ((*destination* 'TRASH))
                (c2expr* form)
                ;; recycle lex locations
                (setq *lex* lex)))
        ;; Since PROGN does not have tags, any transfer of control means leaving
        ;; the current PROGN statement.
        until (member (c1form-name form) '(CL:GO CL:RETURN-FROM))))

(defun c2if (c1form fmla form1 form2)
  ;; FIXME! Optimize when FORM1 or FORM2 are constants
  (cond ((type-true-p (c1form-primary-type fmla))
         ;; The true branch is always taken
         (warn-dead-code form2 c1form "the test ~S always evaluates to true" fmla)
         (let ((*destination* 'TRASH))
           (c2expr* fmla))
         (c2expr form1))
        ((type-false-p (c1form-primary-type fmla))
         ;; The false branch is always taken
         (warn-dead-code form1 c1form "the test ~S always evaluates to false" fmla)
         (let ((*destination* 'TRASH))
           (c2expr* fmla))
         (c2expr form2))
        ((and (eq *destination* 'TRASH)
              (eq (c1form-name form2) 'LOCATION))
         ;; The value produced by the false branch is not used
         (with-exit-label (false-label *exit*)
           (let ((*destination* `(JUMP-FALSE ,false-label)))
             (c2expr* fmla))
           (c2expr form1)))
        ((and (eq *destination* 'TRASH)
              (eq (c1form-name form1) 'LOCATION))
         ;; The value produced by the true branch is not used
         (with-exit-label (true-label *exit*)
           (let ((*destination* `(JUMP-TRUE ,true-label)))
             (c2expr* fmla))
           (c2expr form2)))
        (t
         (with-exit-label (false-label)
           (let ((*destination* `(JUMP-FALSE ,false-label)))
             (c2expr* fmla))
           (c2expr form1))
         (c2expr form2))))

(defun jump-true-destination-p (dest)
  (declare (si::c-local))
  (and (consp dest) (eq (si:cons-car dest) 'JUMP-TRUE)))

(defun jump-false-destination-p (dest)
  (declare (si::c-local))
  (and (consp dest) (eq (si:cons-car dest) 'JUMP-FALSE)))

(defun c2fmla-not (c1form arg)
  (declare (ignore c1form))
  (let ((dest *destination*))
    (cond ((type-true-p (c1form-primary-type arg))
           (let ((*destination* 'TRASH))
             (c2expr* arg))
           (c2expr (c1nil)))
          ((type-false-p (c1form-primary-type arg))
           (let ((*destination* 'TRASH))
             (c2expr* arg))
           (c2expr (c1t)))
          ((jump-true-destination-p dest)
           (let ((*destination* `(JUMP-FALSE ,@(cdr dest))))
             (c2expr arg)))
          ((jump-false-destination-p dest)
           (let ((*destination* `(JUMP-TRUE ,@(cdr dest))))
             (c2expr arg)))
          (t
           (with-inline-blocks ()
             (unwind-exit (negate-argument arg dest)))))))

(defun c2fmla-and (c1form butlast last)
  (flet ((c2expr-and-arguments (eval-dest exit-dest)
           (loop with *destination* = eval-dest
                 for expr in butlast
                 for remaining-exprs on butlast
                 for type = (c1form-primary-type expr)
                 do (cond ((type-false-p type)
                           (warn-dead-code (append (rest remaining-exprs) (list last)) c1form
                                           "the test ~S always evaluates to false" expr)
                           (let ((*destination* exit-dest))
                             (c2expr* expr))
                           (return-from c2expr-and-arguments))
                          ((type-true-p type)
                           (let ((*destination* 'TRASH))
                             (c2expr* expr)))
                          (t
                           (c2expr* expr)))
                 finally
                    (let ((*destination* exit-dest))
                      (c2expr last)))))
    (if (jump-false-destination-p *destination*)
        (c2expr-and-arguments *destination* *destination*)
        (with-exit-label (normal-exit)
          (with-exit-label (false-label)
            (c2expr-and-arguments `(JUMP-FALSE ,false-label) *destination*))
          (unwind-exit *vv-nil*)))))

(defun c2fmla-or (c1form butlast last)
  (flet ((c2expr-or-arguments (eval-dest exit-dest operation)
           (loop with *destination* = eval-dest
                 for expr in butlast
                 for remaining-exprs on butlast
                 for type = (c1form-primary-type expr)
                 do (cond ((type-true-p type)
                           (warn-dead-code (append (rest remaining-exprs) (list last)) c1form
                                           "the test ~S always evaluates to true" expr)
                           (let ((*destination* 'VALUE0))
                             (c2expr* expr))
                           (return-from c2expr-or-arguments))
                          ((type-false-p type)
                           (let ((*destination* 'TRASH))
                             (c2expr* expr)))
                          (t
                           (funcall operation expr)))
                 finally
                    (let ((*destination* exit-dest))
                      (c2expr last)))))
    (cond ((jump-true-destination-p *destination*)
           (c2expr-or-arguments *destination* *destination* #'c2expr*))
          ((jump-false-destination-p *destination*)
           (with-exit-label (true-label)
             (c2expr-or-arguments `(JUMP-TRUE ,true-label) *destination* #'c2expr*))
           (unwind-exit *vv-t*))
          (t
           (with-exit-label (common-exit)
             (with-exit-label (normal-exit)
               (c2expr-or-arguments *destination* *destination*
                                    (lambda (expr)
                                      (let ((*destination* 'VALUE0))
                                        (c2expr* expr))
                                      (unwind-cond normal-exit :jump-t 'VALUE0))))
             (unwind-exit 'VALUE0))))))

(defun c2mv-prog1 (c1form form body)
  (declare (ignore c1form))
  (with-stack-frame (frame)
    (let ((*destination* 'VALUEZ))
      (c2expr* form))
    (push-instruction :frame-push-values frame)
    (let ((*destination* 'TRASH))
      (mapc #'c2expr* body))
    (push-instruction :frame-pop-values frame)
    (unwind-exit 'VALUEZ)))

(defun c2values (c1form forms)
  (declare (ignore c1form))
  (cond
    ;; When the values are not going to be used, then just process each form
    ;; separately.
    ((eq *destination* 'TRASH)
     (mapc #'c2expr* forms)
     ;; We really pass no value, but we need UNWIND-EXIT to trigger all the
     ;; frame-pop and all other exit forms.
     (unwind-exit 'VALUE0))
    ;; For (VALUES) we can replace the output with either NIL (if the value is
    ;; actually used) and set only NVALUES when the value is the output of a
    ;; function.
    ((endp forms)
     (push-instruction :clear-values)
     (case *destination*
       (VALUEZ    (unwind-exit 'VALUEZ))
       (LEAVE     (unwind-exit 'LEAVE))
       (otherwise (unwind-exit *vv-nil*))))
    ;; For a single form, we must simply ensure that we only take a single
    ;; value of those that the function may output.
    ((endp (rest forms))
     (let ((form (first forms)))
       (if (or (not (member *destination* '(LEAVE VALUEZ)))
               (c1form-single-valued-p form))
           (c2expr form)
           (progn
             (let ((*destination* 'VALUE0))
               (c2expr* form))
             (unwind-exit 'VALUE0)))))
    ;; In all other cases, we store the values in the VALUES vector,
    ;; and force the compiler to retrieve anything out of it.
    (t
     (with-inline-blocks ()
       (let ((forms (coerce-args (inline-args forms))))
         (apply #'push-instruction :store-values forms)
         (unwind-exit 'VALUEZ))))))
