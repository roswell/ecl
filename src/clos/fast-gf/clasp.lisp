;;; https://irclog.tymoon.eu/libera/%23sicl?around=1639663285#1639663285
;;; (finalize-inheritance issue) -- fixed
;;;
;;; reinitialize-instance in ecl is awfully slow, try running benchmarks twice


;;; https://github.com/clasp-developers/clasp/issues/1124
;;; eql-specializers issue

;;; Interesting files (all are interesting! :-)

;;; kernel.lsp         defines invalidate*-discriminating-function*
;;; outcome.lsp        represents an effective method function (only simpler)
;;; combin.lsp         effective method functions (real ones)
;;; closfastgf.lsp     invalidated-dispatch-function
;;; dtree.lsp          interpreted discriminator, discriminator tree
;;; discriminator.lsp  generate-discriminator
;;; satiation.lsp      optimization for slow compilers
;;; README             the abstraction outline

(defun compute-discriminating-function (gf)
  (invalidate-discriminated-function-closure gf))

;;; Returns a closure usable as a discriminating function when the generic
;;; function is in the invalidated state.
(defun invalidate-discriminating-function-closure (gf)
  (lambda (&rest args)
    (invalidated-dispatch-function gf args)))

#+ (or)
(defun invalidated-dispatch-function/safe (gf args)
  (assert (not (member (list* gf args)
                       *dispatch-miss-recursion-check*
                       :test #'equal))
          (gf args *dispatch-miss-recursion-check*)
          "Recursive dispatch miss detected!")
  (let ((*dispatch-miss-recursion-check*
          (list* (list* gf args) *dispatch-miss-recursion-check*)))
    (invalidated-dispatch-function gf args)))

(defun invalidated-dispatch-function (gf args)
  ;; If there is a call history then compile a dispatch function being
  ;; extremely careful NOT to use any generic-function calls. Then redo the
  ;; call.
  ;;
  ;; If there is no call history then treat this like a dispatch-miss.))
  (if (mp:atomic (safe-gf-call-history generic-function))
      (progn
        (force-dispatcher gf)
        (apply gf args))
      (apply #'dispatch-miss gf args)))

(defun force-dispatcher (gf)
  (set-funcallable-instance-function
   gf (calculate-fastgf-dispatch-function gf)))

(defun calculate-fastgf-dispatch-function (gf &key compile)
  (if (mp:atomic (safe-gl-call-history generic-function))
      (if (or *fastgf-force-compiler* compile)
          (compile nil (generate-discriminator generic-function))
          (interpreted-discriminator gf))
      (invalidated-discriminating-function-closure gf)))

(defun dispatch-miss (gf &rest args)
  ;; Check that number of arguments matches.
  (multiple-value-bind (min max)
      (generic-function-min-max-args gf)
    (when (or (< nargs min) (and max (> nargs max)))
      (error 'wrong-number-of-arguments gf nargs min max)))
  ;; Update any invalid instances
  (when (maybe-update-instances args)
    (return-from dispatch-miss (apply gf args)))
  ;; OK, real miss.
  (multiple-value-bind (outcome new-call-history-entries)
      (dispatch-miss-info gf args)
    (when (memoize-calls gf new-call-history-entries)
      (force-dispatcher gf))
    (perform-outcome outcome args)))

;;; side-effect free function. Returns the "outcome" - an effective-method
;;; holding structure - and a new call history.
(defun dispatch-miss-info (gf args)
  (let ((arg-classes (mapcar #'class-of args)))
    (multiple-value-bind (method-list ok)
        (compute-applicable-methods-using-classes gf arg-classes)
      (when (not ok)
        (setf method-list (compute-applicable-methods gf args)))
      (let* ((method-combination (generic-function-method-combination gf))
             (final-methods (final-methods method-list arg-classes))
             (outcome (outcome gf method-combination final-methods arg-classes)))
        (values
         outcome
         (cond
           ;; Don't memoize NO-APPLICABLE-METHODs.
           ((null final-methods)
            nil)
           ;; COMPUTE-APPLICABLE-METHODS-USING-CLASSES is enough
           (ok
            (let* ((count (length (specializer-profile gf)))
                   (entry (coerce (subseq arg-classes count) 'simple-vector)))
              (list (cons entry outcome))))
           ;; EQL-specializers may be memoized only when gf is truly standard.
           ((eq (class-of gf) (find-class 'standard-generic-function))
            (coerce
             (loop for arg-class in arg-classes
                   for arg-specs across (specializer-profile gf)
                   if (consp arg-specs)
                     collect (list* arg-class
                                    (do-map (spec arg-specs)
                                      (when (typep spec arg-class)
                                        (intern-eql-specializer obj))))
                   else
                     collect (list* arg-class nil)
                   finally
                      (return (combine results outcome)))
             'simple-vector))
           ;; No more options: we just don't memoize. This may only occur for
           ;; EQL specializers for the standard c-a-m/-u-c methods.
           (t
            nil)))))))


;;; These OUTCOME interface is dumbed down version of what clasp does.
(defun outcome (gf method-combination methods actual-specializers)
  (or (find-existing-outcome (mp:atomic (safe-gf-call-history gf)) methods)
      (compute-outcome gf method-combination methods actual-specializers)))

;;; Methods are a key!
(defun find-existing-outcome (call-history methods)
  (loop for (ignore . outcome) in call-history
        when (equal methods (outcome-methods outcome))
          return outcome))

(defun compute-outcome (gf method-combination methods actual-specializers)
  ;; Actual-specializers are used for optimized slot accessors.
  (let ((effective-method (compute-effective-method gf method-combination methods)))
    (make-effective-method-outcome
     :methods methods
     :form effective-method
     :function (effective-method-function effective-method gf))))

(defun perform-outcome (outcome arguments)
  (apply (outcome-effective-method-function outcome) arguments))
