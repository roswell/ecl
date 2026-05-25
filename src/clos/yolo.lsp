;;;;
;;;;  Copyright (c) 2026, Daniel Kochmański
;;;;  See file 'LICENSE' for the copyright details.
;;;;

;;; Fast GF dispatch
;;;
;;; This implementation is based on the method described by Robert Strandh in
;;; "Fast generic dispatch for Common Lisp" and on its implementation for Clasp
;;; by Alex Wood.
;;;
;;; An algorithm to build the decision tree is inspired by the macro STRING-CASE
;;; described and implemented by Paul Khuong.
;;; 
;;; http://metamodular.com/SICL/generic-dispatch.pdf
;;; http://pvk.ca/Blog/Lisp/string_case_bis.html
;;; 
;;; FIXME the essence of the method is to work with class stamps instead of full
;;; objects as a more localized approach to computing the effective method. We
;;; could, instead of comparing them with IF, construct a perfect hash table.
;;; 
;;; FIXME we don't maintain a pool time stamps that are free, so the stamp index
;;; always increases and that may be problematic when cl_index is 32bit. Make a
;;; pool of class stamps to reuse and work with stamps that are always 64bit.
;;; 
;;; FIXME this procedure diverges in some repsects from work by RS and AW and
;;; that should be clearly spelled out. For example I've implemented differently
;;; handling of EQL arguments from what Clasp does (or did in 2022 :-).

(defpackage "EU.TURTLEWARE.FASTGF"
  (:use "CL")
  (:nicknames "FGF")
  (:import-from "CLOS" "GENERIC-FUNCTION-CALL-HISTORY"))
(in-package "EU.TURTLEWARE.FASTGF")

#+ (or)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :speed *features*)
  (declaim (optimize (speed 3) (safety 0) (debug 0))))


;;; The fast gf implementation

;; (declaim (notinline dbg dbg*))
(defun dbg (fmt &rest args)
  (declare (ignorable fmt args))
  ;(apply #'format *debug-io* fmt args)
  )

(defun dbg* (fmt &rest args)
  (declare (ignorable fmt args))
  (apply #'format *debug-io* fmt args))

;;; CALL-HISTORY representation:
;;; 
;;; The history is a sequence of entries:
;;;   (SPECIALIZERS . OUTCOME)
;;; 
;;; and SPECIALIZERS is N-element[1] sequence:
;;;   (CLASS . EQL-OBJECTS)
;;;
;;; [1] N is a number of specializable generic function arguments
;;; 

(defun call-history-entry-specializers (entry)
  (car entry))

(defun call-history-entry-outcome (entry)
  (cdr entry))

(defun specializer-stamp (spec)
  (si:instance-get-stamp (car spec)))

(defun specializer-eqls (spec)
  (cdr spec))

(defun class-stamp (object)
  (si:instance-get-stamp object))

;;; FIXME
(defun class-eqls (object)
  (declare (ignore object))
  nil)

(defun find-call-history-entry (classes history)
  (find classes history :key #'car :test #'equal))


(defun compute-effective-outcome (gf args)
  (clos::compute-applicable-method gf args))

#+ (or)
;;; This function is similar to COMPUTE-EFFECTIVE-FUNCTION but with a twist --
;;; when there are EQL specializers, then the computed function does TYPECASE on
;;; all of them. This way the dispatch procedure always operates on class
;;; stamps, and the outcome function handles the rest.
(defun compute-effective-outcome (gf args)
  (let ((classes (mapcar #'class-of args)))
    (multiple-value-bind (method-list ok)
        (mop:compute-applicable-methods-using-classes gf classes)
      (when ok
        (return-from compute-effective-outcome
          (when method-list
            (let ((gf-comb (mop:generic-function-method-combination gf)))
              (mop:compute-effective-method-function gf gf-comb method-list)))))      
      ;; To rephrase what has been told earlier -- here we don't care about ARGS
      ;; but rather we take all combinations of EQL specializers for given
      ;; classes and create an outcome function that dispatches based on them.
      (dbg "[fast] EQL specializer ahead! returning slow path.~%")
      ;; We punt for now to focus on codegen for classes, then this needs to be
      ;; finished. Since this will be installed for argument classes, we need to
      ;; recompute the effective function on each execution of this outcome,
      ;; because the set of arguments may be different in subsequent invocation.
      (lambda (args rest-methods)
        (declare (ignore rest-methods))
        (dbg "Executing slow path due to EQL specializers.~%")
        (let ((method-list (compute-applicable-methods gf args)))
          (if method-list
              (let* ((gf-comb (mop:generic-function-method-combination gf))
                     (gf-func (mop:compute-effective-method-function
                               gf gf-comb method-list)))
                (funcall gf-func args nil))
              (apply #'no-applicable-method gf args))))
      #+ (or)
      (let ((class-marker (gensym))
            (combinations
              (loop for class in classes
                    for (class-p . eqls) in (clos::generic-function-spec-list gf)
                    for eql-objs = (remove-if-not (lambda (o) (typep o class)) eqls)
                    collect eql-objs)))
        ;; combinations contain sequences of eql-specializers
        ;;
        ;;   (a1 ... an) (b1 ... bn) (c1 ... cn)
        ;;
        ;; We want to compute method lists for:
        ;; (a1 b1 c1)
        ;; (a1 b1 cn)
        ;; (a1 b1 class)
        ;; (a1 bn c1)
        ;; ...
        ;; and exapand these to
        ;;
        ;; (case arg1
        ;;   (a1 (case arg2
        ;;         (b1 (case arg3
        ;;               (c1 (funcall effective-method-1 arg1 arg2 arg3))
        ;;               (cn (funcall effective-method-2 arg1 arg2 arg3)))
        ;;               (otherwise (funcall effecitve-method-3 arg1 arg2 arg3)))
        ;;         (b2 (funcall #'no-applicable-method gf arg1 arg2 arg3))
        ;;         (otherwise ...)))
        ;;   (otherwise (funcall (effective-method-classes-only arg1 arg2 arg3))))
        ;; 
        ;; NOTE that some combinations may not have an effective method.
        (labels ((expand-args (args)
                   (compute-applicable-methods gf args))
                 (expand-case (n args vals rest)
                   `(case ,(nth n args)
                      ,@(mapcar (lambda (val vals)
                                  `(,val (expand-case (1+ n)
                                                      ,args
                                                      ,(car rest)
                                                      ,(cdr rest)))
                                  vals))
                      (otherwise
                       ))))
          (map-combinations combinations))

        (labels ((expand-arg (arg-values continuation)
                   (case arg
                     )
                   (do-something)))
          (map-combinations
           (lambda (args)
             ())))))))

;;; This function populates the generic function call history with argument
;;; classes and their effective methods without calling the them.
(defun pseudocall (gf outcome &rest args)
  (let* ((spec-list (clos::generic-function-spec-list gf))
         (classes (loop for i in spec-list ;spec-list arity is for req-args
                        for a in args
                        collect (class-of a))))
    (unless (find-call-history-entry classes (clos::generic-function-call-history gf))
      (dbg* "Updating history with ~a~%" classes)
      (push (cons classes outcome) (clos::generic-function-call-history gf)))))


;;; Here we compute the dispatch tree that tries to minimize the number of
;;; checks.

(defstruct (dispatch-tree (:type vector))
  history common merged outcome to-check split lhist rhist ltree rtree)

(defun print-dispatch-tree (tree)
  (when tree
    (let ((*print-right-margin* 1024))
      (dbg "History:~%~{->  ~a~%~}" (dispatch-tree-history tree))
      (dbg "Common: ~s~%" (dispatch-tree-common tree))
      (dbg "Split: ~s~%" (dispatch-tree-split tree))
      (dbg "To check: ~s~%" (dispatch-tree-to-check tree))
      (dbg ":: ltree --------------------~%")
      (print-dispatch-tree (dispatch-tree-ltree tree))
      (dbg ":: rtree --------------------~%")
      (print-dispatch-tree (dispatch-tree-rtree tree))
      (dbg ":: --------------------------~%"))))

;;; This function returns a list of checks that are common for whole
;;; (sub-)history. When any of checks fail then we have a cache miss.
(defun find-common-checks (history to-check)
  (assert (not (null history)))
  (loop with ((first-call) . other-entries) = history
        for index from 0 below (length first-call)
        for stamp = (class-stamp (elt first-call index))
        collect (and (member index to-check)
                     (every (lambda (entry)
                              (let ((call (car entry)))
                                (= stamp (class-stamp (elt call index)))))
                            other-entries)
                     stamp)))

(defun find-best-split-check (history to-check)
  (let (result-index
        result-stamp
        result-lhist
        result-rhist
        (best-split -1)
        (best-break 1))
    (labels ((tie-breaker (lhist rhist)
               (declare (ignorable lhist rhist))
               ;; This function is used to decide which of even splits is
               ;; better.  We base our decision on the number of unique outcomes
               ;; (per the history length). Smaller value means more uniform
               ;; distribution. This could benefit additionally from promoting
               ;; adjacent branches in the same batch.
               #+ (or)
               (when (and lhist rhist)
                 (let* ((s1 (/ (length (remove-duplicates lhist :key #'cdr))
                               (length lhist)))
                        (s2 (/ (length (remove-duplicates rhist :key #'cdr))
                               (length rhist)))
                        (score (max s1 s2)))
                   (when (< score best-break)
                     score))))
             (try-split (split-index split-stamp)
               (loop for entry in history
                     for (call . outcome) = entry
                     for class = (elt call split-index)
                     for stamp = (class-stamp class)
                     if (<= stamp split-stamp)
                       collect entry into lhist
                     else
                       collect entry into rhist
                     finally
                        (maybe-add-split split-index split-stamp lhist rhist)))
             (maybe-add-split (split-index split-stamp lhist rhist)
               (let* ((split<= (length lhist))
                      (split>/ (length rhist))
                      (smaller (min split<= split>/))
                      (breaker (cond ((< best-split smaller)
                                      best-break)
                                     ((= best-split smaller)
                                      (tie-breaker lhist rhist)))))
                 (when breaker
                   (setf result-index split-index
                         result-stamp split-stamp
                         result-lhist lhist
                         result-rhist rhist
                         best-split smaller
                         best-break breaker)))))
      (loop for index in to-check
            for stamps = (loop for (call . outcome) in history
                               for spec = (elt call index)
                               collect (class-stamp spec))
            do (dolist (split-stamp stamps)
                 (try-split index split-stamp))
            finally (return (values result-index result-stamp
                                    result-lhist result-rhist))))))

(defun generate-branch (history to-check)
  (when (null history)
    (assert (null to-check))
    (return-from generate-branch nil))
  (let ((common (find-common-checks history to-check)))
    (loop for index from 0
          for elt in common
          when elt
            collect index into checked
          finally (setf to-check (sort (set-difference to-check checked) #'<)))
    (multiple-value-bind (index stamp lhist rhist)
        (find-best-split-check history to-check)
      (assert (eq (not (null lhist)) (not (null rhist))))
      (cond
        ((not (null lhist))
         (make-dispatch-tree :history  history
                             :common   common
                             :merged   (make-list (length common))
                             :to-check to-check
                             :split    (cons index stamp)
                             :lhist    lhist
                             :rhist    rhist
                             :ltree    (generate-branch lhist to-check)
                             :rtree    (generate-branch rhist to-check)))
        ;; Class-based dispatch ends here. The remainder is either returning
        ;; the outcome or EQL-based dispatch.
        (t;(null (rest history))
         (make-dispatch-tree :history  history
                             :common   common
                             :merged   (make-list (length common))
                             :outcome  (call-history-entry-outcome (first history))))
        #+ (or)
        (t
         ;; EQL specializers!
         ;(break "Yoo ~a" (mapcar #'call-history-entry-outcome history))
         (make-dispatch-tree :history  history
                             :common   common
                             :merged   (make-list (length common))
                             :outcome  (mapcar #'call-history-entry-outcome history)
                             ))))))

;;; Merging algorithm:
;;;
;;; 1. when the tree has no children return the outcome
;;; 2. invoke merge on both children
;;; 3. when both children are leafs then try to merge:
;;;
;;;   - when outcomes are the same
;;;   - when "common" stamps are adjacent -> merged
;;;   - when "merged" ranges are adjacent -> merged
;;;
;;; When the tree has no children then it has an outcome (and vice versa, it
;;; doesn't have an outcome when it has children).
;;;
;;; "Common" indexes of both children are never equal because if they were
;;; then they would be already common in the parent.
(defun merge-branches-1 (common-1 common-2
                         merged-1-min merged-1-max
                         merged-2-min merged-2-max)
  ;; This index doesn't require merging - that should be tested above.
  (assert (or common-1 common-2 merged-1-min merged-2-min))
  ;; Two leaf nodes without ranges to merge.
  (when (and (null merged-1-min) (null merged-2-min))
    (return-from merge-branches-1
      (if (or (null common-1) (null common-2))
          ;; One branch requires equality, the other doesn't.
          (values nil nil)
          (case (- common-2 common-1)
            (+1 (values common-1 common-2))
            (-1 (values common-2 common-1))
            (otherwise (values nil nil))))))
  ;; First attempt merging COMMON-X with provided ranges. Initially COMMON-1
  ;; is not adjacent with MERGED-1, but after merging COMMON-2 it may change.
  (macrolet ((try-merge (common min max)
               `(when (and ,common ,min ;; ,max (implicit)
                           (<= (1- ,min) ,common (1+ ,max)))
                  (setf ,min (min ,min ,common)
                        ,max (max ,max ,common)
                        ,common nil)
                  t)))
    (when (try-merge common-1 merged-2-min merged-2-max)
      (try-merge common-2 merged-2-min merged-2-max))
    (when (try-merge common-2 merged-1-min merged-1-max)
      (try-merge common-1 merged-1-min merged-1-max)))
  ;; After attempting to merge commons with ranges if either exists then it is
  ;; a game over, we can't merge them:
  (when (or common-1 common-2)
    (return-from merge-branches-1
      (values nil nil)))
  ;; What is left is an attempt to merge both ranges.
  (cond ((null merged-1-min)
         (values merged-2-min merged-2-max))
        ((null merged-2-min)
         (values merged-1-min merged-1-max))
        ((or (< merged-1-max (1- merged-2-min))
             (< merged-2-max (1- merged-1-min))
             (> merged-1-min (1+ merged-2-max))
             (> merged-2-min (1+ merged-1-max)))
         (values nil nil))
        (t
         (values (min merged-1-min merged-2-min)
                 (max merged-1-max merged-2-max)))))

(defun merge-branches (tree)
  (or (dispatch-tree-outcome tree)
      (let* ((ltree (dispatch-tree-ltree tree))
             (rtree (dispatch-tree-rtree tree))
             (outcome-left  (merge-branches ltree))
             (outcome-right (merge-branches rtree)))
        (when (and outcome-left (equal outcome-left outcome-right))
          (loop for index from 0
                for lcommon in (dispatch-tree-common ltree)
                for (lmin lmax) in (dispatch-tree-merged ltree)
                for rcommon in (dispatch-tree-common rtree)
                for (rmin rmax) in (dispatch-tree-merged rtree)
                collect (if (or lcommon rcommon lmin rmin)
                            (multiple-value-bind (l r)
                                (merge-branches-1 lcommon rcommon
                                                  lmin lmax rmin rmax)
                              (unless l
                                (return-from merge-branches nil))
                              (list l r))
                            nil)
                  into merged
                finally
                   (setf 
                    (dispatch-tree-ltree tree) nil
                    (dispatch-tree-rtree tree) nil
                    (dispatch-tree-lhist tree) nil
                    (dispatch-tree-rhist tree) nil
                    (dispatch-tree-split tree) nil
                    (dispatch-tree-merged tree) merged
                    (dispatch-tree-outcome tree) outcome-left))
          outcome-left))))

(defun compute-dispatch-tree (gf)
  (ext:when-let ((history (clos::generic-function-call-history gf)))
    (flet ((indexes ()
             (loop for argument-index from 0
                   for (class-specialized-p . eql-specializers)
                     in (clos::generic-function-spec-list gf)
                   when class-specialized-p
                     collect argument-index)))
      (let ((tree (generate-branch history (indexes))))
        (merge-branches tree)
        tree))))


(defun get-stamp (n)
  (intern (format nil "STAMP-~a" n)))

;;; FIXME even when certain arguments have a specializer profile, our expansion
;;; may be simple (go :cache-miss) due to lack of history. In that case we have
;;; unused bindings.
;;;
;;; NOTE for some reason declaring stamps ignorable breaks performance.
(defun expand-stamp-bindings (gf)
  (flet ((pops (n)
           (if (zerop n)
               `(pop args*)
               `(progn
                  ,@(loop repeat n collect `(pop args*))
                  (pop args*)))))
    (loop with skip = 0
          for i from 0
          for (class . eqls) in (clos::generic-function-spec-list gf)
          for name = (get-stamp i)
          if (or class eqls)
            collect `(,name (si:instance-sig-get ,(pops skip)))
            and do (setf skip 0)
          else
            do (incf skip))))

;;; Codegen based on the dispatch tree
(defun %expand-check-common (tree)
  (loop for index from 0
        for stamp in (dispatch-tree-common tree)
        when stamp
          collect `(= ,(get-stamp index) ,stamp)))

(defun expand-check-common (tree)
  (let ((checks (%expand-check-common tree)))
    (case (length checks)
      (0 nil)
      (1 (first checks))
      (otherwise `(and ,@checks)))))

(defun %expand-check-merged (tree)
  (loop for index from 0
        for (min max) in (dispatch-tree-merged tree)
        when min
          collect `(<= ,min ,(get-stamp index) ,max)))

(defun expand-check-merged (tree)
  (let* ((common-tests (%expand-check-common tree))
         (merged-tests (%expand-check-merged tree))
         (checks (append common-tests merged-tests)))
    (case (length checks)
      (0 nil)
      (1 (first checks))
      (otherwise `(and ,@checks)))))

(defun expand-check-split (tree)
  (let* ((split (dispatch-tree-split tree))
         (ltree (dispatch-tree-ltree tree))
         (rtree (dispatch-tree-rtree tree))
         (index (car split))
         (stamp (cdr split)))
    `(if (<= ,(get-stamp index) ,stamp)
         ,(expand-branch ltree)
         ,(expand-branch rtree))))


;;;
;;; This means that ARG1 has EQL specializers on VAL1, VAL2 and VAL3 but we
;;; don't know whether it has a specialization for its class.
;;;
;;; ARG2 has specializations for VAL1, VAL2 and for its class
(defun expand-outcome (tree)
  `(funcall ,(dispatch-tree-outcome tree) args nil)

  #+ (or) ;; or this?
  (loop for (spec . outcome) in (dispatch-tree-history tree)
        when (class-eqls spec)
          )

  ;; Some arguments may have EQL-specializers. If that is true then the last
  ;; step of a dispatch is:
  #+ (or)
  `(case argument
     (eql-val-1 :eql-outcome-1)
     (eql-val-2 :eql-outcome-2)
     (eql-val-3 :eql-outcome-3)
     (otherwise ,(dispatch-tree-outcome tree))))

(defun expand-cache-miss (tree)
  (declare (ignore tree))
  `(go :cache-miss))

;;; Who doesn't love wild and unnecessary extensions? Let's dispatch based on
;;; the typespec!
#+ (or)
(defun expand-outcome* (tree)
  `(typecase arg
     ((eql eql-val-1) :eql-outcome-1)
     ((eql eql-val-2) :eql-outcome-2)
     ((eql eql-val-3) :eql-outcome-3)
     ((integer 10 15) :int-outcome-4)
     (otherwise ,(dispatch-tree-outcome tree))))

(defun expand-branch (tree)
  (when (null tree)
    (return-from expand-branch
      (expand-cache-miss tree)))
  (ext:if-let ((outcome (dispatch-tree-outcome tree)))
    (ext:if-let ((merged-test (expand-check-merged tree)))
      `(if ,merged-test
           ,(expand-outcome tree)
           ,(expand-cache-miss tree))
      (expand-outcome tree))
    (ext:if-let ((common-test (expand-check-common tree)))
      `(if ,common-test
           ,(expand-check-split tree)
           ,(expand-cache-miss tree))
      (expand-check-split tree))))

(defun expand-fastgf-discriminator (gf)
  (let* ((tree (compute-dispatch-tree gf))
         (code (expand-branch tree))
         (lets (expand-stamp-bindings gf)))
    `(lambda (&rest args)
       #+speed (declare (optimize (speed 3) (safety 0) (debug 0)))
       #-speed (incf (clos::generic-function-cache-pass ,gf))
       (prog* ((args* args)
               ,@lets)
          (declare (ignorable ,@(mapcar #'car lets))
                   (fixnum ,@(mapcar #'car lets)))
          (return ,code)
        :cache-miss
          (dbg* "[fast discriminator] cache miss~%")
          (return (apply #'fastgf-handle-cache-miss ,gf args))))))


;;; NOTE metastability issues are resolved by usning unoptimized dispatch when
;;; we enter a path that needs recomputation. See UNOPTIMIZED-DISPATCH-P and its
;;; callers.

(defvar *dirty-gfs* '())
(defvar *known-gfs* '())
(defvar *unoptimized-functions-p* t)
(defvar *compiled-discriminators* '())

;;; This function computes the discriminating function. This is the heart of the
;;; Fast Generic Function Dispatch.
(defun compile-fastgf-discriminator (gf)
  (let ((body (expand-fastgf-discriminator gf)))
    (values (compile nil body) nil)))

;;; FIXME assuming warm cache, this discriminator compiled with CCMP this works
;;; comparably to our baseline method, but with BCMP it is much slower; so even
;;; after we polish CCMP expansion for best speed, we need to either introduce a
;;; fast interpreter, or fallback to baseline method with BCMP.
;;; 
;;; NOTE benchmarks show, that updating gf stats slots does not break
;;; performance (recompiled/cache-pass/cache-miss).
(defvar *depth* 0)
(defun compute-fastgf-discriminator (gf &aux (*depth* (1+ *depth*)))
  (dbg* "[fast] [~a] Recomputing function for ~a (~a / ~a)~%"
        *depth*
        (clos:generic-function-name gf)
        (clos::generic-function-recompiled gf)
        (length (generic-function-call-history gf)))
  (incf (clos::generic-function-recompiled gf))
  ;; When lambda list changes incompatibly, we should update the call history.
  ;; Perhaps we could truncate/update missing specs with T specializer?
  (compile-fastgf-discriminator gf))

(defun invalidate-fastgf-discriminator (gf)
  (let ((discriminator (make-invalidated-fastgf-discriminator gf)))
    (clos:set-funcallable-instance-function gf discriminator)
    discriminator))

(defun revalidate-fastgf-discriminator (gf)
  (let ((discriminator #+speed (compile-fastgf-discriminator gf)
                       #-speed (compute-fastgf-discriminator gf)))
    (clos:set-funcallable-instance-function gf discriminator)
    discriminator))

(defun unoptimized-dispatch-p (gf)
  (or *unoptimized-functions-p*
      (member gf *compiled-discriminators*)
      ;; XXX badaid(s), but above should suffice!
      (member gf (list #'generic-function-call-history))
      ;; vvvvv can't be, because then we never recompile new functions
      (null (clos::generic-function-call-history gf))
      *compiled-discriminators* ;; slow path on recompile!
      ))

(defun fastgf-handle-cache-miss (gf &rest args)
  (when (unoptimized-dispatch-p gf)
    (let ((*compiled-discriminators* (list* gf *compiled-discriminators*)))
      (dbg* "[fast discriminator] unoptimize ~a~%" (clos:generic-function-name gf))
      (return-from fastgf-handle-cache-miss
        (apply (clos::unoptimized-discriminator gf) args))))
  (let ((*compiled-discriminators* (list* gf *compiled-discriminators*)))
    #-speed
    (let ((name (clos:generic-function-name gf)))
      (incf (clos::generic-function-cache-fail gf))
      (dbg* "[fastgf] cache miss for ~a~%" name))
    (ext:if-let ((func (compute-effective-outcome gf args)))
      (progn
        (apply #'pseudocall gf func args)
        (revalidate-fastgf-discriminator gf)
        (funcall func args nil))
      (apply #'no-applicable-method gf args))))

(defun make-invalidated-fastgf-discriminator (gf)
  (pushnew gf *dirty-gfs*)
  (pushnew gf *known-gfs*)
  (lambda (&rest args)
    (if (unoptimized-dispatch-p gf)
        ;; FIXME we should use the function with static cache.
        (apply (clos::unoptimized-discriminator gf) args)
        (progn
          (dbg* "[inva discriminator] cache miss")
          (apply #'fastgf-handle-cache-miss gf args)))))



(defun clos::std-compute-discriminating-function (gf)
  (values (make-invalidated-fastgf-discriminator gf) nil))

;; (defun clos::std-compute-discriminating-function (gf)
;;   (clos::unoptimized-discriminator gf))

;; (defun clos::std-compute-discriminating-function (gf)
;;   (clos::soft-legacy-discriminator gf))

(defun enable-fgf ()
  (let ((start (get-universal-time)))
    (dbg* "--- Enabling FastGF dispatch, please wait...~%")
    (let ((count (length *dirty-gfs*))
          (names (mapcar #'clos:generic-function-name *dirty-gfs*)))
      (setf *unoptimized-functions-p* nil)
      (loop for name in names
            for i from 1
            for gf in *dirty-gfs*
            do (dbg* "[~4a/~4a] ~a ... ~%" i count name)
               (invalidate-fastgf-discriminator gf)
               (dbg* "... ~a~%" `(OK))))
    (setf *dirty-gfs* '())
    (dbg* "--- Enabling FastGF dispatch completed in ~As!~%"
          (- (get-universal-time) start))))

(defun print-gf (gf &optional (stream *standard-output*))
  (print-unreadable-object (gf stream :type nil)
    (format stream "~38a [~2a/~8a] (~4a/~4a)"
            (clos:generic-function-name gf)
            (clos::generic-function-cache-fail gf)
            (clos::generic-function-cache-pass gf)
            (clos::generic-function-recompiled gf)
            (length (clos::generic-function-call-history gf))))
  gf)

(defun print-fast-gfs ()
  (let ((total-fail 0)
        (total-call 0)
        (total-comp 0)
        (total-zero 0))
    (format t "  ~38a [~5a/~5a] (~4a/~4a)~%~a~%"
           "Function name" "miss" "call" "comp" "hist"
           "--------------------------------------------------------------------")
   (dolist (gf *dirty-gfs*)
     (incf total-call (clos::generic-function-cache-pass gf))
     (incf total-fail (clos::generic-function-cache-fail gf))
     (incf total-comp (clos::generic-function-recompiled gf))
     (when (zerop (length (clos::generic-function-call-history gf)))
       (incf total-zero))
     (print-gf gf)
     (terpri))
    (format t "~a~%  ~38a [~9,5f %] (~4a/~4a)~%"
            "--------------------------------------------------------------------"
            (format nil "Totals, ~a/~a never called"
                    total-zero
                    (length *dirty-gfs*))
            (* 100 (/ total-fail total-call))
            total-comp
            total-zero)))


;;; Smoke tests for fast gf

#+ (or)
(progn
  (defclass fast-generic-function (standard-generic-function)
    ((call-history :initform '() :accessor generic-function-call-history)))

  (defmacro define-fast-gf (name args &rest options)
    `(defgeneric ,name ,args
       ,@options
       (:generic-function-class fast-generic-function)))

  (defmethod clos:compute-discriminating-function
      ((gf fast-generic-function))
    (fastgf-discriminator gf))

;;; FIXME when the function is redefined with a different specializer arity, we
;;; should invalidate the call history.

;;; ARG3 in this test is purposefully not specialized(!)
  (define-fast-gf test-fun (arg1 arg2 arg3)
    (:method ((arg1 integer) (arg2 integer) arg3)
      (format t "~&~a~%" `(hi-int-int ,arg1 ,arg2)))
    (:method ((arg1 fixnum) (arg2 fixnum) arg3)
      (format t "~&~a~%" `(hi-fix-fix ,arg1 ,arg2)))
    (:method ((arg1 string) (arg2 integer) arg3)
      (format t "~&~a~%" `(hi-str-int ,arg1 ,arg2)))
    (:method ((arg1 string) (arg2 string) arg3)
      (format t "~&~a~%" `(hi-str-str ,arg1 ,arg2)))
    (:method ((arg1 (eql 42)) (arg2 integer) arg3)
      (format t "~&~a~%" `(hi-e42-int ,arg1 ,arg2)))
    (:method ((arg1 integer) (arg2 (eql 13)) arg3)
      (format t "~&~a~%" `(hi-int-e13 ,arg1 ,arg2)))
    (:method ((arg1 integer) (arg2 symbol) arg3)
      (format t "~&~a~%" `(hi-int-sym ,arg1 ,arg2))))

  (pseudocall #'test-fun 15       3     "uu")
  (pseudocall #'test-fun "HE"     3     14)
  (pseudocall #'test-fun "HE"     "HE"  3.22)

;;; cached call
  #+ (or) (test-fun "HE" 3 2)
;;; eql specializers for classes
  #+ (or) (test-fun 15 3 1)
;;; cache miss no method
  #+ (or) (test-fun "HE" 3.13 2)
;;; cache miss yes method
  #+ (or) (test-fun 13 'x 2)

  ;; (clos::generic-function-call-history #'test-fun)

  ;; (((#<The BUILT-IN-CLASS STRING>
  ;;          #<The BUILT-IN-CLASS STRING>
  ;;          #<The BUILT-IN-CLASS SINGLE-FLOAT>)
  ;;   . #<compiled-closure 0x7f29a0473180>)
  ;;  ((#<The BUILT-IN-CLASS STRING>
  ;;          #<The BUILT-IN-CLASS FIXNUM>
  ;;          #<The BUILT-IN-CLASS FIXNUM>)
  ;;   . #<compiled-closure 0x7f29a04735d0>)
  ;;  ((#<The BUILT-IN-CLASS FIXNUM>
  ;;          #<The BUILT-IN-CLASS FIXNUM>
  ;;          #<The BUILT-IN-CLASS STRING>)
  ;;   . #<compiled-closure 0x7f29a0473a50>))

  nil)




;;; clim visualizer
#+mcclim
(progn
  (defvar *win*)

  (defmacro with-win ((var) &body body)
    `(let ((*standard-output* *win*)
           (,var *win*))
       (clim:with-drawing-options
           (,var :text-style (clim:make-text-style :fix nil :normal)
                 :ink clim:+grey8+)
         (fresh-line ,var)
         ,@body
         (finish-output ,var)
         (clim:change-space-requirements ,var))))

  (clim:define-application-frame fastgf-viz ()
    ((simulation :initarg :simulation :accessor simulation :initform nil))
    (:geometry :width 1280 :height 720)
    (:pane :application :display-function (lambda (frame *win*)
                                            (show-simulation (simulation frame)))
           :text-style (clim:make-text-style :fix nil nil))
    (:reinitialize-frames t))

  (clim:find-application-frame 'fastgf-viz)

  (define-fastgf-viz-command (com-visualize :name t)
      ((arg 'generic-function))
    (setf (simulation clim:*application-frame*) arg))

  (defun spec-role (tree entry index)
    (cond ((nth index (dispatch-tree-merged tree))
           :merged)
          ((nth index (dispatch-tree-common tree))
           :common)
          ((alexandria:when-let* ((split (dispatch-tree-split tree))
                                  (split-index (car split))
                                  (split-stamp (cdr split))
                                  (class-stamp (class-stamp (elt (car entry) index))))
             (and (= split-index index)
                  (= split-stamp class-stamp)))
           :split)
          ((not (member index (dispatch-tree-to-check tree)))
           :empty)
          (t
           :other)))

  (defun format-dispatch-node (object entry stream)
    (loop with (call . outcome) = entry
          for ind from 0
          for spec in call
          do (clim:with-drawing-options
                 (t :ink (ecase (spec-role object entry ind)
                           (:common clim:+blue+)
                           (:merged clim:+deep-pink+)
                           (:split  clim:+red+)
                           (:empty  clim:+light-slate-grey+)
                           (:other  clim:+black+)))
               (clim:surrounding-output-with-border
                   (t :ink (if (class-eqls spec)
                               clim:+light-yellow+
                               clim:+transparent-ink+)
                      :outline-ink (if (class-eqls spec)
                                       clim:+black+
                                       clim:+transparent-ink+)
                      :filled t :padding 0 :line-thickness 1)
                 (format t "~4d" (class-stamp spec))))))

  (defun format-dispatch-tree (object stream)
    (clim:formatting-table (stream)
      (when (dispatch-tree-lhist object)
        (clim:surrounding-output-with-border (stream :padding 0 :filled t :ink clim:+light-pink+)
          (clim:formatting-column (stream)
            (loop for entry in (dispatch-tree-lhist object)
                  do (clim:formatting-cell (stream)
                       (format-dispatch-node object entry stream))))))
      (when (dispatch-tree-rhist object)
        (clim:surrounding-output-with-border (stream :padding 0 :filled t :ink clim:+light-steel-blue+)
          (clim:formatting-column (stream)
            (loop for entry in (dispatch-tree-rhist object)
                  do (clim:formatting-cell (stream)
                       (format-dispatch-node object entry stream))))))
      (let ((strays (loop for entry in (dispatch-tree-history object)
                          unless (or (member entry (dispatch-tree-lhist object))
                                     (member entry (dispatch-tree-rhist object)))
                            collect entry)))
        (when strays
          (clim:surrounding-output-with-border (stream :padding 0 :filled t :ink clim:+light-grey+)
            (clim:formatting-column (stream)
              (loop for entry in strays
                    do (clim:formatting-cell (stream)
                         (format-dispatch-node object entry stream)))))))))

  (clim:define-presentation-type dispatch-tree ())
  (clim:define-presentation-method clim:present
      (object (type dispatch-tree) stream view &key)
    (format-dispatch-tree object stream))

  (defun format-dispatch-outcome (object stream)
    (clim:with-drawing-options
        (stream :ink clim:+dark-green+ :text-face :bold :text-size :large)
      (format stream "0x~x" (si:pointer object))))

  (clim:define-presentation-type dispatch-outcome ())
  (clim:define-presentation-method clim:present
      (object (type dispatch-outcome) stream view &key)
    (format-dispatch-outcome object stream))

  (defun show-dispatch-tree (tree)
    (with-win (*win*)
      (terpri)
      (clim:format-graph-from-roots
       (list tree)
       (lambda (object stream)
         (declare (ignore stream))
         #+ (or)    ;FIXME (eql specs?) should work when we are done with
                    ;fastgf, temporarily specializations does not "catch"
         (if (arrayp object)
             (clim:present object 'dispatch-tree)
             (clim:present object 'dispatch-outcome))
         #- (or)
         (if (arrayp object)
             (format-dispatch-tree object stream)
             (format-dispatch-outcome object stream)))
       (lambda (object)
         (and (vectorp object)
              (progn
                (assert (eq (not (null (dispatch-tree-ltree object)))
                            (not (null (dispatch-tree-rtree object)))))
                (if (dispatch-tree-ltree object)
                    (list (dispatch-tree-ltree object)
                          (dispatch-tree-rtree object))
                    (list (dispatch-tree-outcome object))))))
       :merge-duplicates t
       :maximize-generations t
       :stream *win*)
      (terpri)
      (print (expand-branch tree))))

  (defun show-simulation (simulation)
    (format *win* "Dispatch for ~s~%" simulation)
    (when (null simulation)
      (return-from show-simulation))
    (format *win* "Specializer profile: ~a~%" (clos::generic-function-spec-list simulation))
    (format *win* "History:~%~A~%" (clos::generic-function-call-history simulation))
    (format *win* "History*:~%~A~%"
            (loop for entry in (clos::generic-function-call-history simulation)
                  for specs = (call-history-entry-specializers entry)
                  do (dbg* "Analyzing ~a~%" entry)
                  collect (cons (mapcar #'class-stamp specs)
                                (call-history-entry-outcome entry))))
    ;(clim:window-clear *win*)
    (let* ((tree (compute-dispatch-tree simulation)))
      (clim:surrounding-output-with-border (*win*)
        (show-dispatch-tree tree)))
    (clim:change-space-requirements *win*)
    (finish-output *win*)))
