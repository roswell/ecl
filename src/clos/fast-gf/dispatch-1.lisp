#-mcclim
(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload 'mcclim))

(defpackage "EU.TURTLEWARE.FASTGF"
  (:use "CLIM-LISP" "CLIM-USER")
  (:local-nicknames ("ALX" "ALEXANDRIA")))

(in-package "EU.TURTLEWARE.FASTGF")



;;; The decision tree build procedure is inspired by the macro STRING-CASE:
;;; http://pvk.ca/Blog/Lisp/string_case_bis.html

;;; Representation:
;;; 
;;; Each call is represented by a N-element[1] sequence. Each element is
;;; either a CLASS or a cons (CLASS . EQL-SPECIALIZER-OBJECTs).
;;;
;;; [1] N is a number of specializable generic function arguments

(defparameter *stamp-ids* 0)
(defparameter *simulation-hash* (make-hash-table))

(defun class-stamp (spec)
  (let ((class (if (atom spec) spec (car spec))))
    (or (gethash class *simulation-hash*)
        (setf (gethash class *simulation-hash*)
              (incf *stamp-ids*)))))

(defun class-eqls (spec)
  (if (atom spec)
      nil
      (cdr spec)))

(defun call-history-entry-outcome (entry)
  (cdr entry))

(defun call-history-entry-specializers (entry)
  (car entry))

(defun pseudocall (call outcome)
  (cons (loop for spec in call
              for class-name = (if (atom spec) spec (car spec))
              for class-eqls = (if (atom spec)  nil (cdr spec))
              ;; for class = (find-class class-name)
              for stamp = (class-stamp class-name)
              if class-eqls
                collect (list* class-name class-eqls)
              else
                collect class-name)
        outcome))

;;; Populate stamps
(pseudocall '(number integer real float single-float double-float complex) nil)
(pseudocall '(array simple-array string vector bitvector) nil)

(defmacro define-simulation (name profile &rest calls)
  (loop for (specs . outcome) in calls
        do (assert (= (length profile) (length specs))))
  `(defparameter ,name
     (list ',profile
           ,@(mapcar (lambda (call)
                       (destructuring-bind (specs outcome) call
                         `(pseudocall ',specs ,outcome)))
                     calls))))

(defun simulation-profile (simulation)
  (first simulation))

(defun simulation-history (simulation)
  (rest simulation))

(defun simulation-indexes (simulation)
  (loop for p in (simulation-profile simulation)
        for i from 0
        when p
          collect i))

(define-simulation *smoke-test* (t t t nil)
  ((number number integer array)  :out1)
  ((integer integer number array) :out1))

(define-simulation *bigger-test* ((3 2 2) t t t nil)
  ((integer        string       integer string unspec-1) :out1)
  (((integer 3)    symbol       integer string unspec-1) :out1)
  (((integer 2)    string       integer string unspec-1) :out2)
  (((integer 2)    symbol       integer string unspec-1) :out2)
  ((number         symbol       integer string unspec-2) :out1)
  ((number         string       integer string unspec-3) :out1)
  ((number         string       integer string unspec-3) :out2)
  ((float          string       integer string unspec-4) :out1)
  ((float          string        fixnum string unspec-5) :out1)
  ((float          bolloc  double-float string unspec-6) :out2)
  ((float          vector         float string unspec-7) :out2)
  ((double-float     list  single-float string unspec-8) :out1)
  ((long-float   sequence  single-float string unspec-9) :out1))

(define-simulation *test-merge* (t t t nil)
  ((number  number       integer array)  :out1)
  ((integer integer      number  array)  :out1)
  ((float   float        real    string) :out1)
  ((real    real         real    vector) :out1))

(define-simulation *test-merge2* (t t t nil)
  ((number  number       integer array)  :out1)
  ((integer integer      number  array)  :out1)
  ((float   float        real    string) :out1)
  ((real    real         real    vector) :out1)
  ((complex vector       real    vector) :out1))

(define-simulation *test-merge3* (t t t nil)
  ((number  number       integer array)  :out1)
  ((integer integer      number  array)  :out1)
  ((float   float        real    string) :out1)
  ((real    real         real    vector) :out1)
  ((complex vector       real    vector) :out1)
  ((complex string       real    vector) :out1)
  ((complex real         real    vector) :out1)
  ((complex complex      real    vector) :out1))

(define-simulation *break-ties* (t t t t)
  ((number  number       integer array)  :out1)
  ((integer integer      number  array)  :out1)
  ((float   float        real    string) :out2)
  ((real    real         real    vector) :out2))

(define-simulation *break-ties2* (t t t t)
  ((number  number       integer array)  :out1)
  ((integer integer      number  array)  :out2)
  ((float   float        real    string) :out2)
  ((real    real         real    vector) :out1))

(define-simulation *break-ties3* (t t t t)
  ((number  number       real  array)      :out1)
  ((integer integer      integer   array)  :out2)
  ((float   float        float string)     :out1)
  ((real    real         number    vector) :out2))

(define-simulation *eql-spec-test* ((3) t t)
  (((number  3)  number real)     :out1)
  (((integer 3) number integer)   :out2)
  (((real    3) number integer)   :out3)
  (((real    3) number (integer 2)) :out4)
  (((real    3) number (integer 3)) :out5)
  ((number  number real)            :out6)
  ((integer number integer)         :out7)
  ((string number string) :out8)
  ((string number vector) :out8))

(define-simulation *sample* (t t)
  ((string string)     :out1)
  ((integer string)    :out2)
  ((string (integer 12))   :out3))

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
               ;; better.  We base our decision on the number of unique
               ;; outcomes (per the history length). Smaller value means more
               ;; uniform distribution. This could be benefit additionally
               ;; from promoting adjacent branches in the same batch.
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

(defstruct (dispatch-tree (:type vector))
  history common merged outcome to-check split lhist rhist ltree rtree)

(defun print-dispatch-tree (tree)
  (when tree
   (let ((*print-right-margin* 1024))
     (format *debug-io* "History:~%~{->  ~a~%~}" (dispatch-tree-history tree))
     (format *debug-io* "Common: ~s~%" (dispatch-tree-common tree))
     (format *debug-io* "Split: ~s~%" (dispatch-tree-split tree))
     (format *debug-io* "To check: ~s~%" (dispatch-tree-to-check tree))
     (format *debug-io* ":: ltree --------------------~%")
     (print-dispatch-tree (dispatch-tree-ltree tree))
     (format *debug-io* ":: rtree --------------------~%")
     (print-dispatch-tree (dispatch-tree-rtree tree))
     (format *debug-io* ":: --------------------------~%"))))

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
        ((null (rest history))
         (make-dispatch-tree :history  history
                             :common   common
                             :merged   (make-list (length common))
                             :outcome  (call-history-entry-outcome (first history))))
        (t
         ;; EQL specializers!
         (make-dispatch-tree :history  history
                             :common   common
                             :merged   (make-list (length common))
                             :outcome  (mapcar #'call-history-entry-outcome history)))))))

(let* ((simulation *sample*)
       (history (simulation-history simulation))
       (indexes (simulation-indexes simulation))
       (tree (generate-branch history indexes)))
  (print-dispatch-tree tree))

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

(defun %expand-check-common (tree)
  (loop for index from 0
        for stamp in (dispatch-tree-common tree)
        when stamp
          collect `(= (svref args ,index) ,stamp)))

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
          collect `(<= ,min (svref args ,index) ,max)))

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
    `(if (<= (svref args ,index) ,stamp)
         ,(expand-branch ltree)
         ,(expand-branch rtree))))


;;;
;;; This means that ARG1 has EQL specializers on VAL1, VAL2 and VAL3 but we
;;; don't know whether it has a specialization for its class.
;;;
;;; ARG2 has specializations for VAL1, VAL2 and for its class
(defun expand-outcome (tree)
  #+ (or)
  (loop for (spec . outcome) in (dispatch-tree-history tree)
        when (class-eqls spec)
          )

  (dispatch-tree-outcome tree)

  #+ ()
  ;; Some arguments may have EQL-specializers. If that is true then the last
  ;; step of a dispatch is:
  `(case argument
     (eql-val-1 :eql-outcome-1)
     (eql-val-2 :eql-outcome-2)
     (eql-val-3 :eql-outcome-3)
     (otherwise ,(dispatch-tree-outcome tree))))

;;; Who doesn't love wild and unnecessary extensions? Let's dispatch based on
;;; the typespec!
(defun expand-outcome* (tree)
  `(typecase arg
     ((eql eql-val-1) :eql-outcome-1)
     ((eql eql-val-2) :eql-outcome-2)
     ((eql eql-val-3) :eql-outcome-3)
     ((integer 10 15) :int-outcome-4)
     (otherwise ,(dispatch-tree-outcome tree))))

(defun expand-branch (tree)
  (alx:if-let ((outcome (dispatch-tree-outcome tree)))
    (alx:if-let ((merged-test (expand-check-merged tree)))
      `(if ,merged-test
           ,(expand-outcome tree)
           (error 'cache-miss))
      (expand-outcome tree))
    (alx:if-let ((common-test (expand-check-common tree)))
      `(if ,common-test
           ,(expand-check-split tree)
           (error 'cache-miss))
      (expand-check-split tree))))


;;; In history we have arguments with or without the EQL-specialization, also
;;; the function may have methods EQL-specialized on arguments that are not
;;; yet covered in the history (so we don't know the outcome).
;;;
;;; The generic function specializer profile for each required argument stores
;;; a union of all specializers in the form (CLASS . EQL-SPECIALIZED-OBJECTS).
;;;
;;; Q: What happens when the eql-specialized object changes its class?
;;; A: The former and new class (in generic functions history) should be
;;; invalidated when that happens (like when you change the class itself).
;;;
;;; For example:
#+ (or)
(progn
  (defmethod example-method ((arg1 (eql val1)) (arg2 (eql val1))))
  (defmethod example-method ((arg1 (eql val1)) (arg2 (eql val2))))
  (defmethod example-method ((arg1 (eql val1)) (arg2 some-class)))

  (defmethod example-method ((arg1 (eql val2)) (arg2 some-class)))
  (defmethod example-method ((arg1 (eql val3)) (arg2 some-class))))
;;; 
#+ (or) (case arg1
          (arg1-eql-val1-from-history
           (case arg2
             (arg2-eql-val1-from-history #:outcome-1)
             (arg2-eql-val2-from-history #:outcome-2)
             (otherwise                  #:outcome-3)))
          (arg1-eql-val2-from-history
           (case arg2
             (arg2-eql-val1-from-profile #:cache-miss)
             (arg2-eql-val2-from-profile #:cache-miss)
             (otherwise                  #:outcome-4)))
          (arg1-eql-val3-from-history #:outcome-5)
          (otherwise                  #:cache-miss))


(defvar *win* (clim:open-window-stream :width 800 :height 600))

(defmacro with-win ((var) &body body)
  `(let ((*standard-output* *win*)
         (,var *win*))
     (clime:with-temporary-margins (,var :left 5 :top 20)
       (clim:with-drawing-options
           (,var :text-style (clim:make-text-style :fix nil :normal)
                 :ink clim:+grey8+)
         (fresh-line ,var)
         ,@body
         (finish-output ,var)
         (clim:change-space-requirements ,var)))))

(clim:define-presentation-type dispatch-tree ())

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

(clim:define-presentation-method clim:present
    (object (type dispatch-tree) stream view &key)
  (clim:formatting-table (stream :equalize-column-widths t)
    (loop for entry in (dispatch-tree-history object)
          for (call . outcome) = entry
          for ink = (cond ((member entry (dispatch-tree-lhist object))
                           clim:+light-pink+)
                          ((member entry (dispatch-tree-rhist object))
                           clim:+light-blue+)
                          (t
                           clim:+light-grey+))
          do (clim:surrounding-output-with-border
                 (t :padding 0 :padding-bottom 2 :filled t :ink ink)
               (clim:formatting-row (stream)
                 (loop for ind from 0
                       for spec in call
                       do (clim:formatting-cell
                              (stream :align-x :right :min-width '(3 :character))
                            (clim:with-drawing-options
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
                                (format t "~2d " (class-stamp spec))))))))
             (terpri))))

(clim:define-presentation-type dispatch-outcome ())
(clim:define-presentation-method clim:present
    (object (type dispatch-outcome) stream view &key)
  (clim:with-drawing-options
      (t :ink clim:+dark-green+
         :text-face :bold
         :text-size :large)
    (princ object)))

(defun show-dispatch-tree (tree)
  (with-win (*win*)
    (terpri)
    (clim:format-graph-from-roots
     (list tree)
     (lambda (object stream)
       (declare (ignore stream))
       (if (vectorp object)
           (clim:present object 'dispatch-tree)
           (clim:present object 'dispatch-outcome)))
     (lambda (object)
       (and (vectorp object)
            (progn
              (assert (eq (not (null (dispatch-tree-ltree object)))
                          (not (null (dispatch-tree-rtree object)))))
              (if (dispatch-tree-ltree object)
                  (list (dispatch-tree-ltree object)
                        (dispatch-tree-rtree object))
                  (list (dispatch-tree-outcome object))))))
     :stream *win*)
    (terpri)
    (print (expand-branch tree))))



(defun show-simulation (simulation)
  (format *win* "Specializer profile: ~a~%" (simulation-profile simulation))
  (clim:window-clear *win*)
  (clim:formatting-item-list (*win* :n-columns 2)
    (let* ((history (simulation-history simulation))
           (indexes (simulation-indexes simulation))
           (tree (generate-branch history indexes)))
      ;#+ (or)
      (clim:formatting-cell (*win*)
        (clim:surrounding-output-with-border (*win*)
          (show-dispatch-tree tree)))
      (merge-branches tree)
      (clim:formatting-cell (*win*)
        (clim:surrounding-output-with-border (*win*)
          (show-dispatch-tree tree)))))
  (clim:change-space-requirements *win*)
  (finish-output *win*))

(clim:define-application-frame sim ()
  ((simulation :initarg :simulation :accessor simulation))
  (:geometry :width 1280)
  (:pane :application :display-function (lambda (frame *win*)
                                          (show-simulation (simulation frame))))
  (:reinitialize-frames t))


(clim:find-application-frame 'sim :simulation *test-merge2*)

;(show-simulation *test-merge3*)

#+ ()
(let* ((simulation *sample*)
       (history (simulation-history simulation))
       (indexes (simulation-indexes simulation))
       (tree (generate-branch history indexes)))
  (print-dispatch-tree tree))

