(defpackage "EU.TURTLEWARE.FAST-GF"
  (:use "CL"))
(in-package "EU.TURTLEWARE.FAST-GF")

(defun compute-effective-function (gf args)
  (let ((classes (mapcar #'class-of args)))
    (multiple-value-bind (method-list ok)
        (mop:compute-applicable-methods-using-classes gf classes)
      (when (not ok)
        (format *debug-io* "EQL specializer ahead! slow path.~%")
        (setf method-list (compute-applicable-methods gf args)))
      (if (null method-list)
          nil
          (mop:compute-effective-method-function
           gf
           (mop:generic-function-method-combination gf)
           method-list)))))

(defgeneric fraudulently-compile-function (gf)
  (:method ((gf standard-generic-function)) nil))


;;; This is the implementation of PCL generic dispatch function. It is much
;;; slower than what ECL does, because it uses C code in src/c/clos/gfun.d, so
;;; that the cache access is much faster. I'm implementing it here to have a
;;; baseline and a reference. After all is worked out, we will comapre both C
;;; implementatinos in the benchmark.
;;;
;;; Simplifications:
;;; - assume a single thread
;;; - ignore frame optimizations (bytevm calling convention)
;;; - cache is unlimited and does not eject old entries
;;;

;;; Cache is a table of entries:
;;;
;;; [key=[gf spec1 spec2] val=effective-method]

(defstruct entry key val)
(defstruct (cache (:constructor %make-cache)) keys table)

(defun make-cache (key-size cache-size)
  (let ((cache (%make-cache))
        (table (make-hash-table :size cache-size :test #'equal))
        (keys (make-array key-size :fill-pointer 0
                                   :adjustable nil
                                   :initial-element nil)))
    (setf (cache-table cache) table
          (cache-keys cache) keys)
    cache))

(defun fill-spec-vector (vector gf args)
  (setf (aref vector 0) gf)
  (setf (fill-pointer vector) 1)
  (loop for (class-p . eql-specs) in (mop::generic-function-spec-list gf)
        for arg in args
        for eql = (member arg eql-specs)
        do (if eql
               (vector-push-extend eql vector)
               (vector-push-extend (class-of arg) vector)))
  vector)

(defun std-search-cache (cache gf args)
  (fill-spec-vector (cache-keys cache) gf args)
  (let* ((table (cache-table cache))
         (keys (cache-keys cache))
         (entry (gethash keys table)))
    (if entry
        (format *debug-io* "~a -> ~a~%" keys entry)
        (setf entry (make-entry)
              ;; we copy keys to avoid EQ sharing.
              (gethash (copy-seq keys) table) entry))
    entry))

(defvar *slow-gf-cache* (make-cache 64 1024))

(defclass slow-generic-function (standard-generic-function)
  ((method-cache :initform *slow-gf-cache* :accessor method-cache)))

(defmethod fraudulently-compile-function ((gf slow-generic-function))
  )

;;; There is a special version with slot_cache.
(defmethod mop:compute-discriminating-function
    ((gf slow-generic-function))
  (format *debug-io* "[slow] Recomputing function for ~a~%" gf)
  (lambda (&rest args)
    (let* ((cache (method-cache gf))
           (func nil))
      (let ((entry (std-search-cache cache gf args)))
        (if (entry-key entry)
            (setf func (entry-val entry))
            (progn
              (setf func (compute-effective-function gf args))
              (when func
                (setf (entry-key entry) t
                      (entry-val entry) func)))))
      (if (null func)
          (apply #'no-applicable-method gf args)
          (funcall func args nil)))))

;;; And this is an implementation of fast generic dispatch.

(defclass fast-generic-function (standard-generic-function)
  ((call-history :initform '() :accessor call-history)))

(defmethod fraudulently-compile-function ((gf fast-generic-function))
  (setf (call-history gf)
        (sort (call-history gf) #'< :key #'si:instance-get-stamp))
  (mop:set-funcallable-instance-function
   gf
   (compile nil `(lambda (args)
                   ;; DISPATCH-FGF-CALL, on cache miss:
                   ;; - [!] calls FGF-UPDATE-HISTORY 
                   ;; - [?] updates the list of GFs that need to be recompiled
                   ;; - [?] upon certain threashold recompiles GF / GFs
                   (format *debug-io* "~&bazinga!~%")
                   #+ (or)
                   (or (dispatch-fgf-call gf args)
                       (dispatch-std-call gf args))))))

;;; This approach is built to fallback to the core mechanism that uses the
;;; method cache when there is a cache miss. We do that by treating the call
;;; history as cache entries like in the old mechanism. This hybrid approach
;;; capitalizes on users compiling the generic function without giving up on the
;;; old (reasonably performant) mechanism.
(defun fgf-update-history (gf args)
  (let ((classes (mapcar #'class-of args)))
   (if (member classes (call-history gf) :test #'equal)
       (format *debug-io* "call-history: cache hit!~%")
       (progn
         (format *debug-io* "call-history: cache miss!~%")
         (push classes (call-history gf))))))

(defun soft-fastgf-discriminator (gf)
  (let ((indexes (loop for argument-index from 0
                       for (class-specialized-p . eql-specializers)
                         in (clos::generic-function-spec-list gf)
                       when class-specialized-p
                         collect argument-index))
        (history (call-history gf))
        (tree (generate-branch history indexes)))))

(defmethod clos:compute-discriminating-function
    ((gf fast-generic-function))
  (format *debug-io* "[fast] Recomputing function for ~a~%" gf)
  ;; When lambda list changes incompatibly, we should update the call history.
  ;; Perhaps we could truncate/update missing specs with T specializer?
  (lambda (&rest args)
    (format *debug-io* "dispatching! ~s~%" args)
    (fgf-update-history gf args)
    (let ((func (compute-effective-function gf args)))
      (if (null func)
          (apply #'no-applicable-method gf args)
          (funcall func args nil)))))


;;; Tests

(defmacro define-fast-gf (name args &rest options)
  `(defgeneric ,name ,args
     ,@options
     (:generic-function-class fast-generic-function)))

(defmacro define-slow-gf (name args &rest options)
  `(defgeneric ,name ,args
     ,@options
     (:generic-function-class slow-generic-function)))

;;; FIXME ECL doesn't take not of gf changed metaclass. rectify that!
(defmacro define-this-gf (name args &rest options)
  `(define-fast-gf ,name ,args ,@options))

(define-this-gf foo (arg)
  (:method ((arg integer))
    (print `(hi ,arg))))

(define-this-gf bar (arg)
  (:method ((arg integer))
    (print `(hi-int ,arg)))
  (:method ((arg fixnum))
    (print `(hi-fix ,arg)))
  (:method ((arg string))
    (print `(hi-str ,arg)))
  (:method ((arg (eql 42)))
    (print `(hi-e42 ,arg))))


(bar 3)

(fraudulently-compile-function #'bar)

(bar 42)
