
(in-package :cl-test)
(suite 'complex)

;;; Test suite is for testing complex types (added as part of adding
;;; complex float as a native type).

(test complex.0001.upgradex-complex-part-type
      (signals error (upgraded-complex-part-type 'string))
      (mapc (lambda (upgraded-typespec typespec)
              (is (eql upgraded-typespec (upgraded-complex-part-type typespec))
                  "upgraded-complex-part-type of ~s should be ~s, is ~s"
                   typespec upgraded-typespec (upgraded-complex-part-type typespec)))
            #-complex-float (make-list 10 :initial-element 'real)
            #+complex-float (append (make-list 5 :initial-element 'rational)
                                    '(float single-float single-float double-float long-float))
            '(rational integer fixnum (integer 0 1) ratio
              float short-float single-float double-float long-float)))

(test complex.0002.type-of
      (mapc (lambda (type element)
              (is (equal type (type-of element)) "(type-of ~s) not equal to ~s, is ~s"
                  element type (type-of element)))
            #-complex-float (make-list 6 :initial-element '(complex real))
            #+complex-float '((complex rational)
                              (complex rational)
                              (complex single-float)
                              (complex single-float)
                              (complex double-float)
                              (complex long-float))
            (list #c(1 2)
                  #c(1/2 3/2)
                  #c(1.2 3.2)
                  #c(1.2s0 3.2s0)
                  #c(1.2d0 3.2d0)
                  #c(1.2l0 3.2l0))))

;;; ensures that common arithmetic operations doesn't fail
(defmacro without-fpe-traps (&body body)
  `(let ((bits (si:trap-fpe 'cl:last t)))
     (unwind-protect
          (progn (si:trap-fpe t nil)
                 ,@body)
       (si:trap-fpe bits t))))

(defmacro should-signal (type &body body)
  `(handler-case (progn ,@body)
     (:no-error (&rest args)
       (finishes (error "should signal ~s" ',type)))
     (,type () t)))

(test complex.0003.arith
      (without-fpe-traps
        (flet ((check-op-1 (op)
                 "Check if 1-argument operation succeeds."
                 (mapc op *complexes*))
               (check-op-2 (op)
                 "Check if pair-wise operation succeeds."
                 (mapc (lambda (elt)
                         (mapc (lambda (set)
                                 (dolist (v set)
                                   (funcall op elt v)
                                   (funcall op v elt)))
                               (list *integers* *floats* *ratios* *complexes*)))
                       *complexes*)))
          (finishes
           (mapc (lambda (op) (finishes (check-op-1 op)))
                 (list #'conjugate #'realpart #'imagpart ; complexes
                       #'atan #'sin #'sinh #'tan #'tanh #'cos #'cosh ; trigonometry
                       #'- #'+ #'* #'/ #'1- #'1+ ; basic artih
                       #'abs #'exp #'log #'sqrt  ; exponentials
                       #'= #'/= #'zerop          ; predicates
                       ))
           "complex valid operations crashed")
          ;; operations without meaning for complex
          (finishes
           (mapc (lambda (op) (should-signal type-error (check-op-1 op)))
                 (list #'ceiling #'floor #'minusp #'plusp #'round #'truncate
                       #'< #'<= #'> #'>= #'min #'max)))
          ;; trigonometry (atan2 corner cases)
          (check-op-1 (lambda (elt) (should-signal error (atan elt 42))))
          (check-op-1 (lambda (elt) (should-signal error (atan elt #c(1 1)))))
          (check-op-1 (lambda (elt) (should-signal error (atan elt #c(1.0 1.0)))))
          (finishes (check-op-2 #'+))
          (finishes (check-op-2 #'-))
          (finishes (check-op-2 #'*))
          (finishes (check-op-2 (lambda (elt1 elt2)
                                  (unless (zerop elt2)
                                    (/ elt1 elt2)))))
          ;; See https://gitlab.com/embeddable-common-lisp/ecl/issues/485
          (finishes (check-op-2 (lambda (elt1 elt2)
                                  (when (and (< (realpart elt2) 4096)
                                             (> (realpart elt2) -4095))
                                    (unless (and (zerop elt1)
                                                 (not (zerop elt2))
                                                 (not (plusp (realpart elt2))))
                                      (expt elt1 elt2))))))
          ;; comparison
          (mapc (lambda (op)
                  (check-op-2 (lambda (elt1 elt2)
                                (should-signal type-error (funcall op elt1 elt2)))))
                (list #'max #'min #'< #'<= #'> #'>= #'>))
          (finishes (check-op-2 #'=)  "=<2>  complex operation crashed")
          (finishes (check-op-2 #'/=) "/=<2> complex operation crashed"))))

(test complex.0004.contagion
      (mapc (lambda (num type)
              (is (typep num type) "~s is not typep to ~s (type-of is ~s)" num type (type-of num)))
            (list #c(1/4 0) #c(1/4 1) #c(1 0.0) #c(1.0 0)
                  #c(0.0s0 0.0s0) #c(0.0d0 1) #c(0.0 0.0d0) #c(0.0l0 0.0d0)
                  (+ 1.0 #c(1 2)) (+ 1.0 #c(1 2.0d0)) (+ 1.0 #c(1 0)))
            (list 'rational '(complex rational) '(complex single-float) '(complex single-float)
                  '(complex single-float) '(complex double-float) '(complex double-float)
                  '(complex long-float) '(complex single-float) '(complex double-float) 'single-float)) )

(test complex.0005.conjugate          ; also tests imagpart / realpart
      (mapcar (lambda (c1 c2)
                (and (= (realpart c1) (realpart c2))
                     (= (imagpart c1) (- (imagpart c2)))))
              *complexes*
              (mapcar #'conjugate *complexes*)))

(test complex.0006.predicates
      (mapcar (lambda (c)
                (is (numberp c))
                (is (complexp c))
                (is (not (realp c))))
              *complexes*))

;;; These tests are made explicitly for complex-float numbers.
#+complex-float
(let* ((ss1 0.0s0) (ss2 -0.0s0)
       (sf1 1.1)   (sf2 -42.3)
       (sd1 0.0d0) (sd2 +0.2d0)
       (sl1 -2.4l0) (sl2 -3.2l0)
       (cf0 (si:complex-float ss1 ss2))
       (cf1 (si:complex-float sf1 sf2))
       (cf2 (si:complex-float sd1 sd2))
       (cf3 (si:complex-float sl1 sl2))
       (all-cfloats (list cf0 cf1 cf2 cf3))
       (rf0 #c(1 1)))

  (test cfloat.0000.type
        (is      (every (lambda (elt) (typep elt 'number))  all-cfloats))
        (is      (every (lambda (elt) (typep elt 'complex)) all-cfloats))
        (is (not (some  (lambda (elt) (typep elt 'float))   all-cfloats)))
        (is      (every (lambda (elt) (complexp elt))       all-cfloats))
        (is (not (some  (lambda (elt) (realp elt))          all-cfloats)))
        (is (not (some  (lambda (elt) (floatp elt))         all-cfloats)))
        (is (not (some  (lambda (elt) (rationalp elt))      all-cfloats)))
        (is (subtypep 'si:complex-single-float 'si:complex-float))
        (is (subtypep 'si:complex-double-float 'si:complex-float))
        (is (subtypep 'si:complex-long-float 'si:complex-float))
        (is (subtypep 'si:complex-float 'complex))
        (is (subtypep 'si:complex-float 'number))
        (is (not (subtypep 'si:complex-float 'float)))
        (is (not (subtypep 'si:complex-float 'rational)))
        (is (not (typep cf1 'si:complex-double-float)))
        (is (not (typep cf1 'si:complex-long-float)))
        (is (not (typep cf2 'si:complex-single-float)))
        (is (not (typep cf2 'si:complex-long-float)))
        (is (not (typep cf3 'si:complex-single-float)))
        (is (not (typep cf3 'si:complex-double-float))))

  (test cfloat.0001.predicate
        (is (every #'si:complex-float-p all-cfloats))
        (is (not (some #'si:complex-float-p (list nil #\c 3 sf1 rf0)))))

  ;; all numerical operators which doesn't apply to complex
  (test cfloat.0002.invalid-type
        ;; unary operators
        (let ((operations (list #'plusp #'minusp #'max #'min #'evenp #'oddp
                                #'> #'>= #'< #'<= #'lcm #'isqrt #'cis #'complex
                                #'numerator #'denominator #'rationalize #'float)))
          (mapc (lambda (op)
                  (mapc (lambda (num)
                          (signals type-error (funcall op num) "~a" op))
                        all-cfloats))
                operations))
        ;; nary operators (implement me!)
        )

  (test cfloat.0003.zerop
        (mapc (lambda (num)
                (if (and (zerop (realpart num))
                         (zerop (imagpart num)))
                    (is (zerop num))
                    (is (not (zerop num)))))
              (list* (si:complex-float 0.0s0 0.0s0)
                     (si:complex-float 0.0d0 0.0d0)
                     (si:complex-float 0.0l0 0.0l0)
                     all-cfloats)))
  
  (test cfloat.0004.realpart/imagpart
        (mapc (lambda (num parts)
                (is (= (car parts) (realpart num)))
                (is (= (cdr parts) (imagpart num))))
              all-cfloats
              (list (cons ss1 ss2)
                    (cons sf1 sf2)
                    (cons sd1 sd2)
                    ;; TDD is great! I did put "long" in instead of "long double".
                    (cons sl1 sl2))))

  (test cfloat.0005.conjugate
        (mapc (lambda (num parts)
                (is (= (car parts) (realpart num)))
                (is (= (- (cdr parts)) (imagpart num))))
              (mapcar #'conjugate all-cfloats)
              (list (cons ss1 ss2)
                    (cons sf1 sf2)
                    (cons sd1 sd2)
                    ;; TDD is great! I did put "long" in instead of "long double".
                    (cons sl1 sl2))))

  (test cfloat.0006.abs
        (is (every (lambda (num) (typep (abs num) 'real)) all-cfloats))
        (mapc (lambda (elt)
                (let ((imag (coerce 0 (type-of elt))))
                  (is (= (abs elt) (abs (si:complex-float elt imag))))))
              (list -4.1234s0 +1.000103 +0.1243d0 -2.1234l0)))

  (test cfloat.0007.exponentiation
        ;; exp, sqrt, log and log1p
        (finishes (mapc (lambda (cf) (exp cf))       all-cfloats) "exp1")
        (finishes (mapc (lambda (cf) (sqrt cf))      all-cfloats) "sqrt")
        (finishes (mapc (lambda (cf) (if (zerop cf)
                                         (signals division-by-zero (log cf))
                                         (log cf)))
                        all-cfloats) "log1")
        (finishes (mapc (lambda (cf) (si:log1p cf))  all-cfloats) "log1p")
        ;; log operations on floats should give corresponding cfloat type
        (mapc (lambda (num)
                ;; we assume all only negative or complex are passed here
                (let ((result-type (typecase num
                                     (si:complex-single-float 'si:complex-single-float)
                                     (si:complex-double-float 'si:complex-double-float)
                                     (si:complex-long-float   'si:complex-long-float)
                                     (short-float   'si:complex-single-float)
                                     (single-float  'si:complex-single-float)
                                     (double-float  'si:complex-double-float)
                                     (long-float    'si:complex-long-float)
                                     (otherwise               'si:complex-single-float))))
                  (is (typep (log num) result-type) "log(~s) is not of type ~s" num result-type)
                  (is (typep (si:log1p num) result-type) "log(~s) is not of type ~s" num result-type)))
              (list -1.12s0 -1.12 -1.12d0 -1.12l0 -4 #c(1/3 3)
                    (1- most-negative-fixnum) cf1 cf2 cf3)))

  (test cfloat.0008.trig/hyper.1        ; only 1-argument variants
        ;; trigonometric
        (finishes (mapc (lambda (cf) (sin cf)) all-cfloats) "sin")
        (finishes (mapc (lambda (cf) (cos cf)) all-cfloats) "cos")
        (finishes (mapc (lambda (cf) (tan cf)) all-cfloats) "tan")
        ;; trigonometric arcus
        (finishes (mapc (lambda (cf) (asin cf)) all-cfloats) "asin")
        (finishes (mapc (lambda (cf) (acos cf)) all-cfloats) "acos")
        (finishes (mapc (lambda (cf) (atan cf)) all-cfloats) "atan")
        ;; hyperbolic
        (finishes (mapc (lambda (cf) (sinh cf)) all-cfloats) "sinh")
        (finishes (mapc (lambda (cf) (cosh cf)) all-cfloats) "cosh")
        (finishes (mapc (lambda (cf) (tanh cf)) all-cfloats) "tanh")
        ;; hyperbolic arcus
        (finishes (mapc (lambda (cf) (asinh cf)) all-cfloats) "asinh")
        (finishes (mapc (lambda (cf) (acosh cf)) all-cfloats) "acosh")
        (finishes (mapc (lambda (cf) (atanh cf)) all-cfloats) "atonh"))

  (test cfloat.0008.make-complex
        (signals type-error (complex #c(1.0 1.0) 14))
        (signals type-error (complex 14 #c(1.0 1.0)))
        (signals type-error (complex 14 "foobar"))
        (is (typep (complex 1 0) 'fixnum))
        (is (typep (complex 1 1) '(complex rational)))
        (is (typep (complex 0 1) '(complex rational)))
        (is (typep (complex (1+ most-positive-fixnum) 1/4) '(complex rational)))
        (is (typep (complex 1/4 (1+ most-positive-fixnum)) '(complex rational)))
        (is (typep (complex 1.4 (1+ most-positive-fixnum)) '(complex single-float)))
        (is (typep (complex (1+ most-positive-fixnum) 1.4) '(complex single-float)))
        (is (typep (complex 1.4d0 1) '(complex double-float)))
        (is (typep (complex 1.4d0 1.0l0) '(complex long-float)))
        (is (typep (complex 1.4 1.0d0) '(complex double-float))))

  (test cfloat.0009.c-sharp-reader
        ;; reader conses complex-floats (instead of a generic type)
        (is (typep #c(1.0s0 2.3)   'si:complex-single-float))
        (is (typep #c(1.0d0 2.3s0) 'si:complex-double-float))
        (is (typep #c(1.0d0 2.3l0) 'si:complex-long-float))
        (is (typep #c(1 2.3l0)     'si:complex-long-float))
        (is (typep #c(1.0 0)       'si:complex-float))
        (is (not (typep #c(1 1)    'si:complex-float)))
        (is (not (typep #c(0 1)    'si:complex-float)))
        (is (not (typep #c(1 0)    'si:complex-float))))

  ;; Date: 2019-12-30
  ;; URL: https://gitlab.com/embeddable-common-lisp/ecl/issues/547
  ;; From: Karsten Poeck
  ;; Fixed: 2019-12-30 (Daniel Kochmański)
  ;; Description:
  ;;
  ;;     (EXPT #C(1.0 0.0) 2) causes unrecoverable error.
  ;;
  (test.csfloat.0010.issue-547
   (finishes (expt #c(1.0 0.0) 2))))
