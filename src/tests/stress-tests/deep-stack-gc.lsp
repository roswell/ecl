
(in-package :cl-test)

(suite 'stress)

;;; Reporter: Andrey Chesnokov
;;; Issue: https://gitlab.com/embeddable-common-lisp/ecl/-/work_items/860
;;; Date: 2026-10-28
;;; Description:
;;;
;;;   Pushing other roots to GC could exhaust the mark stack on single-threaded
;;;   builds. See #860.
;;; 
;;;   This test is inherently flaky -- if another test extends the mark stack,
;;;   then it won't trigger the condition. Limits are OS-dependent too. That
;;;   said it reliably triggers the issue on single-threaded vulnerable builds.
;;;   The test failure will crash the process, but that's fine for stress tests.
(deftest deep-stack-gc ()
  (labels ((deep (n)
             (let ((x (cons n nil)))
               (if (zerop n)
                   (progn (ext:gc t) 0)
                   (+ (deep (1- n)) (car x))))))
    (loop for n from 3000 upto 5000 do
      (finishes (deep n)))))
