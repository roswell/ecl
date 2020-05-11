;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test that serve-event doesn't leak its handlers to other threads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'serve-event)

(defun test-leak (&aux exit)
  (let ((out *standard-output*))
    (print "Press enter." out)
    (let* ((p1 (mp:process-run-function
                'stdin-2
                (lambda ()
                  (serve-event:with-fd-handler
                      (0 :input #'(lambda (fd)
                                    (declare (ignore fd))
                                    (format out "WRONG!~%")))
                    (sleep most-positive-fixnum)))))
           (p2 (mp:process-run-function
                'stdin-1
                (lambda ()
                  (serve-event:with-fd-handler
                      (0 :input #'(lambda (fd)
                                    (declare (ignore fd))
                                    (format out"GOOD!~%")))
                    (unwind-protect (serve-event:serve-event)
                      (mp:interrupt-process p1 (lambda ()
                                                 (mp:exit-process)))))))))
      (mp:process-join p1)
      (mp:process-join p2))))

(test-leak)
