
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'serve-event)

(defun test-stdin (&aux exit)
  (format t "DOING STDIN. Type Q to exit.~%")
  (serve-event:with-fd-handler
      (0 :input #'(lambda (fd)
                    (declare (ignore fd))
                    (let ((ch (read-char)))
                      (format t "Got data ~s~%" ch)
                      (when (char= ch #\Q)
                        (setf exit t)))))
    (loop until exit
          do (format t "Entering serve-all-events...~%")
             (force-output)
             (serve-event:serve-all-events 5)
             (format t "Events served~%"))))

(test-stdin)
