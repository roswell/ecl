(in-package :cl-test)

(suite 'run-program)

;;; I was wondering about the program which we could could use to test
;;; the interface (i.e both on Linux and Windows). Easy! ECL is a
;;; perfect program for that.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *binary* (si:argv 0))
  (defparameter *program* (namestring (merge-pathnames "external-process-programs.lisp" *aux-dir*))))

(defmacro with-run-program ((name args &rest params) &body body)
  `(multiple-value-bind (,name code process)
       (ext:run-program *binary*
                        '("--norc"
                          "--eval" ,(format nil "(setf *args-number* ~a)" (+ 13 (length args)))
                          "--eval" "(setf *load-verbose* nil)"
                          "--load" ,*program*
                          "--eval" ,(format nil "(~a)" name)
                          "--eval" "(quit)"
                          "--" ,@args)
                        ,@params
                        :wait nil)
     (declare (ignorable ,name code))
     (let ((result (progn ,@body)))
       (cons result (multiple-value-list (ext:external-process-wait process t))))))

(defun slurp (stream)
  (do ((line #1=(read-line stream nil :eof) #1#)
       (last nil line))
      ((eql line :eof) last)))


(test arg-test
  (is (equal '(nil :exited 0)
             (with-run-program (arg-test ("a" "b c" "d \\" "e\ 4\\
")))) "ext:run-program doesn't escape arguments properly")
  #+windows
  (is (null (equal '(nil :exited 0)
                   (with-run-program (arg-test ("a" "b c" "d \\" "e\ 4\\
") :escape-arguments nil)))) "ext:run-program :ESCAPE-ARGUMENTS NIL doesn't work"))

(test output-streams
  ;; error is a separate stream
  (is-equal '(("Hello stdout" "Hello stderr") :exited 0)
            (with-run-program
                (print-test nil :output :stream :error :stream)
              (let ((print-test-err (ext:external-process-error-stream process)))
                (list (slurp print-test) (slurp print-test-err)))))
  ;; :error :output
  (is-equal '(("Hello stderr" nil) :exited 0)
            (with-run-program
                (print-test nil :output :stream :error :output)
              (let ((print-test-err (ext:external-process-error-stream process)))
                ;; print-test-err is drained by reading from print-test
                (list (slurp print-test) (slurp print-test-err))))))

(test interactive-input
  (is-equal '(nil :exited 0)
            (with-run-program (io/err nil)
              (format io/err "42~%")))
  ;; process will have :eof on input and should quit with "1"
  (is-equal '(nil :exited 1) (with-run-program (io/err nil :input nil))))

(test stream-values ()
  (finishes (with-run-program (print-test nil :output nil :error nil :input nil)))
  (finishes (with-run-program (print-test nil :output nil :error nil :input t)))
  (finishes (with-run-program (print-test nil :output nil :error nil :input :stream)))
  (finishes (with-run-program (print-test nil :output nil :error :output :input nil)))
  (finishes (with-run-program (print-test nil :output nil :error :output :input :stream)))
  (finishes (with-run-program (print-test nil :output t :error nil :input nil)))
  (finishes (with-run-program (print-test nil :output t :error :output :input nil)))
  (finishes (with-run-program (print-test nil :output t :error :stream :input nil)))
  (finishes (with-run-program (print-test nil :output t :error nil :input nil)))
  (finishes (with-run-program (print-test nil :output t :error :output :input nil)))
  (finishes (with-run-program (print-test nil :output t :error :stream :input nil)))
  (finishes (with-run-program
	     (print-test nil :output :stream :error :output :input :stream)))
  (finishes (with-run-program
	     (print-test nil :output :stream :error :stream :input :stream)))
  (signals simple-error
    (with-run-program (print-test nil :output :bam :error :stream :input :stream))))


(test terminate-process
  (is-equal #-windows `(t :signaled ,ext:+sigterm+)
	    #+windows `(t :exited -1)
            (with-run-program (terminate nil)
              (is-eql :running (ext:external-process-wait process nil))
              (finishes (ext:terminate-process process))
              (sleep 1)
              #-windows(is-eql :signaled (ext:external-process-wait process nil))
              #+windows(is-eql :exited (ext:external-process-wait process nil))))
  (is-equal #-windows `(t :signaled ,ext:+sigkill+)
	    #+windows `(t :exited -1)
            (with-run-program (terminate nil)
              (is-eql :running (ext:external-process-wait process nil))
              (finishes (ext:terminate-process process t))
              (sleep 1)
              #-windows(is-eql :signaled (ext:external-process-wait process nil))
              #+windows(is-eql :exited (ext:external-process-wait process nil))
              (finishes (ext:external-process-status process)))))

;;; We may want to craft it into an interface. Suspend/Resume *is* possible on Windows:
;;; http://stackoverflow.com/questions/11010165/how-to-suspend-resume-a-process-in-windows
#-(or windows cygwin)
(test suspend-resume
  (is-equal `(t :signaled ,ext:+sigkill+)
            (with-run-program (heartbeat nil)
              (let ((pid (ext:external-process-pid process)))
                (is-eql :running (ext:external-process-wait process nil))
                (si:killpid pid ext:+sigstop+)
                (sleep 2)
                (is-eql :stopped (ext:external-process-wait process nil))
                (si:killpid pid ext:+sigcont+)
                (sleep 2)
                (is-eql :resumed (ext:external-process-wait process nil))
                (finishes (ext:terminate-process process t))))))

;;; Cygwin programs seems not to react to signals. We use a stub to
;;; avoid infintie wait for process termination.
#+cygwin
(test suspend-resume
  (is (null "killpid doesn't seem to work on cygwin.")))

#+threads
(test no-fd-streams
  (with-output-to-string (output-stream)
    (with-output-to-string (error-stream)
      ;; note the space – otherwise reader waits for next character
      (with-input-from-string (input-stream "42 ")
        (with-run-program (io/err nil
                                  :input input-stream
                                  :output output-stream
                                  :error error-stream)))
      (is (null (zerop (length (get-output-stream-string output-stream)))))
      (is (null (zerop (length (get-output-stream-string error-stream)))))
      (mapc #'close (list output-stream error-stream)))))

#+threads
(test empty-string-input-stream
  (with-output-to-string (output-stream)
    (with-output-to-string (error-stream)
      (with-input-from-string (input-stream "")
        (is-equal '(nil :exited 1)
                  (with-run-program (io/err nil
                                            :input input-stream
                                            :output output-stream
                                            :error error-stream))))
      (is (null (zerop (length (get-output-stream-string output-stream)))))
      (is (null (zerop (length (get-output-stream-string error-stream)))))
      (mapc #'close (list output-stream error-stream)))))

#-threads
(test no-fd-streams
  (with-output-to-string (output-stream)
    (with-output-to-string (error-stream)
      ;; note the space – otherwise reader waits for next character
      (with-input-from-string (input-stream "42 ")
        (ext:run-program *binary* `("--norc"
                                    "--load" ,*program*
                                    "--eval" ,(format nil "(~a)" 'io/err)
                                    "--eval" "(quit)")
                         :input input-stream
                         :output output-stream
                         :error  error-stream
                         :wait t))
      (is (null (zerop (length (get-output-stream-string output-stream)))))
      (is (null (zerop (length (get-output-stream-string error-stream)))))
      (mapc #'close (list output-stream error-stream)))))

(test process-environ
  (flet ((run-env-program (&rest args)
           ;; use :input nil :output nil :error nil as default to stop
           ;; pipes from filling up and the process deadlocking
           (setf args (append args '(:input nil :output nil :error nil)))
           #-windows (apply #'ext:run-program "env" nil args)
           #+windows (apply #'ext:run-program "cmd" '("/k" "set" "&" "exit") args))
         (run-printenv-program (var &rest args)
           #-windows (apply #'ext:run-program "printenv" (list var) args)
           #+windows (apply #'ext:run-program "cmd"
  	                       (list "/k" (concatenate 'string "echo %" var "%")
  		                          "&" "exit")
  	                       args)))
    (is-equal 0 (nth-value 1 (run-env-program)))
    (is-equal 0 (nth-value 1 (run-env-program :environ :default)))
    (is-equal "bar"
              (delete #\Space
                      (read-line
                       (run-printenv-program
  	                     "foo"
  	                     :environ (list "foo=bar"
  		                                 (format nil "PATH=~A" (ext:getenv "PATH"))))
                       nil nil)))
    (signals simple-error (run-env-program :environ :bam) nil nil)
    #-cygwin ;; Cygwin always injects `WINDIR=C:\\Windows' variable.
    (is (null (slurp (run-env-program :environ nil))))))

;;; Date: 2022-10-22
;;; From: Marius Gerbershagen
;;; Description:
;;;
;;;     Check that run-program works correctly with different
;;;     encodings
;;;
(test run-program-encoding
  (let* ((skeleton "
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define expected_length ~S

int main (int argc, char **argv) {
  unsigned char expected[expected_length+1] = {~{~S,~}};
  if (argc != 2) {
    return 1;
  }
  if (strlen(argv[1]) != expected_length) {
    return 2;
  }
  if (strcmp(argv[1], (char*)expected) != 0) {
    return 3;
  }
  if (strcmp(getenv(\"ECLTESTVAR\"), (char*)expected) != 0) {
    return 4;
  }
  printf(\"%s\", argv[1]);
  return 0;
}"))
    (flet ((test-with-encoding (encoding test-string)
             (let* ((ext:*default-external-format* encoding)
                    (encoded-test-string
                     (coerce (ext:string-to-octets test-string
  				                                       :null-terminate t
  				                                       :external-format encoding)
  	                          'list)))
               (multiple-value-bind (return-code output)
                   (test-C-program (format nil skeleton
  			                                  (1- (length encoded-test-string))
  			                                  encoded-test-string)
  		                             :args (list test-string)
  		                             :environ (list (concatenate 'string "ECLTESTVAR=" test-string))
  		                             :capture-output :string)
                 (is (zerop return-code))
                 (is (string= test-string (delete #\newline output)))))))
      (test-with-encoding ext:*default-external-format* "default-äöüλ🙋")
      #+unicode (progn (test-with-encoding :utf8 "utf8-äöüλ🙋")
                       (test-with-encoding :latin-1 "latin-1-äöü")
                       (test-with-encoding :greek "greek-λ")))))

