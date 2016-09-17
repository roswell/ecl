;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Embedding regression tests

(in-package :cl-test)

(suite 'regressions/emb)

(defun test-C-program (c-code &key capture-output)
  (ensure-directories-exist "tmp/")
  (with-open-file (s "tmp/aux.c" :direction :output :if-exists :supersede
                     :if-does-not-exist :create)
    (princ c-code s))
  (c::compiler-cc "tmp/aux.c" "tmp/aux.o")
  (c::linker-cc "tmp/aux.exe" '("tmp/aux.o"))
  (ecase capture-output
    ((nil)
     (return-from test-C-program (zerop (si::system "tmp/aux.exe"))))
    ((string :string)
     (with-output-to-string (s)
       (let ((in (si::run-program "tmp/aux.exe" '() :output :stream))
             line)
         (loop
          (setf line (read-line in nil))
          (unless line (return))
          (write-line line s)))))
    ((t forms :forms)
     (do* ((all '())
           (x t)
           (in (si::run-program "tmp/aux.exe" '() :output :stream)))
       ((null in) all)
       (setf x (read in nil nil))
       (unless x (return all))
       (push x all)))))

;;; Date: 21/06/2006 (goffioul)
;;; Fixed: 23/06/2006 (juanjo)
;;; Description:
;;;
;;;     Multiple invocations of cl_shutdown() can hang ECL. Also,
;;;     cl_shutdown() is still invoked at exit (registered with
;;;     atexit()) even if cl_shutdown was previously invoked.
;;;
;;; Fixed: 03/2006 (juanjo)
;;;
(test emb.0001.shutdown
  (is (equal
       (let* ((skeleton "
#include <ecl/ecl.h>
#include <stdlib.h>
int main (int argc, char **argv) {
  cl_object x;
  cl_boot(argc, argv);
  si_safe_eval(3, x = c_string_to_object(~S), Cnil, Cnil);
  cl_shutdown();
  exit(0);
}")
              (form '(push (lambda () (print :shutdown)) si::*exit-hooks*))
              (c-code (format nil skeleton (format nil "~S" form)))
              (data (test-C-program c-code :capture-output t)))
         data)
       '(:shutdown))))

;;; Date: 2016-05-25 (Vadim Penzin)
;;; Date: 2016-05-27 (Vadim Penzin)
;;; Description:
;;;
;;;     ECL_HANDLER_CASE C macro misses condition handlers because the
;;;     macro looks up handler tags in env->values[1] instead of
;;;     env->values[0] and copies the condition object from
;;;     env->values[0] instead of env->values[1].
;;;
;;; Case study: http://penzin.net/ecl-handler-case.html
;;; Bug: https://gitlab.com/embeddable-common-lisp/ecl/issues/248
;;; Notes:
;;;
;;;     ECL_RESTART_CASE is very similar, but testing would require
;;;     user interaction (ie picking the restart), hence we only test
;;;     the ECL_HANDLER_CASE.
;;;
(test emb.0002.handlers
  (is-true
   (let* ((c-code "
#include <stdio.h>
#include <ecl/ecl.h>

int
main ( const int argc, const char * const argv [] )
{
    cl_boot ( argc, (char **) argv );
    int result = 1;

    cl_env_ptr const environment = ecl_process_env ();
    const cl_object const conditions =
        ecl_list1 ( ecl_make_symbol ( \"DIVISION-BY-ZERO\", \"CL\" ) );

    ECL_HANDLER_CASE_BEGIN ( environment, conditions ) {
        ecl_divide ( ecl_make_fixnum ( 1 ), ecl_make_fixnum ( 0 ) );
    } ECL_HANDLER_CASE ( 1, condition ) {
        result = 0;
    } ECL_HANDLER_CASE_END;

    return result;
}
"))
     (test-C-program c-code))))
