;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Embedding regression tests

(in-package :cl-test)

(suite 'emb)

(defun test-C-program (c-code &key capture-output)
  (ensure-directories-exist "tmp/")
  (with-open-file (s "tmp/ecl-aux.c" :direction :output :if-exists :supersede
                     :if-does-not-exist :create)
    (princ c-code s))
  (c::compiler-cc "tmp/ecl-aux.c" "tmp/ecl-aux.o")
  (c::linker-cc "tmp/ecl-aux.exe" '("tmp/ecl-aux.o"))
  (let ((environment 
         (append #+windows (list (format nil "PATH=~a;~a"
                                         (ext:getenv "PATH")
                                         c::*ecl-library-directory*))
                 #+cygwin (list (format nil "PATH=~a:~a"
                                        (ext:getenv "PATH")
                                        c::*ecl-library-directory*))
                 #-(or windows cygwin) (list (format nil "LD_LIBRARY_PATH=~a:~a"
                                                     (ext:getenv "LD_LIBRARY_PATH")
                                                     c::*ecl-library-directory*))
                 (ext:environ))))
    (ecase capture-output
      ((nil)
       (multiple-value-bind (stream return-code)
           (si::run-program "tmp/ecl-aux.exe" '()
                            :output t :error t
                            :environ environment)
         (declare (ignore stream))
         (zerop return-code)))
      ((string :string)
       (with-output-to-string (s)
         (let ((in (si::run-program "tmp/ecl-aux.exe" '() :output :stream
                                    :environ environment))
               line)
           (loop
            (setf line (read-line in nil))
            (unless line (return))
            (write-line line s)))))
      ((t forms :forms)
       (do* ((all '())
             (x t)
             (in (si::run-program "tmp/ecl-aux.exe" '() :output :stream
                                  :environ environment)))
            ((null in) all)
         (setf x (ignore-errors (read in nil nil)))
         (unless x (return all))
         (push x all))))))

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
  (is-equal
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
   '(:shutdown)))

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
    const cl_object conditions =
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

;;; Date: 2020-01-11 (Marius Gerbershagen)
;;; Description:
;;;
;;;     Verify that the ECL_WITH_LISP_FPE macro works correctly
;;;
(test emb.0003.with-lisp-fpe
  (is-true
   (let ((c-code "
#include <math.h>
#include <ecl/ecl.h>

int main(int argc, char **argv) {
  double a, b;
  int ret = 1;
  cl_env_ptr env;
  cl_object conditions;

  ECL_WITH_LISP_FPE_BEGIN {
    cl_boot(argc, argv);
  } ECL_WITH_LISP_FPE_END;

  env = ecl_process_env();
  conditions  = ecl_list1(ecl_make_symbol(\"ARITHMETIC-ERROR\", \"CL\"));
  ECL_HANDLER_CASE_BEGIN(env, conditions) {
    a = 1.0 / (double) (argc - 1); /* equivalent of 1.0/0.0 but without
                                      compilers complaining about it */
  } ECL_HANDLER_CASE(1, condition) {
    ret = 2;
    goto out;
  } ECL_HANDLER_CASE_END;

  ECL_WITH_LISP_FPE_BEGIN {
    ECL_HANDLER_CASE_BEGIN(env, conditions) {
      b = ecl_to_double(ecl_make_double_float(1.0));
    } ECL_HANDLER_CASE(1, condition) {
      ret = 3; /* Exception bits being set before ECL_WITH_LISP_FPE_BEGIN
                * shouldn't lead to a floating point exception being
                * signaled when creating a double float ...
                */
      goto out;
    } ECL_HANDLER_CASE_END;
  } ECL_WITH_LISP_FPE_END;

  ECL_WITH_LISP_FPE_BEGIN {
    ECL_HANDLER_CASE_BEGIN(env, conditions) {
      b = ecl_to_double(cl_N(2, ecl_make_double_float(1.0), ecl_make_double_float((double) (argc - 1))));
    } ECL_HANDLER_CASE(1, condition) {
      b = 0.0; /* ... but dividing by a zero float should definitely do so
                */
    } ECL_HANDLER_CASE_END;
  } ECL_WITH_LISP_FPE_END;

  if (isinf(a) &&
#ifdef ECL_AVOID_FPE_H
      isinf(b)
#else
      b == 0.0
#endif
      ) {
    ret = 0;
  }

out:
  cl_shutdown();
  return ret;
}
"))
     (test-C-program c-code))))


;;; Date: 2020-02-29 (Marius Gerbershagen)
;;; Description:
;;;
;;;     Verify that ecl_import_current_thread works correctly
;;;
#+threads
(test emb.0004.import-current-thread
  (is-true
   (let* ((c-code "
#define NUM_THREADS 50
#define BUFSIZE 1024

#include <stdio.h>
#include <stdlib.h>
#include <ecl/ecl.h>
/* Force use of ordinary thread creation functions instead of gc defines like
 * GC_CreateThread, GC_pthread_create. */
#undef CreateThread
#undef pthread_create
#undef pthread_join
#ifdef _WIN32
# include <windows.h>
#else
# include <unistd.h>
# include <pthread.h>
#endif

#ifdef ECL_WINDOWS_THREADS
static DWORD WINAPI
#else
static void *
#endif
thread_entry_point(void *data)
{
        cl_object form = (cl_object)data;

        if (!ecl_import_current_thread(ECL_NIL, ECL_NIL)) {
                exit(3);
        }

        cl_eval(form);

        ecl_release_current_thread();
#ifdef ECL_WINDOWS_THREADS
        return 1;
#else
        return NULL;
#endif
}


int main(int narg, char **argv)
{
#ifdef ECL_WINDOWS_THREADS
        HANDLE child_thread[NUM_THREADS];
        DWORD thread_id[NUM_THREADS];
#else
        pthread_t child_thread[NUM_THREADS];
#endif
        int i, code;
        char buffer[BUFSIZE];
        volatile cl_object forms[NUM_THREADS];

        cl_boot(narg, argv);
        snprintf(buffer, BUFSIZE, \"(defparameter *test-results* (make-array %d :initial-element nil))\", NUM_THREADS);
        cl_eval(c_string_to_object(buffer));

        for (i = 0; i < NUM_THREADS; i++) {
                snprintf(buffer, BUFSIZE, \"(setf (elt *test-results* %d) %d)\", i, i);
                forms[i] = c_string_to_object(buffer);
#ifdef ECL_WINDOWS_THREADS
                child_thread[i] = (HANDLE)CreateThread(NULL, 0, thread_entry_point,
                                                       (void*)forms[i], 0, &thread_id[i]);
                if (child_thread[i] == NULL) {
                        exit(1);
                }
#else
                code = pthread_create(&child_thread[i], NULL, thread_entry_point,
                                      (void*)forms[i]);
                if (code) {
                        exit(1);
                }
#endif
        }

        for (i = 0; i < NUM_THREADS; i++) {
#ifdef ECL_WINDOWS_THREADS
                WaitForSingleObject(child_thread[i], INFINITE);
                CloseHandle(child_thread[i]);
#else
                pthread_join(child_thread[i], NULL);
#endif
        }

        for (i = 0; i < NUM_THREADS; i++) {
                snprintf(buffer, BUFSIZE, \"(elt *test-results* %d)\", i);
                cl_object result = cl_eval(c_string_to_object(buffer));
                if (!ecl_eql(result, ecl_make_fixnum(i))) {
                        exit(2);
                }
        }

        cl_shutdown();
        return 0;
}"))
     (test-C-program c-code))))
