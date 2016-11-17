(in-package #:common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (require :package-locks))

(let ((cache (merge-pathnames "./cache/" *default-pathname-defaults*)))
  (ensure-directories-exist cache)
  (setf asdf:*user-cache* cache)
  (asdf:load-asd (merge-pathnames "ecl-tests.asd" *load-pathname*)))

(asdf:operate 'asdf:load-source-op 'ecl-tests)
#+ (or) (2am-ecl:run 'cl-test::make-check)
