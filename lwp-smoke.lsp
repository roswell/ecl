(in-package "CL-USER")

(format t "Hello world!~%")

(format t "--- Creating accessors and entries~%")
(defun thread-continuation (o)
  (check-type o si::thread)
  (ffi:c-inline (o) (:object)
                :object "#0->thread.cont" :one-liner t :side-effects nil))

(defun thread-function (o)
  (check-type o si::thread)
  (ffi:c-inline (o) (:object)
                :object "#0->thread.fun" :one-liner t :side-effects nil))

(defun yield (thread)
  (si:pass (thread-continuation thread)))

(defun example-function (this-thread)
  (format t "Example function!~%")
  (yield this-thread)
  (format t "Example function resumed!~%")
  (yield this-thread))

(format t "--- Disabling Garbage Collector~%")
(let ((module (find "BDW-GC" (si::list-modules) :test #'string=
                                                :key #'si::module-name)))
  (ext:gc t)
  (format t "disabling ~a~%" module)
  (si::module-disable module))

(format t "-- Creating a continuation!~%")
(defparameter *cont* (si:make-continuation (si:make-thread #'example-function)))

(format t "Running a continuation!~%")
(si:pass *cont*)
(format t "Continuation returned; again!~%")
(si:pass *cont*)
(format t "Continuation returned; OK!~%")

(dotimes (v 3) (terpri))

(format t "Creating a lot of these, repeat 1024~%") 
(dotimes (v (* 32 1024))
  (let ((cont (si:make-continuation (si:make-thread #'example-function))))
    (format t "run ~a ~a~%" v cont)
    (si:pass cont)
    (format t "res ~a ~a~%" v cont)
    (si:pass cont)
    (format t "qed ~a ~a~%" v cont)) )
