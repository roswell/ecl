#|
Build this module with (compile-file "uffi.lsp")
Load it with (load "uffi.fas")
|#
;;
;; This toplevel statement notifies the compiler that we will
;; need this shared library at runtime. We do not need this
;; statement in windows and macOS.
;;
#-(or windows darwin)
(uffi:load-foreign-library "/lib64/libm.so.6") ;; adjust the library path/name as needed
;;
;; With this other statement, we import the C function sin(),
;; which operates on IEEE doubles.
;;
(uffi:def-function ("sin" c-sin) ((arg :double))
                   :returning :double)
;;
;; We now use this function and compare with the lisp version.
;;
(format t "~%Lisp sin:~t~d~%C sin:~t~d~%Difference:~t~d"
	(sin 1.0d0) (c-sin 1.0d0) (- (sin 1.0d0) (c-sin 1.0d0)))
