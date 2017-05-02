;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author: Daniel Kochmański
;;;; Created: 2016-11-09
;;;; Contains: PACKAGE extension tests
;;;; Easter: Trump won the election today, we're doomed...

(in-package :cl-test)

(suite 'package-ext)

(defmacro with-fresh-package (name &body body)
  `(progn
     (defpackage ,name)
     ,@body
     (ext:unlock-package ',name)
     (delete-package ',name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors (require :package-locks)))


;; package locks

#+package-locks
(test package-locks.trivial
  (is-true (ext:package-locked-p "CL"))
  (is-false (ext:package-locked-p "CL-USER")))

#+package-locks
(test package-locks.coerce
  (is-true (ext:package-locked-p :CL))
  (is-true (ext:package-locked-p (find-package "CL")))
  (is-true (ext:package-locked-p "CL"))
  (signals error (ext:package-locked-p "CL1"))
  (signals error (ext:package-locked-p :CL1))
  (signals error (ext:package-locked-p 'CL1))
  (signals error (ext:package-locked-p 3)))

#+package-locks
(test package-locks.lock/unlock
  (defpackage test-pack-locks)
  (finishes (ext:unlock-package 'test-pack-locks))
  (is-false (ext:package-locked-p 'test-pack-locks))
  (finishes (ext:lock-package 'test-pack-locks))
  (is-true (ext:package-locked-p 'test-pack-locks))
  (signals package-error (delete-package 'test-pack-locks))
  (ext:unlock-package 'test-pack-locks)
  (finishes (delete-package 'test-pack-locks)))

#+package-locks
(test package-locks.intern-locked/without-package-locks
  (with-fresh-package test-pack
    (ext:lock-package 'test-pack)
    (signals package-error (intern "BAH" 'test-pack))
    (ext:unlock-package 'test-pack)
    (finishes (intern "BAH" 'test-pack))
    (ext:lock-package 'test-pack)
    (ext:without-package-locks
      (is-true (ext:package-locked-p 'test-pack))
      (finishes (intern "BAH2" 'test-pack)))
    (signals package-error (intern "BAH3" 'test-pack))))

#+package-locks
(test package-locks.with-unlocked-packages
  (with-fresh-package test-pack
    (with-fresh-package test-pack2
      (with-fresh-package test-pack3
        (ext:lock-package 'test-pack)
        (ext:lock-package 'test-pack2)
        (is-true (ext:package-locked-p 'test-pack))
        (is-true (ext:package-locked-p 'test-pack2))
        (is-false (ext:package-locked-p 'test-pack3))
        (ext:with-unlocked-packages (test-pack test-pack2 test-pack3)
          (is-false (ext:package-locked-p 'test-pack))
          (is-false (ext:package-locked-p 'test-pack2))
          (is-false (ext:package-locked-p 'test-pack3)))
        (is-true (ext:package-locked-p 'test-pack))
        (is-true (ext:package-locked-p 'test-pack2))
        (is-false (ext:package-locked-p 'test-pack3))

        (signals error
          (ext:with-unlocked-packages (test-pack test-pack2 test-pack3)
            (is-false (ext:package-locked-p 'test-pack))
            (is-false (ext:package-locked-p 'test-pack2))
            (is-false (ext:package-locked-p 'test-pack3))
            (error "bah")))
        (is-true (ext:package-locked-p 'test-pack))
        (is-true (ext:package-locked-p 'test-pack2))
        (is-false (ext:package-locked-p 'test-pack3))))))

#+package-locks
(test package-locks.defpackage
  (defpackage test-pack1 (:lock t))
  (defpackage test-pack2 (:lock nil))
  (defpackage test-pack3)
  (is-true (ext:package-locked-p :test-pack1))
  (is-false (ext:package-locked-p :test-pack2))
  (is-false (ext:package-locked-p :test-pack3))
  (ext:unlock-package :test-pack1)
  (delete-package :test-pack1)
  (delete-package :test-pack2)
  (delete-package :test-pack3))


;; package local nicknames

#+package-local-nicknames
(test package-local-nicknames.interface
  (defpackage test-pack1 (:local-nicknames ("L" #:CL)))
  (defpackage test-pack2 (:local-nicknames ("L" #:TEST-PACK1)))
  (defpackage test-pack3
    (:lock t)
    (:local-nicknames ("L1" #:TEST-PACK1)
                      ("L2" #:TEST-PACK2)))

  (is (= 1 (length (ext:package-local-nicknames :test-pack1))))
  (is (= 1 (length (ext:package-local-nicknames :test-pack2))))
  (is (= 2 (length (ext:package-local-nicknames :test-pack3))))

  (is (= 2 (length (ext:package-locally-nicknamed-by-list :test-pack1))))
  (is (= 1 (length (ext:package-locally-nicknamed-by-list :test-pack2))))
  (is (= 1 (length (ext:package-locally-nicknamed-by-list :cl))))

  (signals package-error (delete-package :test-pack3))
  (signals package-error (ext:add-package-local-nickname "L0" :cl :test-pack3))

  (is (ext:add-package-local-nickname "L0" :cl :test-pack2))
  (is (= 2 (length (ext:package-local-nicknames :test-pack2))))
  (is (= 2 (length (ext:package-locally-nicknamed-by-list :cl))))

  (is (ext:remove-package-local-nickname "L0" :test-pack2))
  (is (= 1 (length (ext:package-local-nicknames :test-pack2))))
  (is (= 1 (length (ext:package-locally-nicknamed-by-list :cl))))

  ;; check if nicknaming works for real
  (signals simple-error (eval (read-from-string "(L:cons 1 2)")))
  (let ((*package* (find-package :test-pack1)))
    (is-equal '(1 . 2) (eval (read-from-string "(L:cons 1 2)"))))

  ;; test-pack3 has a reference to test-pack1, but is locked
  (signals package-error (delete-package :test-pack1))

  (ext:package-lock :test-pack3 nil)
  (finishes (delete-package :test-pack1))
  (is (= 0 (length (ext:package-local-nicknames :test-pack2))))
  (is (= 0 (length (ext:package-locally-nicknamed-by-list :cl))))

  (ignore-errors (delete-package :test-pack1))
  (ignore-errors (delete-package :test-pack2))
  (ignore-errors (delete-package :test-pack3)))
