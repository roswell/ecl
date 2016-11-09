;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;; Author: Daniel Kochmański
;;;; Created: 2016-11-09
;;;; Contains: PACKAGE-LOCKS interface tests
;;;; Easter: Trump won the election today, we're doomed...

(in-package :cl-test)

(suite 'features/package-locks)

(defmacro with-fresh-package (name &body body)
  `(progn
     (defpackage ,name)
     ,@body
     (ext:unlock-package ',name)
     (delete-package ',name)))

(test package-locks.trivial
  (is-true (ext:package-locked-p "CL"))
  (is-false (ext:package-locked-p "CL-USER")))

(test package-locks.coerce
  (is-true (ext:package-locked-p :CL))
  (is-true (ext:package-locked-p (find-package "CL")))
  (is-true (ext:package-locked-p "CL"))
  (signals error (ext:package-locked-p "CL1"))
  (signals error (ext:package-locked-p :CL1))
  (signals error (ext:package-locked-p 'CL1))
  (signals error (ext:package-locked-p 3)))

(test package-locks.lock/unlock
  (defpackage test-pack-locks)
  (finishes (ext:unlock-package 'test-pack-locks))
  (is-false (ext:package-locked-p 'test-pack-locks))
  (finishes (ext:lock-package 'test-pack-locks))
  (is-true (ext:package-locked-p 'test-pack-locks))
  (signals package-error (delete-package 'test-pack-locks))
  (ext:unlock-package 'test-pack-locks)
  (finishes (delete-package 'test-pack-locks)))

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
