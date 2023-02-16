
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See file 'LICENSE' for the copyright details.

(in-package #:compiler)

(defun declare-special (name &optional (env *cmp-env*))
  (when (cmp-env-search-symbol-macro name env)
    (cmperr "Symbol ~A cannot be declared special and appear in a symbol-macrolet." name))
  (cmp-env-register-var (make-global-var name :warn nil :kind 'SPECIAL) env nil))
