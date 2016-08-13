;;(require 'asdf)
(push "./" asdf:*central-registry*)

(asdf:make-build :hello-lisp-system
                 :type :fasl
                 :monolithic t
                 :move-here "./")
(quit)
