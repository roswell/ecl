;;(require 'asdf)
(push "./" asdf:*central-registry*)

(asdf:make-build :lisp-envi
                 :type :static-library
                 :move-here "qt/")
(quit)

