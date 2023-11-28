(in-package "COMPILER")

(defgeneric codegen (compiler opecode instruction))

(defmacro define-codegen ((compiler opcode) (instruction) &body body)
  `(defmethod codegen ((compiler (eql ,compiler))
                       (opcode (eql ,opcode))
                       (,instruction instruction))
     ,@body))
