
(ffi::clines "extern const char *hello_string;")

(ffi::def-foreign-var ("hello_string" +hello-string+) (* :char) nil)

(print (ffi:convert-from-foreign-string +hello-string+))

