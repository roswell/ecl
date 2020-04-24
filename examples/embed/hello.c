#include <stdio.h>
#include <ecl/ecl.h>

int main (int argc, char **argv) {
  /* Initialize ECL */
  cl_boot(argc, argv);

  /* Initialize the library we linked in. Each library
   * has to be initialized. It is best if all libraries
   * are joined using ASDF:MAKE-BUILD.
   */
  extern void init_lib_HELLO_LISP(cl_object);
  ecl_init_module(NULL, init_lib_HELLO_LISP);

  cl_object result = cl_eval(c_string_to_object("(hello-lisp)"));
  ecl_print(result, ECL_T);

  cl_object my_fun = cl_eval(c_string_to_object("(lambda (x) (1+ x))"));
  ecl_print(my_fun, ECL_T);

  result=cl_funcall(2, my_fun, ecl_make_fixnum(8));
  ecl_print(result, ECL_T);

  ecl_terpri(ECL_T);

  cl_shutdown();
  return 0;
}
