#include <stdio.h>
#include <ecl/ecl.h>

int main (int argc, char **argv) {
  /* Initialize ECL */
  cl_boot(argc, argv);

  cl_object my_fun = cl_eval(c_string_to_object("(lambda (x) (sleep x))"));
  ecl_print(my_fun, ECL_T);

  cl_funcall(2, my_fun, ecl_make_fixnum(0));

  ecl_terpri(ECL_T);

  cl_shutdown();
  return 0;
}
