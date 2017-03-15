#include <ecl/ecl.h>

int main (int argc, char **argv) {
  extern void init_lib_EXAMPLE_WITH_DEP__ALL_SYSTEMS(cl_object);
  
  cl_boot(argc, argv);
  ecl_init_module(NULL, init_lib_EXAMPLE_WITH_DEP__ALL_SYSTEMS);

  /* do things with the Lisp library */
  cl_eval(c_string_to_object("(example:test-function 5)"));

  cl_shutdown();
  return 0;
}
