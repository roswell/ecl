#include <iostream>
#include <ecl/ecl.h>

extern "C" {
  extern void init_lib_CORE_LISP(cl_object);
}

int main(int argc, char** argv) {
  cl_boot(argc, argv);
  ecl_init_module(NULL, init_lib_CORE_LISP);
  cl_eval(c_string_to_object("(hello-world)"));
  cl_shutdown();
  return 0;
}
