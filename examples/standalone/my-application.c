#include <ecl/ecl.h>

int main (int argc, char **argv)
{
  extern void init_lib_MY_APPLICATION (cl_object);
  
  cl_boot (argc, argv);
  ecl_init_module (NULL, init_lib_MY_APPLICATION);

  cl_eval (c_string_to_object ("(my-application:main)"));

  cl_shutdown ();

  return 0;
}
