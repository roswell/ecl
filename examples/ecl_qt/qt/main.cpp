#include "hybrid_main.h"
#include <QApplication>
#include "cl_bridge_utils.hpp"

string  CL_MAIN_FASB = "\"hello-lisp-system--all-systems.fasb\"";
string  CL_MAIN_PACKAGE_NAME = "hello-lisp";

/* Initialization.
 * This time we load the fasb file after
 * the Lisp Environment is booted.
 * */
#define __cl_init_name init_lib_LISP_ENVI

extern "C"{

  extern void __cl_init_name(cl_object);

}

void init_cl_env(int argc, char * argv[]){
  /* Initialize CL environment */
  cl_boot(argc, argv);
  ecl_init_module(NULL, __cl_init_name);
  /* load fasb */
  cl_eval("load", CL_MAIN_FASB);
  /* set context to current package */
  cl_eval("in-package", CL_MAIN_PACKAGE_NAME);
  /* hook for shutting down cl env */
  atexit(cl_shutdown);
}

#undef __cl_init_name




int main(int argc, char *argv[]){
  QApplication a(argc, argv);
  hybrid_main w;
  w.show();
  init_cl_env(argc, argv); /* init env */
  return a.exec();
}
