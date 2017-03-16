		━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
		 BUILD AN ASDF SYSTEM WITH DEPENDENCIES

		       Bo Yao <icerove@gmail.com>
		━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


Table of Contents
─────────────────

1 Example code to build
2 Build it as an single executable
3 Build it as shared library and use in C
4 Build it as static library and use in C


Besides the simple situation that we write Lisp without depending on any
other Lisp libraries, a more practical example is build a library
depends on other asdf systems or Quicklisp projects. ECL provides a
useful extension for asdf called `asdf:make-build', it's almost as easy
as build a library without dependencies. Because Quicklisp also uses
asdf to load systems with dependencies, just make sure you have
successfully load and run your library in ECL REPL (or
`*slime-repl*'). Don't worry Quicklisp, asdf, swank and other unused
libraries are packed into the executable or library, ECL will only build
and pack libraries your project depends on (that is, all dependence you
put in your `.asd' file, and their dependencies, nothing more even you
are build in a image already load with lots of other libraries).


1 Example code to build
═══════════════════════

  We use a simple project depends on alexandria to demonstrate the
  steps. Consists of `example-with-dep.asd', `package.lisp' and
  `example.lisp'. For convenience, we list these files here:

  ┌────
  │ ;;;; example-with-dep.asd
  │ (defsystem :example-with-dep
  │   :serial t
  │   :depends-on (:alexandria)
  │   :components ((:file "package")
  │ 	       (:file "example")))
  └────

  ┌────
  │ ;;;; package.lisp
  │ (in-package :cl-user)
  │ 
  │ (defpackage :example
  │   (:use :cl)
  │   (:export :test-function))
  └────

  ┌────
  │ ;;;; example.lisp
  │ (in-package :example)
  │ 
  │ (defun test-function (n)
  │   (format t "Factorial of ~a is: ~a~%" n (alexandria:factorial n)))
  └────

  Before any kind you build, you need to push full path of this
  directory (`asdf_with_dependence/') into `asdf:*central-registry*'.


2 Build it as an single executable
══════════════════════════════════

  Use this in REPL to make a executable:

  ┌────
  │ (asdf:make-build :example-with-dep
  │ 		 :type :program
  │ 		 :move-here #P"./"
  │ 		 :epilogue-code '(progn (example:test-function 5)
  │ 					(si:exit)))
  └────
  Here the `:epilogue-code' is what to do after loading our library, we
  can use arbitrary Lisp forms here. You can also write this code in
  your Lisp files and directly build them without this `:epilogue-code'
  option to have the same effect.  Run the program in console will
  display the following and exit:

  ┌────
  │ Factorial of 5 is: 120
  └────


3 Build it as shared library and use in C
═════════════════════════════════════════

  Use this in REPL to make a shared library:
  ┌────
  │ (asdf:make-build :example-with-dep
  │ 		 :type :shared-library
  │ 		 :move-here #P"./"
  │ 		 :monolithic t)
  └────

  Here `:monolithic t' means to let ECL solve dependence and build all
  dependence into one library named `example-with-dep--all-systems.so'
  in this directory.

  To use it, we use a simple C program:

  ┌────
  │ /* test.c */
  │ #include <ecl/ecl.h>
  │ 
  │ int main (int argc, char **argv) {
  │   extern void init_dll_EXAMPLE_WITH_DEP__ALL_SYSTEMS(cl_object);
  │ 
  │   cl_boot(argc, argv);
  │   ecl_init_module(NULL, init_dll_EXAMPLE_WITH_DEP__ALL_SYSTEMS);
  │ 
  │   /* do things with the Lisp library */
  │   cl_eval(c_string_to_object("(example:test-function 5)"));
  │ 
  │   cl_shutdown();
  │   return 0;
  │ }
  └────

  Note the name convention here: an asdf system named `example-with-dep'
  will compiled to `example-with-dep--all-systems.so' and in the C code
  should be init with `init_dll_EXAMPLE_WITH_DEP__ALL_SYSTEMS'. Compile
  it using:

  ┌────
  │ gcc test.c example-with-dep--all-systems.so -o test -lecl
  └────

  ECL's library path and current directory may not be in your
  `LD_LIBRARY_PATH', so call `./test' using:

  ┌────
  │ LD_LIBRARY_PATH=/usr/local/lib/:. ./test
  └────

  This will show:

  ┌────
  │ Factorial of 5 is: 120
  └────

  You can also build all dependent libraries separately as several `.so'
  files and link them together. For example, if you are building a
  library called `complex-example', that depends on `alexandria' and
  `cl-fad', you can also do these in ECL REPL:

  ┌────
  │ (asdf:make-build :complex-example
  │ 		 :type :shared-library
  │ 		 :move-here #P"./")
  │ (asdf:make-build :alexandria
  │ 		 :type :shared-library
  │ 		 :move-here #P"./")
  │ (asdf:make-build :cl-fad
  │ 		 :type :shared-library
  │ 		 :move-here #P"./")
  │ (asdf:make-build :bordeaux-threads
  │ 		 :type :shared-library
  │ 		 :move-here #P"./")
  └────

  Note here is no `:monolithic t' and we also need to build
  `bordeaux-threads' because `cl-fad' depends on it. The building
  sequence doesn't matter and the result `.so' files can also be used in
  your future program if these libraries are not modified.  And We need
  to initialize all these modules using `ecl_init_module', the name
  convention is to initialize `cl-fad' you need:

  ┌────
  │ extern void init_dll_CL_FAD(cl_object);
  │ 
  │ /* after cl_boot(argc, argv); 
  │    and if B depends on A, you should first init A then B. */
  │ ecl_init_module(NULL, init_dll_CL_FAD);
  └────

  You can easily figure out name conventions with other libraries.


4 Build it as static library and use in C
═════════════════════════════════════════

  To build a static library, use:

  ┌────
  │ (asdf:make-build :example-with-dep
  │ 		 :type :static-library
  │ 		 :move-here #P"./"
  │ 		 :monolithic t)
  └────

  That will generate a `example-with-dep--all-systems.a' in current
  directory and we need to replace
  `init_dll_EXAMPLE_WITH_DEP__ALL_SYSTEMS' with
  `init_lib_EXAMPLE_WITH_DEP__ALL_SYSTEMS'. (The code is given in
  test-static.c) And compile it using:

  ┌────
  │ gcc test-static.c example-with-dep--all-systems.a -o test-static -lecl
  └────

  Then run it:

  ┌────
  │ LD_LIBRARY_PATH=/usr/local/lib/ ./test-static
  └────

  Note we don't need to give current path in `LD_LIBRARY_PATH' here,
  since our Lisp library is statically bundled to the executable.  The
  result is same as the shared library example above. You can also build
  all dependent libraries separately to static libraries. To use them
  you also need replace names like `init_dll_CL_FAD' to
  `init_lib_CL_FAD'.
