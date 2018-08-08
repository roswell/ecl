# Description
This example shows how to setup CMake to build C++ project which uses ECL library.

In `src/lisp` is definition of `core-lisp` system that's being loaded into C++
program.

Functions defined in `src/lisp/core-lisp.lisp`:
```
(defun hello-world () (format t "Hello World!~%"))
```
are used in `src/cxx/main.cpp`:
```
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
```

## CMakeLists.txt
For more information about setup read `CMakeLists.txt` comments.

# Build
Run:
```
$ mkdir build
$ cd build
```
If ECL is built and installed with non-default prefix use:

```
$ cmake -DCMAKE_PREFIX_PATH=/home/user/local_prefix/ ..
```

Otherwise you don't have to set `CMAKE_PREFIX_PATH`:
```
$ cmake ..
```

Finally run:
```
$ make
```

It shall produce: `cmake_ecl` executable and `core-lisp.a` static library that
has been linked to executable.


# Run
```
$ ./cmake_ecl
Hello World!
```

# Notes
  1. You don't have to remove `./build` directory if you want to change option
     in `CMakeLists.txt`. Just run `make`.

  2. If you see `undefined reference` errors look at `nm --demangle core-lisp.a`
     output. You may have forgot setting `:init-name` in `CMakeLists.txt`
     
  3. To reuse this example you must copy `cmake` directory to your project. It
     contains `FindECL.cmake`
     
  4. You don't have to have `ecl` executable in `$PATH` environment variable.
     `FindECL.cmake` shall find it.
