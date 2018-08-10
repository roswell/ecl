# Build
Run:
```
$ mkdir build
$ cd build
$ cmake ..
```
If ECL is installed in non-default location set prefix instead:
```
$ cmake -DCMAKE_PREFIX_PATH=/home/mkolenda/Programowanie/C++/ ..
```

It shall produce: `cmake_ex` executable and `core-lisp.a` static library.


# Run
```
$ ./cmake_ex
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
