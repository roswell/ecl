This demo shows how to embed ECL into Qt5 and serve as kernel. This
also discuss how to compile ECL with C++(14). You can extend on this
demo to form a more complicate and productive project.

# Preparation 
Before you build the demo, make sure you have those dependencies installed: 
1. ECL, of course. We recommend version 16.1.2.
2. g++/clang compiler with at least C++14 support.
3. make
4. Qt5.x with Qt Creator.
5. Quicklisp installed on your ECL.

We use the external Lisp package :lparallel so you better download
that package in advance using `(ql:quickload :lparallel)`.

# Build

## Build CL Library and FASB

Run `make` in current directory and you get two files, if
successful. `lisp-envi.a` and `hello-lisp-system--all-systems.fasb`.

## Configure and build your Qt Project

To build the example it is enough to change to the `qt/` directory,
generate a Makefile with `qmake` and to call `make`.

```shell
cd qt/
qmake
make
```

If you want to change your Qt project, open it with the `Qt
Creator`. It can build the executable for you (instead of manually
working with make).

# Run

After you go through the steps above, go for the executable file and
try that demo.

Happy hacking with ECL!

ntr(Lexicall)
