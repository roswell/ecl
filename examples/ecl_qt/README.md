This demo shows how to embed ECL into Qt5 and serve as kernel. This also discuss how to compile ECL with C++(14). You can extend on this demo to form a more complicate and productive project.

# Preparation 
Before you build the demo, make sure you have those dependencies installed: 
1. ECL, of course. We recommend version 16.1.2.
2. g++/clang compiler with at least C++14 support.
3. make
4. Qt5.x with Qt Creator.
5. Quicklisp installed on your ECL.

We use the external Lisp package :lparallel so you better download that package in advance using (ql:quickload :lparallel).

# Build  
## Build CL Library and FASB
Run `make` in current directory and you get two files, if successful. `lisp-envi.a` and `hello-lisp-system--all-systems.fasb`.
## Configure your Qt Project
cd to the directory `qt` and open that Qt project with your Qt Creator. Change the three paths I marked for you, if necessary.
1. `INCLUDEPATH`: The path that contains ecl/ecl.h. 
In Linux it may be `/usr/include/`.
2. `LIBS`:The path that leads to the shared library of ECL. 
In Linux, it may be `/usr/lib/libecl.so/`.
## Build Qt Project
Build your Qt Project. This will generate an executable file for you.
## Engage `fasb` file
After your Qt project is built, move the `hello-lisp-system--all-systems.fasb` file that generated in build step 1 into the directory containing the executable file.

# Run
After you go through the steps above, go for the executable file and try that demo.

Happy hacking with ECL!

ntr(Lexicall)
