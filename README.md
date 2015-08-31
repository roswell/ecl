ECL stands for Embeddable Common-Lisp. The ECL project aims to
produce an implementation of the Common-Lisp language which complies
to the ANSI X3J13 definition of the language.

The term embeddable refers to the fact that ECL includes a Lisp to C
compiler, which produces libraries (static or dynamic) that can be
called from C programs. Furthermore, ECL can produce standalone
executables from Lisp code and can itself be linked to your programs
as a shared library. It also features an interpreter for situations
when a C compiler isn't available.

ECL supports the operating systems Linux, FreeBSD, NetBSD, DragonFly
BSD, OpenBSD, Solaris (at least v. 9), Microsoft Windows (MSVC, MinGW
and Cygwin) and OSX, running on top of the Intel, Sparc, Alpha, ARM
and PowerPC processors.  Porting to other architectures should be
rather easy.