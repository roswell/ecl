#!/bin/bash
#
# This script checks if cross compiling ECL itself from x86 to x86_64
# and vice versa works. It assumes that you are running an x86_64
# system which can also run and compile x86 programs. You might have
# to install additional packages for that (for example on debian, the
# gcc-multilib package is needed).
#
# Four versions of ECL will be compiled:
# - ecl-x86[_64]-native: direct (i.e. same host and target) x86[_64] build
# - ecl-x86[_64]-native: cross build for x86[_64] target
#
# The results of running the test suite will be put in the files
# test-results-x86[_64]-[native/cross]. It is recommended to also run
# the ansi-tests for the output binaries.
#
set -e
rm -rf build/; CFLAGS="-g -O2" ./configure --prefix=`pwd`/ecl-x86_64-native && make -j${JOBS} && rm -rf ecl-x86_64-native && make install && make check > test-results-x86_64-native
rm -rf build/; ABI=32 CFLAGS="-g -O2 -m32" LDFLAGS="-m32" ./configure --prefix=`pwd`/ecl-x86-native && make -j${JOBS} && rm -rf ecl-x86-native && make install && make check > test-results-x86-native
rm -rf build/; CFLAGS="-g -O2" ECL_TO_RUN=`pwd`/ecl-x86-native/bin/ecl ./configure --prefix=`pwd`/ecl-x86_64-cross --build=x86_64-pc-linux-gnu --host=x86-pc-linux-gnu --with-cross-config=`pwd`/src/util/x86_64-linux-gnu.cross_config && make -j${JOBS} && rm -rf ecl-x86_64-cross && make install && make check > test-results-x86_64-cross
rm -rf build/; ABI=32 CFLAGS="-g -O2 -m32" LDFLAGS="-m32" ECL_TO_RUN=`pwd`/ecl-x86_64-native/bin/ecl ./configure --prefix=`pwd`/ecl-x86-cross --build=x86-pc-linux-gnu --host=x86_64-pc-linux-gnu --with-cross-config=`pwd`/src/util/x86-linux-gnu.cross_config && make -j${JOBS} && rm -rf ecl-x86-cross && make install && make check > test-results-x86-cross
