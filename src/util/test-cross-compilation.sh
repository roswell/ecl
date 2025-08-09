#!/bin/bash
#
# This script checks if cross compiling ECL itself from x86 to x86_64
# and vice versa works. It assumes that you are running an x86_64
# system which can also run and compile x86 programs. You might have
# to install additional packages for that (for example on debian, the
# gcc-multilib package is needed).
#
# Additional configure options for the host and target system may be
# given in the environment variables HOST_CONFIGURE_OPTS and
# TARGET_CONFIGURE_OPTS. This allows for instance to test cross
# compilation with mismatching features in both systems.
#
# Four versions of ECL will be compiled:
# - ecl-x86[_64]-native: direct (i.e. same host and target) x86[_64] build
# - ecl-x86[_64]-native: cross build for x86[_64] target
#
# The results of running the test suite will be put in the files
# test-results/[make-check/ansi-test]-x86[_64]-[native/cross-core/cross-user].
# The difference between cross-core and cross-user is that for the
# former, only the ECL core is cross compiled while the tests are run
# natively while for the latter both ECL core and the tests are cross
# compiled.
#
set -e

mkdir -p test-results
rm -rf build/
CFLAGS="-g -O2" ./configure --prefix=`pwd`/ecl-x86_64-native ${HOST_CONFIGURE_OPTS}
make -j4
rm -rf ecl-x86_64-native
make install
set +e
make check > test-results/make-check-x86_64-native
make ansi-test > test-results/ansi-test-x86_64-native

set -e
rm -rf build/
ABI=32 CFLAGS="-g -O2 -m32" LDFLAGS="-m32" ./configure --prefix=`pwd`/ecl-x86-native ${HOST_CONFIGURE_OPTS}
make -j4
rm -rf ecl-x86-native
make install
set +e
make check > test-results/make-check-x86-native
make ansi-test > test-results/ansi-test-x86-native

set -e
rm -rf build/
CFLAGS="-g -O2" ECL_TO_RUN=`pwd`/ecl-x86-native/bin/ecl ./configure --prefix=`pwd`/ecl-x86_64-cross --build=x86-pc-linux-gnu --host=x86_64-pc-linux-gnu --with-cross-config=`pwd`/src/util/x86_64-linux-gnu.cross_config ${TARGET_CONFIGURE_OPTS}
make -j4
rm -rf ecl-x86_64-cross
make install
set +e
make check > test-results/make-check-x86_64-cross-core
ECL_TO_RUN=`pwd`/ecl-x86-native/bin/ecl make cross-check > test-results/make-check-x86_64-cross-user
make ansi-test > test-results/ansi-test-x86_64-cross-core
cp src/tests/ansi-test-expected-failures.sexp build/tests/ansi-test/expected-failures/ecl.sexp
# Bugs
echo "EPSILONS.8 EPSILONS.12" >> build/tests/ansi-test/expected-failures/ecl.sexp
# Test framework issues
echo "EVAL-WHEN.1 DEFINE-COMPILER-MACRO.3 DEFINE-COMPILER-MACRO.8 COMPILE-FILE.3 COMPILE-FILE.15 COMPILE-FILE.19 MISC.629 MISC.638" >> build/tests/ansi-test/expected-failures/ecl.sexp
ECL_TO_RUN=`pwd`/ecl-x86-native/bin/ecl EXPECTED_FAILURES="ansi-test/expected-failures/ecl.sexp" make cross-ansi-test > test-results/ansi-test-x86_64-cross-user

set -e
rm -rf build/
CFLAGS="-g -O2 -m32" LDFLAGS="-m32" ECL_TO_RUN=`pwd`/ecl-x86_64-native/bin/ecl ./configure --prefix=`pwd`/ecl-x86-cross --build=x86_64-pc-linux-gnu --host=x86-pc-linux-gnu --with-cross-config=`pwd`/src/util/x86-linux-gnu.cross_config ${TARGET_CONFIGURE_OPTS}
make -j4
rm -rf ecl-x86-cross
make install
set +e
make check > test-results/make-check-x86-cross-core
ECL_TO_RUN=`pwd`/ecl-x86_64-native/bin/ecl make cross-check > test-results/make-check-x86-cross-user
make ansi-test > test-results/ansi-test-x86-cross-core
cp src/tests/ansi-test-expected-failures.sexp build/tests/ansi-test/expected-failures/ecl.sexp
# Bugs
echo "EPSILONS.1 EPSILONS.2 EPSILONS.8 EPSILONS.12" >> build/tests/ansi-test/expected-failures/ecl.sexp
# Test framework issues
echo "EVAL-WHEN.1 DEFINE-COMPILER-MACRO.3 DEFINE-COMPILER-MACRO.8 COMPILE-FILE.3 COMPILE-FILE.15 COMPILE-FILE.19 MISC.629 MISC.638" >> build/tests/ansi-test/expected-failures/ecl.sexp
ECL_TO_RUN=`pwd`/ecl-x86_64-native/bin/ecl EXPECTED_FAILURES="ansi-test/expected-failures/ecl.sexp" make cross-ansi-test > test-results/ansi-test-x86-cross-user
