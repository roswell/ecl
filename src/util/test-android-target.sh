#!/bin/sh
export TMPDIR=/data/local/tmp/
cd ${TMPDIR}/ecl-android/
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${TMPDIR}/ecl-android/lib/
export ECLDIR=${TMPDIR}/ecl-android/lib/ecl-$(./bin/ecl --eval "(princ (lisp-implementation-version))" --eval "(ext:quit)")/
./bin/ecl -norc -eval "(ext:install-bytecodes-compiler)" -load "tests/doit.lsp" -eval "(in-package cl-test)" -eval "(2am-ecl:run 'make-check)" -eval "(ext:exit)"
rm -r ${TMPDIR}/ecl-android/
