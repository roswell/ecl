#!/bin/bash
#
# This script allows running the ECL test suite on android. USB
# debugging via adb needs to be enabled in order for this to work.
# Simply execute this file from the ECL toplevel directory.
#
# The script works by pushing the generated binary in `ecl-android/`
# to a temporary directory on the device together with the
# `test-android-target.sh` script which sets up paths and starts ECL
# on the phone.
#

export TMPDIR=/data/local/tmp/

adb push ecl-android/ ${TMPDIR}
adb push src/tests/ ${TMPDIR}/ecl-android/
adb push src/util/test-android-target.sh ${TMPDIR}/ecl-android/
adb shell "sh ${TMPDIR}/ecl-android/test-android-target.sh"
