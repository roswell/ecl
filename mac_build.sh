#!/bin/bash

mkdir -p build_mac
cd build_mac

# export CFLAGS="-arch i386"
# export LDFLAGS="-arch i386"

../src/configure --prefix=/usr/local/opt/ecl-ue4-mac --enable-unicode=yes --enable-threads=yes --with-cxx=yes
# ../src/configure --prefix=/usr/local/opt/ecl-ue4-mac --enable-unicode=yes --enable-threads=yes --with-cxx=yes --enable-debug --with-debug-cflags="-O0" --disable-shared
# ../src/configure --prefix=/usr/local/opt/ecl-ue4-mac --enable-unicode=yes --enable-threads=yes --with-cxx=yes --enable-debug --with-debug-cflags="-O0" --enable-shared

make -j8
