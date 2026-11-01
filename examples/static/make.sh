#!/bin/sh

# ./configure --disable-threads --disable-unicode --with-dffi=no --enable-gmp=included --disable-shared --enable-static --disable-dlopen --prefix=`pwd`/ecl-static-builds/ && make -j32 && make install

# ./configure --disable-shared --enable-static --disable-dlopen --prefix=`pwd`/ecl-static-builds/ && make -j32 && make install


# Depending on flags, these may need to be necesasry
# static: -leclgc leclgmp
# shared:  -lffi -lgmp -lpthread -ldl

# Static linking (:libs)
# 64bit rss: 10312KB
function ver2 () {
    ECL_PATH="/home/jack/Workshop/Repositories/ecl/ecl-static-builds"
    CFLAGS="-Dlinux   -I${ECL_PATH}/include" 
    LFLAGS="-L${ECL_PATH}/lib64 -lecl -leclgc -lffi -lgmp -lpthread -lm"

    gcc ${CFLAGS} hello.c -o test.out ${LFLAGS}
    # /usr/bin/time -v ./test.out
    ./rss test.out
}

# Static linking (:libc)
# 64bit rss: 9040KB
function ver3 () {
    ECL_PATH="/home/jack/Workshop/Repositories/ecl/ecl-static-builds"
    CFLAGS="-Dlinux   -I${ECL_PATH}/include" 
    LFLAGS="-static -L${ECL_PATH}/lib64 -lecl -leclgc -leclgmp -lm"
    # 32bit musl
    #ECL_PATH="/root/ecl/ecl-static-builds"
    #CFLAGS="-Dlinux   -I${ECL_PATH}/include" 
    #LFLAGS="-static -no-pie -L${ECL_PATH}/lib -lecl -leclgc -leclgmp -lm"


    gcc ${CFLAGS} hello.c -o test.out ${LFLAGS}
    # /usr/bin/time -v ./test.out
    ./rss test.out
}

gcc rss.c -o rss
ver2
