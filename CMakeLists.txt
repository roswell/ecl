cmake_minimum_required(VERSION 3.20)
project(ecl LANGUAGES C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Отключаем предупреждения об устаревшей функции gets()
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    message(NOTICE "GNU")
    add_compile_options(-Wno-deprecated-declarations)
endif()

include_directories(${libatomic_ops_SOURCE_DIR}/src)
include_directories(${bdwgc_SOURCE_DIR}/src/include)

option(ECL_WITH_TCP ON)     # yes, no
option(ECL_TCP_BUILTIN OFF) # builtin

if(CMAKE_HOST_SYSTEM_PROCESSOR MATCHES "x86_64")
	set(ECL_FPE_CODE "arch/fpe_x86.c")
endif()

# ============================

# TODO create defines.cmake?

include(cmake/options.cmake)

# Defines LSP_FEATURES
include(cmake/core.cmake)

include(cmake/functions.cmake)
include(cmake/headers.cmake)
include(cmake/types.cmake)
include(cmake/complex.cmake)
include(cmake/unicode.cmake)
include(cmake/gc.cmake)
include(cmake/libatomic.cmake)
include(cmake/libffi.cmake)
include(cmake/tcp.cmake)
include(cmake/newline.cmake)
include(cmake/stack.cmake)
include(cmake/sse.cmake)
include(cmake/fpe.cmake)
include(cmake/file_cnt.cmake)
include(cmake/atomic.cmake)
include(cmake/boehm.cmake)

foreach(v ${LSP_FEATURES})
    message(STATUS "LSP Feature ${v}")
endforeach()

# Generate defines
include(cmake/configures.cmake)

include(cmake/targets.cmake)
# ============================

set(LSP_FEATURES ${LSP_FEATURES} CACHE INTERNAL "LSP_FEATURES")
