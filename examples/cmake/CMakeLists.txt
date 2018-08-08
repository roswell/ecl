# Copyright (C) 2018 marcinkolenda419@gmail.com
#
# Permission to use, copy, modify, and/or distribute this software for
# any purpose with or without fee is hereby granted.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
# AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
# OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

cmake_minimum_required(VERSION 3.10)
project(cmake_ex_proj)
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Find ECL library. Refer to ./cmake/Modules/FindECL.cmake and 
# https://cmake.org/cmake/help/v3.0/command/find_package.html
find_package(ECL REQUIRED)
include_directories(${ECL_INCLUDE_DIR})

set(CMAKE_CXX_STANDARD 14) # doesn't apply to ECL compiler

# Put lisp sources and other files relevant for compilation here
# Any change of that files will trigger recompilation of this target
set (CORE_LISP_SOURCES
        src/lisp/core-lisp.asd
        src/lisp/core-lisp.lisp
        )

# Specify how `core-lisp` library is build by ECL
add_custom_command (OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/core-lisp.a
  COMMAND ${ECL_BIN_PATH} --norc
  --eval '(require :asdf)'
  --eval '(princ (asdf:asdf-version))'

  # remember to end path with / here. Notably ....src/lisp/, not ....src/lisp
  --eval '(push \"${CMAKE_CURRENT_SOURCE_DIR}/src/lisp/\" asdf:*central-registry*)'

  --eval '(asdf:make-build :core-lisp :type :static-library :move-here \"${CMAKE_CURRENT_BINARY_DIR}\")'
  --eval '(quit)'
  DEPENDS ${CORE_LISP_SOURCES}
        )
add_custom_target(core-lisp ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/core-lisp.a)

add_executable(cmake_ex
  src/cxx/main.cpp)

add_dependencies(cmake_ex core-lisp)

target_link_libraries(cmake_ex
  ${ECL_LIBRARIES}
  ${CMAKE_CURRENT_BINARY_DIR}/core-lisp.a
  )

set_target_properties(cmake_ex PROPERTIES
  CXX_STANDARD 17
  CXX_EXTENSION ON
  )
