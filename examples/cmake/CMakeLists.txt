#=============================================================================
# CMake project configuration file.
# Documentation: https://cmake.org/cmake/help/v3.6/
#=============================================================================
cmake_minimum_required(VERSION 3.6)  # released in 2016
project(cmake_ecl_proj)
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Find ECL library. Refer to ./cmake/Modules/FindECL.cmake and
# https://cmake.org/cmake/help/v3.0/command/find_package.html
find_package(ECL REQUIRED)
include_directories(${ECL_INCLUDE_DIR})


# Put lisp sources and other files relevant for compilation here
# Any change of that files will trigger recompilation of `core-lisp` target
set(CORE_LISP_SOURCES
        src/lisp/core-lisp.asd
        src/lisp/core-lisp.lisp)

# Specify how `core-lisp` library is build by ECL
# The library is going to be built statically and moved to
# ${CMAKE_CURRENT_BINARY_DIR}.
#
# How to customize
# ================
# You can change "core-lisp" consistently to different name.
# Value of `:init-name` keyword must match extern declaration in C++ code.
add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/core-lisp.a
  COMMAND ${ECL_BIN_PATH} --norc
  --eval '(require :asdf)'
  # Remember to end path with / here. Notably ....src/lisp/, not ....src/lisp
  --eval '(push \"${CMAKE_CURRENT_SOURCE_DIR}/src/lisp/\" asdf:*central-registry*)'
  --eval '(asdf:make-build :core-lisp :type :static-library :move-here \"${CMAKE_CURRENT_BINARY_DIR}\" :init-name \"init_lib_CORE_LISP\")'
  --eval '(quit)'
  DEPENDS ${CORE_LISP_SOURCES})
# This goes in pair with `add_custom_command` above.
add_custom_target(core-lisp ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/core-lisp.a)

# Define executable to build. You can add other sources here.
# You may change "cmake_ecl" consistently into any name.
add_executable(cmake_ecl
  src/cxx/main.cpp)

# Make cmake_ecl depend on core-lisp so it's going to be build before executable
add_dependencies(cmake_ecl core-lisp)

# Set what should be linked into `cmake_ecl` executable. 
target_link_libraries(cmake_ecl
  ${ECL_LIBRARIES}
  ${CMAKE_CURRENT_BINARY_DIR}/core-lisp.a)

set_target_properties(cmake_ecl PROPERTIES
  # Set c++ standard
  CXX_STANDARD 17
  # Do you want to use custom compiler extensions?
  CXX_EXTENSION ON)

