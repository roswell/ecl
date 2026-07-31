
configure_file(
  ${CMAKE_CURRENT_LIST_DIR}/defines-function.cmake
  ${PROJECT_BINARY_DIR}/include/ecl/defines-function.h
)

configure_file(
  ${CMAKE_CURRENT_LIST_DIR}/defines-header.cmake
  ${PROJECT_BINARY_DIR}/include/ecl/defines-header.h
)

configure_file(
  ${CMAKE_CURRENT_LIST_DIR}/defines-features.cmake
  ${PROJECT_BINARY_DIR}/include/ecl/defines-feature.h
)

configure_file(
  ${CMAKE_CURRENT_LIST_DIR}/defines-unicode.cmake
  ${PROJECT_BINARY_DIR}/include/ecl/defines-unicode.h
)

configure_file(
  ${CMAKE_CURRENT_LIST_DIR}/defines-types.cmake
  ${PROJECT_BINARY_DIR}/include/ecl/defines-types.h
)

configure_file(
  ${CMAKE_CURRENT_LIST_DIR}/defines-complex.cmake
  ${PROJECT_BINARY_DIR}/include/ecl/defines-complex.h
)

configure_file(
  ${CMAKE_CURRENT_LIST_DIR}/defines-boehm.cmake
  ${PROJECT_BINARY_DIR}/include/ecl/defines-boehm.h
)

configure_file(
  ${CMAKE_CURRENT_LIST_DIR}/config.h
  ${PROJECT_BINARY_DIR}/include/ecl/config.h
)

configure_file(
  ${CMAKE_CURRENT_LIST_DIR}/config-internal.h
  ${PROJECT_BINARY_DIR}/include/ecl/config-internal.h
)

set(LSP_FEATURES_OLD ${LSP_FEATURES})
list(JOIN LSP_FEATURES " " LSP_FEATURES)

set(COMPILATION_FEATURES_OLD ${COMPILATION_FEATURES})
list(JOIN COMPILATION_FEATURES " " COMPILATION_FEATURES)

configure_file(
  ${PROJECT_SOURCE_DIR}/src/compile.lsp.in 
  ${PROJECT_BINARY_DIR}/compile.lsp
)

set(COMPILATION_FEATURES ${COMPILATION_FEATURES_OLD})

### Copy header files

set(BUNDLE "${PROJECT_BINARY_DIR}/bundle/")
set(BUNDLE_INCLUDE "${BUNDLE}/include/ecl/")
set(BUNDLE_BIN "${BUNDLE}/bin/")
set(BUNDLE_LIB "${BUNDLE}/lib/")
set(BUNDLE_MAN "${BUNDLE}/share/man/man1/")

file(MAKE_DIRECTORY "${BUNDLE_INCLUDE}")
file(MAKE_DIRECTORY "${BUNDLE_INCLUDE}/impl/")
file(MAKE_DIRECTORY "${BUNDLE_BIN}")
file(MAKE_DIRECTORY "${BUNDLE_LIB}")
file(MAKE_DIRECTORY "${BUNDLE_MAN}")

set(HGENERATED
  config.h
  config-internal.h
  defines-boehm.h
  defines-complex.h
  defines-feature.h
  defines-function.h
  defines-header.h
  defines-types.h
  defines-unicode.h
)

foreach(HGEN ${HGENERATED}) 
  file(COPY_FILE ${PROJECT_BINARY_DIR}/include/ecl/${HGEN} ${BUNDLE_INCLUDE}/${HGEN})
endforeach()

set(HFILES
  bytecodes.h
  cache.h 
  cons.h
  cs.h 
  ecl-cmp.h 
  ecl-inl.h 
  ecl.h 
  ecl_atomics.h
  external.h 
  impl/math_dispatch.h 
  impl/math_dispatch2.h
  impl/math_fenv.h
  internal.h 
  legacy.h 
  nucleus.h
  number.h 
  object.h
  page.h 
  stack-resize.h
  stacks.h 
  threads.h 
)

if (ECL_LIBATOMIC_INCLUDED)
  file(COPY_FILE ${libatomic_ops_SOURCE_DIR}/src/atomic_ops.h ${BUNDLE_INCLUDE}/atomic_ops.h)
endif()

foreach(HFILE ${HFILES})
    file(COPY_FILE "${PROJECT_SOURCE_DIR}/src/h/${HFILE}" "${BUNDLE_INCLUDE}/${HFILE}")
endforeach()
