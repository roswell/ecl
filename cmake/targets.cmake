
include_directories(${PROJECT_BINARY_DIR}/bundle/include/)
include_directories(${PROJECT_SOURCE_DIR}/src/c/)

add_executable(cut src/util/cut.c)
add_executable(dpp src/c/dpp.c)

set(D_SOURCES
  src/c/compiler
  src/c/read
  src/c/format
  src/c/pathname
  src/c/hash
  src/c/apply
  #src/c/cfun_dispatch
  src/c/unixint
  src/c/array
  src/c/unixfsys
  src/c/interpreter
  src/c/alloc_2
  src/c/package
  src/c/stacks
  src/c/ffi
  src/c/string
  src/c/number
  src/c/main
  src/c/list
  src/c/cons
  src/c/disassembler
  src/c/serialize
  src/c/unixsys
  src/c/error
#  src/c/big
  src/c/predicate
  src/c/num_log
  src/c/character
  src/c/symbol
  src/c/tcp
  src/c/load
  src/c/stream
  src/c/print
  src/c/num_co
  src/c/all_symbols
  src/c/typespec
  src/c/num_rand
  src/c/assignment
  src/c/cfun
  src/c/sequence
  src/c/eval
  src/c/cmpaux
  src/c/time
  src/c/cinit
  src/c/mapfun
  src/c/backq
  src/c/macros
  src/c/reference
  src/c/file
  src/c/sse2
  src/c/structure
  src/c/char_ctype
#  src/c/big_ll
  src/c/vector_push
  src/c/process
  src/c/num_arith
  src/c/num_pred
  src/c/memory
  src/c/atomic
  src/c/multival

  src/c/clos/instance
  src/c/clos/gfun
  src/c/clos/accessor
  src/c/clos/cache

  src/c/ffi/libraries
  src/c/ffi/backtrace
  src/c/ffi/mmap
  src/c/ffi/cdata

  src/c/numbers/abs
  src/c/numbers/atan
  src/c/numbers/ceiling
  src/c/numbers/conjugate
  src/c/numbers/cos
  src/c/numbers/cosh
  src/c/numbers/divide
  src/c/numbers/exp
  src/c/numbers/expt
  # src/c/numbers/float_fix_compare
  src/c/numbers/floor
  src/c/numbers/log
  src/c/numbers/minmax
  src/c/numbers/minus
  src/c/numbers/minusp
  src/c/numbers/negate
  src/c/numbers/number_compare
  src/c/numbers/number_equalp
  src/c/numbers/one_minus
  src/c/numbers/one_plus
  src/c/numbers/plus
  src/c/numbers/plusp
  src/c/numbers/round
  src/c/numbers/sin
  src/c/numbers/sinh
  src/c/numbers/sqrt
  src/c/numbers/tan
  src/c/numbers/tanh
  src/c/numbers/times
  src/c/numbers/truncate
  src/c/numbers/zerop

  src/c/printer/write_ugly
#  src/c/printer/float_string_old
  src/c/printer/float_to_digits
  src/c/printer/write_symbol
  src/c/printer/write_array
  src/c/printer/write_object
  src/c/printer/float_to_string
  src/c/printer/integer_to_string
  src/c/printer/print_unreadable
  src/c/printer/write_sse
  src/c/printer/write_code
  src/c/printer/write_list

  src/c/reader/parse_number
  src/c/reader/parse_integer
     
  src/c/streams/strm_binary
  src/c/streams/strm_clos
  src/c/streams/strm_common
  src/c/streams/strm_composite
  src/c/streams/strm_eformat
  src/c/streams/strm_os
  src/c/streams/strm_sequence
  src/c/streams/strm_string

  src/c/threads/thread
  src/c/threads/mutex
  src/c/threads/semaphore
  src/c/threads/barrier
  src/c/threads/mailbox
  src/c/threads/condition_variable
  src/c/threads/rwlock
)    

set(C_DIRS "clos" "ffi" "numbers" "printer" "reader" "streams" "threads")

foreach(C_DIR ${C_DIRS})
    message(STATUS "make dir: ${PROJECT_BINARY_DIR}/src/c/${C_DIR}/")
    file(MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/src/c/${C_DIR}/")
endforeach()

set(GENERATED_SOURCES "")

foreach(SRC_FILE ${D_SOURCES})
    set(OUT_FILE "${PROJECT_BINARY_DIR}/${SRC_FILE}.c")
    #get_filename_component(FILE_BASE ${SRC_FILE} NAME_WE)

    # Generate a custom rule per file
    add_custom_command(
        OUTPUT "${OUT_FILE}"
        COMMAND ${PROJECT_BINARY_DIR}/dpp ${PROJECT_SOURCE_DIR}/${SRC_FILE}.d ${OUT_FILE}
        DEPENDS "${SRC_FILE}.d" dpp
        VERBATIM
    )

    # Track the output files
    #message(STATUS "dpp ${PROJECT_SOURCE_DIR}/${SRC_FILE}.d ${OUT_FILE}")
    list(APPEND GENERATED_SOURCES "${OUT_FILE}")
endforeach()

add_custom_target(gen_c_d DEPENDS ${GENERATED_SOURCES})

include_directories(${PROJECT_BINARY_DIR}/bundle/include/ecl/)

if (ECL_LIBATOMIC_INCLUDED)
  include_directories(${libatomic_ops_SOURCE_DIR}/src/)
endif()

add_library(all_symbols  OBJECT ${PROJECT_BINARY_DIR}/src/c/all_symbols.c)
add_library(all_symbols2 OBJECT ${PROJECT_BINARY_DIR}/src/c/all_symbols.c)
target_compile_definitions(all_symbols2 PRIVATE ECL_FINAL)

add_dependencies(all_symbols  gen_c_d)
add_dependencies(all_symbols2 gen_c_d)

# Pass the final list of generated files to your target
#add_executable(my_app main.cpp ${GENERATED_SOURCES})

add_library(eclmin $<TARGET_OBJECTS:all_symbols> $<TARGET_OBJECTS:all_symbols2> ${GENERATED_SOURCES})

if (NOT WIN32)
  target_compile_definitions(eclmin PUBLIC ECLMIN=${PROJECT_BINARY_DIR})
  target_link_libraries(eclmin PRIVATE m)
endif()
