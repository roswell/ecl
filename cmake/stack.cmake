set(ECL_DOWN_STACK "" CACHE STRING "Set if stack grows downwards")
set(ECL_CAN_SET_STACK_SIZE "" CACHE STRING "Set if we can set the stack size at runtime")
set(ECL_DEFAULT_C_STACK_SIZE "1048576" CACHE STRING "Default stack size")

try_run(
    STACKDOWN_RUN_RESULT                  # Variable to store the exit code (e.g., 0)
    STACKDOWN_COMPILE_RESULT              # Variable to store if compilation succeeded (TRUE/FALSE)
    ${CMAKE_BINARY_DIR}                   # Where to perform the test build
    ${CMAKE_SOURCE_DIR}/cmake/stackdown.c # Your source file
    RUN_OUTPUT_VARIABLE RUN_OUT           # Capture the program's stdout/stderr
)

if(STACKDOWN_COMPILE_RESULT AND STACKDOWN_RUN_RESULT EQUAL 0)
  set(ECL_DOWN_STACK 1)
endif()

if(HAVE_SYS_RESOURCE_H)
  include(CheckSymbolExists)
  check_symbol_exists(RLIMIT_STACK "sys/resource.h" HAVE_RLIMIT_STACK)
  if(HAVE_RLIMIT_STACK)
    set(ECL_CAN_SET_STACK_SIZE 1)
  endif()
endif()