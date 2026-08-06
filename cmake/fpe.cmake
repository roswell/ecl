set(ECL_FPE_CODE "arch/fpe_none.c" CACHE STRING "Choose the type of code to detect floating point exceptions and raise them.")
set(HAVE_FEENABLEEXCEPT "0" CACHE STRING "Check if we have feenableexcept and the hardware generates floating point exceptions.")

if (ECL_WITH_FPE)

if(${CMAKE_HOST_SYSTEM_PROCESSOR} MATCHES "i686|i586|pentium*|athlon*|x86_64*|AMD64*")
  set(ECL_FPE_CODE "arch/fpe_x86.c")
endif()

try_run(
    FPE_RUN_RESULT
    FPE_COMPILE_RESULT
    ${CMAKE_BINARY_DIR}/
    ${CMAKE_SOURCE_DIR}/cmake/fpe.c
    RUN_OUTPUT_VARIABLE FPE_RUN_OUT
    LINK_LIBRARIES -lm
)

if(FPE_COMPILE_RESULT AND FPE_RUN_RESULT EQUAL 0)
  message(STATUS "FP exception available")
  set(HAVE_FEENABLEEXCEPT 1)
else()
    message(STATUS "FP exception not available")
    set(ECL_AVOID_FPE_H 1)
endif()

endif()

### FP, FPE, Signed zero
if(ECL_WITH_IEEE_FP)
    list(APPEND LSP_FEATURES :ieee-floating-point)
    set(ECL_IEEE_FP 1)
    set(ECL_WITH_SIGNED_ZERO ON)
endif()

if (ECL_WITH_SIGNED_ZERO)
    set(ECL_SIGNED_ZERO 1)
endif()

if ((NOT ECL_WITH_IEEE_FP) OR ECL_WITH_FPE)
    list(APPEND LSP_FEATURES :floating-point-exceptions)
endif()
