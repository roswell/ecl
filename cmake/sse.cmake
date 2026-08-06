set(ECL_SSE2 "" CACHE STRING "SSE2 intrinsics are available")

if (ECL_WITH_SSE)

cmake_host_system_information(RESULT HAS_SSE2 QUERY HAS_SSE2)

set(ECL_SSE2 ${HAS_SSE2})

if(ECL_SSE2)
    list(APPEND LSP_FEATURES :sse2)
endif()

endif()