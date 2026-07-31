if (ECL_ENABLE_GENGC AND NOT ECL_ENABLE_BOEHM)
  message(FATAL_ERROR "ECL_ENABLE_GENGC requires ECL_ENABLE_BOEHM to be turned ON")
endif()

if (ECL_ENABLE_PRECISEGC AND NOT ECL_ENABLE_BOEHM)
  message(FATAL_ERROR "ECL_ENABLE_GENGC requires ECL_ENABLE_BOEHM to be turned ON")
endif()

if (ECL_ENABLE_GENGC AND ECL_ENABLE_PRECISEGC)
  message(FATAL_ERROR "ECL_ENABLE_GENGC and ECL_ENABLE_PRECISEGC can not be turned ON together")
endif()

if (ECL_ENABLE_BOEHM)

if (ECL_BOEHM_INCLUDED)
    set(BUILD_SHARED_LIBS OFF CACHE BOOL "Build GC as static library" FORCE)
    message(STATUS "Set up included Boehm GC")
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/bdwgc/)
    message(STATUS "boehm source dir ${gc_SOURCE_DIR}")
    message(STATUS "boehm lib dir ${gc_BINARY_DIR}")

    set(ECL_BOEHM_GC_HEADER "gc/gc.h")
    set(GBC_BOEHM 1)
    if (ECL_THREADS)
        set(GC_THREADS 1)
    endif()
    list(APPEND LSP_FEATURES :boehm-gc)
    list(APPEND LSP_FEATURES :ecl-weak-hash)
else()
    message(FATAL_ERROR "Boehm GC get from system (Find...)")
endif()

if (ECL_ENABLE_GENGC)
    set(GBC_BOEHM_GENGC "1" CACHE STRING "Boehm Gen GC")
endif()

if (ECL_ENABLE_PRECISEGC)
    set(GBC_BOEHM_PRECISE "1" CACHE STRING "Boehm GC precise mode")
endif()

if (ECL_ENABLE_SMALLCONS)
    set(ECL_SMALL_CONS 1)
endif()

endif()

