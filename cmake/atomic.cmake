if (ECL_ENABLE_LIBATOMIC)

if (ECL_LIBATOMIC_INCLUDED)
    message(STATUS "Set up included libatomic_ops library")
    add_subdirectory(${CMAKE_SOURCE_DIR}/src/bdwgc/libatomic_ops/)
    message(STATUS "libatomic_ops source dir ${libatomic_ops_SOURCE_DIR}")
    message(STATUS "libatomic_ops lib dir ${libatomic_ops_BINARY_DIR}")
    set(ECL_LIBATOMIC_OPS_H 1)
else()
    message(FATAL_ERROR "libatomic_ops get from system (Find...)")
endif()

endif()