if (ECL_ENABLE_LIBATOMIC)

include(FetchContent)

FetchContent_Declare(
  libatomic_ops
  GIT_REPOSITORY         https://github.com/bdwgc/libatomic_ops.git
  GIT_TAG                v7.10.0
  GIT_SUBMODULES_RECURSE TRUE
)

#FetchContent_MakeAvailable(libatomic_ops)

# set(ECL_LIBATOMIC_OPS_H )

endif()
