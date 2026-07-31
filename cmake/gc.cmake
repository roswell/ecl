if (ECL_ENABLE_SMALLCONS AND NOT ECL_ENABLE_BOEHM)
  message(FATAL_ERROR "ECL_ENABLE_SMALLCONS requires ECL_ENABLE_BOEHM to be turned ON")
endif()

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

include(FetchContent)

FetchContent_Declare(
  bdwgc
  GIT_REPOSITORY         https://github.com/bdwgc/bdwgc.git
  GIT_TAG                v8.2.12
  GIT_SUBMODULES_RECURSE TRUE
)

#FetchContent_MakeAvailable(bdwgc)

  set(ECL_BOEHM_GC_HEADER "gc/gc.h")

#   FASL_LIBS="${FASL_LIBS} -lgc"
#   EXTRA_OBJS="${EXTRA_OBJS} alloc_2.${OBJEXT}"
#   AC_DEFINE(GBC_BOEHM, [1], [Use Boehm's garbage collector])
  set(GBC_BOEHM 1)
endif()
