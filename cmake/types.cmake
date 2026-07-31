set(ECL_LONG_LONG_BITS "64" CACHE STRING "Size of long long type")
set(ecl_long_long_t "" CACHE STRING "long long data type synonym")
set(ecl_ulong_long_t "" CACHE STRING "unsigned long long data type synonym")

set(ECL_UINT8_T  "" CACHE STRING "There is an uint8_t type")
set(ECL_UINT16_T "" CACHE STRING "There is an uint16_t type")
set(ECL_UINT32_T "" CACHE STRING "There is an uint32_t type")
set(ECL_UINT64_T "" CACHE STRING "There is an uint64_t type")

set(ECL_INT8_T  "" CACHE STRING "There is an int8_t type")
set(ECL_INT16_T "" CACHE STRING "There is an int16_t type")
set(ECL_INT32_T "" CACHE STRING "There is an int32_t type")
set(ECL_INT64_T "" CACHE STRING "There is an int64_t type")

set(ecl_int8_t  "" CACHE STRING "There is an int8_t type")
set(ecl_int16_t "" CACHE STRING "There is an int16_t type")
set(ecl_int32_t "" CACHE STRING "There is an int32_t type")
set(ecl_int64_t "" CACHE STRING "There is an int64_t type")

set(CL_FIXNUM_MIN "" CACHE STRING "Minimal value of fixnum type")
set(CL_FIXNUM_MAX "" CACHE STRING "Maximal value of fixnum type")

set(CL_SHORT_BITS "" CACHE STRING "Number of bits in short type")
set(CL_INT_BITS   "" CACHE STRING "Number of bits in int type")
set(CL_LONG_BITS  "" CACHE STRING "Number of bits in long type")

set(CL_FIXNUM_TYPE "" CACHE STRING "Name of type for CL_FIXNUM")

set(ECL_STDINT_HEADER "" CACHE STRING "Header with int/uint types")
set(HAVE_POSIX_RWLOCK "" CACHE STRING "POSIX read/write locks are available")

set(ECL_BIGENDIAN "no" CACHE STRING "Big endianess")
set(WORDS_BIGENDIAN "" CACHE STRING "Big endianess for libraries")

include(CheckTypeSize)

## long long type check
check_type_size("long long" LONG_LONG_SIZE_BYTES)

if(LONG_LONG_SIZE_BYTES)
  set(ecl_long_long_t "long long")
  set(ecl_ulong_long_t "unsigned long long")
  math(EXPR ECL_LONG_LONG_SIZE "${LONG_LONG_SIZE_BYTES} * 8")
  list(APPEND LSP_FEATURES :long-long)
  message(STATUS "long long type found")
else()
  message(WARNING "long long type not found")
endif()

## Fixnum checks
check_type_size("void*" POINTER_SIZE_BYTES)
check_type_size("short" SHORT_SIZE_BYTES)
check_type_size("int"   INT_SIZE_BYTES)
check_type_size("long"  LONG_SIZE_BYTES)

math(EXPR POINTER_SIZE       "${POINTER_SIZE_BYTES}   * 8")
math(EXPR CL_SHORT_SIZE      "${SHORT_SIZE_BYTES}     * 8")
math(EXPR CL_INT_SIZE        "${INT_SIZE_BYTES}       * 8")
math(EXPR CL_LONG_SIZE       "${LONG_SIZE_BYTES}      * 8")

# Base fixnum range on pointer size
math(EXPR CL_FIXNUM_MIN "~0 << (${POINTER_SIZE} - 3)")
math(EXPR CL_FIXNUM_MAX "-(${CL_FIXNUM_MIN}+1)")

message(STATUS "void* size: ${POINTER_SIZE}")
message(STATUS "short size: ${CL_SHORT_SIZE}")
message(STATUS "int size:   ${CL_INT_SIZE}")
message(STATUS "long size:  ${CL_LONG_SIZE}")

# Select appropriate type for fixnum integers
if(CL_LONG_SIZE EQUAL POINTER_SIZE) 
  set(CL_FIXNUM_TYPE "long")
elseif(CL_INT_SIZE EQUAL POINTER_SIZE)
  set(CL_FIXNUM_TYPE "int")
elseif(CL_SHORT_SIZE EQUAL POINTER_SIZE)
  set(CL_FIXNUM_TYPE "short")
elseif(ecl_int64_t EQUAL POINTER_SIZE)
  set(CL_FIXNUM_TYPE "int64_t")
elseif(ECL_LONG_LONG_SIZE EQUAL POINTER_SIZE)
  set(CL_FIXNUM_TYPE "long long")
else()
  message(FATAL_ERROR "Unknown size for fixnum. Pointer size: ${POINTER_SIZE}")
endif()

set(CL_SHORT_BITS  ${CL_SHORT_SIZE})
set(CL_INT_BITS    ${CL_INT_SIZE})
set(CL_LONG_BITS   ${CL_LONG_SIZE})
set(CL_FIXNUM_BITS ${POINTER_SIZE})

set(ECL_INT_BITS             ${CL_INT_BITS})
set(ECL_LONG_BITS            ${CL_LONG_BITS})
set(ECL_FIXNUM_BITS          ${CL_FIXNUM_BITS})
                           
set(MOST_POSITIVE_FIXNUM     ${CL_FIXNUM_MAX})
set(MOST_NEGATIVE_FIXNUM     ${CL_FIXNUM_MIN})
set(MOST_POSITIVE_FIXNUM_VAL ${CL_FIXNUM_MAX})
set(MOST_NEGATIVE_FIXNUM_VAL ${CL_FIXNUM_MIN})

## Check if intX_t/uintX_t types are available
foreach(INT_T 8; 16; 32; 64) 

  check_type_size("int${INT_T}_t"  HAS_INT${INT_T}_T)
  check_type_size("uint${INT_T}_t"  HAS_UINT${INT_T}_T)
  
  if (HAS_INT${INT_T}_T)
    set(ECL_INT${INT_T}_T "int${INT_T}_t")
    set(ecl_int${INT_T}_t "int${INT_T}_t")
  endif()

  if (HAS_UINT${INT_T}_T)
    set(ECL_UINT${INT_T}_T "uint${INT_T}_t")
    set(ecl_uint${INT_T}_t "uint${INT_T}_t")
    list(APPEND LSP_FEATURES :uint${INT_T}-t)
  endif()

endforeach()

## Include file for int/uint types
include(CheckIncludeFiles)

check_include_files(stdint.h HAVE_STDINT_H)

if(HAVE_STDINT_H)
  set(ECL_STDINT_HEADER "#include <stdint.h>")
endif()

check_include_files(inttypes.h HAVE_INTTYPES_H)

if(NOT HAVE_STDINT_H AND HAVE_INTTYPES_H)
  set(ECL_STDINT_HEADER "#include <inttypes.h>")
endif()

## Endianess
if(CMAKE_C_BYTE_ORDER STREQUAL "BIG_ENDIAN")
  set(WORDS_BIGENDIAN 1)
  set(ECL_BIGENDIAN "yes")
  list(APPEND LSP_FEATURES :big-endian) 
elseif(CMAKE_C_BYTE_ORDER STREQUAL "LITTLE_ENDIAN")
  set(WORDS_BIGENDIAN FALSE)
  list(APPEND LSP_FEATURES :little-endian) 
else()
  set(WORDS_BIGENDIAN FALSE)
  message(WARNING "Endianness could not be determined. Set to big endian.")
endif()

check_type_size("pthread_rwlock_t" POSIX_RWLOCK_SIZE)
if (POSIX_RWLOCK_SIZE)
  set(HAVE_POSIX_RWLOCK 1)
endif()
