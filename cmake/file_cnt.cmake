
set(ECL_FILE_CNT "0" CACHE STRING "Check whether the FILE structure has a field with the number of characters left in the buffer.")

include(CheckCSourceCompiles)

set(FILE_CNT_1_TEST_CODE "
#include <stdio.h>
#include <stdio_ext.h>
int main() {
    FILE *f = fopen(\"conftestval\",\"w\");
    if (__freadahead((f)))
        return 1;
    return 0;
}")

set(FILE_CNT_2_TEST_CODE "
#include <stdio.h>
int main() {
    FILE *f = fopen(\"conftestval\",\"w\");
    if ((f)->_IO_read_end - (f)->_IO_read_ptr)
        return 1;
    return 0;
}")

set(FILE_CNT_3_TEST_CODE "
#include <stdio.h>
int main() {
    FILE *f = fopen(\"conftestval\",\"w\");
    if ((f)->_r)
        return 1;
    return 0;
}")

set(FILE_CNT_4_TEST_CODE "
#include <stdio.h>
int main() {
    FILE *f = fopen(\"conftestval\",\"w\");
    if ((f)->_cnt)
        return 1;
    return 0;
}")

check_c_source_compiles("${FILE_CNT_1_TEST_CODE}" HAVE_CNT_1)
check_c_source_compiles("${FILE_CNT_2_TEST_CODE}" HAVE_CNT_2)
check_c_source_compiles("${FILE_CNT_3_TEST_CODE}" HAVE_CNT_3)
check_c_source_compiles("${FILE_CNT_4_TEST_CODE}" HAVE_CNT_4)

if(HAVE_CNT_1)
  set(ECL_FILE_CNT 1)
endif()

if(HAVE_CNT_2)
  set(ECL_FILE_CNT 2)
endif()

if(HAVE_CNT_3)
  set(ECL_FILE_CNT 3)
endif()

if(HAVE_CNT_4)
  set(ECL_FILE_CNT 4)
endif()
