
/* -- imports ------------------------------------------------------- */

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/external.h>

int main() {
  ecl_boot();
  printf("Hello ECL! %p\n", ecl_core.first_env);
  return 0;
}
