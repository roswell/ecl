#include <stddef.h>
#include <stdlib.h>

ptrdiff_t f2(const char *d) {
  char c[2];
  return c-d;
}

ptrdiff_t f1(const char *d) {
  char c[2];
  return c+1-d;
}

typedef ptrdiff_t (*f_ptr)(const char *);
f_ptr f[2] = { f1, f2 };

ptrdiff_t signo() {
  char d[1];
  return f[rand() & 1](d);
}

int main() {
  if (signo() > 0)
    return 1;
  else
    return 0;
}
