#define _GNU_SOURCE
#include <fenv.h>
#include <signal.h>
#include <stdlib.h>

const int traps = FE_DIVBYZERO | FE_OVERFLOW;

void fpe_handler(int code) {
	if (code == SIGFPE)
		exit(0);
}

double raises_fpe(double x) {
	return x / 0.0;
}

int main() {
	signal(SIGFPE, fpe_handler);
	feclearexcept(traps);
	feenableexcept(traps);
	raises_fpe(1.0);
	return 1;
}
