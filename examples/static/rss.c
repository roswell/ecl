#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/resource.h>

int main(int argc, char **argv) {
    pid_t p = fork();
    if (!p) { execv(argv[1], argv + 1); _exit(127); }
    int st; struct rusage ru;
    wait4(p, &st, 0, &ru);
    printf("peak RSS: %ld KiB\n", ru.ru_maxrss);
    return 0;
}
