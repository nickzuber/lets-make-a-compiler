#include <inttypes.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>

void print_int(int x) {
  printf("%d\n", x);
}

void print_bool(int x) {
  if (x == 1) {
    printf("true\n");
  } else if (x == 0) {
    printf("false\n");
  } else {
    printf("invalid bool\n");
  }
}

int read_int() {
  int x = 0;
  scanf("%d", &x);
  return x;
}
