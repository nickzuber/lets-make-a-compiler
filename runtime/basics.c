#include <inttypes.h>
#include <stdio.h>
#include <stdint.h>

void print_int (int64_t x) {
  printf("%" PRId64 "\n", x);
}

int64_t read_int () {
  int64_t x = 0;
  scanf("%" SCNd64, &x);
  return x;
}

extern int64_t asm_main ();

int main () {
  int64_t ans = asm_main();
  print_int(ans);
  return 0;
}
