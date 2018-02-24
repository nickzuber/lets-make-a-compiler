#include <inttypes.h>
#include <stdio.h>
#include <stdint.h>

void print_int(int x)
{
  printf("%d\n", x);
}

int read_int()
{
  int x = 0;
  scanf("%d", &x);
  return x;
}

extern int asm_main();

int main()
{
  int ans = asm_main();
  print_int(ans);
  return 0;
}
