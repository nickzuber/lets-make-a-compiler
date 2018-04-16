#include <inttypes.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>
#include <stdlib.h>

static int _DEBUG = 1;

int rootstack_size = 0;
int heap_size = 0;
int64_t* free_ptr = NULL;

static int64_t* tospace_begin = NULL;
static int64_t* tospace_end = NULL;

static int64_t* fromspace_begin = NULL;
static int64_t* fromspace_end = NULL;

int64_t* rootstack_ptr = NULL;
static int64_t* rootstack_begin = NULL;
static int64_t* rootstack_end = NULL;

static void initialize_space (const char* label, int64_t** begin, int64_t** end, int size) {
  *begin = malloc(size);
  if (!*begin) {
    fprintf(stderr, "Unable to initialize space for %s\n", label);
    exit(1);
  }
  *(uint8_t**)end = *(uint8_t**)begin + size;
  if (_DEBUG) {
    fprintf(stderr, "%s = [%p, %p] (%d)", label, begin, end, size);
  }
  memset(*begin, 0, size);
}

void initialize (int rs, int hs) {
  rootstack_size = rs;
  heap_size = hs;
}

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
