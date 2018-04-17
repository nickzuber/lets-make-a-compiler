#include <inttypes.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>
#include <stdlib.h>

void print_void ();
void print_int (int x);
void print_bool (int x);
void print_vector (int* tag, int* x);
void print_result(int* tag, int x);

// types
int64_t ty_void = 0;
int64_t ty_bool = 1;
int64_t ty_int = 2;
int64_t ty_vector = 3;

static int _DEBUG = 1;

int rootstack_size = 0;
int heap_size = 0;
int64_t* free_ptr = NULL;

int64_t* tospace_begin = NULL;
int64_t* tospace_end = NULL;

int64_t* fromspace_begin = NULL;
int64_t* fromspace_end = NULL;

int64_t* rootstack_ptr = NULL;
int64_t* rootstack_begin = NULL;
int64_t* rootstack_end = NULL;

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

void collect (int* rsp) {
  // ...
}

void print_void () {
  printf("void");
}

void print_int (int x) {
  printf("%d", x);
}

void print_bool (int x) {
  if (x == 1) {
    printf("true");
  } else if (x == 0) {
    printf("false");
  } else {
    printf("BAD BOOL TYPE");
  }
}

void print_vector (int* tag, int* x) {
  for (int i = 0; i < tag[1]; i++) {
    print_result((int*)tag[2+i], x[1+i]);
    if (i + 1 < tag[1]) {
      printf(" ");
    }
  }
  printf("\n");
}

void print_result(int* tag, int x) {
  if (tag[0] == ty_void) { print_void(); }
  else if (tag[0] == ty_int) { print_int(x); }
  else if (tag[0] == ty_bool) { print_bool(x); }
  else if (tag[0] == ty_vector) { print_vector(tag, (int*)x); }
  else { printf("UNKNOWN TAG TYPE %p", tag); }
  printf("\n");
}

int read_int() {
  int x = 0;
  scanf("%d", &x);
  return x;
}
