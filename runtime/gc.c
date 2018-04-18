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
void print_value(int* tag, int x);
void print_result(int* tag, int x);

// types
int ty_void = 0;
int ty_bool = 1;
int ty_int = 2;
int ty_vector = 3;

static int _DEBUG = 1;

int rootstack_size = 0;
int heap_size = 0;
int* free_ptr = NULL;

int* queue_head = NULL;
int* queue_tail = NULL;

int* tospace_begin = NULL;
int* tospace_end = NULL;

int* fromspace_begin = NULL;
int* fromspace_end = NULL;

int* rootstack_ptr = NULL;
int* rootstack_begin = NULL;
int* rootstack_end = NULL;

static void initialize_space (const char* label, int** begin, int** end, int size) {
  *begin = malloc(size);
  if (!*begin) {
    fprintf(stderr, "Unable to initialize space for %s\n", label);
    exit(1);
  }
  *(uint8_t**)end = *(uint8_t**)begin + size;
  if (_DEBUG) {
    fprintf(stderr, "\x1b[90m[DEBUG]\n%s = [%p, %p] (%d)\x1b[0m\n\n", label, begin, end, size);
  }
  memset(*begin, 0, size);
}

void initialize (int rs, int hs) {
  rootstack_size = rs;
  heap_size = hs;

  initialize_space("fromspace", &fromspace_begin, &fromspace_end, heap_size);
  initialize_space("tospace", &tospace_begin, &tospace_end, heap_size);
  initialize_space("rootstack", &rootstack_begin, &rootstack_end, rootstack_size);

  free_ptr = fromspace_begin;
  rootstack_ptr = rootstack_begin;

  return;
}

void collect (int* updated_rootstack_ptr) {
  rootstack_ptr = updated_rootstack_ptr;
  queue_head = tospace_begin;
  queue_tail = tospace_begin;

  if (_DEBUG) {
    fprintf(stderr, "\x1b[90m[DEBUG]\ncollect was called");
  }
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
    print_value((int*)tag[2+i], x[1+i]);
    if (i + 1 < tag[1]) {
      printf(" ");
    }
  }
  printf("\n");
}

void print_value(int* tag, int x) {
  if (tag[0] == ty_void) { print_void(); }
  else if (tag[0] == ty_int) { print_int(x); }
  else if (tag[0] == ty_bool) { print_bool(x); }
  else if (tag[0] == ty_vector) { print_vector(tag, (int*)x); }
  else { printf("UNKNOWN TAG TYPE %p", tag); }
}

void print_result(int* tag, int x) {
  print_value(tag, x);
  if (tag[0] == ty_void) { printf("\x1b[90m : void\x1b[0m"); }
  else if (tag[0] == ty_int) { printf("\x1b[90m : int\x1b[0m"); }
  else if (tag[0] == ty_bool) { printf("\x1b[90mbool: \x1b[0m"); }
  else if (tag[0] == ty_vector) { printf("\x1b[90m : vector\x1b[0m"); }
  else { printf("UNKNOWN TAG TYPE %p", tag); }
  printf("\n");
}

int read_int() {
  int x = 0;
  scanf("%d", &x);
  return x;
}
