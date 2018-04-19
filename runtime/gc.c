#include <inttypes.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>
#include <stdlib.h>

void print_void ();
void print_int (int64_t x);
void print_bool (int64_t x);
void print_vector (int64_t* x);
void print_value(int64_t* tag, int64_t x);
void print_result(int64_t* tag, int64_t x);

// types
int64_t ty_void = 0;
int64_t ty_bool = 1;
int64_t ty_int = 2;
int64_t ty_vector = 3;

static int64_t _DEBUG = 1;

int64_t rootstack_size = 0;
int64_t heap_size = 0;
int64_t* free_ptr = NULL;

int64_t* queue_head = NULL;
int64_t* queue_tail = NULL;

int64_t* tospace_begin = NULL;
int64_t* tospace_end = NULL;

int64_t* fromspace_begin = NULL;
int64_t* fromspace_end = NULL;

int64_t* rootstack_ptr = NULL;
int64_t* rootstack_begin = NULL;
int64_t* rootstack_end = NULL;

void show_freeptr () {
  fprintf(stderr, "\x1b[90m[DEBUG]\nfree_ptr = %p\x1b[0m\n", free_ptr);
}

static void initialize_space (const char* label, int64_t** begin, int64_t** end, int64_t size) {
  *begin = malloc(size);
  if (!*begin) {
    fprintf(stderr, "Unable to initialize space for %s\n", label);
    exit(1);
  }
  *(uint8_t**)end = *(uint8_t**)begin + size;
  if (_DEBUG) {
    fprintf(stderr, "\x1b[90m[DEBUG]\n%s = [%p, %p] (%lld)\x1b[0m\n\n", label, begin, end, size);
  }
  memset(*begin, 0, size);
}

void initialize (int64_t rs, int64_t hs) {
  rootstack_size = rs;
  heap_size = hs;

  initialize_space("fromspace", &fromspace_begin, &fromspace_end, heap_size);
  initialize_space("tospace", &tospace_begin, &tospace_end, heap_size);
  initialize_space("rootstack", &rootstack_begin, &rootstack_end, rootstack_size);

  free_ptr = fromspace_begin;
  rootstack_ptr = rootstack_begin;

  return;
}

void collect (int64_t* updated_rootstack_ptr) {
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

void print_int (int64_t x) {
  printf("%lld", x);
}

void print_bool (int64_t x) {
  if (x == 1) {
    printf("true");
  } else if (x == 0) {
    printf("false");
  } else {
    printf("BAD BOOL TYPE");
  }
}

void print_vector (int64_t* x) {
  int64_t* tag;
  for (int64_t i = 0; i < x[1]; i++) {
    tag = x[i + 2];
    if (tag[0] == ty_int) { print_int(tag[1]); }
    else if (tag[0] == ty_bool) { print_bool(tag[1]); }
    else { print_vector(tag); }
    if (i + 1 < x[1]) {
      printf(", ");
    }
  }
}

void print_value(int64_t* tag, int64_t x) {
  if (tag[0] == ty_void) { print_void(); }
  else if (tag[0] == ty_int) { print_int(x); }
  else if (tag[0] == ty_bool) { print_bool(x); }
  else if (tag[0] == ty_vector) { printf("("); print_vector(x); }
  else { printf("\x1b[31mUnknown Tag Type\n\x1b[0m%lld <%p>", tag[0], tag); }
}

void print_result(int64_t* tag, int64_t x) {
  print_value(tag, x);
  if (tag[0] == ty_void) { printf("\x1b[90m : void\x1b[0m"); }
  else if (tag[0] == ty_int) { printf("\x1b[90m : int\x1b[0m"); }
  else if (tag[0] == ty_bool) { printf("\x1b[90m : bool \x1b[0m"); }
  else if (tag[0] == ty_vector) { printf(")\x1b[90m : vector\x1b[0m"); }
  else { printf("\x1b[90m : undefined\x1b[0m"); }
  printf("\n");
}

int64_t read_int() {
  int64_t x = 0;
  scanf("%lld", &x);
  return x;
}
