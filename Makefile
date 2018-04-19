OCB_FLAGS = -use-ocamlfind -pkg core,batteries,ounit,ocamlgraph,bheap -tags thread -no-hygiene
OCB =       ocamlbuild $(OCB_FLAGS)

MODULES = src \
					src/transformers \
					utils

INCLUDE_MODULES = $(foreach dir, $(MODULES), -I $(dir))

all: build try
test: build-test run-test

try:
	./main.native

build:
	$(OCB) $(INCLUDE_MODULES) src/main.native

run-test:
	./test_main.native

build-test:
	$(OCB) $(INCLUDE_MODULES) tests/test_main.native

build-with-runtime:
	gcc -c runtime/gc.c -o runtime/gc.o
	gcc runtime/gc.o assembly.s -o program

clean:
	$(OCB) -clean
	rm -f runtime/*.o
	rm -f ./program
	rm -f ./testing_program
	rm -f ./assembly.s

.PHONY: all run build build-test build-with-runtime test try clean
