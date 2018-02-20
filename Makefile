OCB_FLAGS = -use-ocamlfind -pkg core,batteries,ounit,ocamlgraph -tags thread -no-hygiene
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
	cc -c runtime/basics.c -o runtime/basics.o
	cc runtime/basics.o assembly.s -o program

clean:
	$(OCB) -clean
	rm runtime/*.o
	rm ./program

.PHONY: all run build build-test test try clean
