OCB_FLAGS = -use-ocamlfind -pkg core,batteries -tags thread
OCB =       ocamlbuild $(OCB_FLAGS)

MODULES = src

INCLUDE_MODULES = $(foreach dir, $(MODULES), -I $(dir))

all: build try

try:
	./compiler.native

build:
	$(OCB) $(INCLUDE_MODULES) src/compiler.native

clean:
	$(OCB) -clean

.PHONY: all run build version test generate-tests try try-save try-pretty view clean
