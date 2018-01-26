OCB_FLAGS = -use-ocamlfind -pkg core,batteries -tags thread
OCB =       ocamlbuild $(OCB_FLAGS)

MODULES = src \
					src/transformers \
					src/utils

INCLUDE_MODULES = $(foreach dir, $(MODULES), -I $(dir))

all: build try

try:
	./main.native

build:
	$(OCB) $(INCLUDE_MODULES) src/main.native

clean:
	$(OCB) -clean

.PHONY: all run build version test generate-tests try try-save try-pretty view clean
