.PHONY: all clean install build
all: build test doc

NAME=cstruct

export OCAMLRUNPARAM=b

setup.bin: setup.ml
	ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	./setup.bin -configure

build: setup.data setup.bin
	./setup.bin -build

doc: setup.data setup.bin
	./setup.bin -doc

install: setup.bin
	./setup.bin -install

test: 
	camlp4o -printer o ./_build/syntax/cstruct-syntax.cma ./lib_test/ipv4.ml

reinstall: setup.bin
	ocamlfind remove $(NAME) || true
	./setup.bin -reinstall

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log setup.bin
