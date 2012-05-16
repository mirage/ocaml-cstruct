.PHONY: all clean distclean setup build doc install test-build test 
all: build

NAME=cstruct

export OCAMLRUNPARAM=b

clean: setup.data
	./setup.bin -clean

distclean: setup.data
	./setup.bin -distclean
	$(RM) setup.bin

setup: setup.data

setup.bin: setup.ml
	ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	./setup.bin -configure

setup.ml: _oasis
	oasis setup

build: setup.data $(wildcard lib/*.ml)
	./setup.bin -build

doc: setup.data setup.bin
	./setup.bin -doc

# must have cstruct installed and accessible to ocamlfind first *)
test: 
	cd lib_test && \
	  ocamlbuild -clean && \
	  ocamlbuild -classic-display -use-ocamlfind pcap.native && \
	  ocamlbuild -classic-display -use-ocamlfind enum.native && \
	  ocamlbuild -classic-display -use-ocamlfind basic.native && \
	  ./_build/enum.native && ./_build/basic.native && ./_build/pcap.native

install: setup.data $(NAME).a
	ocamlfind remove $(NAME)
	./setup.bin -install

$(NAME).a: build

reinstall: setup.bin
	ocamlfind remove $(NAME) || true
	./setup.bin -reinstall
