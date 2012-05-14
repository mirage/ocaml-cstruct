.PHONY: all clean install build
all: build

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

# must have cstruct installed and accessible to ocamlfind first *)
test: 
	cd lib_test && \
	  ocamlbuild -clean && \
	  ocamlbuild -classic-display -use-ocamlfind pcap.native && \
	  ocamlbuild -classic-display -use-ocamlfind enum.native && \
	  ocamlbuild -classic-display -use-ocamlfind basic.native && \
	  ./_build/enum.native && ./_build/basic.native && ./_build/pcap.native

reinstall: setup.bin
	ocamlfind remove $(NAME) || true
	./setup.bin -reinstall

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log setup.bin
