all: build

NAME=cstruct
J=4

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure

build: setup.data setup.ml
	ocaml setup.ml -build -j $(J)

doc: setup.data setup.ml
	ocaml setup.ml -doc -j $(J)

install: setup.data setup.ml
	ocaml setup.ml -install

test: setup.ml build
	./test.sh

reinstall: setup.ml
	ocamlfind remove $(NAME) || true
	ocaml setup.ml -reinstall

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log
