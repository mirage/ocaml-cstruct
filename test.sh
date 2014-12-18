#!/bin/sh -ex

bytes=`ocamlfind query bytes -format "-I %d %a" -predicates native,archive`
endian=`ocamlfind query ocplib-endian.bigstring -format "-I %d %a" -predicates native,archive`
sexplib=`ocamlfind query -r sexplib -format "-I %d %a" -predicates native,archive`
sexplibi=`ocamlfind query -r sexplib -format "-I %d" -predicates native`

test() {
echo $1
mkdir -p _build/lib_test
cp lib_test/$1.ml _build/lib_test/$1.ml
camlp4orf -printer o _build/syntax/cstruct-syntax.cma lib_test/$1.ml > _build/lib_test/$1.gen.ml
camlp4orf -printer o _build/syntax/cstruct-syntax.cma lib_test/$1.mli > _build/lib_test/$1.gen.mli
ocamlopt -pp 'camlp4orf _build/syntax/cstruct-syntax.cma' -I _build/lib -I _build/unix $sexplibi -i lib_test/$1.ml > _build/lib_test/$1.inferred.mli
cp _build/lib_test/$1.inferred.mli _build/lib_test/$1.mli
rm -f _build/lib_test/$1.cmi
cd _build/lib_test
ocamlopt -pp 'camlp4orf ../syntax/cstruct-syntax.cma' -I ../lib -I ../unix $sexplibi -c $1.mli
ocamlopt -pp 'camlp4orf ../syntax/cstruct-syntax.cma' -I ../lib -I ../unix $sexplibi -c $1.ml
ocamlopt -I ../lib -I ../unix $bytes $endian $sexplib cstruct.cmxa unix_cstruct.cmxa $1.cmx -o $1.opt
time ./$1.opt
#cp $1.gen.mli $1.mli
#ocamlopt -pp 'camlp4orf ../syntax/cstruct-syntax.cma' -I ../lib -c $1.mli
#ocamlopt -pp 'camlp4orf ../syntax/cstruct-syntax.cma' -I ../lib -c $1.ml
#ocamlopt -I ../lib unix.cmxa bigarray.cmxa $endian cstruct.cmxa $1.cmx -o $1.opt
#time ./$1.opt
cd ../..
}

test basic
test enum
mkdir -p _build/lib_test
ln -nsf ../../lib_test/http.cap _build/lib_test/http.cap
test pcap
