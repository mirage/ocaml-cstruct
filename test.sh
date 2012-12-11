#!/bin/sh -e

function test {
echo $1
mkdir -p _build/lib_test
cp lib_test/$1.ml _build/lib_test/$1.ml
camlp4orf -printer o _build/syntax/cstruct-syntax.cma lib_test/$1.ml > _build/lib_test/$1.gen.ml
camlp4orf -printer o _build/syntax/cstruct-syntax.cma lib_test/$1.mli > _build/lib_test/$1.gen.mli
ocamlc -pp 'camlp4orf -printer o _build/syntax/cstruct-syntax.cma' -I _build/lib -i lib_test/$1.ml > _build/lib_test/$1.inferred.mli
cp _build/lib_test/$1.inferred.mli _build/lib_test/$1.mli
rm -f _build/lib_test/$1.cmi
cd _build/lib_test
ocamlc -pp 'camlp4orf ../syntax/cstruct-syntax.cma' -I ../lib -c $1.mli
ocamlc -pp 'camlp4orf ../syntax/cstruct-syntax.cma' -I ../lib -c $1.ml
ocamlc -custom -I ../lib unix.cma bigarray.cma cstruct.cma $1.cmo -o $1.byte
./$1.byte
cp $1.gen.mli $1.mli
ocamlc -pp 'camlp4orf ../syntax/cstruct-syntax.cma' -I ../lib -c $1.mli
ocamlc -pp 'camlp4orf ../syntax/cstruct-syntax.cma' -I ../lib -c $1.ml
ocamlc -custom -I ../lib unix.cma bigarray.cma cstruct.cma $1.cmo -o $1.byte
./$1.byte
cd ../..
}

test basic
test enum
cp lib_test/http.cap _build/lib_test/http.cap
test pcap
