#!/bin/sh -e
# Script that invokes ocamlbuild commands for various targets
# environment variables:
### OCAMLBUILD overrides ocamlbuild binary
### OCAMLFIND overrides ocamlfind
### OCAMLBUILD_FLAGS defaults to verbose
### NJOBS decides the parallelisation level
### NO_NATIVE forces disabling native builds
### NO_NATDYNLINK forces disabling natdynlink

njobs=${NJOBS:-2}

cmd=$1
shift

# source the package variables
if [ -e "_vars" ]; then
  . ./_vars
fi

OCAMLBUILD=${OCAMLBUILD:-`which ocamlbuild`}
OCAMLFIND=${OCAMLFIND:-`which ocamlfind`}
OCAMLBUILD_FLAGS="-classic-display -j ${njobs}"

# create entries in the _config/ directory 
configure() {
  rm -rf _config && mkdir -p _config
  ${OCAMLFIND} query -r -i-format ${DEPS} > _config/flags.ocaml
  ${OCAMLFIND} query -r -a-format -predicates native ${DEPS} > _config/archives.native
  ${OCAMLFIND} query -r -a-format -predicates byte ${DEPS} > _config/archives.byte
  ${OCAMLFIND} query -r -predicates syntax,preprocessor -format '-I %d %A' ${DEPS} ${SYNTAX_DEPS} > _config/flags.camlp4
  ${OCAMLFIND} query -r -predicates byte -format '-I %d %A' str >> _config/flags.camlp4
  ${OCAMLFIND} query -r -predicates syntax,preprocessor -format '-I %d' camlp4.quotations.o camlp4.lib camlp4.extend > _config/syntax.build
  ${OCAMLFIND} query -r -predicates syntax,preprocessor -format '-I %d' camlp4.quotations.r camlp4.lib camlp4.extend ${SYNTAX_DEPS} > _config/syntax.build.r
 
  echo ${NAME} > _config/name
  echo ${DEPS} > _config/deps
  echo ${SYNTAX} > _config/syntax
  echo ${RUNTIME} > _config/clibs
  echo ${LIB} > _config/lib
  echo ${CFLAGS} > _config/cflags
  echo ${EXTRA} > _config/extra

  ocamlopt -v >/dev/null 2>&1 && touch _config/flag.opt
  if [ ! -z "${NO_NATIVE}" ]; then rm -f _config/flag.opt; fi
  # TODO: how to detect natdynlink? test for dynlink.cmxa?
  # touch _config/flag.natdynlink
  if [ ! -z "${NO_NATDYNLINK}" ]; then rm -f _config/flag.natdynlik; fi
}

# invoke native code and byte code compiler targets
compile() {
  ${OCAMLBUILD} ${OCAMLBUILD_FLAGS} ${NAME}.all
}

# generate META file and invoke ${OCAMLFIND} installation
install()  {
  sed -e "s/@VERSION@/${VERSION}/g" < META.in > _config/META
  ${OCAMLFIND} remove ${NAME} || true
  t=`sed -e 's,^,_build/,g' < _build/${NAME}.all`
  if [ ! -z "${DESTDIR}" ]; then
    OCAMLFIND_FLAGS="${OCAMLFIND_FLAGS} -destdir ${DESTDIR}"
  fi
  ${OCAMLFIND} install ${OCAMLFIND_FLAGS} ${NAME} _config/META ${t}
}

# tests also include the built syntax extensions (if any)
run_tests() {
  for test in ${TESTS}; do
    t="lib_test/$test.byte"
    [ -e _config/flag.opt ] && t="${t} lib_test/$test.native"
    ${OCAMLBUILD} ${OCAMLBUILD_FLAGS} ${t}
  done
}

clean() {
  ${OCAMLBUILD} -clean
  rm -rf _config
}

case "$cmd" in
conf*) configure ;;
compile|build) compile ;;
install) install ;;
clean) clean ;;
doc) doc ;;
test) run_tests ;;
*) echo unknown command: $cmd; exit 1 ;;
esac
