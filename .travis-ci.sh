OPAM_DEPENDS="ocplib-endian lwt async sexplib"

case "$OCAML_VERSION" in
3.12) ppa=avsm/ocaml312+opam12 ;;
4.00) ppa=avsm/ocaml40+opam12  ;;
4.01) ppa=avsm/ocaml41+opam12  ;;
4.02) ppa=avsm/ocaml42+opam12  ;;
*) echo Unknown $OCAML_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time

export OPAMYES=1
export OPAMVERBOSE=1
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

opam init
opam install ${OPAM_DEPENDS}

eval `opam config env`
make
./test.sh

make clean
opam pin cstruct .
unset OPAMVERBOSE
if [ "$OCAML_VERSION" = "4.00" ]; then
  opam remove async
fi
opam install mirage-www
