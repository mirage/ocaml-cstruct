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
opam init
opam pin add cstruct . -n
opam install async lwt
opam install --deps-only cstruct

eval `opam config env`

./configure --enable-lwt --enable-asynx
make
./test.sh

make clean
./configure --enable-lwt --enable-async --enable-tests
make test

unset OPAMVERBOSE
if [ "$OCAML_VERSION" = "4.01" ]; then
  opam remove async
fi

opam install mirage-www
