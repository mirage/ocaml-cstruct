# OPAM packages needed to build tests
OPAM_PACKAGES="ocplib-endian"

export OPAM_PACKAGES
sudo apt-get install python-software-properties
echo "yes" | sudo add-apt-repository ppa:avsm/ppa-testing
sudo apt-get update -qq
sudo apt-get install -qq ocaml opam
export OPAMYES=1
opam init 
opam install ${OPAM_PACKAGES}
eval `opam config -env`
make 

