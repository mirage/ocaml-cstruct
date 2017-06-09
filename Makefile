.PHONY: all clean

all:
	jbuilder build

clean:
	rm -rf _build *.install

test:
	jbuilder runtest

install:
	jbuilder install

js-install:
	# whatever

js-uninstall:
	# whatever

REPO=../../mirage/opam-repository
PACKAGES=$(REPO)/packages
# until we have https://github.com/ocaml/opam-publish/issues/38
pkg-%:
	topkg opam pkg -n $*
	mkdir -p $(PACKAGES)/$*
	cp -r _build/$*.* $(PACKAGES)/$*/
	cd $(PACKAGES) && git add $*

PKGS=$(basename $(wildcard *.opam))
opam-pkg:
	$(MAKE) $(PKGS:%=pkg-%)

