BINDIR?=/tmp/

.PHONY: build install uninstall clean

build: setup.data version.ml
	ocaml setup.ml -build

setup.data: _oasis
	ocaml setup.ml -configure

version.ml: VERSION
	echo "let version = \"$(shell cat VERSION)\"" > version.ml

install:
	install -m 0755 main.native ${BINDIR}/sm-libvirt

uninstall:
	rm -f ${BINDIR}/sm-libvirt

clean:
	ocaml setup.ml -clean
