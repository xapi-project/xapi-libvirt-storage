BINDIR?=/tmp/

.PHONY: build install uninstall clean

build: dist/setup version.ml
	obuild build

version.ml: VERSION
	echo "let version = \"$(shell cat VERSION)\"" > version.ml

dist/setup: sm-libvirt.obuild
	obuild configure

install:
	install -m 0755 dist/build/sm-libvirt/sm-libvirt ${BINDIR}

uninstall:
	rm -f ${BINDIR}/sm-libvirt

clean:
	rm -rf dist
