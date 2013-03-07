BINDIR?=/tmp/

dist/build/sm-libvirt/sm-libvirt:
	obuild configure
	obuild build

install:
	install -m 0755 dist/build/sm-libvirt/sm-libvirt ${BINDIR}

uninstall:
	rm -f ${BINDIR}/sm-libvirt

.PHONY: clean
clean:
	rm -rf dist
