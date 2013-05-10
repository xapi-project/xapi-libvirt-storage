BINDIR?=/tmp/

.PHONY: build
build: configure.done
	obuild build

configure.done: sm-libvirt.obuild
	obuild configure
	touch configure.done

install:
	install -m 0755 dist/build/sm-libvirt/sm-libvirt ${BINDIR}

uninstall:
	rm -f ${BINDIR}/sm-libvirt

.PHONY: clean
clean:
	rm -rf dist configure.done
