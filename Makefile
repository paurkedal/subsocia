.PHONY: test all doc install uninstall clean distclean

prefix = $(shell opam config var prefix)

OCAMLBUILD_PLUGINS = -plugin-tag 'package(ocamlbuild-eliom-dev)'
OCAMLBUILD = OCAMLPATH=. ocamlbuild -use-ocamlfind $(OCAMLBUILD_PLUGINS)

all:
	ocaml pkg/pkg.ml build

test:
	$(OCAMLBUILD) tests/testsuite.native
	_build/tests/testsuite.native

doc:
	OCAMLPATH=. topkg doc

clean:
	$(OCAMLBUILD) -clean

install:
	opam-installer --prefix $(prefix) subsocia.install

uninstall:
	opam-installer --prefix $(prefix) -u subsocia.install

distclean: clean
	rm -f subsocia.install
