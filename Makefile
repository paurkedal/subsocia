.PHONY: test all doc install uninstall clean distclean

prefix = $(shell opam config var prefix)

OCAMLBUILD_PLUGINS = -plugin-tag 'package(ocamlbuild-eliom-dev)'
OCAMLBUILD = ocamlbuild -use-ocamlfind $(OCAMLBUILD_PLUGINS)

all:
	ocaml pkg/pkg.ml build

test: all
	ocaml pkg/pkg.ml test

doc:
	topkg doc

clean:
	ocaml pkg/pkg.ml clean

install:
	opam-installer --prefix $(prefix) subsocia.install

uninstall:
	opam-installer --prefix $(prefix) -u subsocia.install

distclean: clean
	rm -f subsocia.install
