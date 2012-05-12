all:
	oasis setup
	-rm setup.data
	ocaml setup.ml -configure
	ocaml setup.ml -build
clean:
	ocaml setup.ml -clean
