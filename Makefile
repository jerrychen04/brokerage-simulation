.PHONY: test

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec ui/main.exe

view_stock:
	OCAMLRUNPARAM=b dune exec operate/main.exe

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

zip:
	rm -f brokerage.zip
	zip -r brokerage.zip .