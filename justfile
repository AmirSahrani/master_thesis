build: 
	dune build --root="src/ocaml/"

run:
	dune exec deliberation_model --root="src/ocaml/"

sync:
	uv sync

clean: 
	dune clean --root="src/ocaml/"

install:
	opam install ./src/ocaml --deps-only
