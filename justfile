build: 
	dune build --root="src/ocaml/" --profile=dev --debug-backtraces --instrument-with ocamlearlybird

test: 
	dune runtest --root="src/ocaml/" --profile=dev --debug-backtraces
run:
	dune exec deliberation_model --root="src/ocaml/" --instrument-with ocamlearlybird


sync:
	uv sync

clean: 
	dune clean --root="src/ocaml/"

install:
	opam install ./src/ocaml --deps-only

vis: 
	uv run ./src/python/visualize.py

debug: 
	ocamldebug src/ocaml/_build/default/bin/main.bc
