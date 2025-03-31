build: 
	dune build --root="src/bin/" --profile=dev --debug-backtraces --instrument-with ocamlearlybird

test: 
	dune runtest --root="src/bin/" --profile=dev --debug-backtraces

run type:
	dune exec deliberation_model {{type}} --root="src/bin/" --instrument-with ocamlearlybird


sync:
	uv sync

clean: 
	dune clean --root="src/bin/"

install:
	opam install ./src/ocaml --deps-only

vis: 
	uv run ./src/python/visualize.py

debug type: 
	ocamldebug src/bin/_build/default/bin/main.bc {{type}}
