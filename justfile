build: 
	dune build --root="src/bin/" --profile=dev --debug-backtraces --instrument-with ocamlearlybird

test: 
	dune runtest --root="src/bin/" --profile=dev --debug-backtraces

run type file:
	dune exec deliberation_model {{type}} {{file}} --root="src/bin/" --instrument-with ocamlearlybird

run_all pattern type:
	find {{pattern}} -maxdepth 1 -type f | xargs -n 1 just run {{type}} 

sync:
	uv sync

clean: 
	dune clean --root="src/bin/"

install:
	opam install ./src/bin --deps-only

vis: 
	uv run ./src/scripts/visualize.py

debug type: 
	ocamldebug src/bin/_build/default/bin/main.bc {{type}}
