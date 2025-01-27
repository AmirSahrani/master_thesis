# Deliberation and Meta Agreement

## Installation

This project uses [opam](https://opam.ocaml.org/) for all Ocaml's dependencies
and [dune](https://dune.build/) as its build system.
[uv](https://docs.astral.sh/uv/) is used for the python dependencies.
Finally, [just](https://github.com/casey/just) is used to deal running
automating building running, and installing all software.
The following recipes are provided:

To set up your environment run

```bash
just sync    # runs uv sync
just install # Installs all ocaml dependencies
```

Once your environment is properly setup, you should be able to run the
following to build the Ocaml executable

```bash
just build

```

Instead of `just build`, you can also directly run the simulation with the following

```bash
just run
