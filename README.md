# Deliberation and Meta Agreement

Deliberation and Meta-Agreement is a simulation-based study of how structured
deliberation can lead to meta-agreement and single-peaked preferences, based on
adaptations of the DeGroot model and replication of Rad and Roy’s work.

## Installation

This project uses [opam](https://opam.ocaml.org/) (v2.3.0) for all Ocaml's dependencies
and [dune](https://dune.build/) as its build system.
[Uv](https://docs.astral.sh/uv/) is used for the python dependencies.
Finally, [just](https://github.com/casey/just) is used to streamline running
building, and installing all software through one interface.
The following recipes are provided:

To set up your environment run

```bash
just sync    # runs uv sync
just install # Installs all ocaml dependencies
```

Once your environment is properly set up, you should be able to run the
following to build the Ocaml executable

```bash
just build

```
## Structure
The repository has the following structure:

```
.
├── configs/
├── data/
├── figures/
├── graphs/
├── results/
├── src/
├── justfile
└── uv.lock

```

* `configs/` – Configuration files (YAML) used for DeGroot model experiments
* `data/` – The *America in One Room* dataset
* `figures/` – All figures included in the accompanying thesis
* `graphs/` – Graphs from the [Network Repository](https://networkrepository.com/), used to model trust matrices for the control group
* `results/` – Output data from each simulation
* `src/` – Contains the core OCaml and Python code for simulations and analysis
* `justfile` – Recipe file for managing tasks using `just`
* `uv.lock` – Lockfile for Python dependencies managed by `uv`



## Rad and Roy experiments

In order to generate the data for the Rad and Roy experiments you run the following command:

```{bash}
just run rad
```

This will generate all the data to generate the figures. This data is already
contained within the repository. The figures can be generated using:

```{bash}
just vis
```

## Adapted DeGroot model

All DeGroot based experiments require a configuration file in the YAML format.
Those used for the thesis can be found in `configs/`. In order to run a single configuration file, run 

```{bash}
just run degroot {PATH/TO/FILE}
```

If multiple file need to be ran, this can be done using a pattern in the following command

```{bash}
just run_all "configs/*convergence*"
```

This runs all configuration files matching the given glob pattern. For example,
use configs/*convergence* to run all convergence experiments.

All statistics and data cleaning/analysis are done in `marimo` notebooks, which can be opened by running

```{bash}
uv run marimo edit
```
