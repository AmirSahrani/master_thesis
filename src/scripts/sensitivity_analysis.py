from subprocess import run

from numpy import indices
from SALib.sample import saltelli
from SALib import ProblemSpec
from SALib.plotting.bar import plot as barplot
from SALib.analyze import sobol
import pandas as pd
import matplotlib.pyplot as plt
from typing import Tuple, List

plt.rcParams.update(
    {
        "font.size": 20,
        "figure.figsize": [10, 8],
        "axes.linewidth": 1,
        "axes.grid": True,
        "grid.linewidth": 1,
        "lines.color": "#008B72",
        "grid.alpha": 0.3,
        "image.cmap": "cividis",
        "text.usetex": True,
        "font.family": "Charter",
    }
)
# seed = None;
# pre_data;
# post_data;
# knowledge_data;
# knowledge_bool;
# credibility_bool;
# graph = out_graph;
# n_voters = num_voters;
# n_candidates = num_candidates;
# timesteps;
# cand_method = methd_alt;
# bias_method = (methd_bias * factor) List;

global sensitivity_vars
global output_vars
n_methods = 2
sensitivity_vars = [
    ("Knowledge", [0, 1]),
    ("Credibility", [0, 1]),
    ("Meta", [0.9, 1]),
    ("Substantive", [0.9, 1]),
    ("Self Knowledge", [0, 1]),
    ("Self Ego", [0, 1]),
    ("Similarity", [0, 1]),
    ("Number of Voters", [3, 31]),
    ("Number of Candidates", [3, 7]),
    ("Timesteps", [0, 20]),
    ("Bias Factor", [0.01, 10]),
    ("Candidate Generator", [0, 1]),
]
output_vars = [
    "PB_score",
]


def map_get_fst(lst: [Tuple]):
    return list(map(lambda x: x[0], lst))


def map_get_scd(lst: [Tuple]):
    return list(map(lambda x: x[1], lst))


def get_problem():

    return ProblemSpec({
        "num_vars": len(sensitivity_vars),
        "names": map_get_fst(sensitivity_vars),
        "bounds": list(map_get_scd(sensitivity_vars)),
    })


def get_analysis_inputs(n_samples):
    # Define the model inputs

    # Generate samples and run a dummy evaluation
    problem = get_problem()
    param_values = saltelli.sample(problem, n_samples)
    return list(map(tuple, param_values))


def run_analysis(outputs, problem):
    # Perform Sobol sensitivity analysis
    sobol_indices = sobol.analyze(problem, outputs)
    return sobol_indices


if __name__ == "__main__":
    data = pd.read_csv("results/sensivity.csv")
    problem = get_problem()

    for var in output_vars:
        data_out = data[var].to_numpy().squeeze()
        sobol_result = run_analysis(data_out, problem)
        axes = sobol_result.plot()

        plt.tight_layout()
        plt.show()
