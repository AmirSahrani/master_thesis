from subprocess import run

import numpy as np
from SALib.sample import saltelli
from SALib import ProblemSpec
from SALib.plotting.bar import plot as barplot
from SALib.analyze import sobol
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib as mpl
from typing import OrderedDict, Tuple, List

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
    ("Ego", [0, 1]),
    ("Self Knowledge", [0, 1]),
    ("Similarity", [0, 1]),
    ("Number of Voters", [3, 31]),
    ("Timesteps", [0, 20]),
    ("Bias Factor", [0.01, 10]),
]
output_vars = [
    "PBS_simulated",
]


def map_get_fst(lst: [Tuple]):
    return list(map(lambda x: x[0], lst))


def map_get_scd(lst: [Tuple]):
    return list(map(lambda x: x[1], lst))


def get_problem():
    return ProblemSpec(
        {
            "num_vars": len(sensitivity_vars),
            "names": map_get_fst(sensitivity_vars),
            "bounds": list(map_get_scd(sensitivity_vars)),
        }
    )


def get_analysis_inputs(n_samples):
    # Generate samples and run a dummy evaluation
    problem = get_problem()
    param_values = saltelli.sample(problem, n_samples)
    effective_n = param_values.shape[0]

    all_vars = {
        "Knowledge": [],
        "Credibility": [0]*effective_n,
        "Ego": [],
        "Self Knowledge": [],
        "Similarity": [],
        "Meta": [0]*effective_n,
        "Substantive": [1]*effective_n,
        "Number of Voters": [],
        "Number of Candidates": [1] * effective_n,
        "Timesteps": [],
        "Bias Factor": [],
        "Candidate Generator": [1] * effective_n,
    }
    for i, (var, _) in enumerate(sensitivity_vars):
        all_vars[var] = param_values.T[i]

    param_values = pd.DataFrame.from_dict(all_vars)
    print(param_values.head())

    return list(map(tuple, param_values.to_numpy()))


def run_analysis(outputs, problem):
    # Perform Sobol sensitivity analysis
    sobol_indices = sobol.analyze(problem, outputs)
    return sobol_indices


if __name__ == "__main__":
    data = pd.read_csv("results/sensivity.csv")
    problem = get_problem()

    for var in output_vars:
        data_out = data[var].to_numpy().squeeze()
        st, s1, s2 = run_analysis(data_out, problem).to_df()
        fig, ax = plt.subplots(1, 3, figsize=(21, 7))
        # Suppose these are your variables
        variables = list(st.index)
        n = len(variables)

        # Initialize matrices
        s2_data = np.full((n, n), np.nan)
        s2_sign = np.zeros((n, n), dtype=int)

        # Create a mapping from variable name to index for lookup
        var_idx = {var: i for i, var in enumerate(variables)}

        # Loop through all entries in the s2 DataFrame
        for (var1, var2), row in s2.iterrows():
            if var1 in var_idx and var2 in var_idx:
                i = var_idx[var1]
                j = var_idx[var2]
                # s2_data[i, j] = row["S2"]
                s2_data[j, i] = row["S2"]

                # Significance: here, checking if CI excludes 0
                low = row["S2"] - row["S2_conf"]
                high = row["S2"] + row["S2_conf"]
                if low > 0 or high < 0 and row["S2"] > 0:
                    print(var1, var2)
                    s2_sign[j, i] = 1

        ax[0].bar(st.index, st["ST"], 0.5, yerr=st["ST_conf"], color="#008B72")
        ax[0].tick_params(axis="x", rotation=90)
        ax[0].set_title("Total Order")

        ax[1].bar(st.index, s1["S1"], 0.5, yerr=s1["S1_conf"], color="#008B72")
        ax[1].tick_params(axis="x", rotation=90)
        ax[1].set_title("First Order")
        im = ax[2].imshow(s2_data, cmap="viridis")
        ax[2].set_xticks(range(0, len(st.index)), st.index, rotation=90)
        ax[2].set_title("Second Order")
        fig.colorbar(
            im,
            ax=ax[2],
            orientation="vertical",
            location="right",
            fraction=0.046,  # size of the colorbar relative to the Axes
            pad=0.05,
        )
        for i in range(len(st.index)):
            for j in range(len(st.index)):
                if s2_sign[i, j] == 1:
                    ax[2].text(
                        j,
                        i - 0.1,
                        "*",
                        ha="center",
                        va="center",
                        color="black",
                        fontsize=28,
                        fontweight="bold",
                    )
                    ax[2].set_yticks(range(0, len(st.index)), st.index)

        ax[2].grid(False)
        plt.tight_layout()
        plt.savefig("figures/senstivity_analysis.png")
        plt.show()
