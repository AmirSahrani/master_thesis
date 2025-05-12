from SALib.sample import saltelli
from SALib.analyze import sobol

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
    ("knowledge_bool", [0, 1]),
    ("credibility_bool", [0, 1]),
    ("grouped_bool", [0, 1]),
    ("n_voters", [9, 101]),
    ("n_candidates", [3, 7]),
    ("timesteps", [1, 151]),
    ("cand_mathod", [0, 1]),
    # ("bias_methods", [0, n_methods - 1]),
    ("bias_factors", [0, 2]),
]
output_vars = [
    "is_cyclic",
    "ks_distance",
]


def map_get_fst(lst: [tuple]):
    return map(lambda x: x[0], lst)


def map_get_scd(lst: [tuple]):
    return map(lambda x: x[1], lst)


def get_analysis_inputs():
    # Define the model inputs

    problem = {
        "num_vars": len(sensitivity_vars),
        "names": map_get_fst(sensitivity_vars),
        "bounds": list(map_get_scd(sensitivity_vars)),
        "outputs": output_vars,
    }

    # Generate samples and run a dummy evaluation
    param_values = saltelli.sample(problem, 4)
    return list(map(tuple, param_values))


def run_analysis(outputs, problem):
    # Perform Sobol sensitivity analysis
    sobol_indices = sobol.analyze(problem, outputs)
    print(sobol_indices)
