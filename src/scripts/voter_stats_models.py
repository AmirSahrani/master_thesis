import sklearn
import scipy
from scipy.optimize import quadratic_assignment
import numpy as np
import time
from numba import njit, prange


def adjancy_to_distance(matrix):
    return np.clip(scipy.sparse.csgraph.shortest_path(matrix), 0, 1000)


@njit()
def objective(order, mat1, mat2):
    total = 0.0
    for i in range(len(order)):
        row_idx = int(order[i])
        for j in range(mat1.shape[1]):  # Assuming 2D matrices
            total += abs(mat1[row_idx, j] - mat2[i, j]) ** 2
    return total


def objective_wrapper(x, mat1, mat2):
    # Convert continuous values to permutation using argsort
    order = np.argsort(x)
    return objective(order, mat1, mat2)


def map_voters_to_nodes_on_graph(voter_opinion_distance_matrix, node_distance_matrix):
    """
    Map a two distance matrices to each other, the first matrix is assumed to be a
    matrix of distance in opinions, as measured by a questionnaire, the second
    matrix is a matrix of shortest path distances on a graph.

    @params:
        voter_opinion_distance_matrix (np.ndarray): an N x N matrix of distances
        node_distance_matrix (np.ndarray): an N x N matrix of distances
    @return:
        ordering (List): A list where index i represents agent i and the value
        represents its associated node
    """
    max_distance = np.max(node_distance_matrix)
    normalized_opinions = voter_opinion_distance_matrix / max_distance
    n = normalized_opinions.shape[0]

    # Initial distance
    initial_guess = np.arange(0, n, step=1, dtype=np.int64)
    initial_val = objective(
        initial_guess, normalized_opinions, node_distance_matrix)
    res = quadratic_assignment(
        A=normalized_opinions,
        B=node_distance_matrix,
        method="faq",
        options={"maximize": False},
    )

    final_val = objective(
        res.col_ind, normalized_opinions, node_distance_matrix)
    if initial_val > final_val:
        return initial_guess
    return res.col_ind.tolist()
