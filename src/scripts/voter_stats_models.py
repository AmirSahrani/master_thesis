import sklearn
import scipy
import numpy as np
from numba import njit, prange


def fit_SpectralClustering(data, n_clusters, affinity):
    print(data)
    model = sklearn.cluster.SpectralClustering(
        n_clusters=n_clusters, affinity=affinity)
    return model.fit(data).labels_


def fit_TSNE(data, n_components, seed=None):
    model = sklearn.manifold.TSNE(n_components=n_components, seed=seed)
    return model.fit(data)


def procrustes(data1, data2):
    (out_data1, out_data2, disparity) = scipy.spatial.procrustes(data1, data2)
    return out_data1, out_data2, disparity


def transform(data, model):
    return model.transform(data)


def predict(data, model):
    return model.predict(data)


def adjance_to_distance(matrix):
    return np.clip(scipy.sparse.csgraph.shortest_path(matrix), 0, 1000)


@njit
def get_neighbor(path, selection):
    """Returns a random neighbor of the path"""
    node1 = np.random.randint(0, len(path))
    node2 = np.random.randint(0, len(path))
    while node1 == node2:
        node2 = np.random.randint(0, len(path))

    new_path = path.copy()

    if selection == 0:
        # Inverse
        new_path[min(node1, node2): max(node1, node2)] = new_path[
            min(node1, node2): max(node1, node2)
        ][::-1]

    elif selection == 1:
        # Swap
        new_path[node1], new_path[node2] = new_path[node2], new_path[node1]

    elif selection == 2:
        # Swap routes
        start, end = min(node1, node2), max(node1, node2)
        subroute = path[start:end]
        new_path = np.concatenate((path[:start], path[end:]))
        insertion_point = np.random.randint(0, len(new_path))
        new_path = np.concatenate(
            (new_path[:insertion_point], subroute, new_path[insertion_point:])
        )

    else:
        # Handle invalid selection
        raise ValueError("Invalid selection value")

    assert len(new_path) == len(path), "Operation caused an error"
    assert len(np.unique(new_path)) == len(
        new_path
    ), f"Operation caused an error using slection {selection}\n {new_path}"
    return new_path


@njit()
def objective(order, mat1, mat2):
    total = 0.0
    for i in range(len(order)):
        row_idx = int(order[i])
        for j in range(mat1.shape[1]):  # Assuming 2D matrices
            total += abs(mat1[row_idx, j] - mat2[i, j])
    return total


def objective_wrapper(x, mat1, mat2):
    # Convert continuous values to permutation using argsort
    order = np.argsort(x)
    return objective(order, mat1, mat2)


def next_step(order):
    method = np.random.choice([0, 1, 2])
    return get_neighbor(order, method)


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
    print(
        f"Initial distance: {objective(
            initial_guess, normalized_opinions, node_distance_matrix)}"
    )

    # Use differential evolution with bounds
    # Using values that will be converted to permutation
    bounds = [(0, 1) for _ in range(n)]
    result = scipy.optimize.differential_evolution(
        func=objective_wrapper,
        bounds=bounds,
        args=(normalized_opinions, node_distance_matrix),
        popsize=15,
        updating="deferred",
        workers=-1,
    )

    # Convert final solution to permutation
    final_order = np.argsort(result.x).astype(int)
    print(
        f"Final distance: {objective(final_order,
                                     normalized_opinions, node_distance_matrix)}"
    )

    return final_order.tolist()
