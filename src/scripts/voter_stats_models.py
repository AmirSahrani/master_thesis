import sklearn
import math
print("reading this file")


def fit_SpectralClustering(data, n_clusters, affinity, seed=None):
    model = sklearn.cluster.SpectralClustering(
        n_clusters=n_clusters, affinity=affinity, seed=seed
    )
    return model.fit(data)


def fit_TSNE(data, n_components, seed=None):
    model = sklearn.manifold.TSNE(n_components=n_components, seed=seed)
    return model.fit(data)


def procrustes(data1, data2):
    (out_data1, out_data2, disparity) = sklearn.spatial.procrustes(data1, data2)
    return out_data1, out_data2, disparity


def transform(data, model):
    return model.transform(data)


def predict(data, model):
    return model.predict(data)
