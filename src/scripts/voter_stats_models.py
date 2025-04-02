import sklearn
import scipy

print("reading this file")


def fit_SpectralClustering(data, n_clusters, affinity):
    print(data)
    model = sklearn.cluster.SpectralClustering(n_clusters=n_clusters, affinity=affinity)
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
