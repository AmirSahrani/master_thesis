import numpy as np

s = 4
x = np.random.random((s, s))
x = x / x.sum(axis=1)

for i in range(s):
    for j in range(s):
        x[i, j] = x[i].sum() - x[i, j]

print(x)

for f in [0.5, 1, 2]:
    idx = np.diag_indices(x.shape[0])

    x_f = x.copy()
    x_f[idx] *= f
    x_f = x_f
    print()
    print(x_f)
