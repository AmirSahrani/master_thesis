import numpy as np

N_VOTERS = 51
N_ALTERNATIVES = 5
BIAS_LOW = 0.5
BIAS_HIGH = 0.95
BIAS_RES = 0.1


def bias_range():
    np.arange(BIAS_LOW, BIAS_HIGH, BIAS_RES)
