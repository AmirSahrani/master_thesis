import numpy as np


def makeVoter(num_alts: int, bias: float):
    return (np.random.permutation([x for x in range(1, num_alts + 1)]).tolist(), bias)


def generateVoters(num_voters: int, num_alternatives: int, bias: float = 0.5):
    return [makeVoter(num_alternatives, bias) for _ in range(num_voters)]
