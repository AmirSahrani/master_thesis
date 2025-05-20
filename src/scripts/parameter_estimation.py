import pyabc
import pandas as pd


def de_groot_model():
    pass


def pbs_model(params):
    mask = pd.Series(True, index=opinion_df.index)
    for k, v in params.items():
        mask &= opinion_df[k] == v
    simulated = opinion_df[mask]["PBS_simulated"]

    # Ensure it's aligned and same length
    return {"data": simulated.reset_index(drop=True)}

# Define distance function


def d(x, x0):
    return (x["data"] - x0["data"]).abs().mean()


# Prior
prior = pyabc.Distribution(
    bias=pyabc.RV("uniform", 0, 10),
    knowledge=pyabc.RV("bernoulli", 0.5),
    credibility=pyabc.RV("bernoulli", 0.5),
    ego=pyabc.RV("bernoulli", 0.5),
    similarity=pyabc.RV("bernoulli", 0.5)
)

# ABC-SMC setup
abc = pyabc.ABCSMC(pbs_model, prior, d, population_size=1000)

# Observed data
observed = {"data": 0}

# Run ABC
abc.new("sqlite:///data/abc.db", observed)
history = abc.run()
