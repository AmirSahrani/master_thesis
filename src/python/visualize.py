import matplotlib.pyplot as plt
import numpy as np
from numpy.random.mtrand import get_bit_generator
import pandas as pd

plt.rcParams.update(
    {
        "font.size": 23,
        "axes.labelsize": 23,
        "axes.titlesize": 23,
        "xtick.labelsize": 23,
        "ytick.labelsize": 23,
        "legend.fontsize": 16,
        "axes.linewidth": 1,
        "grid.linewidth": 1,
        "grid.alpha": 0.3,
        "image.cmap": "viridis",
        "text.usetex": True,
        "font.family": "Computer Modern",
    }
)


def read_data(filename):
    return pd.read_csv(filename)


def compute_cyclic_proportion(data):
    # Ensure 'cyclic_start' and 'cyclic_end' are numeric
    data["cyclic_start"] = data["cyclic_start"].astype(float)
    data["cyclic_end"] = data["cyclic_end"].astype(float)

    # Group by bias
    aggregated_start = data.groupby("bias")["cyclic_start"].mean()
    aggregated_end = data.groupby("bias")["cyclic_end"].mean()

    # Compute proportion (avoid division by zero)
    aggregated_prop = (
        (aggregated_end / aggregated_start).replace(np.inf, np.nan).fillna(0)
    )

    # Convert Series to DataFrame and reset index
    return aggregated_prop.reset_index(name="cyclic_proportion")


if __name__ == "__main__":
    data = pd.read_csv("results/data.csv")
    cyclic_proportion = compute_cyclic_proportion(data)

    # Plot
    plt.plot(
        cyclic_proportion["bias"],
        cyclic_proportion["cyclic_proportion"],
        marker="o",
        linestyle="-",
    )
    plt.xlabel("Bias")
    plt.ylabel("Proportion of Cyclic Profiles Remaining")
    plt.title("Proportion of Cyclic Profiles After Deliberation")
    plt.grid(True)
    plt.show()
