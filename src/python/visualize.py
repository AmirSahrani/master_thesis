import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Generate a color cycle from the 'viridis' colormap

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


def compute_proportion(data, col_start, col_end, new_col):
    # Ensure 'cyclic_start' and 'cyclic_end' are numeric
    data[col_start] = data[col_start].astype(float)
    data[col_end] = data[col_end].astype(float)

    # Group by bias
    aggregated_start = data.groupby(["bias", "metric_space"])[col_start].mean()
    aggregated_end = data.groupby(["bias", "metric_space"])[col_end].mean()

    # Compute proportion (avoid division by zero)
    agg_prop = (aggregated_end / aggregated_start).replace(np.inf, 0)

    # Convert Series to DataFrame and reset index
    return agg_prop.reset_index(name=new_col)


def plot(data, col, ylab):
    spaces = ["KS", "DP", "CS"]
    marker = ["o", "^", "s"]
    colors = ["#A93C93", "#008B72", "#613F99"]
    for marker, space, color in zip(marker, spaces, colors):
        subset = data[data["metric_space"] == space]
        x = subset["bias"]
        y = subset[col]

        # Plot
        plt.plot(
            x,
            y,
            label=space,
            marker=marker,
            color=color,
            linestyle="-",
        )
    plt.xticks(x)
    plt.xlabel("Bias")
    plt.ylabel(ylab)
    plt.legend()
    plt.savefig(f"figures/{ylab}.pdf")
    plt.show()


if __name__ == "__main__":
    data = pd.read_csv("results/data.csv")
    cyclic_proportion = compute_proportion(
        data, "cyclic_start", "cyclic_end", "cyclic_proportion"
    )
    condorcet_proportion = compute_proportion(
        data, "condorcet_start", "condorcet_end", "condorcet_proportion"
    )
    plot(
        cyclic_proportion,
        "cyclic_proportion",
        "Proportion of Cyclic Profiles Remaining",
    )
    plot(
        condorcet_proportion,
        "condorcet_proportion",
        "Proportion of Condorcet Winners Remaining",
    )
